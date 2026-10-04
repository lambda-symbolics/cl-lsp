(in-package #:cl-lsp)

;;;; -- Proposed Workspace Changes --

(-> lsp-edit--invalid (string string) null)
(-> lsp-edit--object (t string) json-object)
(-> lsp-edit--array (t string) vector)
(-> lsp-edit--string (t string) string)
(-> lsp-edit--uri (t) string)
(-> lsp-edit--position-offset (string t) integer)
(-> lsp-edit--range (string t) (values integer integer))
(-> lsp-edit--annotation (json-object json-object json-object) json-object)
(-> lsp-edit--annotations (json-object) json-object)
(-> lsp-edit--text-operation (t t t &key (:snapshot (option function))
                                      (:annotations json-object)) json-object)
(-> lsp-edit--resource-operation (json-object json-object) json-object)
(-> lsp-edit--apply-text (string vector) string)
(-> lsp-edit--uri-below-p (string string) boolean)
(-> lsp-edit--virtual-document (string list function) (values t t))

(define-condition lsp-protocol-error (lsp-error)
  ((field :initarg :field :reader lsp-protocol-error-field
          :documentation "Malformed protocol field or unavailable snapshot."))
  (:documentation "A server result cannot be represented as a valid proposed change."))

(define-condition lsp-unsupported (lsp-error)
  ((operation :initarg :operation :reader lsp-unsupported-operation
              :documentation "The operation not advertised by the server."))
  (:documentation "The server did not advertise a requested semantic operation."))

(defun lsp-edit--invalid (field message)
  "Signal a typed protocol failure for FIELD with MESSAGE."
  (error 'lsp-protocol-error :field field :message message))

(defun lsp-edit--object (value field)
  "Require a JSON object VALUE for FIELD."
  (unless (json-object-p value)
    (lsp-edit--invalid field "Expected a protocol object."))
  value)

(defun lsp-edit--array (value field)
  "Require a JSON array VALUE for FIELD."
  (unless (and (vectorp value) (not (stringp value)))
    (lsp-edit--invalid field "Expected a protocol array."))
  value)

(defun lsp-edit--string (value field)
  "Require a nonempty string VALUE for FIELD."
  (unless (and (stringp value) (plusp (length value)))
    (lsp-edit--invalid field "Expected a nonempty protocol string."))
  value)

(defun lsp-edit--uri (value)
  "Require an absolute URI without interpreting it as a host pathname."
  (lsp-edit--string value "uri")
  (let ((colon (position #\: value)))
    (unless (and colon (plusp colon) (alpha-char-p (char value 0))
                 (every (lambda (character)
                          (or (alphanumericp character) (find character "+-.")))
                        (subseq value 0 colon))
                 (notany (lambda (character) (or (<= (char-code character) 32)
                                                 (char= character #\Backslash))) value))
      (lsp-edit--invalid "uri" "Expected an absolute document URI.")))
  value)

(defun lsp-edit--position-offset (text position)
  "Validate a UTF-16 POSITION and return its character offset in TEXT."
  (lsp-edit--object position "position")
  (let ((line (json-get position "line"))
        (column (json-get position "character"))
        (current-line 0)
        (current-column 0)
        (index 0))
    (unless (and (typep line '(integer 0)) (typep column '(integer 0)))
      (lsp-edit--invalid "position" "Positions need nonnegative integer coordinates."))
    (loop
      (when (and (= current-line line) (= current-column column))
        (return-from lsp-edit--position-offset index))
      (when (>= index (length text)) (return))
      (let ((character (char text index)))
        (cond
          ((or (char= character #\Return) (char= character #\Newline))
           (when (= current-line line) (return))
           (when (and (char= character #\Return) (< (1+ index) (length text))
                      (char= (char text (1+ index)) #\Newline))
             (incf index))
           (incf current-line)
           (setf current-column 0))
          (t
           (incf current-column (if (> (char-code character) #xffff) 2 1)))))
      (incf index))
    (lsp-edit--invalid "position" "Position lies outside the snapshot or splits a surrogate pair.")))

(defun lsp-edit--range (text range)
  "Return start and end offsets for RANGE in TEXT, rejecting reversed ranges."
  (lsp-edit--object range "range")
  (let ((start (lsp-edit--position-offset text (json-get range "start")))
        (end (lsp-edit--position-offset text (json-get range "end"))))
    (when (> start end) (lsp-edit--invalid "range" "Edit range is reversed."))
    (values start end)))

(defun lsp-edit--annotation (object annotations result)
  "Copy and validate OBJECT's optional annotation reference into RESULT."
  (multiple-value-bind (identifier present-p) (json-get-present object "annotationId")
    (when present-p
      (lsp-edit--string identifier "annotationId")
      (unless (nth-value 1 (gethash identifier annotations))
        (lsp-edit--invalid "annotationId" "Edit refers to an unknown change annotation."))
      (setf (gethash "annotationId" result) identifier)))
  result)

(defun lsp-edit--annotations (edit)
  "Validate change annotations, preserving their approval metadata."
  (let ((annotations (json-get edit "changeAnnotations" (json-object))))
    (lsp-edit--object annotations "changeAnnotations")
    (maphash
     (lambda (identifier annotation)
       (lsp-edit--string identifier "annotationId")
       (lsp-edit--object annotation "changeAnnotation")
       (unless (stringp (json-get annotation "label"))
         (lsp-edit--invalid "label" "An annotation requires a string label."))
       (multiple-value-bind (description present-p) (json-get-present annotation "description")
         (when (and present-p (not (stringp description)))
           (lsp-edit--invalid "description" "Annotation description must be a string.")))
       (multiple-value-bind (confirmation present-p)
            (gethash "needsConfirmation" annotation)
         (when (and present-p (not (member confirmation (list t (json-false)))))
           (lsp-edit--invalid "needsConfirmation" "Confirmation must be a JSON boolean."))))
     annotations)
    annotations))

(defun lsp-edit--text-operation (uri version edits &key snapshot annotations)
  "Normalize simultaneous edits against a caller-provided immutable snapshot."
  (lsp-edit--uri uri)
  (unless (or (null version) (integerp version))
    (lsp-edit--invalid "version" "Document version must be an integer or null."))
  (lsp-edit--array edits "edits")
  (unless snapshot
    (lsp-edit--invalid "snapshot" "Text edits require a caller-provided document snapshot."))
  (multiple-value-bind (text current-version) (funcall snapshot uri)
    (unless (stringp text)
      (lsp-edit--invalid "snapshot" "No text snapshot is available for an affected URI."))
    (when (and version (not (eql version current-version)))
      (lsp-edit--invalid "version" "Workspace edit document version is stale or unknown."))
    (let* ((intervals nil)
           (normalized
            (map 'vector
                 (lambda (edit)
                   (lsp-edit--object edit "textEdit")
                   (unless (stringp (json-get edit "newText"))
                     (lsp-edit--invalid "newText" "Replacement text must be a string."))
                   (let ((range (json-get edit "range")))
                     (multiple-value-bind (start end) (lsp-edit--range text range)
                       (push (cons start end) intervals)
                       (lsp-edit--annotation
                        edit annotations
                        (json-object "range" range "startOffset" start "endOffset" end
                                     "newText" (json-get edit "newText"))))))
                 edits)))
      ;; Equal-position insertions are ordered by their original array order.
      (let ((previous-end -1))
        (dolist (interval (stable-sort intervals
                                     (lambda (left right)
                                       (or (< (first left) (first right))
                                           (and (= (first left) (first right))
                                                (< (rest left) (rest right)))))))
          (when (< (first interval) previous-end)
            (lsp-edit--invalid "edits" "Text edit ranges overlap."))
          (setf previous-end (max previous-end (rest interval)))))
      (json-object "kind" "text" "uri" uri "version" version
                   "snapshotVersion" current-version "edits" normalized))))

(defun lsp-edit--resource-operation (operation annotations)
  "Normalize one create, rename or delete operation with its standard options."
  (let* ((kind (json-get operation "kind"))
         (result (json-object "kind" kind))
         (option-names (cond
                         ((member kind '("create" "rename") :test #'equal)
                          '("overwrite" "ignoreIfExists"))
                         ((equal kind "delete") '("recursive" "ignoreIfNotExists"))
                         (t (lsp-edit--invalid "kind" "Unknown resource operation.")))))
    (dolist (field (if (equal kind "rename") '("oldUri" "newUri") '("uri")))
      (setf (gethash field result) (lsp-edit--uri (json-get operation field))))
    (multiple-value-bind (options present-p) (json-get-present operation "options")
      (when present-p
        (lsp-edit--object options "options")
        (maphash (lambda (name value)
                   (unless (and (member name option-names :test #'equal)
                                (member value (list t (json-false))))
                     (lsp-edit--invalid "options" "Invalid resource operation option.")))
                 options)
        (setf (gethash "options" result) options)))
    (lsp-edit--annotation operation annotations result)))

(-> lsp-normalize-workspace-edit (t &key (:snapshot (option function))
                                      (:position-encoding string)) json-object)
(defun lsp-normalize-workspace-edit (edit &key snapshot (position-encoding "utf-16"))
  "Return an ordered JSON proposal without reading or writing host files.

SNAPSHOT receives a URI and returns its initial text and document version as two
values, or NIL/NIL for an absent document. Every existing text document requires
a snapshot; versioned edits require an exact version. Offsets count Common Lisp
characters, while ranges count UTF-16 units. Ordered changes are validated against
an in-memory overlay, including creates, moves, deletes and preceding text edits.
Initial snapshots are cached. Resource options and annotations are preserved;
the caller owns resource preconditions, authority and atomic application. Null
EDIT yields an empty proposal. Only UTF-16 is negotiated."
  (unless (equal position-encoding "utf-16")
    (lsp-edit--invalid "positionEncoding" "Only negotiated UTF-16 positions are supported."))
  (when (null edit) (setf edit (json-object)))
  (lsp-edit--object edit "workspaceEdit")
  (let ((annotations (lsp-edit--annotations edit))
        (operations nil)
        (history nil)
        (initial (make-hash-table :test #'equal)))
    (labels ((initial-document (uri)
               (multiple-value-bind (entry present-p) (gethash uri initial)
                 (unless present-p
                   (setf entry (multiple-value-list
                                (if snapshot (funcall snapshot uri) (values nil nil)))
                         (gethash uri initial) entry))
                 (values (first entry) (second entry))))

             (current-document (uri)
               (lsp-edit--virtual-document uri history #'initial-document))

             (text-operation (uri version edits)
               (let ((operation (lsp-edit--text-operation
                                 uri version edits :snapshot #'current-document
                                 :annotations annotations)))
                 (multiple-value-bind (text current-version) (current-document uri)
                   (push (json-object "kind" "snapshot" "uri" uri "version" current-version
                                      "text" (lsp-edit--apply-text text (json-get operation "edits")))
                         history))
                 (push operation operations))))
      (multiple-value-bind (changes changes-p) (json-get-present edit "changes")
        (multiple-value-bind (documents documents-p) (json-get-present edit "documentChanges")
          (when (and changes-p documents-p)
            (lsp-edit--invalid "workspaceEdit" "WorkspaceEdit contains both edit representations."))
          (when changes-p
            (lsp-edit--object changes "changes")
            (dolist (uri (sort (loop for uri being the hash-keys of changes collect uri) #'string<))
              (text-operation uri nil (gethash uri changes))))
          (when documents-p
            (lsp-edit--array documents "documentChanges")
            (loop for operation across documents
                  do (lsp-edit--object operation "documentChange")
                     (if (nth-value 1 (gethash "kind" operation))
                         (let ((resource (lsp-edit--resource-operation operation annotations)))
                           (push resource operations)
                           (push resource history))
                         (let ((document (lsp-edit--object (json-get operation "textDocument")
                                                          "textDocument")))
                           (unless (nth-value 1 (gethash "version" document))
                             (lsp-edit--invalid "version" "TextDocumentEdit requires a version field."))
                           (text-operation (json-get document "uri")
                                           (gethash "version" document)
                                           (json-get operation "edits"))))))))
      (json-object "operations" (coerce (nreverse operations) 'vector)
                   "annotations" annotations "positionEncoding" position-encoding))))

(defun lsp-edit--apply-text (text edits)
  "Apply validated simultaneous EDITS to an in-memory TEXT value only."
  (let ((cursor 0)
        (ordered (stable-sort (coerce edits 'list)
                              (lambda (left right)
                                (let ((start (json-get left "startOffset"))
                                      (other (json-get right "startOffset")))
                                  (or (< start other)
                                      (and (= start other)
                                           (< (json-get left "endOffset")
                                              (json-get right "endOffset")))))))))
    (with-output-to-string (output)
      (dolist (edit ordered)
        (write-string text output :start cursor :end (json-get edit "startOffset"))
        (write-string (json-get edit "newText") output)
        (setf cursor (json-get edit "endOffset")))
      (write-string text output :start cursor))))

(defun lsp-edit--uri-below-p (uri root)
  "Return true if URI names ROOT or a descendant URI."
  (or (equal uri root)
      (uiop:string-prefix-p (if (uiop:string-suffix-p root "/") root
                                (concatenate 'string root "/")) uri)))

(defun lsp-edit--virtual-document (uri history snapshot)
  "Resolve URI's in-memory document after HISTORY, newest operation first."
  (if (null history)
      (if snapshot (funcall snapshot uri) (values nil nil))
      (let* ((operation (first history))
             (older (rest history))
             (kind (json-get operation "kind"))
             (options (json-get operation "options"))
             (overwrite (and options (eq (json-get options "overwrite") t)))
             (ignore (and options (eq (json-get options "ignoreIfExists") t))))
        (cond
          ((and (equal kind "snapshot") (equal uri (json-get operation "uri")))
           (values (json-get operation "text") (json-get operation "version")))
          ((and (equal kind "create") (equal uri (json-get operation "uri")))
           (multiple-value-bind (text version) (lsp-edit--virtual-document uri older snapshot)
             (if (and ignore (not overwrite) text)
                 (values text version)
                 (values "" nil))))
          ((and (equal kind "delete") (lsp-edit--uri-below-p uri (json-get operation "uri")))
           (values nil nil))
          ((equal kind "rename")
           (let ((old (json-get operation "oldUri"))
                 (new (json-get operation "newUri")))
             (multiple-value-bind (target target-version)
                 (if (and ignore (not overwrite))
                     (lsp-edit--virtual-document new older snapshot)
                     (values nil nil))
               (cond
                 ((and ignore (not overwrite) target)
                  (if (equal uri new)
                      (values target target-version)
                      (lsp-edit--virtual-document uri older snapshot)))
                 ((lsp-edit--uri-below-p uri new)
                  (lsp-edit--virtual-document
                   (concatenate 'string old (subseq uri (length new))) older snapshot))
                 ((lsp-edit--uri-below-p uri old)
                  (values nil nil))
                 (t
                  (lsp-edit--virtual-document uri older snapshot))))))
          (t
           (lsp-edit--virtual-document uri older snapshot))))))
