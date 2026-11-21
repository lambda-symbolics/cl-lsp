(in-package #:cl-lsp)

;;;; -- Proposed Workspace Changes --

(defparameter *lsp-maximum-workspace-operations* 4096
  "Maximum operations in a normalized workspace proposal.")

(defparameter *lsp-maximum-workspace-edits* 16384
  "Maximum simultaneous text edits in one document operation.")

(define-condition lsp-protocol-error (lsp-error)
  ((field :initarg :field :reader lsp-protocol-error-field
          :documentation "Malformed protocol field or unavailable snapshot."))
  (:documentation "A server result cannot be represented as a valid proposed change."))

(define-condition lsp-unsupported (lsp-error)
  ((operation :initarg :operation :reader lsp-unsupported-operation
              :documentation "The operation not advertised by the server."))
  (:documentation "The server did not advertise a requested semantic operation."))

(-> lsp-edit--invalid (string string) nil)
(defun lsp-edit--invalid (field message)
  "Signal a typed protocol failure for FIELD with MESSAGE."
  (error 'lsp-protocol-error :field field :message message))

(-> lsp-edit--object (t string) json-object)
(defun lsp-edit--object (value field)
  "Require a JSON object VALUE for FIELD."
  (unless (json-object-p value)
    (lsp-edit--invalid field "Expected a protocol object."))
  value)

(-> lsp-edit--array (t string) vector)
(defun lsp-edit--array (value field)
  "Require a JSON array VALUE for FIELD."
  (unless (and (vectorp value) (not (stringp value)))
    (lsp-edit--invalid field "Expected a protocol array."))
  value)

(-> lsp-edit--string (t string) string)
(defun lsp-edit--string (value field)
  "Require a nonempty string VALUE for FIELD."
  (unless (and (stringp value) (plusp (length value)))
    (lsp-edit--invalid field "Expected a nonempty protocol string."))
  value)

(-> lsp-edit--uri (t) string)
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

(-> lsp-edit--position-offset (string t) integer)
(defun lsp-edit--position-offset (text position)
  "Validate a UTF-16 POSITION and return its character offset in TEXT."
  (lsp-edit--object position "position")
  (let ((line           (json-get position "line"))
        (column         (json-get position "character"))
        (current-line   0)
        (current-column 0)
        (index          0))
    (unless (and (typep line '(integer 0 2147483647))
                 (typep column '(integer 0 2147483647)))
      (lsp-edit--invalid "position" "Positions need protocol unsigned-integer coordinates."))
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

(-> lsp-edit--range (string t) (values integer integer))
(defun lsp-edit--range (text range)
  "Return start and end offsets for RANGE in TEXT, rejecting reversed ranges."
  (lsp-edit--object range "range")
  (let ((start (lsp-edit--position-offset text (json-get range "start")))
        (end   (lsp-edit--position-offset text (json-get range "end"))))
    (when (> start end) (lsp-edit--invalid "range" "Edit range is reversed."))
    (values start end)))

(-> lsp-edit--annotation (json-object json-object json-object) json-object)
(defun lsp-edit--annotation (object annotations result)
  "Copy and validate OBJECT's optional annotation reference into RESULT."
  (multiple-value-bind (identifier present-p) (json-get-present object "annotationId")
    (when present-p
      (lsp-edit--string identifier "annotationId")
      (unless (nth-value 1 (gethash identifier annotations))
        (lsp-edit--invalid "annotationId" "Edit refers to an unknown change annotation."))
      (setf (gethash "annotationId" result) identifier)))
  result)

(-> lsp-edit--annotations (json-object) json-object)
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

(-> lsp-edit--text-operation (t t t &key (:snapshot (option function))
                                      (:annotations json-object)) json-object)
(defun lsp-edit--text-operation (uri version edits &key snapshot annotations)
  "Normalize simultaneous edits against a caller-provided immutable snapshot."
  (lsp-edit--uri uri)
  (unless (or (null version) (typep version '(signed-byte 32)))
    (lsp-edit--invalid "version" "Document version must be a protocol integer or null."))
  (lsp-edit--array edits "edits")
  (when (> (length edits) *lsp-maximum-workspace-edits*)
    (lsp-edit--invalid "edits" "Document text edit limit exceeded."))
  (unless snapshot
    (lsp-edit--invalid "snapshot" "Text edits require a caller-provided document snapshot."))
  (multiple-value-bind (text current-version) (funcall snapshot uri)
    (unless (stringp text)
      (lsp-edit--invalid "snapshot" "No text snapshot is available for an affected URI."))
    (unless (or (null current-version) (typep current-version '(signed-byte 32)))
      (lsp-edit--invalid "snapshotVersion" "Snapshot version must be a protocol integer or null."))
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

(-> lsp-edit--resource-operation (json-object json-object) json-object)
(defun lsp-edit--resource-operation (operation annotations)
  "Normalize one create, rename or delete operation with its standard options."
  (let* ((kind         (json-get operation "kind"))
         (result       (json-object "kind" kind))
         (option-names (cond
                         ((member kind '("create" "rename") :test #'equal)
                          '("overwrite" "ignoreIfExists"))
                         ((equal kind "delete")
                          '("recursive" "ignoreIfNotExists"))
                         (t
                          (lsp-edit--invalid "kind" "Unknown resource operation.")))))
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
values, or NIL/NIL for an absent document. An optional third value identifies
:FILE, :FOLDER or :MISSING; folders need this value for conditional resource edits.
Every existing text document requires a snapshot; versioned edits require an
exact version. Offsets count Common Lisp
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
        (operations  nil))
    (multiple-value-bind (current-document advance effects)
        (lsp-edit--materializer snapshot annotations :validate-resources nil)
      (declare (ignore effects))
      (labels ((text-operation (uri version edits)
                 (let ((operation (lsp-edit--text-operation
                                   uri version edits :snapshot current-document
                                   :annotations                annotations)))
                   (funcall advance operation)
                   (push operation operations))))
       (multiple-value-bind (changes changes-p) (json-get-present edit "changes")
         (multiple-value-bind (documents documents-p) (json-get-present edit "documentChanges")
           ;; Prefer the versioned, ordered representation when both are supplied.
           (when (and changes-p (not documents-p))
             (lsp-edit--object changes "changes")
             (when (> (hash-table-count changes) *lsp-maximum-workspace-operations*)
               (lsp-edit--invalid "changes" "Workspace operation limit exceeded."))
             (dolist (uri (sort (loop for uri being the hash-keys of changes collect uri) #'string<))
               (text-operation uri nil (gethash uri changes))))
           (when documents-p
             (lsp-edit--array documents "documentChanges")
             (when (> (length documents) *lsp-maximum-workspace-operations*)
               (lsp-edit--invalid "documentChanges" "Workspace operation limit exceeded."))
             (loop for operation across documents
                   do (lsp-edit--object operation "documentChange")
                      (if (nth-value 1 (gethash "kind" operation))
                          (let ((resource (lsp-edit--resource-operation operation annotations)))
                             (funcall advance resource)
                             (push resource operations))
                          (let ((document (lsp-edit--object (json-get operation "textDocument")
                                                           "textDocument")))
                            (unless (nth-value 1 (gethash "version" document))
                              (lsp-edit--invalid "version" "TextDocumentEdit requires a version field."))
                            (text-operation (json-get document "uri")
                                            (gethash "version" document)
                                            (json-get operation "edits"))))))))
       (json-object "operations" (coerce (nreverse operations) 'vector)
                    "annotations" annotations "positionEncoding" position-encoding)))))

(-> lsp-edit--apply-text (string vector) string)
(defun lsp-edit--apply-text (text edits)
  "Apply validated simultaneous EDITS to an in-memory TEXT value only."
  (let ((cursor  0)
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
          (let ((start (json-get edit "startOffset"))
                (end   (json-get edit "endOffset")))
            (unless (and (integerp start) (integerp end)
                         (<= cursor start end (length text))
                         (stringp (json-get edit "newText")))
              (lsp-edit--invalid "edits" "Invalid or overlapping normalized text offsets."))
            (write-string text output :start cursor :end start)
            (write-string (json-get edit "newText") output)
            (setf cursor end)))
      (write-string text output :start cursor))))

(-> lsp-edit--uri-below-p (string string) boolean)
(defun lsp-edit--uri-below-p (uri root)
  "Return true if URI names ROOT or a descendant URI."
  (or (equal uri root)
      (uiop:string-prefix-p (if (uiop:string-suffix-p root "/") root
                                (concatenate 'string root "/")) uri)))

(-> lsp-edit--virtual-document (string list function) (values t t t t))
(defun lsp-edit--virtual-document (uri history snapshot)
  "Return text, version, kind and origin URI after HISTORY, newest operation first."
  (if (null history)
      (multiple-value-bind (text version kind) (funcall snapshot uri)
        (let ((kind (or kind (if text ':file ':missing))))
          (unless (and (member kind '(:file :folder :missing))
                       (if (eq kind ':file) (stringp text) (null text)))
            (lsp-edit--invalid "snapshot" "Snapshot content and resource kind disagree."))
          (values text version kind (and (eq kind ':file) uri))))
      (let* ((operation (first history))
             (older     (rest history))
             (kind      (json-get operation "kind"))
             (options   (json-get operation "options"))
             (overwrite (and options (eq (json-get options "overwrite") t)))
             (ignore    (and options (eq (json-get options "ignoreIfExists") t))))
        (cond
          ((and (equal kind "snapshot") (equal uri (json-get operation "uri")))
           (values (json-get operation "text") (json-get operation "version")
                   ':file (json-get operation "originUri")))
          ((and (equal kind "create") (equal uri (json-get operation "uri")))
           (multiple-value-bind (text version resource-kind origin)
               (lsp-edit--virtual-document uri older snapshot)
             (if (and ignore (not overwrite) (not (eq resource-kind ':missing)))
                 (values text version resource-kind origin)
                 (values "" nil ':file nil))))
          ((and (equal kind "delete") (lsp-edit--uri-below-p uri (json-get operation "uri")))
           (values nil nil ':missing))
          ((equal kind "rename")
           (let ((old (json-get operation "oldUri"))
                 (new (json-get operation "newUri")))
             (multiple-value-bind (target target-version target-kind target-origin)
                 (if (and ignore (not overwrite))
                     (lsp-edit--virtual-document new older snapshot)
                     (values nil nil ':missing))
               (cond
                 ((and ignore (not overwrite) (not (eq target-kind ':missing)))
                  (if (equal uri new)
                      (values target target-version target-kind target-origin)
                      (lsp-edit--virtual-document uri older snapshot)))
                 ((lsp-edit--uri-below-p uri new)
                  (lsp-edit--virtual-document
                   (concatenate 'string old (subseq uri (length new))) older snapshot))
                 ((lsp-edit--uri-below-p uri old)
                  (values nil nil ':missing))
                 (t
                  (lsp-edit--virtual-document uri older snapshot))))))
          (t
           (lsp-edit--virtual-document uri older snapshot))))))

(-> lsp-edit--materializer ((option function) json-object &key (:validate-resources boolean))
                          (values function function function))
(defun lsp-edit--materializer (snapshot annotations &key (validate-resources t))
  "Return current-document, advance and final-effects closures for one pure pass.
Normalization defers resource preconditions to the host; final materialization
validates them against the supplied snapshots before returning any effects."
  (let ((initial (make-hash-table :test #'equal))
        (history nil)
        (order   nil))
    (labels ((initial-document (uri)
               (multiple-value-bind (entry present-p) (gethash uri initial)
                 (unless present-p
                   (setf entry (multiple-value-list
                                (if snapshot (funcall snapshot uri) (values nil nil)))
                         (gethash uri initial) entry))
                 (when (and validate-resources (eq (third entry) ':folder))
                   (lsp-edit--invalid "snapshot" "Final file effects do not support folder snapshots."))
                 (unless (or (null (second entry)) (typep (second entry) '(signed-byte 32)))
                   (lsp-edit--invalid "snapshotVersion" "Invalid snapshot document version."))
                 (values (first entry) (second entry) (third entry))))

             (current-document (uri)
               (lsp-edit--virtual-document uri history #'initial-document))

             (touch (uri)
               (pushnew uri order :test #'equal))

             (advance (operation)
               (lsp-edit--object operation "operation")
               (let ((kind (json-get operation "kind"))
                     (uri  (json-get operation "uri")))
                 (when validate-resources
                   (dolist (field (if (equal kind "rename") '("oldUri" "newUri") '("uri")))
                     (initial-document (lsp-edit--uri (json-get operation field)))))
                 (cond
                   ((equal kind "text")
                    (lsp-edit--uri uri)
                    (multiple-value-bind (text version resource-kind origin)
                        (current-document uri)
                      (unless (eq resource-kind ':file)
                        (lsp-edit--invalid "snapshot" "Cannot edit an absent document or folder."))
                      (let ((expected (json-get operation "version"))
                            (edits    (lsp-edit--array (json-get operation "edits") "edits")))
                        (unless (or (null expected) (typep expected '(signed-byte 32)))
                          (lsp-edit--invalid "version" "Invalid document version."))
                        (when (and expected (not (eql expected version)))
                          (lsp-edit--invalid "version" "Workspace edit document version is stale or unknown."))
                        (when (> (length edits) *lsp-maximum-workspace-edits*)
                          (lsp-edit--invalid "edits" "Document text edit limit exceeded."))
                        (loop for edit across edits
                              do (lsp-edit--object edit "textEdit")
                                 (unless (and (typep (json-get edit "startOffset") '(integer 0))
                                              (typep (json-get edit "endOffset") '(integer 0)))
                                   (lsp-edit--invalid "edits" "Text offsets must be nonnegative integers.")))
                        (push (json-object "kind" "snapshot" "uri" uri "version" version
                                           "originUri" origin
                                           "text" (lsp-edit--apply-text text edits)) history)))
                    (touch uri))
                   (t
                    ;; Validate resource syntax/options in the same boundary as normalization.
                    (setf operation (lsp-edit--resource-operation operation annotations))
                    (unless validate-resources
                      (push operation history)
                      (return-from advance nil))
                    (let* ((options   (json-get operation "options"))
                           (overwrite (and options (eq (json-get options "overwrite") t)))
                           (ignore    (and options (eq (json-get options "ignoreIfExists") t))))
                      (cond
                        ((equal kind "create")
                         (let ((existing (nth-value 2 (current-document uri))))
                           (cond
                             ((and ignore (not overwrite) (not (eq existing ':missing)))
                              (return-from advance nil))
                             ((and (not overwrite) (not (eq existing ':missing)))
                              (lsp-edit--invalid "create" "Create target already exists."))
                             ((eq existing ':folder)
                              (lsp-edit--invalid "create" "Cannot overwrite a folder with a text file."))))
                         (touch uri))
                        ((equal kind "delete")
                         (when (eq (nth-value 2 (current-document uri)) ':missing)
                           (if (and options (eq (json-get options "ignoreIfNotExists") t))
                               (return-from advance nil)
                               (lsp-edit--invalid "delete" "Delete target does not exist.")))
                         (touch uri))
                        ((equal kind "rename")
                         (let* ((old         (json-get operation "oldUri"))
                                (new         (json-get operation "newUri"))
                                (source-kind (nth-value 2 (current-document old)))
                                (target-kind (nth-value 2 (current-document new))))
                           (when (eq source-kind ':missing)
                             (lsp-edit--invalid "rename" "Rename source does not exist."))
                           (when (equal old new) (return-from advance nil))
                           (when (and (not overwrite) (not (eq target-kind ':missing)))
                             (if ignore
                                 (return-from advance nil)
                                 (lsp-edit--invalid "rename" "Rename target already exists.")))
                           (when (and (eq source-kind ':folder) (lsp-edit--uri-below-p new old))
                             (lsp-edit--invalid "rename" "Cannot move a folder inside itself."))
                           (touch old)
                           (touch new))))
                     (push operation history))))))

             (effects ()
               (map 'vector
                    (lambda (uri)
                      (multiple-value-bind (text version kind origin) (current-document uri)
                        (json-object "uri" uri "text" text "version" version
                                     "kind" (string-downcase (symbol-name kind))
                                     "originUri" origin)))
                    (reverse order))))
      (values #'current-document #'advance #'effects))))

(-> lsp-materialize-workspace-plan (json-object &key (:snapshot (option function))) vector)
(defun lsp-materialize-workspace-plan (plan &key snapshot)
  "Return final regular-file effects for a normalized PLAN without host publication.

SNAPSHOT has the normalization text/version contract and is called exactly once
for every addressed initial URI, including conditional operations with no effect.
Folder snapshots signal LSP-PROTOCOL-ERROR. Effects occur in first-touch order and contain uri,
text (NIL for missing), version, kind and originUri. Origin identifies the initial
file supplying the final content through rename chains and edits; a newly
created file has no origin. The host owns authorization, fresh revision checks,
permissions and atomic publication. Invalid operations signal LSP-PROTOCOL-ERROR
before any effects are returned."
  (lsp-edit--object plan "plan")
  (unless (equal (json-get plan "positionEncoding") "utf-16")
    (lsp-edit--invalid "positionEncoding" "Only negotiated UTF-16 positions are supported."))
  (let ((operations (lsp-edit--array (json-get plan "operations") "operations")))
    (when (> (length operations) *lsp-maximum-workspace-operations*)
      (lsp-edit--invalid "operations" "Workspace operation limit exceeded."))
    (multiple-value-bind (current advance effects)
        (lsp-edit--materializer snapshot (lsp-edit--annotations
                                          (json-object "changeAnnotations"
                                                       (or (json-get plan "annotations") (json-object)))))
      (declare (ignore current))
      (loop for operation across operations do (funcall advance operation))
      (funcall effects))))

(-> lsp-edit--copy-json (t) t)
(defun lsp-edit--copy-json (value)
  "Copy JSON containers and strings without retaining mutable input aliases."
  (cond
    ((stringp value)
     (copy-seq value))
    ((hash-table-p value)
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key item)
                  (setf (gethash (copy-seq key) copy) (lsp-edit--copy-json item))) value)
       copy))
    ((vectorp value)
     (map 'vector #'lsp-edit--copy-json value))
    (t
     value)))

(-> lsp-workspace-plan-append (json-object vector &key (:snapshot function)) json-object)
(defun lsp-workspace-plan-append (plan operations &key snapshot)
  "Return a detached PLAN followed by normalized OPERATIONS, validated in order.
Resource operations use the standard LSP shape. Validate the combined overlay
against SNAPSHOT, preserving annotations and origin tracking without publishing
files. A willRenameFiles response alone never implies a physical rename."
  (lsp-edit--object plan "plan")
  (lsp-edit--array operations "operations")
  (let ((prior (lsp-edit--array (json-get plan "operations") "operations")))
    (when (> (+ (length prior) (length operations)) *lsp-maximum-workspace-operations*)
      (lsp-edit--invalid "operations" "Workspace operation limit exceeded."))
    (let ((result (lsp-edit--copy-json plan)))
      (setf (gethash "operations" result)
            (concatenate 'vector (json-get result "operations") (lsp-edit--copy-json operations)))
      (lsp-materialize-workspace-plan result :snapshot snapshot)
      result)))
