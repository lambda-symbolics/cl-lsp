(in-package #:cl-lsp)

(-> edit-tests--range (integer integer) json-object)
(defun edit-tests--range (start end)
  "Construct a one-line range from START to END UTF-16 units."
  (json-object "start" (json-object "line" 0 "character" start)
               "end" (json-object "line" 0 "character" end)))

(-> edit-tests--edit (integer integer string) json-object)
(defun edit-tests--edit (start end text)
  "Construct a one-line text edit."
  (json-object "range" (edit-tests--range start end) "newText" text))

(-> edit-tests--refused-p (function &optional t) boolean)
(defun edit-tests--refused-p (function &optional (type 'lsp-protocol-error))
  "Return true if FUNCTION signals TYPE."
  (handler-case (progn (funcall function) nil)
    (error (condition) (typep condition type))))

(-> test-lsp-workspace-edit-normalization nil null)
(defun test-lsp-workspace-edit-normalization ()
  "Exercise multi-file edits, versions, annotations and ordered resources."
  (let* ((uri         "file:///a")
         (text        (format nil "a~Cb~C~%next" (code-char #x1f600) #\Return))
         (snapshot    (lambda (name) (declare (ignore name)) (values text 7)))
         (annotations (json-object "review" (json-object "label" "Review" "needsConfirmation" t)))
         (edit        (edit-tests--edit 1 3 "x"))
         (document    (json-object "textDocument" (json-object "uri" uri "version" 7)
                                   "edits" (vector edit))))
    (setf (gethash "annotationId" edit) "review")
    (let* ((plan       (lsp-normalize-workspace-edit
                        (json-object "changeAnnotations" annotations
                                     "documentChanges"
                                     (vector (json-object "kind" "create" "uri" "file:///b"
                                                          "options" (json-object "ignoreIfExists" t))
                                             document
                                             (json-object "kind" "rename" "oldUri" uri "newUri" "file:///c")
                                             (json-object "kind" "delete" "uri" "file:///d")))
                        :snapshot snapshot))
           (operations (json-get plan "operations"))
           (normalized (aref (json-get (aref operations 1) "edits") 0)))
      (tests--assert (= (length operations) 4) "all resource and document operations survive")
      (tests--assert (equal (map 'list (lambda (op) (json-get op "kind")) operations)
                            '("create" "text" "rename" "delete")) "documentChanges order survives")
      (tests--assert (and (= (json-get normalized "startOffset") 1)
                          (= (json-get normalized "endOffset") 2)) "UTF-16 becomes character offsets")
      (tests--assert (equal (json-get normalized "annotationId") "review") "annotation references survive")
      (tests--assert (= (json-get (aref operations 1) "version") 7) "document version survives"))
    (let ((changes (json-object "file:///z" (vector (edit-tests--edit 0 1 "z"))
                                "file:///a" (vector (edit-tests--edit 0 1 "a")))))
      (tests--assert
       (equal (map 'list (lambda (op) (json-get op "uri"))
                   (json-get (lsp-normalize-workspace-edit (json-object "changes" changes)
                                                          :snapshot snapshot) "operations"))
              '("file:///a" "file:///z")) "unordered changes have deterministic URI order"))
    (dolist (bad (list (edit-tests--edit 2 3 "x") (edit-tests--edit 4 3 "x")
                      (edit-tests--edit 0 99 "x")
                      (json-object "range" (edit-tests--range -1 0) "newText" "x")
                      (json-object "range" (edit-tests--range 0 0) "newText" 1)))
      (tests--assert
       (edit-tests--refused-p
        (lambda () (lsp-normalize-workspace-edit
                    (json-object "changes" (json-object uri (vector bad))) :snapshot snapshot)))
       "invalid UTF-16 coordinates and edits signal typed protocol failures"))
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit
                  (json-object "changes" (json-object uri (vector (edit-tests--edit 0 3 "x")
                                                                  (edit-tests--edit 1 4 "y"))))
                  :snapshot snapshot))) "overlapping replacements are rejected")
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit
                  (json-object "documentChanges" (vector document))
                  :snapshot (lambda (name) (declare (ignore name)) (values text 6)))))
     "stale document versions are rejected")
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit (json-object "documentChanges" (vector document)))))
     "missing snapshots are refused instead of reading files")
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit
                  (json-object "documentChanges" (vector document)) :snapshot snapshot)))
     "unknown annotations are refused")
    (tests--assert
     (edit-tests--refused-p (lambda () (lsp-normalize-workspace-edit nil :position-encoding "utf-8")))
     "unsupported encoding is refused")
    (tests--assert (zerop (length (json-get (lsp-normalize-workspace-edit nil) "operations")))
                   "null edit yields an empty plan")
    (tests--assert (= (lsp-edit--position-offset text (json-object "line" 1 "character" 0)) 5)
                   "CRLF is a single line ending")
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-edit--position-offset text (json-object "line" 0 "character" 5))))
     "positions cannot lie inside CRLF"))
  nil)

(-> edit-tests--document-change (string vector &optional t) json-object)
(defun edit-tests--document-change (uri edits &optional version)
  "Construct a versioned document change."
  (json-object "textDocument" (json-object "uri" uri "version" version) "edits" edits))

(-> test-lsp-workspace-edit-overlay nil null)
(defun test-lsp-workspace-edit-overlay ()
  "Validate ordered creates, folder moves, repeated edits and deletes in memory."
  (let ((calls       0)
        (source      "file:///old/a.txt")
        (destination "file:///new/a.txt"))
    (let* ((plan
             (lsp-normalize-workspace-edit
              (json-object "documentChanges"
                           (vector
                            (edit-tests--document-change source (vector (edit-tests--edit 0 1 "hello")) 3)
                            (json-object "kind" "rename" "oldUri" "file:///old" "newUri" "file:///new")
                            (edit-tests--document-change destination (vector (edit-tests--edit 4 5 "!")) 3)
                            (json-object "kind" "create" "uri" "file:///empty")
                            (edit-tests--document-change "file:///empty"
                                                         (vector (edit-tests--edit 0 0 "first")
                                                                 (edit-tests--edit 0 0 "second")))
                            (edit-tests--document-change "file:///empty"
                                                         (vector (edit-tests--edit 5 11 "third")))))
              :snapshot (lambda (uri)
                          (incf calls)
                          (when (equal uri source) (values "x" 3)))))
           (operations (json-get plan "operations")))
      (tests--assert (= calls 2) "initial source and absent create target are observed once")
      (tests--assert (= (json-get (aref (json-get (aref operations 2) "edits") 0) "startOffset") 4)
                     "post-move edits use text changed by preceding edits")
      (tests--assert (= (json-get (aref (json-get (aref operations 5) "edits") 0) "endOffset") 11)
                     "same-position inserts retain their array order in the virtual snapshot"))
    (dolist (operations
             (list (vector (json-object "kind" "delete" "uri" "file:///old")
                           (edit-tests--document-change source (vector (edit-tests--edit 0 0 "x"))))
                   (vector (json-object "kind" "rename" "oldUri" "file:///old" "newUri" "file:///new")
                           (edit-tests--document-change source (vector (edit-tests--edit 0 0 "x"))))))
      (tests--assert
       (edit-tests--refused-p
        (lambda () (lsp-normalize-workspace-edit
                    (json-object "documentChanges" operations)
                    :snapshot (lambda (uri) (declare (ignore uri)) (values "x" 3)))))
       "text edits on deleted or moved-away resources fail"))
    (dolist (version (list (json-false) "3" 3.0))
      (tests--assert
       (edit-tests--refused-p
        (lambda () (lsp-normalize-workspace-edit
                    (json-object "documentChanges"
                                 (vector (edit-tests--document-change source #() version)))
                    :snapshot (lambda (uri) (declare (ignore uri)) (values "x" 3)))))
       "noninteger and false versions do not pass as null")))
  (let* ((a "file:///a") (b "file:///b") (c "file:///c") (d "file:///d")
         (gone     "file:///gone")
         (calls    (make-hash-table :test #'equal))
         (snapshot (lambda (uri)
                     (incf (gethash uri calls 0))
                     (cond ((equal uri a)
                            (values "abc" 7))
                           ((equal uri gone)
                            (values "remove" nil))
                           (t
                            (values nil nil)))))
         (edit     (json-object
                    "documentChanges"
                    (vector (edit-tests--document-change a (vector (edit-tests--edit 0 1 "long")) 7)
                            (json-object "kind" "rename" "oldUri" a "newUri" b)
                            (edit-tests--document-change b (vector (edit-tests--edit 6 6 "!")) 7)
                            (json-object "kind" "create" "uri" c)
                            (edit-tests--document-change c (vector (edit-tests--edit 0 0 "first")
                                                                 (edit-tests--edit 0 0 "second")))
                            (json-object "kind" "delete" "uri" gone)
                            (json-object "kind" "rename" "oldUri" b "newUri" d))))
         (plan     (lsp-normalize-workspace-edit edit :snapshot snapshot)))
    (clrhash calls)
    (let ((effects (lsp-materialize-workspace-plan plan :snapshot snapshot)))
      (tests--assert (equal (map 'list (lambda (effect) (json-get effect "uri")) effects)
                            (list a b c gone d)) "final effects retain first-touch order")
      (tests--assert (equal (map 'list (lambda (effect) (json-get effect "text")) effects)
                            '(nil nil "firstsecond" nil "longbc!"))
                     "ordered edits, equal-position inserts, creates, deletes and rename chains materialize")
      (tests--assert (and (equal (json-get (aref effects 4) "originUri") a)
                         (= (json-get (aref effects 4) "version") 7)
                         (null (json-get (aref effects 2) "originUri")))
                     "content origins and versions follow renames while creates have no donor")
      (tests--assert (loop for count being the hash-values of calls always (= count 1))
                     "each initial URI is observed once during materialization"))
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-materialize-workspace-plan
                  plan :snapshot (lambda (uri)
                                   (if (equal uri a) (values "abc" 8) (values nil nil))))))
     "materialization rejects stale versioned snapshots")
    (dolist (operation
             (list (json-object "kind" "create" "uri" a)
                   (json-object "kind" "delete" "uri" b)
                   (json-object "kind" "rename" "oldUri" b "newUri" c)
                   (json-object "kind" "rename" "oldUri" a "newUri" gone)
                   (json-object "kind" "unknown" "uri" a)
                   (json-object "kind" "text" "uri" a "edits"
                                (vector (json-object "startOffset" 2 "endOffset" 1 "newText" "x")))
                   (json-object "kind" "text" "uri" a "edits"
                                (vector (json-object "startOffset" "0" "endOffset" 1 "newText" "x")))
                   (json-object "kind" "text" "uri" a "edits"
                                (vector (json-object "startOffset" 0 "endOffset" 2 "newText" "x")
                                        (json-object "startOffset" 1 "endOffset" 3 "newText" "y")))))
      (tests--assert
       (edit-tests--refused-p
        (lambda () (lsp-materialize-workspace-plan
                    (json-object "positionEncoding" "utf-16" "operations"
                                 (vector (json-object "kind" "create" "uri" c) operation))
                    :snapshot snapshot)))
       "invalid later operations fail the whole pure materialization"))
    (dolist (operation
             (list (json-object "kind" "create" "uri" a "options" (json-object "ignoreIfExists" t))
                   (json-object "kind" "delete" "uri" b "options" (json-object "ignoreIfNotExists" t))
                   (json-object "kind" "rename" "oldUri" a "newUri" gone
                                "options" (json-object "ignoreIfExists" t))))
      (tests--assert
       (zerop (length (lsp-materialize-workspace-plan
                       (json-object "positionEncoding" "utf-16" "operations" (vector operation))
                       :snapshot snapshot)))
       "ignored resource operations produce no effects")))
  nil)

(-> test-lsp-workspace-edit-conditional-resources nil null)
(defun test-lsp-workspace-edit-conditional-resources ()
  "Validate conditional resource operations using folder observations and bounds."
  (let* ((source   "file:///old/a.txt")
         (folder   "file:///new")
         (snapshot (lambda (uri)
                     (cond ((equal uri source)
                            (values "name" 4 :file))
                           ((equal uri folder)
                            (values nil nil :folder))
                           (t
                            (values nil nil :missing)))))
         (move     (json-object "kind" "rename" "oldUri" "file:///old" "newUri" folder
                                "options" (json-object "ignoreIfExists" t)))
         (change   (edit-tests--document-change source (vector (edit-tests--edit 0 4 "new")) 4)))
    (tests--assert (= (length (json-get
                               (lsp-normalize-workspace-edit
                                (json-object "documentChanges" (vector move change)) :snapshot snapshot)
                               "operations")) 2)
                   "ignored folder moves leave source document text and version available")
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit
                  (json-object "documentChanges"
                               (vector (json-object "kind" "create" "uri" folder
                                                    "options" (json-object "ignoreIfExists" t))
                                       (edit-tests--document-change folder #())))
                  :snapshot snapshot))) "ignored creates do not turn existing folders into text documents")
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit
                  (json-object "documentChanges" (vector change))
                  :snapshot (lambda (uri) (declare (ignore uri)) (values "name" 4 :missing)))))
     "inconsistent snapshot kind and content signal a protocol failure")
    (tests--assert (= (length (json-get
                               (lsp-normalize-workspace-edit
                                (json-object "changes" (json-object "file:///unused" #())
                                             "documentChanges" (vector change)) :snapshot snapshot)
                               "operations")) 1)
                   "the ordered versioned representation takes precedence over legacy changes")
    (let ((plan (lsp-normalize-workspace-edit
                 (json-object "documentChanges"
                              (vector change (json-object "kind" "delete" "uri" folder)))
                 :snapshot snapshot)))
      (tests--assert (= (length (json-get plan "operations")) 2)
                     "normalization supports folder resource overlays")
      (tests--assert
       (edit-tests--refused-p (lambda () (lsp-materialize-workspace-plan plan :snapshot snapshot)))
       "regular-file materialization rejects folders before returning any effects"))
    (let ((*lsp-maximum-workspace-operations* 1))
      (tests--assert
       (edit-tests--refused-p
        (lambda () (lsp-normalize-workspace-edit (json-object "documentChanges" (vector move change))
                                                :snapshot snapshot)))
       "oversized operation arrays are rejected before normalization"))
    (let ((*lsp-maximum-workspace-edits* 1))
      (tests--assert
       (edit-tests--refused-p
        (lambda () (lsp-normalize-workspace-edit
                    (json-object "changes"
                                 (json-object source (vector (edit-tests--edit 0 0 "a")
                                                             (edit-tests--edit 0 0 "b"))))
                    :snapshot snapshot))) "oversized text edit arrays are rejected"))
    (tests--assert
     (edit-tests--refused-p
      (lambda () (lsp-normalize-workspace-edit
                  (json-object "documentChanges"
                               (vector (edit-tests--document-change source #() (expt 2 32))))
                  :snapshot snapshot))) "document versions follow protocol signed integer bounds"))
  nil)
