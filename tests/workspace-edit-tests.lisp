(in-package #:cl-lsp)

(defun edit-tests--range (start end)
  "Construct a one-line range from START to END UTF-16 units."
  (json-object "start" (json-object "line" 0 "character" start)
               "end" (json-object "line" 0 "character" end)))

(defun edit-tests--edit (start end text)
  "Construct a one-line text edit."
  (json-object "range" (edit-tests--range start end) "newText" text))

(defun edit-tests--refused-p (function &optional (type 'lsp-protocol-error))
  "Return true if FUNCTION signals TYPE."
  (handler-case (progn (funcall function) nil)
    (error (condition) (typep condition type))))

(defun test-lsp-workspace-edit-normalization ()
  "Exercise multi-file edits, versions, annotations and ordered resources."
  (let* ((uri "file:///a")
         (text (format nil "a~Cb~C~%next" (code-char #x1f600) #\Return))
         (snapshot (lambda (name) (declare (ignore name)) (values text 7)))
         (annotations (json-object "review" (json-object "label" "Review" "needsConfirmation" t)))
         (edit (edit-tests--edit 1 3 "x"))
         (document (json-object "textDocument" (json-object "uri" uri "version" 7)
                                "edits" (vector edit))))
    (setf (gethash "annotationId" edit) "review")
    (let* ((plan (lsp-normalize-workspace-edit
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

(defun edit-tests--document-change (uri edits &optional version)
  "Construct a versioned document change."
  (json-object "textDocument" (json-object "uri" uri "version" version) "edits" edits))

(defun test-lsp-workspace-edit-overlay ()
  "Validate ordered creates, folder moves, repeated edits and deletes in memory."
  (let ((calls 0)
        (source "file:///old/a.txt")
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
  nil)
