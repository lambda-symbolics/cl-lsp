(in-package #:cl-lsp)

(defun semantic-tests--capabilities ()
  "Return static semantic capabilities for a fixture client."
  (let* ((filter (json-object "scheme" "file" "pattern"
                             (json-object "glob" "**/*.txt" "matches" "file")))
         (will-rename (json-object "filters" (vector filter))))
    (json-object "positionEncoding" "utf-16"
                 "renameProvider" (json-object "prepareProvider" t)
                 "codeActionProvider" (json-object "resolveProvider" t)
                 "workspace" (json-object "fileOperations"
                                          (json-object "willRename" will-rename)))))

(defun semantic-tests--document (client)
  "Register an in-memory fixture document without reading its path."
  (let* ((document (make-instance 'lsp-document :path #p"/fixture/a.txt" :text "name"))
         (uri (lsp-path-uri (lsp-document-path document))))
    (setf (gethash uri (lsp-client-documents client)) document)
    document))

(defun semantic-tests--workspace-edit (document)
  "Return a fixture rename proposal for DOCUMENT and another URI."
  (json-object "documentChanges"
               (vector (edit-tests--document-change
                        (lsp-path-uri (lsp-document-path document))
                        (vector (edit-tests--edit 0 4 "new")) 1))))

(defun test-lsp-semantic-proposals ()
  "Exercise capability gates, params, normalized results and command preservation."
  (let* ((client (lsp-client-tests--client #p"/" :capabilities (semantic-tests--capabilities)))
         (document (semantic-tests--document client))
         (position (json-object "line" 0 "character" 1))
         (calls nil)
         (reply nil)
         (raw-action (json-object "title" "Fix name" "kind" "quickfix"
                                  "data" (json-object "ticket" "opaque")))
         (command (json-object "title" "Build" "command" "fixture.build" "arguments" (vector 1))))
    (tests--call-with-replacements
     (list (list 'lsp-transport-request
                 (lambda (transport method params &key timeout)
                   (declare (ignore transport))
                   (tests--assert (= timeout 1) "semantic operations use configured request deadline")
                   (push (cons method params) calls)
                   reply)))
     (lambda ()
       (setf reply (json-object "range" (edit-tests--range 0 4) "placeholder" "name"))
       (tests--assert (equal (json-get (lsp-client-prepare-rename client document position) "placeholder")
                             "name") "prepare rename returns validated placeholder")
       (setf reply (edit-tests--range 0 4))
       (tests--assert (= (json-get (lsp-client-prepare-rename client document position) "endOffset") 4)
                      "prepare rename accepts a bare range")
       (setf reply (json-object "defaultBehavior" t))
       (tests--assert (eq (json-get (lsp-client-prepare-rename client document position) "defaultBehavior") t)
                      "prepare rename accepts default behavior")
       (setf reply nil)
       (tests--assert (null (lsp-client-prepare-rename client document position))
                      "null preparation reports no rename target")
       (setf reply (semantic-tests--workspace-edit document))
       (tests--assert (= (length (json-get (lsp-client-rename client document :position position
                                                           :new-name "new") "operations")) 1)
                      "rename normalizes against synchronized text")
       (tests--assert (equal (json-get (rest (first calls)) "newName") "new")
                      "rename forwards the proposed symbol name")
       (setf reply (vector raw-action command
                           (json-object "title" "Immediate" "edit" (semantic-tests--workspace-edit document)
                                        "disabled" (json-object "reason" "Needs review"))))
       (let ((rows (lsp-client-code-actions client document :range (edit-tests--range 0 4)
                                                              :only (vector "quickfix"))))
         (tests--assert (= (length rows) 3) "code action and command union is normalized")
         (tests--assert (eq (json-get (aref rows 0) "action") raw-action)
                        "opaque resolve data and action metadata are preserved")
         (tests--assert (eq (json-get (aref rows 1) "command") command)
                        "commands are proposed without execution")
         (tests--assert (null (json-get (aref rows 1) "resolveSupported"))
                        "commands are not resolvable code actions")
         (tests--assert (json-object-p (json-get (aref rows 2) "edit")) "inline action edits are normalized")
         (setf reply (json-object "title" "Fix name" "data" (json-get raw-action "data")
                                  "edit" (semantic-tests--workspace-edit document)))
         (tests--assert (json-object-p
                        (json-get (lsp-client-resolve-code-action client (json-get (aref rows 0) "action"))
                                  "edit")) "resolved action edits are normalized")
         (tests--assert (eq (rest (first calls)) raw-action) "resolution forwards raw action including DATA"))
       (setf reply (semantic-tests--workspace-edit document))
       (let ((plan (lsp-client-will-rename-files
                    client (vector (json-object "oldUri" "file:///a.txt" "newUri" "file:///b.txt")
                                   (json-object "oldUri" "file:///a.c" "newUri" "file:///b.c"))
                    :file-kind (lambda (uri) (declare (ignore uri)) :file))))
         (tests--assert (= (length (json-get plan "operations")) 1) "file rename returns server workspace edits")
         (tests--assert (= (length (json-get (rest (first calls)) "files")) 1)
                        "registration filters select file moves"))
       (let ((count (length calls)))
         (tests--assert (zerop (length (json-get
                                       (lsp-client-will-rename-files
                                        client (vector (json-object "oldUri" "file:///a.c" "newUri" "file:///b.c")))
                                       "operations"))) "unmatched file moves yield an empty proposal")
         (tests--assert (= count (length calls)) "unmatched moves produce no server request"))
       (dolist (capability (list nil (json-false) t))
         (setf (lsp-client-capabilities client) (json-object "renameProvider" capability))
         (tests--assert
          (edit-tests--refused-p (lambda () (lsp-client-prepare-rename client document position))
                                 'lsp-unsupported)
          "prepare requires an advertised prepareProvider flag"))
       (setf (lsp-client-capabilities client) (json-object "renameProvider" (json-false)))
       (let ((count (length calls)))
         (tests--assert
          (edit-tests--refused-p (lambda () (lsp-client-rename client document :position position :new-name "x"))
                                 'lsp-unsupported) "false rename capability is unsupported")
         (tests--assert (= count (length calls)) "unsupported operations produce no request"))
       (setf (lsp-client-capabilities client) (semantic-tests--capabilities))
       (dolist (bad (list (json-object "title" "Invalid" "command" (json-false))
                         (json-object "title" "Invalid" "edit" (json-false))
                         (json-object "title" "Invalid" "isPreferred" 1)
                         (json-object "title" "Invalid" "disabled" (json-object))))
         (setf reply (vector bad))
         (tests--assert
          (edit-tests--refused-p
           (lambda () (lsp-client-code-actions client document :range (edit-tests--range 0 4))))
          "malformed code actions signal typed protocol failures")))))
  nil)

(defun test-lsp-file-operation-filters ()
  "Exercise LSP glob syntax, schemes, case options and kind observation."
  (dolist (case '(("**/*.txt" "/a.txt" t) ("**/*.txt" "/x/y/a.txt" t)
                  ("*.txt" "/a.txt" nil) ("/x/*" "/x/y/a" nil)
                  ("/x/**" "/x/y/a" t) ("**/*.{lisp,asd}" "/a.asd" t)
                  ("**/*.{lisp,asd}" "/a.c" nil) ("/a[0-9].txt" "/a2.txt" t)
                  ("/a[!0-9].txt" "/ab.txt" t) ("/a?.txt" "/ab.txt" t)
                  ("/a?.txt" "/a/b.txt" nil) ("**/{a,{b,c}}.txt" "/c.txt" t)))
    (destructuring-bind (glob path expected) case
      (tests--assert (eq (lsp-file-filter--glob-p glob path) expected) "LSP file glob behavior")))
  (let ((filter (json-object "scheme" "file" "pattern"
                            (json-object "glob" "**/*.TXT" "matches" "file"
                                         "options" (json-object "ignoreCase" t)))))
    (tests--assert (lsp-file-filter--matches-p filter "file:///space%20name/a.txt"
                                              (lambda (uri) (declare (ignore uri)) :file))
                   "case-insensitive glob matches decoded URI path")
    (tests--assert (not (lsp-file-filter--matches-p filter "file:///a.txt"
                                                   (lambda (uri) (declare (ignore uri)) :folder)))
                   "file/folder filters use caller observations")
    (tests--assert (not (lsp-file-filter--matches-p filter "untitled:///a.txt" nil))
                   "URI schemes restrict filters")
    (tests--assert (edit-tests--refused-p
                   (lambda () (lsp-file-filter--matches-p filter "file:///a.txt" nil)))
                   "matching kind-dependent filters require an observation callback"))
  nil)

(defun semantic-tests--server-form ()
  "Return a standalone ASCII fixture for negotiated semantic requests and errors."
  '(labels ((read-frame ()
              (let ((size nil))
                (loop for line = (read-line *standard-input* nil nil)
                      while (and line (plusp (length (string-trim '(#\Return) line))))
                      do (when (search "Content-Length:" line)
                           (setf size (parse-integer line :start 15 :junk-allowed t))))
                (when size
                  (let ((body (make-string size)))
                    (read-sequence body *standard-input*) body))))

            (write-frame (body)
              (format t "Content-Length: ~D~C~C~C~C~A" (length body)
                      #\Return #\Newline #\Return #\Newline body)
              (finish-output))

            (reply (identifier result)
              (write-frame (format nil "{\"jsonrpc\":\"2.0\",\"id\":~D,\"result\":~A}"
                                   identifier result))))
     (loop for body = (read-frame) while body
           for id-position = (search "\"id\":" body)
           when id-position
             do (let ((identifier (parse-integer body :start (+ id-position 5) :junk-allowed t)))
                  (cond
                    ((search "\"initialize\"" body)
                     (unless (and (search "prepareSupport" body)
                                  (search "resolveSupport" body)
                                  (search "changeAnnotationSupport" body))
                       (sb-ext:exit :code 4))
                     (reply identifier
                            "{\"capabilities\":{\"positionEncoding\":\"utf-16\",\"renameProvider\":{\"prepareProvider\":true},\"codeActionProvider\":{\"resolveProvider\":true},\"workspace\":{\"fileOperations\":{\"willRename\":{\"filters\":[{\"pattern\":{\"glob\":\"**/*.txt\"}}]}}}}}"))
                    ((search "textDocument/prepareRename" body)
                     (reply identifier "{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":4}}"))
                    ((search "textDocument/rename" body)
                     (cond
                       ((search "forbidden" body)
                        (write-frame (format nil "{\"jsonrpc\":\"2.0\",\"id\":~D,\"error\":{\"code\":-32602,\"message\":\"Rename refused\"}}" identifier)))
                       (t
                        (write-frame "{\"jsonrpc\":\"2.0\",\"id\":\"apply\",\"method\":\"workspace/applyEdit\",\"params\":{\"edit\":{}}}")
                        (unless (search "false" (read-frame)) (sb-ext:exit :code 5))
                        (reply identifier "{\"changes\":{\"file:///fixture/a.txt\":[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":4}},\"newText\":\"new\"}]}}"))))
                    ((search "codeAction/resolve" body)
                     (unless (search "opaque" body) (sb-ext:exit :code 6))
                     (reply identifier "{\"title\":\"Fix\",\"edit\":{\"changes\":{\"file:///fixture/a.txt\":[{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":4}},\"newText\":\"fixed\"}]}}}"))
                    ((search "textDocument/codeAction" body)
                     (reply identifier "[{\"title\":\"Fix\",\"data\":{\"ticket\":\"opaque\"}}]"))
                    ((search "workspace/willRenameFiles" body) (reply identifier "null"))
                    ((search "shutdown" body) (reply identifier "null") (return))
                    (t (sb-ext:exit :code 7)))))))

(defun test-lsp-semantic-process-boundary ()
  "Exercise semantic negotiation, callbacks, resolution and RPC failure on real pipes."
  (with-test-directory (root)
    (let* ((configuration
             (make-instance 'lsp-server-configuration :name "semantic-fixture"
                            :command (namestring sb-ext:*runtime-pathname*)
                            :arguments (list "--noinform" "--disable-debugger" "--no-sysinit"
                                             "--no-userinit" "--non-interactive" "--eval"
                                             (with-standard-io-syntax
                                               (let ((*package* (find-package '#:cl-lsp)))
                                                 (prin1-to-string (semantic-tests--server-form)))))
                            :extensions '(".txt") :language-id "text" :timeout-seconds 5))
           (client (lsp-client-start configuration root))
           (document (semantic-tests--document client))
           (position (json-object "line" 0 "character" 1))
           (process (lsp-transport-process (lsp-client-transport client))))
      (unwind-protect
           (progn
             (tests--assert (= (json-get (lsp-client-prepare-rename client document position) "endOffset") 4)
                            "real server prepares a rename")
             (tests--assert (= (length (json-get (lsp-client-rename client document
                                                                  :position position :new-name "new")
                                                 "operations")) 1)
                            "rename handles server applyEdit denial before returning a proposal")
             (let* ((rows (lsp-client-code-actions client document :range (edit-tests--range 0 4)))
                    (resolved (lsp-client-resolve-code-action client (json-get (aref rows 0) "action"))))
               (tests--assert (equal (json-get (aref (json-get
                                                   (aref (json-get (json-get resolved "edit") "operations") 0)
                                                   "edits") 0) "newText") "fixed")
                              "real code action resolution preserves opaque data and returns edits"))
             (tests--assert (zerop (length (json-get
                                           (lsp-client-will-rename-files
                                            client (vector (json-object "oldUri" "file:///a.txt"
                                                                        "newUri" "file:///b.txt")))
                                           "operations"))) "null file rename result is an empty proposal")
             (tests--assert
              (handler-case
                  (progn (lsp-client-rename client document :position position :new-name "forbidden") nil)
                (lsp-rpc-error (condition) (= (lsp-rpc-error-code condition) -32602)))
              "semantic requests retain typed remote failures"))
        (lsp-client-close client))
      (tests--assert (not (process-alive-p process)) "semantic fixture process is reaped")))
  nil)
