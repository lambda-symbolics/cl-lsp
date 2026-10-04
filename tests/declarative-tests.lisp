(in-package #:cl-lsp)

;;;; -- Declarative Configuration, Positions, Queries and Fan-Out --

(defun declarative-tests--error-field (pathname)
  "Return the field of the configuration error reading PATHNAME signals, or :NONE."
  (handler-case (progn (lsp-read-configurations pathname) :none)
    (lsp-configuration-error (condition)
      (or (lsp-configuration-error-field condition) :unnamed))))

(-> test-lsp-configuration-files nil null)
(defun test-lsp-configuration-files ()
  "Test reading declarative server configurations and refusing malformed ones."
  (with-test-directory (root)
    (let ((pathname (merge-pathnames "lsp.sexp" root)))
      (tests--write-text pathname "(:version 1 :servers
  ((:name \"lisp\" :command \"lisp-ls\" :arguments (\"--stdio\" \"\")
    :extensions (\".lisp\" \".asd\") :language-id \"lisp\" :root-markers (\".git\")
    :initialization-options \"{\\\"a\\\": 1}\" :settings \"{\\\"b\\\": {\\\"c\\\": true}}\"
    :timeout-seconds 5)
   (:name \"text\" :command \"text-ls\" :extensions (\".txt\") :language-id \"text\"
    :disabled-p t)))")
      (destructuring-bind (lisp text) (lsp-read-configurations pathname)
        (tests--assert (and (string= (lsp-server-configuration-name lisp) "lisp")
                            (equal (lsp-server-configuration-arguments lisp) '("--stdio" ""))
                            (equal (lsp-server-configuration-extensions lisp) '(".lisp" ".asd"))
                            (equal (lsp-server-configuration-root-markers lisp) '(".git"))
                            (= (lsp-server-configuration-timeout-seconds lisp) 5)
                            (not (lsp-server-configuration-disabled-p lisp)))
                       "a server definition reads into its configuration")
        (tests--assert (and (eql (json-get (lsp-server-configuration-initialization-options lisp) "a")
                                 1)
                            (hash-table-p (json-get (lsp-server-configuration-settings lisp) "b")))
                       "JSON options and settings are decoded")
        (tests--assert (and (= (lsp-server-configuration-timeout-seconds text) 30)
                            (lsp-server-configuration-disabled-p text)
                            (null (lsp-server-configuration-initialization-options text))
                            (zerop (hash-table-count (lsp-server-configuration-settings text))))
                       "optional fields take their defaults"))
      (loop for (source field) in
            '(("(:version 2 :servers nil)" :version)
              ("(:version 1)" :servers)
              ("(:version 1 :servers nil :extra 1)" :unnamed)
              ("(:version 1 :servers ((:name \"a\" :command \"b\" :language-id \"c\")))" :extensions)
              ("(:version 1 :servers ((:name \"a\" :command \"\" :extensions (\".a\") :language-id \"c\")))" :command)
              ("(:version 1 :servers ((:name \"a\" :command \"b\" :extensions (\".a\") :language-id \"c\" :root-markers (\"../x\"))))" :root-markers)
              ("(:version 1 :servers ((:name \"a\" :command \"b\" :extensions (\".a\") :language-id \"c\" :timeout-seconds 0)))" :timeout-seconds)
              ("(:version 1 :servers ((:name \"a\" :command \"b\" :extensions (\".a\") :language-id \"c\" :settings \"[1]\")))" :settings)
              ("(:version 1 :servers ((:name \"a\" :command \"b\" :extensions (\".a\") :language-id \"c\" :disabled-p 1)))" :disabled-p)
              ("(:version 1 :servers ((:name \"a\" :command \"b\" :extensions (\".a\") :language-id \"c\") (:name \"a\" :command \"b\" :extensions (\".a\") :language-id \"c\")))" :name)
              ("(:version 1 :servers ((:name \"a\" :name \"b\")))" :name)
              ("(:version 1 :servers #.(error \"evaluated\"))" :unnamed))
            do (tests--write-text pathname source)
               (tests--assert (eq (declarative-tests--error-field pathname) field)
                              (format nil "~A is refused naming ~S" source field)))
      (tests--write-text pathname (format nil "(:version 1 :servers nil)~A"
                                          (make-string (1+ *lsp-configuration-maximum-bytes*)
                                                       :initial-element #\Space)))
      (tests--assert (eq (declarative-tests--error-field pathname) :unnamed)
                     "an oversized configuration is refused before reading")))
  nil)

(-> test-lsp-text-positions nil null)
(defun test-lsp-text-positions ()
  "Test the inverse of LSP-POSITION across line endings and surrogate pairs."
  (let ((text (format nil "ab~C~%c~Cd~Ce" #\Return (code-char #x1F600) #\Return)))
    (loop for end from 0 to (length text)
          for position = (lsp-position (subseq text 0 end))
          unless (and (< 0 end (length text))
                      (char= (char text (1- end)) #\Return)
                      (char= (char text end) #\Newline))
            do (tests--assert (equalp (lsp-text-position text (json-get position "line")
                                                         (json-get position "character"))
                                      position)
                              "every position LSP-POSITION reports maps back to itself"))
    (dolist (coordinates '((0 9) (5 0) (1 2)))
      (tests--assert (handler-case (progn (apply #'lsp-text-position text coordinates) nil)
                       (lsp-error () t))
                     "positions past a line, past the text or inside a surrogate pair fail")))
  nil)

(-> test-lsp-client-queries nil null)
(defun test-lsp-client-queries ()
  "Test capability gates and request parameters of read-only queries."
  (with-test-directory (root)
    (let* ((path (lsp-client-tests--write root "file.txt" "text"))
           (document (make-instance 'lsp-document :path path :text "text"))
           (client (lsp-client-tests--client
                    root :capabilities (json-object "referencesProvider" t
                                                    "workspaceSymbolProvider" t
                                                    "documentSymbolProvider" t)))
           (requests nil))
      (lsp-client-tests--with-replacements
       (list (list 'lsp-transport-request
                   (lambda (transport method params &key timeout)
                     (declare (ignore transport))
                     (push (list method params timeout) requests)
                     :answer)))
       (tests--assert (eq :answer (lsp-client-query client "references" document
                                                    :position (json-object "line" 0 "character" 1)))
                      "an advertised query reaches the server")
       (destructuring-bind (method params timeout) (first requests)
         (tests--assert (and (string= method "textDocument/references")
                             (eq (json-get (json-get params "context") "includeDeclaration") t)
                             (json-get params "position")
                             (= timeout 1))
                        "references carry the declaration context, position and timeout"))
       (lsp-client-query client "workspace-symbols" document :query "main")
       (tests--assert (equal (json-get (second (first requests)) "query") "main")
                      "workspace symbols send their query")
       (lsp-client-query client "document-symbols" document)
       (tests--assert (null (json-get (second (first requests)) "position"))
                      "document symbols need no position")
       (dolist (call (list (lambda () (lsp-client-query client "hover" document
                                                        :position (json-object)))
                           (lambda () (lsp-client-query client "rename" document))
                           (lambda () (lsp-client-query client "references" document))
                           (lambda () (lsp-client-query client "workspace-symbols" document
                                                        :query 7))))
         (tests--assert (handler-case (progn (funcall call) nil) (lsp-error () t))
                        "unadvertised, unknown or underspecified queries fail locally"))
       (tests--assert (= (length requests) 3) "refused queries never reach the server"))))
  nil)

(-> test-lsp-manager-map-file nil null)
(defun test-lsp-manager-map-file ()
  "Test extension fan-out with independent per-server results."
  (with-test-directory (root)
    (let* ((path (lsp-client-tests--write root "file.txt" "text"))
           (manager (make-instance 'lsp-manager :client-name "Fan" :client-version "1"))
           (good (lsp-client-tests--configuration :name "good"))
           (bad (lsp-client-tests--configuration :name "bad"))
           (other (make-instance 'lsp-server-configuration
                                 :name "other" :command "x" :extensions '(".lisp")
                                 :language-id "lisp"))
           (off (make-instance 'lsp-server-configuration
                               :name "off" :command "x" :extensions '(".txt")
                               :language-id "text" :disabled-p t)))
      (setf (lsp-manager-configurations manager) (list good bad other off))
      (tests--assert (equal (mapcar #'lsp-server-configuration-name
                                    (lsp-configurations-for-path (list good other off) path))
                            '("good"))
                     "only enabled servers handling the extension are selected")
      (lsp-client-tests--with-replacements
       (list (list 'lsp-manager-client
                   (lambda (manager configuration root)
                     (declare (ignore manager root))
                     (if (eq configuration bad)
                         (error 'lsp-error :message "server failed to start")
                         (lsp-client-tests--client root))))
             (list 'lsp-client-resync (lambda (client) client))
             (list 'lsp-client-sync (lambda (client path) (declare (ignore client)) path)))
       (let ((rows (lsp-manager-map-file manager path root
                                         (lambda (client document)
                                           (declare (ignore client))
                                           (namestring document)))))
         (tests--assert (and (= (length rows) 2)
                             (equal (getf (first rows) :result) (namestring path))
                             (equal (getf (first rows) :root) root)
                             (typep (getf (second rows) :error) 'lsp-error))
                        "each server answers or fails on its own")))
      (tests--assert (handler-case
                         (progn (lsp-manager-map-file manager (merge-pathnames "x.md" root) root
                                                      #'identity)
                                nil)
                       (lsp-error () t))
                     "a file no server handles fails")))
  nil)
