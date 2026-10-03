(in-package #:cl-lsp)

(defun lsp-client-tests--configuration
       (&key (name "test") (command "test-lsp"))
  "Build a small server configuration for client tests."
  (make-instance 'lsp-server-configuration :name name :command command
                 :arguments nil :extensions '(".txt") :language-id "text"
                 :root-markers nil :initialization-options (json-object)
                 :settings (json-object) :timeout-seconds 1 :disabled-p nil))

(defun lsp-client-tests--client (root &key capabilities transport)
  "Build a client without starting a process."
  (let ((client
         (make-instance 'lsp-client :configuration
                        (lsp-client-tests--configuration) :root
                        (uiop/pathname:ensure-directory-pathname root))))
    (setf (lsp-client-capabilities client) (or capabilities (json-object))
          (lsp-client-transport client) transport)
    client))

(defmacro lsp-client-tests--assert (form)
  "Record a client-test assertion with a stable description."
  `(tests--assert ,form "LSP client assertion"))

(defmacro lsp-client-tests--with-replacements (replacements &body body)
  "Run BODY while temporarily replacing the listed functions."
  `(tests--call-with-replacements ,replacements (lambda () ,@body)))

(defun lsp-client-tests--write (configuration name text)
  "Write TEXT beneath CONFIGURATION's temporary workspace."
  (let ((path (merge-pathnames name configuration)))
    (ensure-directories-exist path)
    (with-open-file
        (stream path :direction ':output :if-exists ':supersede
         :if-does-not-exist ':create)
      (write-string text stream))
    path))

(defun test-lsp-client-position-and-sync-options ()
  "Test UTF-16 positions and advertised synchronization modes."
  (lsp-client-tests--assert (= 3 (json-get (lsp-position "a😀") "character")))
  (with-test-directory (configuration)
   (let ((client (lsp-client-tests--client configuration)))
     (setf (lsp-client-capabilities client) (json-object "textDocumentSync" 2))
     (multiple-value-bind (kind open save)
         (lsp-client--sync-options client)
       (lsp-client-tests--assert (and (= kind 2) open (null save))))
     (setf (lsp-client-capabilities client)
             (json-object "textDocumentSync"
                          (json-object "change" 1 "openClose" t "save"
                                       (json-object "includeText" t))))
     (multiple-value-bind (kind open save)
         (lsp-client--sync-options client)
       (lsp-client-tests--assert
        (and (= kind 1) open (json-get save "includeText")))))))

(defun test-lsp-client-handshake-callbacks ()
  "Test initialize negotiation and initialized notifications."
  (with-test-directory (configuration)
   (let ((requests nil) (notifications nil) (identity nil) (transport (list :live t)))
     (lsp-client-tests--with-replacements
      (list
       (list 'lsp-transport-open
             (lambda (&rest arguments) (declare (ignore arguments)) transport))
       (list 'lsp-transport-request
             (lambda (ignored method params &key timeout)
                (declare (ignore ignored timeout))
                (setf identity (json-get params "clientInfo"))
               (push method requests)
               (json-object "capabilities"
                            (json-object "positionEncoding" "utf-16"))))
       (list 'lsp-transport-notify
             (lambda (ignored method params)
               (declare (ignore ignored params))
               (push method notifications)))
       (list 'lsp-transport-close
             (lambda (ignored)
               (declare (ignore ignored))
               (setf (getf transport :live) nil))))
      (let ((client
             (lsp-client-start (lsp-client-tests--configuration)
                                configuration :client-name "Fixture" :client-version "2.3")))
         (tests--assert
          (and (string= "Fixture" (json-get identity "name"))
               (string= "2.3" (json-get identity "version"))
               (string= "Fixture" (lsp-client-name client))
               (string= "2.3" (lsp-client-version client)))
          "initialize advertises the caller's client identity")
        (lsp-client-tests--assert
         (string=
          (json-get (lsp-client-capabilities client) "positionEncoding")
          "utf-16"))
        (lsp-client-tests--assert (equal (reverse requests) '("initialize")))
        (lsp-client-tests--assert
         (equal (reverse notifications)
                '("initialized" "workspace/didChangeConfiguration"))))))))

(defun test-lsp-client-full-and-incremental-sync ()
  "Test full-document and ranged incremental synchronization notifications."
  (with-test-directory (configuration)
   (let* ((path (lsp-client-tests--write configuration "a.txt" "hello"))
          (sent nil)
          (client
           (lsp-client-tests--client configuration :transport (list :live t))))
     (setf (lsp-client-capabilities client)
             (json-object "textDocumentSync"
                          (json-object "change" 2 "openClose" t)))
     (lsp-client-tests--with-replacements
      (list
       (list 'lsp-transport-notify
             (lambda (transport method params)
               (declare (ignore transport))
               (push (list method params) sent))))
      (lsp-client-sync client path)
      (with-open-file (stream path :direction ':output :if-exists ':supersede)
        (write-string "hello!" stream))
      (lsp-client-sync client path))
     (lsp-client-tests--assert (= 2 (length sent)))
     (lsp-client-tests--assert
      (string= (first (first sent)) "textDocument/didChange"))
     (let* ((change (aref (json-get (second (first sent)) "contentChanges") 0))
            (end (json-get (json-get change "range") "end")))
       (lsp-client-tests--assert
        (and (= (json-get end "line") 0) (= (json-get end "character") 5)
             (string= (json-get change "text") "hello!")))))))

(defun test-lsp-client-diagnostics-and-stale-invalidation ()
  "Test stale diagnostic rejection and invalidation after source changes."
  (with-test-directory (configuration)
   (let* ((path (lsp-client-tests--write configuration "a.txt" "x"))
          (client
           (lsp-client-tests--client configuration :transport (list :live t))))
     (lsp-client-sync client path)
     (let ((document
            (gethash (lsp-path-uri path) (lsp-client-documents client))))
       (lsp-client--publish-diagnostics client
                                        (json-object "uri" (lsp-path-uri path)
                                                     "version" 1 "diagnostics"
                                                     (vector
                                                      (json-object "message"
                                                                   "old"))))
       (lsp-client-tests--assert
        (= 1 (length (lsp-document-diagnostics document))))
       (with-open-file (stream path :direction ':output :if-exists ':supersede)
         (write-string "xx" stream))
       (lsp-client-sync client path)
       (lsp-client-tests--assert
        (zerop (length (lsp-document-diagnostics document))))
       (lsp-client--publish-diagnostics client
                                        (json-object "uri" (lsp-path-uri path)
                                                     "version" 1 "diagnostics"
                                                     #()))
       (lsp-client-tests--assert
        (zerop (length (lsp-document-diagnostics document))))))))

(defun test-lsp-client-document-bounds ()
  "Test document byte and open-document limits."
  (with-test-directory (configuration)
   (let* ((path (lsp-client-tests--write configuration "large.txt" "12345"))
          (client
           (lsp-client-tests--client configuration :transport (list :live t))))
     (let ((*lsp-maximum-document-bytes* 4))
       (lsp-client-tests--assert
        (handler-case (progn (lsp-client-sync client path) nil)
                      (lsp-error nil t))))
     (let ((*lsp-maximum-document-bytes* 100) (*lsp-maximum-open-documents* 0))
       (lsp-client-tests--assert
        (handler-case (progn (lsp-client-sync client path) nil)
                      (lsp-error nil t)))))))

(defun test-lsp-client-diagnostics-push-pull ()
  "Test pending versus empty push reports and pull diagnostics."
  (with-test-directory (configuration)
   (let* ((path (lsp-client-tests--write configuration "a.txt" "x"))
          (requests 0)
          (client
           (lsp-client-tests--client configuration :transport (list :live t))))
     (lsp-client-sync client path)
     (let ((document
            (gethash (lsp-path-uri path) (lsp-client-documents client))))
       (let ((pending (lsp-client-diagnostics client document :wait-seconds 0)))
         (lsp-client-tests--assert
          (string= (json-get pending "state") "pending")))
       (lsp-client--publish-diagnostics client
                                        (json-object "uri" (lsp-path-uri path)
                                                     "version" 1 "diagnostics"
                                                     #()))
       (let ((empty (lsp-client-diagnostics client document :wait-seconds 0)))
         (lsp-client-tests--assert
          (and (string= (json-get empty "state") "received")
               (zerop (length (json-get empty "items"))))))
       (setf (lsp-client-capabilities client)
               (json-object "diagnosticProvider" t))
       (lsp-client-tests--with-replacements
        (list
         (list 'lsp-transport-request
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (incf requests)
                 (json-object "kind" "full" "items" #()))))
        (let ((empty (lsp-client-diagnostics client document :wait-seconds 0)))
          (lsp-client-tests--assert
           (and (string= (json-get empty "state") "received")
                (zerop (length (json-get empty "items")))))
          (lsp-client-tests--assert (= 1 requests))))))))

(defun test-lsp-client-manager-reuse-restart-and-cleanup ()
  "Test lazy client reuse, dead-client restart, and manager cleanup."
  (with-test-directory (configuration)
   (let* ((root configuration)
          (server-configuration (lsp-client-tests--configuration))
          (starts 0)
          (closes 0)
          (manager (make-instance 'lsp-manager :client-name "Pool" :client-version "4.5")))
     (lsp-client-tests--with-replacements
      (list
       (list 'lsp-client-start
              (lambda (config directory &key client-name client-version)
                (declare (ignore config directory))
                (tests--assert (and (string= "Pool" client-name)
                                    (string= "4.5" client-version))
                               "manager forwards client identity on every start")
               (incf starts)
               (lsp-client-tests--client root :transport
                (make-instance 'lsp-transport :process nil :input nil :output
                               nil :error-output nil :request-handler nil
                               :notification-handler nil))))
       (list 'lsp-client-close
             (lambda (client &key detach-p)
               (declare (ignore detach-p))
               (incf closes)
               (setf (lsp-client-transport client) nil))))
      (let ((one (lsp-manager-client manager server-configuration root)))
        (lsp-client-tests--assert
         (eq one (lsp-manager-client manager server-configuration root)))
        (setf (lsp-client-transport one) nil)
        (lsp-manager-client manager server-configuration root)
        (lsp-client-tests--assert (= starts 2))
        (lsp-manager-close manager)
        (lsp-client-tests--assert (= closes 2))
        (lsp-client-tests--assert
         (zerop (hash-table-count (lsp-manager-clients manager)))))))))

(-> test-lsp-client-independent-diagnostic-sources nil null)

(defun test-lsp-client-independent-diagnostic-sources ()
  "Keep push and pull reports independent across replacement, clearing, and edits."
  (with-test-directory (configuration)
   (let* ((path (lsp-client-tests--write configuration "mixed.txt" "x"))
          (client
           (lsp-client-tests--client configuration :capabilities
            (json-object "diagnosticProvider" t)))
          (document (lsp-client-sync client path))
          (compiler-error (json-object "message" "compiler error"))
          (native-error (json-object "message" "native error"))
          (pulled #()))
     (labels ((publish
                  (items
                   &key (source ':push)
                   (version (lsp-document-version document)))
                (lsp-client--publish-diagnostics client
                                                 (json-object "uri"
                                                              (lsp-path-uri
                                                               path)
                                                              "version" version
                                                              "diagnostics"
                                                              items)
                                                 :source source))
              (messages (report)
                (map 'list (lambda (item) (json-get item "message"))
                     (json-get report "items"))))
       (lsp-client-tests--with-replacements
        (list
         (list 'lsp-transport-request
               (lambda (&rest arguments)
                 (declare (ignore arguments))
                 (json-object "kind" "full" "items" pulled))))
        (publish (vector compiler-error))
        (dotimes (iteration 2)
          (tests--assert
           (equal '("compiler error")
                  (messages
                   (lsp-client-diagnostics client document :wait-seconds 0)))
           "repeated empty pulls cannot erase pushed compiler errors"))
        (setf pulled (vector native-error compiler-error))
        (tests--assert
         (equal '("compiler error" "native error")
                (messages
                 (lsp-client-diagnostics client document :wait-seconds 0)))
         "both diagnostic sources are included without duplicate items")
        (publish #())
        (tests--assert (= 2 (length (lsp-document-diagnostics document)))
         "clearing push reports does not clear pull reports")
        (setf pulled #())
        (tests--assert
         (null
          (messages (lsp-client-diagnostics client document :wait-seconds 0)))
         "each source can clear its own report")
        (publish (vector compiler-error) :version nil)
        (let ((report (lsp-client-diagnostics client document :wait-seconds 0)))
          (tests--assert
           (and (string= "unversioned" (json-get report "state"))
                (argo:json-false-p (gethash "versioned" report)))
           "a versioned pull does not upgrade an unversioned push"))
        (let ((*lsp-maximum-diagnostics* 1))
          (publish (vector compiler-error))
          (setf pulled (vector native-error))
          (let ((report
                 (lsp-client-diagnostics client document :wait-seconds 0)))
            (tests--assert
             (and (= 1 (length (json-get report "items")))
                  (json-get report "truncated"))
             "the combined report obeys the document diagnostic bound")))
        (tests--write-text path "xx") (lsp-client-sync client path)
        (dolist (source '(:push :pull))
          (publish (vector compiler-error) :source source :version 1))
        (let ((report (lsp-document--diagnostic-snapshot document t)))
          (tests--assert
           (and (zerop (length (json-get report "items")))
                (string= "pending" (json-get report "state")))
           "an edit invalidates both reports and rejects stale replacements"))))))
  nil)
