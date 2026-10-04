(asdf:defsystem #:cl-lsp
  :description "Bounded stdio Language Server Protocol clients."
  :author "Lukáš Hozda"
  :version "0.1.0"
  :license "COLL-Attribution"
  :depends-on (#:argo #:bordeaux-threads #:quri #:serapeum #:sexp-config)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "configuration")
                             (:file "pathnames")
                             (:file "transport")
                             (:file "configuration-file")
                             (:file "client")
                             (:file "queries"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-lsp/tests))))

(asdf:defsystem #:cl-lsp/tests
  :description "Independent transport, process, synchronization and diagnostic tests."
  :depends-on (#:cl-lsp #:flexi-streams)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "transport-tests")
                             (:file "client-tests")
                             (:file "encoding-tests")
                             (:file "declarative-tests")
                             (:file "test-cases"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-lsp '#:run-tests)))
