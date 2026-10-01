(asdf:defsystem #:cl-lsp
  :description "Bounded stdio Language Server Protocol clients."
  :author "Lukáš Hozda"
  :version "0.1.0"
  :license "COLL-Attribution"
  :depends-on (#:bordeaux-threads #:flexi-streams #:quri #:serapeum #:yason)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "json")
                             (:file "configuration")
                             (:file "pathnames")
                             (:file "transport")
                             (:file "client"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-lsp/tests))))

(asdf:defsystem #:cl-lsp/tests
  :description "Independent transport, process, synchronization and diagnostic tests."
  :depends-on (#:cl-lsp)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "transport-tests")
                             (:file "client-tests")
                             (:file "encoding-tests")
                             (:file "test-cases"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-lsp '#:run-tests)))
