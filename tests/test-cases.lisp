(in-package #:cl-lsp)

;;;; -- Behavioral Test Entry Point --

(-> run-tests () boolean)
(defun run-tests ()
  "Run the independent transport and client cases; signal any case failures."
  (let ((*test-checks* 0)
        (failures nil)
        (cases '(test-lsp-transport-framing
                 test-lsp-transport-write-frame
                 test-lsp-transport-process-lifecycle
                 test-lsp-client-position-and-sync-options
                 test-lsp-client-handshake-callbacks
                 test-lsp-client-full-and-incremental-sync
                 test-lsp-client-diagnostics-and-stale-invalidation
                 test-lsp-client-document-bounds
                 test-lsp-client-diagnostics-push-pull
                 test-lsp-client-independent-diagnostic-sources
                 test-lsp-client-manager-reuse-restart-and-cleanup
                 test-lsp-file-uris
                 test-lsp-project-roots
                 test-lsp-json-boundaries)))
    (dolist (name cases)
      (handler-case
          (progn (funcall name) (format t "PASS ~(~A~)~%" name))
        (error (condition)
          (push (cons name condition) failures)
          (format t "FAIL ~(~A~): ~A~%" name condition))))
    (format t "~D cases, ~D checks, ~D failed cases.~%"
            (length cases) *test-checks* (length failures))
    (when failures
      (error 'lsp-test-failure :message
             (format nil "~D cl-lsp cases failed." (length failures))))
    t))
