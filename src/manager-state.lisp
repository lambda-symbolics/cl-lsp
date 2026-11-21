(in-package #:cl-lsp)

;;;; -- Manager Observations and Configuration Cache --

(define-condition lsp-no-matching-server (lsp-error)
  ((path :initarg :path :reader lsp-no-matching-server-path
         :documentation "The source pathname without an enabled server."))
  (:documentation "No enabled language server handles a source pathname."))

(-> lsp-manager-configurations-load (lsp-manager function) list)
(defun lsp-manager-configurations-load (manager loader)
  "Cache LOADER's configurations, including NIL, until MANAGER closes.
A failed loader leaves the cache unloaded and may be retried."
  (with-recursive-lock-held ((lsp-manager-lock manager))
    (unless (lsp-manager-loaded-p manager)
      (let ((configurations (funcall loader)))
        (setf (lsp-manager-configurations manager) configurations
              (lsp-manager-loaded-p manager) t)))
    (lsp-manager-configurations manager)))

(-> lsp-manager-status (lsp-manager) list)
(defun lsp-manager-status (manager)
  "Return configurations and client observations without loading or starting servers.
The plist contains :LOADED-P, a copied :CONFIGURATIONS list, and :CLIENTS rows
with :SERVER, :ROOT and :LIVE-P. No mutable manager collection is returned."
  (with-recursive-lock-held ((lsp-manager-lock manager))
    (let ((clients nil))
      (maphash (lambda (key client)
                 (declare (ignore key))
                 (let ((transport (lsp-client-transport client)))
                   (push (list :server (copy-seq (lsp-server-configuration-name
                                                  (lsp-client-configuration client)))
                               :root   (lsp-client-root client)
                               :live-p (and transport (lsp-transport-live-p transport) t))
                         clients)))
               (lsp-manager-clients manager))
      (list :loaded-p       (lsp-manager-loaded-p manager)
            :configurations (copy-list (lsp-manager-configurations manager))
            :clients        (nreverse clients)))))
