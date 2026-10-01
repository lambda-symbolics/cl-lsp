(in-package #:cl-lsp)

(defclass lsp-server-configuration ()
  ((name :initarg :name :reader lsp-server-configuration-name :type string
         :documentation "Unique user-selected server name.")
   (command :initarg :command :reader lsp-server-configuration-command :type string
            :documentation "Trusted executable, resolved on the inherited PATH.")
   (arguments :initarg :arguments :initform nil :reader lsp-server-configuration-arguments :type list
              :documentation "Literal argv strings, without shell expansion.")
   (extensions :initarg :extensions :initform nil :reader lsp-server-configuration-extensions :type list
               :documentation "Filename suffixes handled by this server.")
   (language-id :initarg :language-id :reader lsp-server-configuration-language-id :type string
                :documentation "LSP language identifier sent in didOpen.")
   (root-markers :initarg :root-markers :initform nil :reader lsp-server-configuration-root-markers :type list
                 :documentation "Literal marker filenames used for nearest project-root selection.")
   (initialization-options :initarg :initialization-options :initform nil
                           :reader lsp-server-configuration-initialization-options :type (option hash-table)
                           :documentation "Decoded initializationOptions object, or JSON null.")
   (settings :initarg :settings :initform (json-object) :reader lsp-server-configuration-settings :type hash-table
             :documentation "Decoded workspace configuration object.")
   (timeout-seconds :initarg :timeout-seconds :initform 30
                    :reader lsp-server-configuration-timeout-seconds :type integer
                    :documentation "Startup and request deadline in seconds.")
   (disabled-p :initarg :disabled-p :initform nil :reader lsp-server-configuration-disabled-p :type boolean
               :documentation "Whether this entry is excluded from automatic selection."))
  (:documentation "One strict declarative LSP server configuration."))
