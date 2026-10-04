(defpackage #:cl-lsp
  (:use #:cl)
  (:import-from #:serapeum #:->)
  (:import-from #:argo
                #:json-object #:json-object-p #:json-get #:json-get-present
                #:json-false #:json-string= #:json-decode #:json-encode-utf8
                #:json-error #:json-limit-exceeded #:json-limit-exceeded-limit
                #:make-json-limits)
  (:import-from #:uiop #:process-alive-p)
  (:import-from #:bordeaux-threads
                #:make-lock #:make-recursive-lock #:make-condition-variable
                #:with-lock-held #:with-recursive-lock-held
                #:condition-wait #:condition-notify
                #:make-thread #:current-thread #:thread-alive-p #:destroy-thread)
  (:export
   #:json-object #:json-get #:json-get-present
   #:*lsp-maximum-header-bytes* #:*lsp-maximum-body-bytes*
   #:*lsp-maximum-stderr-characters* #:*lsp-maximum-callbacks*
   #:*lsp-maximum-document-bytes* #:*lsp-maximum-open-documents*
   #:*lsp-maximum-clients* #:*lsp-maximum-diagnostics*
   #:lsp-error #:lsp-error-message
   #:lsp-rpc-error #:lsp-rpc-error-code #:lsp-rpc-error-data
   #:lsp-timeout #:lsp-timeout-request-id
   #:lsp-server-configuration
   #:lsp-server-configuration-name #:lsp-server-configuration-command
   #:lsp-server-configuration-arguments #:lsp-server-configuration-extensions
   #:lsp-server-configuration-language-id #:lsp-server-configuration-root-markers
   #:lsp-server-configuration-initialization-options
   #:lsp-server-configuration-settings #:lsp-server-configuration-timeout-seconds
   #:lsp-server-configuration-disabled-p
   #:lsp-read-message #:lsp-write-message
   #:lsp-transport #:lsp-transport-open #:lsp-transport-request
   #:lsp-transport-notify #:lsp-transport-close #:lsp-transport-detach
   #:lsp-transport-live-p #:lsp-transport-process
   #:lsp-transport-input #:lsp-transport-output #:lsp-transport-error-output
   #:lsp-transport-failure #:lsp-transport-stderr #:lsp-transport-threads
   #:lsp-transport-next-id #:lsp-transport-pending
   #:lsp-client #:lsp-client-name #:lsp-client-version
   #:lsp-client-start #:lsp-client-close #:lsp-client-transport
   #:lsp-client-root #:lsp-client-configuration #:lsp-client-capabilities
   #:lsp-client-documents #:lsp-client-lock
   #:lsp-client-sync #:lsp-client-resync #:lsp-client-diagnostics
   #:lsp-document #:lsp-document-path #:lsp-document-text #:lsp-document-version
   #:lsp-document-push-report #:lsp-document-pull-report #:lsp-document-diagnostics
   #:lsp-diagnostic-report #:lsp-diagnostic-report-items
   #:lsp-diagnostic-report-received-p #:lsp-diagnostic-report-versioned-p
   #:lsp-diagnostic-report-received-at #:lsp-diagnostic-report-truncated-p
   #:lsp-manager #:lsp-manager-client-name #:lsp-manager-client-version
   #:lsp-manager-client #:lsp-manager-close #:lsp-manager-clients
   #:lsp-manager-configurations #:lsp-manager-loaded-p #:lsp-manager-lock
   #:*lsp-maximum-root-depth* #:lsp-project-root
   #:lsp-path-uri #:lsp-position
   #:*lsp-configuration-version* #:*lsp-configuration-maximum-bytes*
   #:*lsp-configuration-maximum-servers* #:*lsp-configuration-maximum-timeout-seconds*
   #:*lsp-configuration-maximum-string-characters*
   #:*lsp-configuration-maximum-list-elements*
   #:lsp-configuration-error #:lsp-configuration-error-pathname
   #:lsp-configuration-error-server-name #:lsp-configuration-error-field
   #:lsp-configuration-error-cause #:lsp-read-configurations
   #:lsp-text-position
   #:*lsp-query-operations* #:*lsp-maximum-symbol-query-characters*
   #:lsp-client-supports-p #:lsp-client-query
   #:lsp-configurations-for-path #:lsp-manager-map-file))

(in-package #:cl-lsp)

(deftype option (type)
  "An optional value of TYPE."
  `(or null ,type))
