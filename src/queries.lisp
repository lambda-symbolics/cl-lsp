(in-package #:cl-lsp)

;;;; -- Positions --

(-> lsp-text-position (string integer integer) json-object)
(defun lsp-text-position (text line character)
  "Return the protocol position of zero-based LINE and UTF-16 CHARACTER in TEXT.

This is the inverse of LSP-POSITION. Signal LSP-ERROR when the coordinates lie
outside TEXT or split a UTF-16 surrogate pair."
  (unless (and (typep line '(integer 0)) (typep character '(integer 0)))
    (error 'lsp-error :message "LSP line and character must be non-negative integers."))
  (let ((current-line      0)
        (current-character 0))
    (loop for index from 0 to (length text)
          do (when (and (= current-line line) (= current-character character))
               (return-from lsp-text-position
                 (json-object "line" line "character" character)))
             (when (= index (length text))
               (return))
             (let ((value (char text index)))
               (cond
                 ((char= value #\Newline)
                  (incf current-line)
                  (setf current-character 0))
                 ((char= value #\Return)
                  (unless (and (< (1+ index) (length text))
                               (char= (char text (1+ index)) #\Newline))
                    (incf current-line)
                    (setf current-character 0)))
                 (t
                  (incf current-character (if (> (char-code value) #xffff) 2 1))))))
    (error 'lsp-error
           :message "LSP position is outside the document or splits a UTF-16 surrogate pair.")))


;;;; -- Capability-Gated Queries --

(defparameter *lsp-query-operations*
  '(("definition" "textDocument/definition" "definitionProvider" :position)
    ("references" "textDocument/references" "referencesProvider" :position)
    ("hover" "textDocument/hover" "hoverProvider" :position)
    ("implementation" "textDocument/implementation" "implementationProvider" :position)
    ("type-definition" "textDocument/typeDefinition" "typeDefinitionProvider" :position)
    ("document-symbols" "textDocument/documentSymbol" "documentSymbolProvider" :document)
    ("workspace-symbols" "workspace/symbol" "workspaceSymbolProvider" :query))
  "Query names, protocol methods, capabilities and input kinds.")

(-> lsp-query-input-kind (string) symbol)
(defun lsp-query-input-kind (operation)
  "Return :POSITION, :DOCUMENT or :QUERY for OPERATION, rejecting unknown names."
  (let ((entry (assoc operation *lsp-query-operations* :test #'equal)))
    (unless entry
      (error 'lsp-error :message (format nil "Unknown LSP query operation ~S." operation)))
    (fourth entry)))

(defparameter *lsp-maximum-symbol-query-characters* 1024
  "The longest workspace symbol query sent to a server.")

(-> lsp-client-supports-p (lsp-client string) boolean)
(defun lsp-client-supports-p (client capability)
  "Return true when CLIENT's server advertised CAPABILITY."
  (not (null (json-get (lsp-client-capabilities client) capability))))

(-> lsp-client-query (lsp-client string lsp-document &key (:position t) (:query t)) t)
(defun lsp-client-query (client operation document &key position query)
  "Run read-only query OPERATION, named in *LSP-QUERY-OPERATIONS*, about DOCUMENT.

POSITION, a protocol position, is required except for document and workspace
symbols; QUERY is the workspace symbol search text. Signal LSP-ERROR for an
unknown operation, a capability the server did not advertise, or missing input."
  (let ((specification (assoc operation *lsp-query-operations* :test #'equal))
        (params        (json-object)))
    (unless specification
      (error 'lsp-error :message (format nil "Unknown LSP query operation ~S." operation)))
    (unless (lsp-client-supports-p client (third specification))
      (error 'lsp-error :message (format nil "Language server does not support ~A." operation)))
    (cond
      ((eq (lsp-query-input-kind operation) ':query)
       (unless (and (stringp query) (<= (length query) *lsp-maximum-symbol-query-characters*))
         (error 'lsp-error
                :message (format nil "A workspace symbol query must be a string of at most ~D characters."
                                 *lsp-maximum-symbol-query-characters*)))
       (setf (gethash "query" params) query))
      (t
       (setf (gethash "textDocument" params)
             (json-object "uri" (lsp-path-uri (lsp-document-path document))))
       (when (eq (lsp-query-input-kind operation) ':position)
         (unless (json-object-p position)
           (error 'lsp-error :message (format nil "LSP ~A needs a position." operation)))
         (setf (gethash "position" params) position))))
    (when (string= operation "references")
      (setf (gethash "context" params) (json-object "includeDeclaration" t)))
    (lsp-transport-request (lsp-client-transport client) (second specification) params
                           :timeout (lsp-server-configuration-timeout-seconds
                                     (lsp-client-configuration client)))))


;;;; -- Extension Fan-Out --

(-> lsp-configurations-for-path (list pathname) list)
(defun lsp-configurations-for-path (configurations path)
  "Return the enabled CONFIGURATIONS with an extension that PATH's name ends with."
  (remove-if-not (lambda (configuration)
                   (and (not (lsp-server-configuration-disabled-p configuration))
                        (some (lambda (extension)
                                (uiop:string-suffix-p (namestring path) extension))
                              (lsp-server-configuration-extensions configuration))))
                 configurations))

(-> lsp-manager-map-file (lsp-manager pathname function &key (:boundary pathname)) list)
(defun lsp-manager-map-file (manager path function
                             &key (boundary (uiop:pathname-directory-pathname path)))
  "Call FUNCTION with a synchronized client and document for every server handling PATH.

Servers come from MANAGER's configurations, each rooted at its nearest marker
directory between PATH and BOUNDARY, which defaults to PATH's directory. Each
server is independent: return one plist per server, (:SERVER NAME :ROOT ROOT
:RESULT VALUE) or (:SERVER NAME :ERROR CONDITION). Signal LSP-ERROR when no
enabled server handles PATH."
  (with-recursive-lock-held ((lsp-manager-lock manager))
    (let ((configurations (lsp-configurations-for-path (lsp-manager-configurations manager)
                                                       path)))
      (unless configurations
        (error 'lsp-no-matching-server :path path
               :message                      "No enabled language server handles this file."))
      (mapcar (lambda (configuration)
                (let ((name (lsp-server-configuration-name configuration)))
                  (handler-case
                      (let* ((root   (lsp-project-root path boundary
                                                       (lsp-server-configuration-root-markers
                                                        configuration)))
                             (client (lsp-manager-client manager configuration root)))
                        (lsp-client-resync client)
                        (list :server name
                              :root   root
                              :result (funcall function client (lsp-client-sync client path))))
                    (error (condition)
                      (list :server name :error condition)))))
              configurations))))
