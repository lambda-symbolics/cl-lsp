(in-package #:cl-lsp)

;;;; -- Capability-Gated Semantic Proposals --

(-> lsp-semantic--capability (lsp-client string &rest string) t)
(-> lsp-semantic--request (lsp-client string json-object) t)
(-> lsp-semantic--document-params (lsp-document t) json-object)
(-> lsp-semantic--snapshot (lsp-client (option function)) function)
(-> lsp-semantic--normalize (lsp-client t (option function)) json-object)
(-> lsp-semantic--action (lsp-client t (option function)) json-object)

(defun lsp-semantic--capability (client operation &rest path)
  "Require a true or options-object capability at PATH for OPERATION."
  (let ((value (reduce (lambda (object name)
                        (and (json-object-p object) (json-get object name)))
                      path :initial-value (lsp-client-capabilities client))))
    (unless (or (eq value t) (json-object-p value))
      (error 'lsp-unsupported :operation operation
             :message (format nil "Language server does not advertise ~A." operation)))
    value))

(defun lsp-semantic--request (client method params)
  "Use CLIENT's bounded request lifecycle for METHOD and PARAMS."
  (lsp-transport-request (lsp-client-transport client) method params
                         :timeout (lsp-server-configuration-timeout-seconds
                                   (lsp-client-configuration client))))

(defun lsp-semantic--document-params (document position)
  "Validate POSITION against DOCUMENT and construct protocol parameters."
  (lsp-edit--position-offset (lsp-document-text document) position)
  (json-object "textDocument" (json-object "uri" (lsp-path-uri (lsp-document-path document)))
               "position" position))

(defun lsp-semantic--snapshot (client snapshot)
  "Use explicit SNAPSHOT, otherwise copy synchronized document text and versions."
  (or snapshot
      (let ((documents (make-hash-table :test #'equal)))
        (with-lock-held ((lsp-client-lock client))
          (maphash (lambda (uri document)
                     (setf (gethash uri documents)
                           (cons (lsp-document-text document) (lsp-document-version document))))
                   (lsp-client-documents client)))
        (lambda (uri)
          (let ((entry (gethash uri documents)))
            (values (first entry) (rest entry)))))))

(defun lsp-semantic--normalize (client edit snapshot)
  "Validate EDIT against supplied or synchronized snapshots."
  (lsp-normalize-workspace-edit
   edit :snapshot (lsp-semantic--snapshot client snapshot)
   :position-encoding (json-get (lsp-client-capabilities client) "positionEncoding" "utf-16")))

(-> lsp-client-prepare-rename (lsp-client lsp-document json-object) t)
(defun lsp-client-prepare-rename (client document position)
  "Ask whether a symbol at POSITION can be renamed, returning null or preparation.

Return {range, startOffset, endOffset, placeholder?} for explicit ranges, or
{defaultBehavior:true}. Null means the server cannot prepare this rename."
  (lsp-semantic--capability client "prepare-rename" "renameProvider" "prepareProvider")
  (let ((result (lsp-semantic--request client "textDocument/prepareRename"
                                       (lsp-semantic--document-params document position))))
    (when (null result) (return-from lsp-client-prepare-rename nil))
    (lsp-edit--object result "prepareRename")
    (cond
      ((nth-value 1 (json-get-present result "defaultBehavior"))
       (unless (eq (json-get result "defaultBehavior") t)
         (lsp-edit--invalid "defaultBehavior" "Preparation defaultBehavior must be true."))
       (json-object "defaultBehavior" t))
      (t
       (let* ((range (json-get result "range" result))
              (normalized (json-object "range" range)))
         (multiple-value-bind (start end) (lsp-edit--range (lsp-document-text document) range)
           (setf (gethash "startOffset" normalized) start
                 (gethash "endOffset" normalized) end))
         (when (and (nth-value 1 (gethash "range" result))
                    (not (nth-value 1 (gethash "placeholder" result))))
           (lsp-edit--invalid "placeholder" "A prepare-rename range object requires a placeholder."))
         (multiple-value-bind (placeholder present-p) (json-get-present result "placeholder")
           (when present-p
             (unless (stringp placeholder)
               (lsp-edit--invalid "placeholder" "Rename placeholder must be a string."))
             (setf (gethash "placeholder" normalized) placeholder)))
         normalized)))))

(-> lsp-client-rename (lsp-client lsp-document &key (:position json-object)
                           (:new-name string) (:snapshot (option function))) json-object)
(defun lsp-client-rename (client document &key position new-name snapshot)
  "Request a symbol rename and return its validated workspace proposal.

SNAPSHOT supplies text and document version for affected URIs not synchronized by
CLIENT. No command or edit is applied. Authority belongs to the caller."
  (lsp-semantic--capability client "rename" "renameProvider")
  (lsp-edit--string new-name "newName")
  (let ((params (lsp-semantic--document-params document position))
        (snapshot (lsp-semantic--snapshot client snapshot)))
    (setf (gethash "newName" params) new-name)
    (lsp-semantic--normalize client
                             (lsp-semantic--request client "textDocument/rename" params) snapshot)))

(defun lsp-semantic--action (client action snapshot)
  "Normalize a Command or CodeAction while retaining the raw action for resolution."
  (lsp-edit--object action "codeAction")
  (unless (stringp (json-get action "title"))
    (lsp-edit--invalid "title" "A code action or command requires a string title."))
  (let* ((command (gethash "command" action))
         (command-only-p (stringp command))
         (disabled (gethash "disabled" action))
         (provider (json-get (lsp-client-capabilities client) "codeActionProvider"))
         (resolve-p (and (json-object-p provider) (eq (json-get provider "resolveProvider") t))))
    (when command
      (if command-only-p
          (lsp-edit--string command "command")
          (progn
            (lsp-edit--object command "command")
            (unless (stringp (json-get command "title"))
              (lsp-edit--invalid "title" "A command requires a title."))
            (lsp-edit--string (json-get command "command") "command")))
      (multiple-value-bind (arguments present-p)
          (gethash "arguments" (if command-only-p action command))
        (when present-p (lsp-edit--array arguments "arguments"))))
    (when disabled
      (lsp-edit--object disabled "disabled")
      (unless (stringp (json-get disabled "reason"))
        (lsp-edit--invalid "disabled" "Disabled actions require a reason.")))
    (when (and command-only-p (json-get action "edit"))
      (lsp-edit--invalid "codeAction" "A Command cannot include a workspace edit."))
    (multiple-value-bind (kind present-p) (gethash "kind" action)
      (when (and present-p (not (stringp kind)))
        (lsp-edit--invalid "kind" "Code action kind must be a string.")))
    (multiple-value-bind (preferred present-p) (gethash "isPreferred" action)
      (when (and present-p (not (member preferred (list t (json-false)))))
        (lsp-edit--invalid "isPreferred" "Action preference must be a boolean.")))
    (when (nth-value 1 (gethash "edit" action))
      (lsp-edit--object (gethash "edit" action) "edit"))
    (json-object "action" action
                 "edit" (when (json-get action "edit")
                          (lsp-semantic--normalize client (json-get action "edit") snapshot))
                 "command" (if command-only-p action command)
                 "resolveSupported" (if (and resolve-p (not command-only-p)) t (json-false)))))

(-> lsp-client-code-actions (lsp-client lsp-document &key (:range json-object)
                                 (:diagnostics vector) (:only (option vector))
                                 (:trigger-kind integer) (:snapshot (option function))) vector)
(defun lsp-client-code-actions (client document &key range (diagnostics #()) only
                                                     (trigger-kind 1) snapshot)
  "Return normalized proposed code actions for RANGE without executing commands.

Each row contains raw ACTION for resolution, normalized EDIT or null, COMMAND or
null, and RESOLVESUPPORTED. Caller retains disabled/review metadata in ACTION."
  (lsp-semantic--capability client "code-actions" "codeActionProvider")
  (lsp-edit--range (lsp-document-text document) range)
  (lsp-edit--array diagnostics "diagnostics")
  (unless (member trigger-kind '(1 2))
    (lsp-edit--invalid "triggerKind" "Code action trigger kind must be 1 or 2."))
  (let ((context (json-object "diagnostics" diagnostics "triggerKind" trigger-kind))
        (snapshot (lsp-semantic--snapshot client snapshot)))
    (when only
      (lsp-edit--array only "only")
      (loop for kind across only do (lsp-edit--string kind "only"))
      (setf (gethash "only" context) only))
    (let ((result (lsp-semantic--request
                   client "textDocument/codeAction"
                   (json-object "textDocument"
                                (json-object "uri" (lsp-path-uri (lsp-document-path document)))
                                "range" range "context" context))))
      (if (null result)
          #()
          (map 'vector (lambda (action) (lsp-semantic--action client action snapshot))
               (lsp-edit--array result "codeActions"))))))

(-> lsp-client-resolve-code-action (lsp-client json-object &key (:snapshot (option function))) json-object)
(defun lsp-client-resolve-code-action (client action &key snapshot)
  "Resolve raw ACTION from a code-action row and return the normalized result.

Opaque server DATA is forwarded unchanged. Commands are never executed."
  (lsp-semantic--capability client "resolve-code-action" "codeActionProvider" "resolveProvider")
  (lsp-edit--object action "codeAction")
  (when (stringp (json-get action "command"))
    (lsp-edit--invalid "codeAction" "Commands cannot be resolved as code actions."))
  (let ((snapshot (lsp-semantic--snapshot client snapshot)))
    (lsp-semantic--action client
                          (lsp-semantic--request client "codeAction/resolve" action) snapshot)))

(-> lsp-client-will-rename-files (lsp-client vector &key (:snapshot (option function))
                                                       (:file-kind (option function))) json-object)
(defun lsp-client-will-rename-files (client files &key snapshot file-kind)
  "Prepare file moves and return related text/resource changes as a proposal.

FILES is a vector of {oldUri,newUri}. Registration filters select matching moves;
FILE-KIND receives the source URI and returns :FILE or :FOLDER when required.
The requested physical moves are not added to the server's WorkspaceEdit; caller
schedules those moves after these edits. No host writes or notifications occur."
  (let* ((capability (lsp-semantic--capability
                      client "will-rename-files" "workspace" "fileOperations" "willRename"))
         (filters (lsp-edit--array (json-get (lsp-edit--object capability "willRename") "filters")
                                  "filters"))
         (snapshot (lsp-semantic--snapshot client snapshot)))
    (lsp-edit--array files "files")
    (loop for file across files
          do (lsp-edit--object file "fileRename")
             (lsp-edit--uri (json-get file "oldUri"))
             (lsp-edit--uri (json-get file "newUri")))
    (let ((selected
            (remove-if-not
             (lambda (file)
               (let* ((old-uri (json-get file "oldUri"))
                      (source-kind (when file-kind
                                     (lambda (uri)
                                       (declare (ignore uri))
                                       (funcall file-kind old-uri)))))
                 (some (lambda (filter)
                         (or (lsp-file-filter--matches-p filter old-uri source-kind)
                             (lsp-file-filter--matches-p filter (json-get file "newUri") source-kind)))
                       filters)))
             files)))
      (lsp-semantic--normalize
       client (when (plusp (length selected))
                (lsp-semantic--request client "workspace/willRenameFiles"
                                       (json-object "files" selected)))
       snapshot))))
