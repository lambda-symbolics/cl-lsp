(in-package #:cl-lsp)

;;;; -- Declarative Configuration Files --

(defparameter *lsp-configuration-version* 1
  "The declarative server configuration version LSP-READ-CONFIGURATIONS accepts.")

(defparameter *lsp-configuration-maximum-bytes* (* 256 1024)
  "The largest configuration file read.")

(defparameter *lsp-configuration-maximum-servers* 32
  "The most server definitions one configuration may hold.")

(defparameter *lsp-configuration-maximum-timeout-seconds* 120
  "The largest startup and request timeout a server may configure.")

(defparameter *lsp-configuration-maximum-string-characters* 8192
  "The longest string a configuration field may hold.")

(defparameter *lsp-configuration-maximum-list-elements* 128
  "The most entries one configuration list field may hold.")

(defparameter *lsp-configuration-server-keys*
  '(:name :command :arguments :extensions :language-id :root-markers
    :initialization-options :settings :timeout-seconds :disabled-p)
  "The keys one server definition may use.")

(define-condition lsp-configuration-error (lsp-error)
  ((pathname :initarg :pathname :initform nil :reader lsp-configuration-error-pathname
             :documentation "The configuration file involved.")
   (server-name :initarg :server-name :initform nil
                :reader lsp-configuration-error-server-name
                :documentation "The server definition involved, when known.")
   (field :initarg :field :initform nil :reader lsp-configuration-error-field
          :documentation "The invalid field, when known.")
   (cause :initarg :cause :initform nil :reader lsp-configuration-error-cause
          :documentation "The underlying reader or filesystem condition, when any."))
  (:documentation "A malformed declarative server configuration."))

(-> lsp-read-configurations (pathname) list)
(defun lsp-read-configurations (pathname)
  "Read the declarative server configuration file at PATHNAME.

The file holds one form, (:VERSION 1 :SERVERS (SERVER ...)), where each SERVER is
a property list of :NAME, :COMMAND, :LANGUAGE-ID and :EXTENSIONS, and optionally
:ARGUMENTS, :ROOT-MARKERS, :INITIALIZATION-OPTIONS and :SETTINGS (JSON object
text), :TIMEOUT-SECONDS and :DISABLED-P. The file is read through a closed
grammar with no evaluation and every bound above. Return the servers as
LSP-SERVER-CONFIGURATION objects in file order; signal LSP-CONFIGURATION-ERROR
for anything else."
  (let* ((form (lsp-configuration--read-form pathname))
         (properties (lsp-configuration--plist form '(:version :servers) pathname)))
    (unless (eql (lsp-configuration--property properties :version pathname :required-p t)
                 *lsp-configuration-version*)
      (lsp-configuration--fail (format nil "The configuration must use version ~D."
                                       *lsp-configuration-version*)
                               :pathname pathname :field :version))
    (let ((servers (lsp-configuration--property properties :servers pathname :required-p t))
          (seen (make-hash-table :test #'equal)))
      (unless (and (lsp-configuration--list-p servers #'identity)
                   (<= (length servers) *lsp-configuration-maximum-servers*))
        (lsp-configuration--fail (format nil ":SERVERS must be a list of at most ~D entries."
                                         *lsp-configuration-maximum-servers*)
                                 :pathname pathname :field :servers))
      (loop for server in servers
            for configuration = (lsp-configuration--server server pathname)
            for name = (lsp-server-configuration-name configuration)
            do (when (gethash name seen)
                 (lsp-configuration--fail "Duplicate server name."
                                          :pathname pathname :server-name name :field :name))
               (setf (gethash name seen) t)
            collect configuration))))

(-> lsp-configuration--fail (string &key (:pathname t) (:server-name t) (:field t) (:cause t)) nil)
(defun lsp-configuration--fail (message &key pathname server-name field cause)
  "Signal an LSP-CONFIGURATION-ERROR with its source context."
  (error 'lsp-configuration-error :message message :pathname pathname
                                  :server-name server-name :field field :cause cause))

(-> lsp-configuration--read-form (pathname) t)
(defun lsp-configuration--read-form (pathname)
  "Read the single declarative form at PATHNAME through a closed, bounded grammar."
  (handler-case
      (sexp-config:read-source-file
       pathname
       (sexp-config:make-source-grammar
        :label "The LSP configuration"
        :keywords (list* :version :servers *lsp-configuration-server-keys*)
        :maximum-depth 32
        :maximum-nodes 16384
        :maximum-string-characters *lsp-configuration-maximum-string-characters*
        :allowed-atom-predicate
        (lambda (value)
          (or (null value) (eq value t) (stringp value) (integerp value) (keywordp value))))
       :maximum-octets *lsp-configuration-maximum-bytes*)
    (sexp-config:sexp-config-error (cause)
      (lsp-configuration--fail (sexp-config:sexp-config-error-message cause)
                               :pathname pathname :cause cause))))

(-> lsp-configuration--plist (t list pathname &key (:server-name t)) list)
(defun lsp-configuration--plist (form allowed pathname &key server-name)
  "Return FORM when it is a property list using each key of ALLOWED at most once."
  (unless (and (listp form)
               (handler-case (evenp (list-length form)) (type-error () nil)))
    (lsp-configuration--fail "The LSP configuration needs an even, proper property list."
                             :pathname pathname :server-name server-name))
  (loop with seen = nil
        for (key nil) on form by #'cddr
        do (unless (member key allowed)
             (lsp-configuration--fail (format nil "Unknown LSP configuration key ~S." key)
                                      :pathname pathname :server-name server-name :field key))
           (when (member key seen)
             (lsp-configuration--fail (format nil "Duplicate LSP configuration key ~S." key)
                                      :pathname pathname :server-name server-name :field key))
           (push key seen))
  form)

(-> lsp-configuration--property (list keyword pathname &key (:required-p t) (:server-name t)) t)
(defun lsp-configuration--property (form key pathname &key required-p server-name)
  "Return FORM's KEY, signalling when a REQUIRED-P key is missing."
  (multiple-value-bind (indicator value)
      (get-properties form (list key))
    (when (and required-p (null indicator))
      (lsp-configuration--fail (format nil "Missing required LSP key ~S." key)
                               :pathname pathname :server-name server-name :field key))
    value))

(-> lsp-configuration--string-p (t &key (:empty-p t)) boolean)
(defun lsp-configuration--string-p (value &key empty-p)
  "Return true for a bounded string without NUL characters, nonempty unless EMPTY-P."
  (and (stringp value)
       (not (find (code-char 0) value))
       (<= (length value) *lsp-configuration-maximum-string-characters*)
       (or empty-p (plusp (length value)))))

(-> lsp-configuration--list-p (t function) boolean)
(defun lsp-configuration--list-p (value predicate)
  "Return true for a bounded proper list whose elements satisfy PREDICATE."
  (and (listp value)
       (handler-case (list-length value) (type-error () nil))
       (<= (length value) *lsp-configuration-maximum-list-elements*)
       (every predicate value)
       t))

(-> lsp-configuration--json-object (t pathname string keyword) (option hash-table))
(defun lsp-configuration--json-object (value pathname server-name field)
  "Return the JSON object VALUE's text decodes to, or NIL for a missing VALUE."
  (when value
    (unless (and (lsp-configuration--string-p value) (argo:json-object-source-p value))
      (lsp-configuration--fail (format nil "LSP ~S must be JSON object text." field)
                               :pathname pathname :server-name server-name :field field))
    (json-decode value)))

(-> lsp-configuration--server (t pathname) lsp-server-configuration)
(defun lsp-configuration--server (form pathname)
  "Validate one server definition FORM and return its configuration."
  (lsp-configuration--plist form *lsp-configuration-server-keys* pathname)
  (let* ((name (lsp-configuration--property form :name pathname :required-p t))
         (command (lsp-configuration--property form :command pathname :required-p t
                                                                       :server-name name))
         (language-id (lsp-configuration--property form :language-id pathname
                                                   :required-p t :server-name name))
         (arguments (lsp-configuration--property form :arguments pathname))
         (extensions (lsp-configuration--property form :extensions pathname))
         (root-markers (lsp-configuration--property form :root-markers pathname))
         (timeout (multiple-value-bind (indicator value)
                      (get-properties form '(:timeout-seconds))
                    (if indicator value 30)))
         (disabled-p (lsp-configuration--property form :disabled-p pathname)))
    (flet ((fail (message field)
             (lsp-configuration--fail message :pathname pathname
                                              :server-name (and (stringp name) name)
                                              :field field)))
      (loop for (value field) in (list (list name :name) (list command :command)
                                       (list language-id :language-id))
            do (unless (lsp-configuration--string-p value)
                 (fail (format nil "LSP ~S must be a bounded string." field) field)))
      (loop for (value field empty-p) in (list (list arguments :arguments t)
                                               (list extensions :extensions nil)
                                               (list root-markers :root-markers nil))
            do (unless (lsp-configuration--list-p
                        value (lambda (item) (lsp-configuration--string-p item :empty-p empty-p)))
                 (fail (format nil "LSP ~S must be a bounded list of strings." field) field)))
      (unless extensions
        (fail "LSP :EXTENSIONS must name at least one filename suffix." :extensions))
      (unless (every (lambda (marker)
                       (and (not (member marker '("." "..") :test #'string=))
                            (not (find-if (lambda (character) (find character "/\\:*?[]"))
                                          marker))))
                     root-markers)
        (fail "LSP :ROOT-MARKERS must be literal filenames." :root-markers))
      (unless (and (integerp timeout)
                   (<= 1 timeout *lsp-configuration-maximum-timeout-seconds*))
        (fail (format nil "LSP :TIMEOUT-SECONDS must be an integer from 1 through ~D."
                      *lsp-configuration-maximum-timeout-seconds*)
              :timeout-seconds))
      (unless (member disabled-p '(nil t))
        (fail "LSP :DISABLED-P must be exactly T or NIL." :disabled-p))
      (make-instance 'lsp-server-configuration
                     :name (copy-seq name)
                     :command (copy-seq command)
                     :arguments (mapcar #'copy-seq arguments)
                     :extensions (mapcar #'copy-seq extensions)
                     :language-id (copy-seq language-id)
                     :root-markers (mapcar #'copy-seq root-markers)
                     :initialization-options
                     (lsp-configuration--json-object
                      (lsp-configuration--property form :initialization-options pathname)
                      pathname name :initialization-options)
                     :settings (or (lsp-configuration--json-object
                                    (lsp-configuration--property form :settings pathname)
                                    pathname name :settings)
                                   (json-object))
                     :timeout-seconds timeout
                     :disabled-p disabled-p))))
