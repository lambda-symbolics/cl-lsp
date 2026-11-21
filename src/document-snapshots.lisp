(in-package #:cl-lsp)

;;;; -- Frozen Synchronized Documents --

(defclass lsp-document-stamp ()
  ((session :initarg :session :reader lsp-document-stamp-session
            :documentation "Opaque client lifecycle identity at capture.")
   (transport :initarg :transport :reader lsp-document-stamp-transport
              :documentation "Exact request transport at capture.")
   (identity :initarg :identity :reader lsp-document-stamp-identity
             :documentation "Opaque document identity, or NIL for an absent URI.")
   (version :initarg :version :reader lsp-document-stamp-version
            :documentation "Synchronized version, or NIL for an absent URI."))
  (:documentation "An opaque freshness token retaining neither document text nor a document object."))

(-> lsp-client-document-snapshot (lsp-client) function)
(defun lsp-client-document-snapshot (client)
  "Freeze CLIENT's synchronized documents under its lock, returning a URI lookup.

The lookup returns four values: text, version, :FILE or :MISSING, and an opaque
freshness stamp. Empty text is a present file. Treat returned text as read-only.
Retain only stamps for addressed URIs after a request, not this lookup, which
owns all frozen texts. The lookup performs no filesystem access."
  (let ((documents (make-hash-table :test #'equal))
        (missing   nil))
    (with-lock-held ((lsp-client-lock client))
      (let ((session   (lsp-client-session client))
            (transport (lsp-client-transport client)))
        (setf missing (make-instance 'lsp-document-stamp :session session :transport transport
                                    :identity nil :version nil))
        (maphash
         (lambda (uri document)
           (setf (gethash uri documents)
                 (cons (copy-seq (lsp-document-text document))
                       (make-instance 'lsp-document-stamp :session session :transport transport
                                      :identity (lsp-document-identity document)
                                      :version  (lsp-document-version document)))))
         (lsp-client-documents client))))
    (lambda (uri)
      (let* ((entry (gethash uri documents))
             (stamp (if entry (rest entry) missing)))
        (values (first entry) (lsp-document-stamp-version stamp)
                (if entry ':file ':missing) stamp)))))

(-> lsp-client-session-snapshot (lsp-client) lsp-document-stamp)
(defun lsp-client-session-snapshot (client)
  "Return an opaque session stamp without retaining synchronized texts."
  (with-lock-held ((lsp-client-lock client))
    (make-instance 'lsp-document-stamp
                   :session (lsp-client-session client) :transport (lsp-client-transport client)
                   :identity nil :version nil)))

(-> lsp-client-snapshot-current-p
    (lsp-client lsp-document-stamp &key (:uri (or null string))) boolean)
(defun lsp-client-snapshot-current-p (client stamp &key uri)
  "Return whether CLIENT's session and optional URI still match STAMP.

An unrelated document change does not invalidate a URI stamp. Opening an absent
URI, closing or reopening its document, and replacing the transport do. Omit
URI to check only the session captured by LSP-CLIENT-SESSION-SNAPSHOT."
  (with-lock-held ((lsp-client-lock client))
    (let ((document (and uri (gethash uri (lsp-client-documents client)))))
      (and (eq (lsp-document-stamp-session stamp) (lsp-client-session client))
           (eq (lsp-document-stamp-transport stamp) (lsp-client-transport client))
           (or (null uri)
               (and (eq (lsp-document-stamp-identity stamp)
                        (and document (lsp-document-identity document)))
                    (eql (lsp-document-stamp-version stamp)
                         (and document (lsp-document-version document)))))
           t))))
