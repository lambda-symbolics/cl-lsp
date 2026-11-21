(in-package #:cl-lsp)

;;;; -- File URIs --

(-> lsp--encode-uri-path (string) string)
(defun lsp--encode-uri-path (name)
  "Percent-encode UTF-8 path segments while preserving slash separators."
  (format nil "~{~A~^/~}"
          (mapcar (lambda (part) (quri:url-encode part :encoding ':utf-8))
                  (uiop:split-string name :separator "/"))))

(-> lsp--windows-path-uri (string) string)
(defun lsp--windows-path-uri (name)
  "Encode a native drive or UNC path without escaping the drive colon."
  (let ((name (substitute #\/ #\\ name)))
    (if (uiop:string-prefix-p "//" name)
        (concatenate 'string "file:" (lsp--encode-uri-path name))
        (concatenate 'string "file:///" (subseq name 0 2)
                     (lsp--encode-uri-path (subseq name 2))))))

(-> lsp-path-uri (pathname) string)
(defun lsp-path-uri (path)
  "Encode an absolute local pathname as a file URI, including drive and UNC paths."
  (let ((name (uiop:native-namestring path)))
    (if (uiop:os-windows-p)
        (lsp--windows-path-uri name)
        (concatenate 'string "file://" (lsp--encode-uri-path name)))))

(-> lsp-file-uri-components (string) (values string t t t))
(defun lsp-file-uri-components (value)
  "Decode a file URI, returning path, authority, query and fragment.
Preserve the leading slash and drive colon; a literal plus is not a space.
Reject malformed escapes and UTF-8. No native pathname conversion, filesystem
access, authority policy or authorization is performed."
  (handler-case
      (let* ((uri  (quri:uri value))
             (path (quri:uri-path uri)))
        (unless (and (equalp "file" (quri:uri-scheme uri))
                     (stringp path) (uiop:string-prefix-p "/" path)
                     (every (lambda (character) (<= 33 (char-code character) 126)) value))
          (error 'lsp-error :message "Expected an absolute ASCII file URI."))
        ;; Quri's form decoder treats plus as space and accepts a trailing percent.
        ;; Validate escapes first, then decode each plus-separated path segment.
        (loop for index below (length path)
              when (char= (char path index) #\%)
                do (unless (and (< (+ index 2) (length path))
                                (digit-char-p (char path (1+ index)) 16)
                                (digit-char-p (char path (+ index 2)) 16))
                     (error 'lsp-error :message "Malformed file URI escape."))
                   (incf index 2))
        (let ((decoded (format nil "~{~A~^+~}"
                               (mapcar (lambda (part) (quri:url-decode part :encoding ':utf-8))
                                       (uiop:split-string path :separator "+")))))
          (when (find #\Null decoded)
            (error 'lsp-error :message "File URI path contains a null character."))
          (values decoded (quri:uri-authority uri) (quri:uri-query uri) (quri:uri-fragment uri))))
    (lsp-error (condition)
      (error condition))
    (error ()
      (error 'lsp-error :message "Malformed file URI or UTF-8 path."))))


;;;; -- Project Roots --

(defparameter *lsp-maximum-root-depth* 64
  "The maximum number of directories searched upward for a project root.")

(-> lsp--marker-present-p (pathname string) boolean)
(defun lsp--marker-present-p (directory marker)
  "Return true when DIRECTORY holds a file or directory named MARKER literally."
  (let ((candidate (merge-pathnames (uiop:parse-native-namestring marker)
                                    directory)))
    (and (or (uiop:file-exists-p candidate)
             (uiop:directory-exists-p candidate))
         t)))

(-> lsp-project-root (pathname pathname list) pathname)
(defun lsp-project-root (path workspace markers)
  "Return the nearest directory holding one of MARKERS, from PATH up to WORKSPACE.

PATH names a file, or a directory as a directory pathname, and WORKSPACE names
a directory. Both must be absolute
and canonical, with links already resolved, because containment is decided on
their namestrings. MARKERS are literal file or directory names such as
\"Cargo.toml\" or \".git\". The search stops at WORKSPACE and after
*LSP-MAXIMUM-ROOT-DEPTH* directories. WORKSPACE is returned when PATH lies
outside it or no marker is found."
  (let* ((workspace (uiop:ensure-directory-pathname workspace))
         (directory (if (uiop:directory-pathname-p path)
                        path
                        (uiop:pathname-directory-pathname path))))
    (unless (uiop:string-prefix-p (namestring workspace) (namestring directory))
      (return-from lsp-project-root workspace))
    (loop for candidate = directory
            then (uiop:pathname-parent-directory-pathname candidate)
          repeat *lsp-maximum-root-depth*
          when (some (lambda (marker) (lsp--marker-present-p candidate marker))
                     markers)
            return candidate
          when (or (equal candidate workspace)
                   (equal candidate
                          (uiop:pathname-parent-directory-pathname candidate)))
            return workspace
          finally (return workspace))))
