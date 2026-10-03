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
    (loop repeat *lsp-maximum-root-depth*
          for candidate = directory
            then (uiop:pathname-parent-directory-pathname candidate)
          when (some (lambda (marker) (lsp--marker-present-p candidate marker))
                     markers)
            return candidate
          when (or (equal candidate workspace)
                   (equal candidate
                          (uiop:pathname-parent-directory-pathname candidate)))
            return workspace
          finally (return workspace))))
