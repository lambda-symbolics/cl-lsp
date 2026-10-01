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
