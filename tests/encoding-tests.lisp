(in-package #:cl-lsp)

;;;; -- URI and JSON Boundaries --

(-> test-lsp-file-uris () null)
(defun test-lsp-file-uris ()
  "Exercise UTF-8 URI segments, drive letters, UNC hosts, and native paths."
  (dolist (entry '(("C:\\src\\a b#%.lisp" "file:///C:/src/a%20b%23%25.lisp")
                   ("D:\\žluť\\😀.txt" "file:///D:/%C5%BElu%C5%A5/%F0%9F%98%80.txt")
                   ("\\\\server\\share\\a b.txt" "file://server/share/a%20b.txt")))
    (tests--assert (string= (second entry) (lsp--windows-path-uri (first entry)))
                   "Windows paths retain drive and UNC URI structure"))
  (tests--assert (string= "/a%20b/%C5%BE%23%25%5C.txt"
                          (lsp--encode-uri-path "/a b/ž#%\\.txt"))
                 "path segments encode literal backslashes and reserved characters")
  (with-test-directory (root)
    (let ((uri (lsp-path-uri (merge-pathnames "a b.txt" root))))
      (tests--assert (and (uiop:string-prefix-p "file:///" uri)
                          (uiop:string-suffix-p uri "a%20b.txt"))
                     "native absolute path produces a file URI")))
  nil)

(-> test-lsp-json-boundaries () null)
(defun test-lsp-json-boundaries ()
  "Accept one framed JSON object and reject anything else, including deep nesting."
  (tests--assert (json-object-p (lsp--decode-message " {\"a\":[1]} "))
                 "a frame holding one JSON object decodes")
  (let ((deep (format nil "{\"a\":~A1~A}"
                      (make-string 70 :initial-element #\[)
                      (make-string 70 :initial-element #\]))))
    (dolist (source (list "{}{}" "{} trailing" "[]" "null" "{\"unterminated\":\"x}" deep))
      (tests--assert (handler-case (progn (lsp--decode-message source) nil)
                       (lsp-error () t))
                     "frames contain exactly one complete, bounded JSON object")))
  nil)
