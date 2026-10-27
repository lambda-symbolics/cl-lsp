(in-package #:cl-lsp)

;;;; -- Types --

(deftype option (type)
  "An optional value of TYPE."
  `(or null ,type))
