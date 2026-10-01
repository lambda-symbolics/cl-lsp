(in-package #:cl-lsp)

(deftype json-object () '(satisfies hash-table-p))

(-> json-object-p (t) boolean)
(defun json-object-p (value)
  "Return true when VALUE is a JSON object representation."
  (hash-table-p value))

(defparameter *json-decoded-false*
  ':json-false
  "The portable internal marker preserving decoded JSON false across encoding.")

(-> json-object (&rest t) json-object)

(defun json-object (&rest key-values)
  "Return a string-keyed JSON object built from alternating KEY-VALUES."
  (unless (evenp (length key-values))
    (error 'lsp-error :message
           "JSON objects require an even number of key and value arguments."))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (unless (stringp key)
               (error 'lsp-error :message
                      (format nil "JSON object key ~S is not a string."
                              key))) (setf (gethash key object) value))
    object))

(-> json-get-present (json-object string) (values t boolean))

(defun json-get-present (object key)
  "Return KEY from JSON OBJECT and whether the key is present.

Decoded JSON false is presented as NIL while its internal marker remains in
OBJECT so that re-encoding preserves the distinction from JSON null."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (values
     (if (eq value *json-decoded-false*)
         nil
         value)
     present-p)))

(-> json-get (json-object string &optional t) t)

(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when the key is absent.

Decoded JSON false is presented as NIL while its internal marker remains in
OBJECT so that re-encoding preserves the distinction from JSON null."
  (multiple-value-bind (value present-p)
      (json-get-present object key)
    (if present-p
        value
        default)))

(-> json-string= (t string) boolean)

(defun json-string= (value expected)
  "Return true when VALUE is the JSON string EXPECTED."
  (and (stringp value) (string= value expected)))

(-> json--encoding-value (t) t)

(defun json--encoding-value (value)
  "Return VALUE with decoded-false markers translated for Yason encoding."
  (cond ((eq value *json-decoded-false*) false)
        ((json-object-p value)
         (let ((copy
                (make-hash-table :test (hash-table-test value) :size
                                 (max 1 (hash-table-count value)))))
           (maphash
            (lambda (key child)
              (setf (gethash key copy) (json--encoding-value child)))
            value)
           copy))
        ((stringp value) value)
        ((vectorp value) (map 'vector #'json--encoding-value value))
        ((consp value) (mapcar #'json--encoding-value value)) (t value)))

(-> json-encode-utf8 (t) (vector (unsigned-byte 8)))

(defun json-encode-utf8 (value)
  "Encode VALUE directly as compact UTF-8 JSON octets."
  (let* ((octet-stream (flexi-streams:make-in-memory-output-stream))
         (character-stream
          (flexi-streams:make-flexi-stream octet-stream :external-format
                                           ':utf-8)))
    (unwind-protect
        (progn
         (yason:encode (json--encoding-value value) character-stream)
         (finish-output character-stream)
         (flexi-streams:get-output-stream-sequence octet-stream))
      (close character-stream))))

(-> json-decode (string) t)

(defun json-decode (source)
  "Decode one JSON value from SOURCE without conflating false and null."
  (let ((yason:*parse-json-arrays-as-vectors* t)
        (yason:*parse-json-booleans-as-symbols* t)
        (yason:true t)
        (false *json-decoded-false*))
    (yason:parse source)))

(-> json-object-source-p (t) boolean)

(defun json-object-source-p (source)
  "Return true when SOURCE contains exactly one JSON object and whitespace."
  (and (stringp source)
       (handler-case
        (with-input-from-string (stream source)
          (let ((yason:*parse-json-arrays-as-vectors* t)
                (yason:*parse-json-booleans-as-symbols* t)
                (yason:true t)
                (false *json-decoded-false*))
            (let ((value (yason:parse stream)))
              (loop for character = (peek-char nil stream nil nil)
                    while (and character
                               (member character
                                       '(#\Space #\Tab #\Newline #\Return)))
                    do (read-char stream))
              (and (json-object-p value)
                   (null (peek-char nil stream nil nil))))))
        (error nil nil))))
