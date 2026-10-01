(in-package #:cl-lsp)

;;;; -- Test Fixtures --

(defparameter *test-checks* 0
  "Assertions recorded by the current test run.")

(define-condition lsp-test-failure (error)
  ((message :initarg :message :reader lsp-test-failure-message
            :documentation "The failed assertion or case summary."))
  (:report (lambda (condition stream)
             (write-string (lsp-test-failure-message condition) stream)))
  (:documentation "A failed cl-lsp behavioral check."))

(-> tests--assert (t string) boolean)
(defun tests--assert (value description)
  "Record VALUE as one assertion or signal its DESCRIPTION."
  (incf *test-checks*)
  (unless value (error 'lsp-test-failure :message description))
  t)

(-> tests--call-with-replacements (list function) t)
(defun tests--call-with-replacements (replacements function)
  "Call FUNCTION with scoped function replacements, restoring every binding."
  (let ((originals nil))
    (unwind-protect
         (progn
           (dolist (replacement replacements)
             (let ((name (first replacement)))
               (push (cons name (symbol-function name)) originals)
               (setf (symbol-function name) (second replacement))))
           (funcall function))
      (dolist (original originals)
        (setf (symbol-function (first original)) (rest original))))))

(defmacro with-test-directory ((root) &body body)
  "Bind ROOT to a fresh temporary directory and remove it on every exit."
  `(let ((,root (merge-pathnames
                (format nil "cl-lsp-~36R-~36R/" (get-universal-time)
                        (random (expt 36 12)))
                (uiop:temporary-directory))))
     (ensure-directories-exist (merge-pathnames "fixture" ,root))
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,root :validate t :if-does-not-exist ':ignore))))

(-> tests--write-text (pathname string) pathname)
(defun tests--write-text (path text)
  "Write UTF-8 TEXT to a test-owned PATH."
  (ensure-directories-exist path)
  (with-open-file (output path :direction ':output :if-exists ':supersede
                              :if-does-not-exist ':create :external-format ':utf-8)
    (write-string text output))
  path)
