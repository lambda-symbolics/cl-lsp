(in-package #:cl-lsp)

(-> test-lsp-workspace-plan-composition () boolean)
(defun test-lsp-workspace-plan-composition ()
  "Compose ordered resource overlays without mutation and capture every addressed URI."
  (let* ((source      "file:///source.txt")
         (target      "file:///target.txt")
         (missing     "file:///missing.txt")
         (calls       (make-hash-table :test #'equal))
         (annotations (json-object "move" (json-object "label" "Move" "needsConfirmation" t)))
         (plan        (json-object "positionEncoding" "utf-16" "annotations" annotations
                                   "operations" (vector (json-object "kind" "delete" "uri" target))))
         (operations  (vector (json-object "kind" "rename" "oldUri" source "newUri" target
                                           "annotationId" "move"))))
    (flet ((snapshot (uri)
             (incf (gethash uri calls 0))
             (cond
               ((equal uri source)
                (values "source" 1 ':file))
               ((equal uri target)
                (values "target" 1 ':file))
               (t
                (values nil nil ':missing)))))
      (let* ((combined (lsp-workspace-plan-append plan operations :snapshot #'snapshot))
             (effects  (lsp-materialize-workspace-plan combined :snapshot #'snapshot))
             (effect   (find target effects :key (lambda (row) (json-get row "uri")) :test #'equal)))
        (tests--assert (= 1 (length (json-get plan "operations"))) "input plan is unchanged")
        (tests--assert (and (equal "source" (json-get effect "text"))
                            (equal source (json-get effect "originUri"))) "ordered donor origin")
        (tests--assert (json-get (json-get (json-get combined "annotations") "move")
                                 "needsConfirmation") "annotations are preserved")
        (setf (gethash "oldUri" (aref (json-get combined "operations") 1)) "changed")
        (tests--assert (equal source (json-get (aref operations 0) "oldUri")) "no operation alias"))
      (tests--assert
       (let ((*lsp-maximum-workspace-operations* 1))
         (handler-case (progn (lsp-workspace-plan-append plan operations :snapshot #'snapshot) nil)
           (lsp-protocol-error () t))) "combined operation bound")
      (clrhash calls)
      (let ((effects
              (lsp-materialize-workspace-plan
               (json-object "positionEncoding" "utf-16" "operations"
                            (vector (json-object "kind" "create" "uri" source
                                                 "options" (json-object "ignoreIfExists" t))
                                    (json-object "kind" "rename" "oldUri" source "newUri" source)
                                    (json-object "kind" "delete" "uri" missing
                                                 "options" (json-object "ignoreIfNotExists" t))))
               :snapshot #'snapshot)))
        (tests--assert (zerop (length effects)) "conditional operations have no effects")
        (tests--assert (and (= 2 (hash-table-count calls))
                            (= 1 (gethash source calls)) (= 1 (gethash missing calls)))
                       "all addressed initial URIs captured exactly once"))))
  t)
