(in-package #:proto)

;;; Parsed pdf walker








;; FIXME: Text seems to be stuck in pdf-stream objects. Not sure how much work to extract...
;; leaving for now
;; see: https://github.com/py-pdf/pypdf

(defgeneric children ())

(defmethod children ((obj pdf::document))
  (slot-value obj 'pdf::objects))

(defmethod children ((obj pdf::indirect-object))
  (let* ((content (slot-value obj 'pdf::content))
         (alist (when (eq (type-of content) 'pdf::dictionary)
                  (slot-value content 'pdf::dict-values)))) fS
    (print "in indrect children")
    (print alist)
    (if alist
        (cdr (assoc "/Contents" alist :test #'equal))
        (if content
            (list content)
            (progn
              (print "empty-indirect:")
              (print obj)
              nil)))))

(defmethod children ((obj pdf::pdf-stream))
  (declare (ignore obj))
  nil)

(defmethod children ((obj pdf::page-node))
  (slot-value obj 'pdf::pages))

(defmethod children ((obj integer))
  nil)

(defmethod children (obj)
  (print "unknown object:")
  (print obj)
  nil)



(defun walk-pdf (pobj func)
  (funcall func pobj)
  (let ((ch (children pobj)))
    (typecase ch
      (list (mapcan (alexandria:rcurry #'walk-pdf func) ch))
      (sequence (map 'list (alexandria:rcurry #'walk-pdf func) ch))
      (pdf::indirect-object (walk-pdf ch func)))))



