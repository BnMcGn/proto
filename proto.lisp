;;;; gadgets.lisp

(in-package #:proto)

;;; "gadgets" goes here. Hacks and glory await!


(defun match-a-symbol (item symbols)
  (first-match (lambda (x) (string-equal x item)) symbols))

(defun match-various (matchables)
  "Returns a function to check if an input string - presumably input from a user - is approximately a member of the matchables list. Matchables can contain symbols, numbers or strings. Match-various will not intern the user input before comparing it to the symbols, preventing mischievous users from stuffing the symbol table."
  (multiple-value-bind (symbols others)
      (splitfilter #'symbolp matchables)
    (lambda (test-string)
      (multiple-value-bind (val sig)
          (match-a-symbol test-string symbols)
        (if sig
            (values val sig)
            (let ((res (member (string-unless-number test-string)
                               others :test #'equal)))
              (if res
                  (values (car res) t)
                  (values nil nil))))))))

(defmacro with-any/all/none (&body body)
  (let ((name (gensym)))
    `(block ,name
       (labels ((returner (rval) (return-from ,name rval)))
         (macrolet ((any (test &optional (retval t))
                      `(when ,test (returner ,retval)))
                    (all (test &optional retval)
                      `(when (not ,test) (returner ,retval)))
                    (none (test &optional retval)
                      `(when ,test (returner ,retval))))
           ,@body)))))

                                        ;anaphoric macro: 2nd expr wraps first (which is contained in it) if true,
                                        ;else expr run plain.
(defmacro awrap-expr-if (pred expr &body cond-expr-with-var-it)
  `(if ,pred
       (funcall
        (lambda (it)
          ,@cond-expr-with-var-it)
        ,expr)
       ,expr))

(defmacro aif2only (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if ,win ,then ,else))))

(defun rotating-cache (&optional initial)
  (lambda (newval)
    (prog1
        initial
      (setf initial newval))))

(defun tracker-same (&key initial (test #'equal))
  (lambda (thing)
    (prog1
        (funcall test initial thing)
      (setf initial thing))))

(defun tracker-different (&key initial (test (complement #'equal)))
  (tracker-same :initial initial :test test))


(defmacro collecting-set ((&key union intersection difference (returns 'list)
                                (test '(function eql)))
                          &body body)
  (with-gensyms (data intersection-d difference-d x)
    `(,(case returns
             (list 'alexandria:hash-table-keys)
             (hash-table 'identity)
             (otherwise (error "Return type not found")))
       (let ((,data (make-hash-table :test ,test))
             ,@(when intersection
                     `((,intersection-d
                        (%set-up-hash-table ,intersection ,test))))
             ,@(when difference
                     `((,difference-d
                        (%set-up-hash-table ,difference ,test)))))
         (labels
             ((set< (item)
                ,@(when intersection
                        `((unless (nth-value 1 (gethash item ,intersection-d))
                            (return-from set<))))
                ,@(when difference
                        `((when (nth-value 1 (gethash item ,difference-d))
                            (return-from set<))))
                (setf (gethash item ,data) nil)))
           (dolist (,x ,union)
             (set< ,x))
           ,@body
           ,data)))))

(defmacro defclock (name ticks-per-second)
  "Defines a function that will return an integer number of ticks since it started."
  `(eval-always
     (def-as-func ,name (make-clock ,ticks-per-second))))

;;FIXME: This should go back in gadgets, but needs some testing and debugging
(defun extend-pathname (path &rest extensions)
  (let ((exts
         (apply #'concatenate 'list
                (mapcar (lambda (x)
                          (if (stringp x)
                              (list x)
                              (let ((pd (pathname-directory x)))
                                (unless (eq (car pd) :relative)
                                  (error "Extension must not be an absolute path"))
                                (cdr pd))))
                        extensions))))
    (make-pathname :defaults path
                   :directory (append (pathname-directory path) exts))))


;; Too ugly for gadgets
(defun map-tuples (&rest funcs-and-input/inputs)
  "Like mapcar, except that multiple functions are permitted, their output - per input element - being gathered as by list*. Map-tuples can be viewed as a combination of mapcar and pairlis. All parameters are presumed to be functions except the last, which is input:
   (map-tuples func1 func2... input1)
To use multiple input lists (like mapcar) insert the keyword :input between functions and inputs:
   (map-tuples func1 func2... :input input1 input2...)"
  (multiple-value-bind (funcs inputs)
      (multiple-value-bind (part1 part2)
          (part-on-true (curry #'eq :input) funcs-and-input/inputs)
        (if part2
            (values part1 (cdr part2))
            (values (butlast part1) (last part1))))
    (apply #'mapcar
           (lambda (&rest items)
             (apply #'list*
                    (mapcar (lambda (func) (apply func items)) funcs)))
           inputs)))

(defun apply-compose (&rest functions)
  (lambda (&rest whatever)
    (labels ((func (data funcs)
               (if funcs
                   (apply (car funcs) (func data (cdr funcs)))
                   whatever)))
      (func whatever functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Limited reader
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass limited-reader nil nil)
(defvar *limited-reader-predicate* nil)

(defmethod eclector.reader:interpret-symbol ((client limited-reader)
                                             input-stream package-indicator symbol-name internp )
  (if (funcall *limited-reader-predicate* symbol-name package-indicator)
      (call-next-method)
      (error "Found an illegal token!")))

(defmethod eclector.reader:interpret-symbol-token ((client limited-reader) input-stream token position-package-marker-1 position-package-marker-2)
  (call-next-method))

(defun limited-read-error (stream char)
  (declare (ignore stream char))
  (error "Unwanted macro char"))

(defparameter *limited-readtable* (copy-readtable nil))
(dolist (char '(#\# #\|))
  (set-macro-character char #'limited-read-error nil *limited-readtable*))

(defun limited-reader (stream symbol-predicate)
  (let ((eclector.reader:*client* (make-instance 'limited-reader))
        (*limited-reader-predicate* symbol-predicate)
        (*readtable* *limited-readtable*))
    (eclector.reader:read stream)))

#|

(defparameter fname (car (uiop:directory-files "~/tmp/opins/")))
(with-open-file (s fname) (let ((eclector.reader::*client* 'warflagger::wf-reader))(eclector.reader:read s)))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fuzzy searcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fuzzy-is-present (spec text test)
  (let ((spos 0))
    (dotimes (i (length text))
      (when (funcall test (elt text i) (elt spec spos))
        (incf spos))
      (when (eq spos (length spec))
        (return-from fuzzy-is-present text)))
    nil))


(defun fuzzy-search-indices (spec text &key max-length-ratio (max-results 0) (test #'eq))
  (when (and (not-empty spec) (not-empty text))
    (let* ((accum nil)
           (slen (length spec))
           (tlen (length text))
           (first (position (elt spec 0) text :test test))
           (last (position (nelt spec 0) text :test test :from-end t))
           (mlen (if max-length-ratio (floor (* slen max-length-ratio)) (- last first))))
      (when (or (< last first)
                (< (- last first) (length spec)))
        (return-from fuzzy-search-indices nil))
      (labels ((finish () (nreverse accum))
               (accumulate (n m)
                 (push (list n m) accum)
                 (when (eq (decf max-results) 0)
                   (return-from fuzzy-search-indices (finish)))))
        (let ((starts (remove-if-not
                       (lambda (i) (funcall test (elt text first) (elt text i)))
                       (range first (- (1+ last) (1- slen))))))
          (loop for rlen from slen to mlen
                do (dolist (start starts)
                     (when (< (+ start rlen) tlen)
                       (when (funcall test (elt text (+ start rlen)) (nelt spec 0))
                         (when (fuzzy-is-present spec (subseq text start (+ start rlen)) test)
                           (accumulate start (1+ (+ start rlen)))))))))
        (finish)))))

(defun fuzzy-search (spec text &key max-length-ratio (max-results 0) (test #'eq))
  (loop
    for (start end)
      in (fuzzy-search-indices
          spec text
          :max-length-ratio max-length-ratio :max-results max-results :test test)
    collect (subseq text start end)))
