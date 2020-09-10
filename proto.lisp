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
