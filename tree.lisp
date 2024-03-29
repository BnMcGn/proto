;;; Borrowed from https://github.com/vseloved/rutils
;;; See for license.

(in-package :proto)

(defvar *tree-leaf-p* nil "Is the node being processed a leaf or a branch?")
(defvar *tree-stack* nil "Pointers to the parentage of the current node. Includes current node.")
(defvar *tree-index-stack* nil "Indices that lead to the address of the current node.")
(defvar *tree-process-branches* t)
(defvar *tree-process-leaves* t)
(defvar *tree-breadth-first* nil)
(defparameter *tree-leaf-test* nil)
(defparameter *tree-branch-filter* nil)

(defun %proc-branch (branch exec)
  (collecting
      (let ((stor nil))
        (loop
          for item-tmp in branch
           for i from 0
           do
             (let* ((*tree-stack* (cons item-tmp *tree-stack*))
                    (*tree-index-stack* (cons i *tree-index-stack*))
                    (item item-tmp)
                    (ts *tree-stack*)
                    (tis *tree-index-stack*))
               (if (funcall *tree-leaf-test* item)
                   (when *tree-process-leaves*
                     (collect
                         (let ((*tree-leaf-p* t))
                           (lambda ()
                             (let ((*tree-stack* ts)
                                   (*tree-index-stack* tis))
                               (funcall exec item)
                               nil)))))
                   (progn
                     (when *tree-process-branches*
                       (collect
                           (let ((*tree-leaf-p* nil))
                             (lambda ()
                               (let ((*tree-stack* ts)
                                     (*tree-index-stack* tis))
                                 (funcall exec item)
                                 nil)))))
                     ;;FIXME: prev will want to be able to effect how and if of
                     ;; execution of following.
                     (let ((sub
                            (lambda ()
                              (let ((*tree-stack* ts)
                                    (*tree-index-stack* tis))
                                (let ((res (%proc-branch
                                            (funcall
                                             (or *tree-branch-filter* #'identity)
                                             item)
                                            exec)))
                                  (mapc #'funcall (butlast res))
                                  (last-car res))))))
                       (if *tree-breadth-first*
                           (push sub stor) ;; Store to execute at end
                           (collect sub))))))) ;; Execute as found
        (collect (nreverse stor)))))

;;FIXME: Doesn't support switching between depth and breadth first mid-tree.
(defun %handle-proc-branch-tail (tail)
  (collecting
      (dolist (item tail)
        (dolist (new (funcall item))
          (when (functionp new) (collect new))))))

(defun call-with-tree (func
                       tree
                       &key
                         (order :depth)
                         (proc-branch t)
                         proc-leaf
                         branch-filter
                         leaf-test)
  (unless (member order '(:depth :breadth))
    (error "Order must be :depth or :breadth"))
  (let ((*tree-process-branches* proc-branch)
        (*tree-process-leaves* proc-leaf)
        (*tree-breadth-first* (eq order :breadth))
        (*tree-leaf-test* (or leaf-test #'atom))
        (*tree-branch-filter* branch-filter))
    (let ((res (%proc-branch
                (funcall (or *tree-branch-filter* #'identity) tree) func)))
      (mapc #'funcall (butlast res))
      (loop
         with items = (last-car res)
         while items
         do (setf items (%handle-proc-branch-tail items))))))

(defmacro dotree ((var-for-leaf/branch
                   tree
                   &key
                   result
                   (order :depth)
                   (proc-branch t)
                   (proc-leaf nil)
                   branch-filter
                   leaf-test)
                  &body body)
  `(progn
     (call-with-tree
      (lambda (,var-for-leaf/branch) ,@body)
      ,tree
      :order ,order :proc-branch ,proc-branch :proc-leaf ,proc-leaf :branch-filter
      ,branch-filter :leaf-test ,leaf-test)
     ,result))

(defmacro doleaves ((node tree &optional result) &body body)
  "Iterate over each leaf NODE of the TREE in depth-first order.
   Optionally return RESULT."
  (with-gensyms (rec child)
    `(labels ((,rec (,node)
                (unless (atom ,node)
                  (dolist (,child ,node)
                    (if (atom ,child)
                        (let ((,node ,child))
                          ,@body)
                        (,rec ,child))))))
       (alexandria:when-let
           ((treex ,tree))
         (,rec treex))
       ,result)))


(defun maptree (fn tree)
  "Map a one-argument function FN over subtree of the TREE
   in depth-first order, using the output of FN as the continuing tree. Note
   that FN must continually return its input in order to map over the whole
   tree."
  (labels ((rec (node)
             (let ((res (funcall fn node)))
               (if (atom res)
                   res
                   (mapcar #'rec res)))))
    (when tree
      (rec tree))))

(defun mapleaves (fn tree)
  "Map a one-argument function FN over each leaf node of the TREE
   in depth-first order, returning a new tree with the same structure."
  (labels ((rec (node)
             (if (atom node)
                 (funcall fn node)
     (mapcar #'rec node))))
    (when tree
      (rec tree))))

(defun tree-size (tree)
  "Returns the number of nodes (internal & external) in the indicated TREE."
  (let ((acc 0))
    (dotree (_ tree)
      (declare (ignore _))
      (incf acc))
    acc))

(defun tree-depth (tree)
  "Returns the length of the largest of nodes from the root tree."
  (cond ((atom tree) 0)
        ((rest tree)
         (1+ (reduce #'max (cons 1 (mapcar #'tree-depth (rest tree))))))
        (t 1)))

;;;End borrowed

(defun mapbranch (fn tree)
  "Like maptree, but atoms will not be passed to the function"
  (labels ((rec (node)
             (let ((res (funcall fn node)))
               (if (atom res)
                   res
                   (mapcar (lambda (x)
                             (if (listp x)
                                 (rec x)
                                 x))
                           res)))))
    (if (listp tree)
        (rec tree)
        tree)))

(defun leaves-search-replace (tree &key (test #'eql) match value
          valuefunc predicate)
  (let ((val (if valuefunc
     valuefunc
     (lambda (x) (declare (ignore x)) value)))
  (tst (if predicate
     predicate
     (lambda (x) (funcall test x match)))))
    (mapleaves
     (lambda (x)
       (if (funcall tst x)
     (funcall val x)
     x))
     tree)))

(defun tree-search-replace (tree &key (test #'eql)
          (key #'identity)
          match value
          valuefunc predicate)
  (let ((val (if valuefunc
     valuefunc
     (lambda (x) (declare (ignore x)) value)))
  (tst (if predicate
     predicate
     (lambda (x) (funcall test x match))))
  (prunings nil))
    (let ((res
     (maptree
      (lambda (x)
        (if (funcall tst (funcall key x))
      (progn
        (push x prunings)
        (funcall val x))
      x))
      tree)))
      (apply #'values res (nreverse prunings)))))

(defun collect-by-feature (items feature-func)
  "Group the items in a hash table by the key found by feature-func."
  (collecting-hash-table (:mode :append :test 'equal)
    (dolist (itm items)
      (collect (funcall feature-func itm) itm))))

(defun tree-by-parent (items feature-func &key root
                                             (format #'identity) (identity-func #'identity))
  "Builds a tree from a collection of ITEMS. Each item must be able to identify its parent when passed to FEATURE-FUNC. Items at the top of the tree will normally return NIL. If another value will indicate the top of the tree, set it with :ROOT. Note that the root item will not be stored in the tree.

If parent items cannot be directly matched by the value returned from FEATURE-FUNC, you may specify a matchable value with :IDENTITY-FUNC.

Items will be stored in the tree as found. To change this behavior, supply a transformation function with :FORMAT."
  (let ((data (collect-by-feature items feature-func)))
    (labels ((construct-tree (node-key)
               (collecting
                 (dolist (item (gethash node-key data))
                   (collect
                       (cons (funcall format item)
                             (construct-tree
                              (funcall identity-func item))))))))
      (construct-tree root))))

(defun tree-by-children (root children-func &key (format #'identity))
  "Builds a tree from ROOT in a situation where each node can identify its own children. CHILDREN-FUNC, given a node, should return a list of the node's children. You may customize the storage of nodes in the tree with the :FORMAT function"
  (let ((children (funcall children-func root)))
    (if children
        (cons (funcall format root)
              (mapcar (lambda (x) (tree-by-children x children-func :format format)) children))
        (funcall format root))))
