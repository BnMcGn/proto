;;;; package.lisp

(defpackage #:proto
  (:use #:cl #:cl-hash-util #:gadgets)
  (:import-from #:cl-utilities
                #:once-only
                #:split-sequence
                #:split-sequence-if
                #:split-sequence-if-not)
  (:import-from #:alexandria
                #:flatten
                #:compose
                #:curry
                #:rcurry
                #:with-gensyms)
  (:import-from #:uiop
                #:strcat)
  (:export
   #:tree-search-replace
   #:get-function-name-in-macro
   #:match-various
   #:keyword-value
   #:watch-for-recompile
   #:dependency-auto-watcher
   #:keyword-splitter
   #:tree-by-feature
   #:funcall-in-macro
   #:strip-keywords
   #:keywordize-foreign
   #:mapleaves
   #:dotree
   #:*tree-stack*
   #:collecting-set
   #:defclock
   #:extend-pathname
   #:set<
   #:map-tuples
   #:apply-compose))
