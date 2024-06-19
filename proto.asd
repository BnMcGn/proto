;;;; gadgets.asd

(asdf:defsystem #:proto
  :serial t
  :description "Describe gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-utilities #:alexandria #:uiop #:gadgets
               #:cl-hash-util #:kebab #:safe-read #:eclector 
               #:trivial-backtrace)
  :components ((:file "package")
               (:file "collecting")
               (:file "proto" :depends-on ( "collecting"))
               (:file "dubious")
               (:file "tree")
               (:file "symtools")
               (:file "recompile")))

