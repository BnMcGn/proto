;;;; gadgets.asd

(asdf:defsystem #:proto
  :serial t
  :description "Describe gadgets here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-utilities #:alexandria #:uiop #:kebab
               #:cl-hash-util) 
  :components ((:file "package")
               (:file "early")
               (:file "collecting")
              ;; (:file "anaphorics")
               (:file "gadgets" :depends-on (;;"anaphorics" 
                                             "early" 
                                             "collecting"))
               (:file "dubious")
               (:file "tree")
               (:file "symtools")
               (:file "recompile")))

