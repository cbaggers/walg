;;;; walg.asd

(asdf:defsystem #:walg
  :description "Describe walg here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:defclass-triv #:fn)
  :components ((:file "package")
               (:file "base")
               (:file "meat")))
