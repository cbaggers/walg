;;;; walg.asd

(asdf:defsystem #:walg
  :description "Describe walg here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :serial t
  :depends-on (#:defclass-triv #:fn)
  :components ((:file "package")
               (:file "base")
               (:file "meat")))
