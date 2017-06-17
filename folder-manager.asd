;;;; folder-manager.asd

(asdf:defsystem #:folder-manager
  :description "Utility tool for managing project folders"
  :author "Marcin Radoszewski <moriturius@gmail.com>"
  :license "I don't know yet"
  :serial t
  :components ((:file "package")
               (:file "setup")
               (:file "fm"))
  :depends-on ("fset" "marshal" "cl-arrows" "rail"))
