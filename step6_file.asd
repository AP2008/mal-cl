
(in-package :asdf-user)
(defsystem "step6_file"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre" "unix-opts")
  :components ((:file "errors")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "step6_file"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step6_file"
  :entry-point "step6_file:main")
