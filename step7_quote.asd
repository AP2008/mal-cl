
(in-package :asdf-user)
(defsystem "step7_quote"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre" "unix-opts")
  :components ((:file "errors")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "step7_quote"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step7_quote"
  :entry-point "step7_quote:main")
