(in-package :asdf-user)
(defsystem "step4_if_fn_do"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "errors")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "step4_if_fn_do"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step4_if_fn_do"
  :entry-point "step4_if_fn_do:main")
