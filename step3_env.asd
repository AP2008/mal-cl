(in-package :asdf-user)
(defsystem "step3_env"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "errors")
               (:file "env")
               (:file "types")
               (:file "reader")
               (:file "printer")
               (:file "step3_env"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step3_env"
  :entry-point "step3_env:main")