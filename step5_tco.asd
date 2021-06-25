(in-package :asdf-user)
(defsystem "step5_tco"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "errors")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "step5_tco"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step5_tco"
  :entry-point "step5_tco:main")
