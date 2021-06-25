
(in-package :asdf-user)
(defsystem "step9_try"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre" "unix-opts")
  :components ((:file "errors")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "step9_try"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../mal"
  :entry-point "step9_try:main")
