
(in-package :asdf-user)
(defsystem "step8_macros"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre" "unix-opts")
  :components ((:file "errors")
               (:file "types")
               (:file "env")
               (:file "reader")
               (:file "printer")
               (:file "core")
               (:file "step8_macros"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step8_macros"
  :entry-point "step8_macros:main")
