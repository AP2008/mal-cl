(in-package :asdf-user)
(defsystem "step2_repl"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "types")
               (:file "errors")
               (:file "reader")
               (:file "printer")
               (:file "step2_repl"))
  :pathname "src/"
  :build-operation "asdf:program-op"
  :build-pathname "../step2_repl"
  :entry-point "step2_repl:main")
