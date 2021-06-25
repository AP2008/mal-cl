(in-package :asdf-user)
(defsystem "step1_read_print"
  :author "Aratrik Pal"
  :serial t
  :depends-on ("cl-ppcre")
  :components ((:file "types")
               (:file "errors")
               (:file "reader")
               (:file "printer")
               (:file "step1_read_print"))
  :pathname "src/")
;  :build-operation "asdf:program-op"
;  :build-pathname "../step1_read_print"
;  :entry-point "step1_read_print:main")
