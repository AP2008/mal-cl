(defpackage step0_repl
  (:use :cl)
  (:export mal-READ mal-EVAL mal-PRINT rep)
)

(in-package :step0_repl)

(defun mal-READ (str)
  (declare (type string str))
  str
)

(defun mal-EVAL (str)
  (declare (type string str))
  str
)

(defun mal-PRINT (str)
  (declare (type string str))
  str
)

(defun rep (str)
  (declare (type string str))
  (mal-PRINT
    (mal-EVAL
      (mal-READ str)))
)

(do ((inp 1))
  ((equal inp ""))
  (format *query-io* "user> ")
  (force-output *query-io*)
  (setf inp (read-line *query-io* nil nil))
  (unless (equal inp "") 
    (format *query-io* "~a~%" (rep inp))
    (force-output *query-io*)))
