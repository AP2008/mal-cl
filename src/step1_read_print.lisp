(defpackage step1_read_print
  (:use :cl :reader :printer :errors)
  (:export mal-READ mal-EVAL mal-PRINT rep main)
)

(in-package :step1_read_print)

(defun mal-READ (str)
  (handler-case (
                 let ((k (read_str str)))
                   k)
    (eof-brackets (err)
                  (format *query-io* "EOF: unmatched parentheses -> ~{~a ~}~%"
                          (text err)))
    (eof-string (err)
                  (format *query-io* "EOF: unmatched string-quotes -> ~a~%"
                          (text err)))
    ))

(defun mal-EVAL (str)
  str
)

(defun mal-PRINT (str)
  (if (not str) nil (format *query-io* "~a~%" (pr_str str)))
)

(defun rep (str)
  (mal-PRINT
    (mal-EVAL
      (mal-READ str)))
)

(defun main ()
(do ((inp 1))
  ((equal inp ""))
  (format *query-io* "user> ")
  (force-output *query-io*)
  (setf inp (read-line *query-io* nil nil))
  (unless (equal inp "") 
    (rep inp)
    (force-output *query-io*))))
