(defpackage step2_repl
  (:use :cl :reader :printer :errors :types :env)
  (:export mal-READ mal-EVAL mal-PRINT rep main)
)

(in-package :step2_repl)

(defmacro sethash (h key value)
  `(setf (gethash ,key ,h) ,value))
(defmacro vals_hash (key h)
  `(multiple-value-bind (x y) (gethash ,key ,h) (list x y)))

(defvar *repl_env* (make-hash-table :test #'equal))

(defmacro calc-opers (sym oper)
  `(sethash *repl_env* ,sym
            #'(lambda (a b) (make-type-val
                              'mal-data-number
                              (,oper (value a) (value b))))))
(calc-opers "+" +)
(calc-opers "-" -)
(calc-opers "*" *)
(calc-opers "/" /)

(defun mal-READ (str)
  (handler-case (
                 let ((k (read_str str)))
                   k)
    (eof (err)
                  (format *query-io* "EOF -> ~{~a ~}~%"
                          (text err))) 
    (eof-quotes (err)
                  (format *query-io* "EOF -> ~a~%"
                          (text err)))  ))

(defun mal-eval_list (lis env)
  (mapcar #'(lambda (ast) (mal-eval ast env)) (value lis)))

(defun mal-eval_ast (ast env)
  (cond
    ((typep ast 'mal-data-symbol) (car (vals_hash (value ast) env)))
    ((typep ast 'mal-data-list) (mal-eval_list ast env))
    ((typep ast 'mal-data-vector) (make-type-val 'mal-data-vector (mal-eval_list ast env)))
    ((typep ast 'mal-data-hash) (make-type-val 'mal-data-hash (mal-eval_list ast env)))
    (t ast)))

(defun mal-EVAL (ast env)
  (cond
    ((typep ast 'mal-data-list) 
        (if (= (length (value ast)) 0)
            ast
            (let ((env_list (mal-eval_ast ast env)))
              (apply (car env_list) (cdr env_list)))))
    (t (mal-eval_ast ast env))))

(defun mal-PRINT (str)
  (if (not str) nil (format *query-io* "~a~%" (pr_str str)))
)

(defun rep (str)
  (mal-PRINT
    (mal-EVAL
      (mal-READ str)
      *repl_env*))
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
