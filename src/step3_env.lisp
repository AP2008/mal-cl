(defpackage step3_env
  (:use :cl :reader :printer :errors :types :env)
  (:export mal-READ mal-EVAL mal-PRINT rep main)
)

(in-package :step3_env)

(defmacro sethash (h key value)
  `(setf (gethash ,key ,h) ,value))
(defmacro vals_hash (key h)
  `(multiple-value-bind (x y) (gethash ,key ,h) (list x y)))

(defvar *repl_env* (make-instance 'Env :outer nil))

(defvar *def!_mal* (make-type-val 'mal-data-symbol "def!"))
(env-set *repl_env* "def!" *def!_mal*)

(defvar *let*_mal* (make-type-val 'mal-data-symbol "let*"))
(env-set *repl_env* "let*" *let*_mal*)

(defmacro calc-opers (sym oper)
  `(env-set *repl_env* ,sym
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

(defun mal-eval_list (lis env &optional (str ""))
  (cond
    ((string= str "def!") (env-set env (value (second (value lis)))
                                       (mal-eval (third (value lis)) env)))
    ((string= str "let*")
     (let ((new_env) (vars (value (second (value lis)))))
       (setf new_env (make-instance 'Env :outer env))
       (loop for i from 0 to (1- (/ (length vars) 2))
              do (env-set new_env
                          (value (elt vars (* 2 i)))
                          (mal-eval (elt vars (1+ (* 2 i))) new_env)))
       (mal-eval (third (value lis)) new_env)))
    (t (mapcar #'(lambda (ast) (mal-eval ast env)) (value lis)))))

(defun mal-eval_ast (ast env)
  (cond
    ((typep ast 'mal-data-symbol) (env-get env (value ast)))
    ((typep ast 'mal-data-list) (mal-eval_list ast env))
    ((typep ast 'mal-data-vector) (make-type-val 'mal-data-vector
                                                 (mal-eval_list ast env)))
    ((typep ast 'mal-data-hash) (make-type-val 'mal-data-hash
                                               (mal-eval_list ast env)))
    (t ast)))

(defun mal-EVAL (ast env)
  (handler-case
    (cond
      ((typep ast 'mal-data-list)
          (cond
            ((= (length (value ast)) 0) ast)
            ((equal (mal-eval_ast (first (value ast)) env) *def!_mal*)
             (mal-eval_list ast env "def!"))
            ((equal (mal-eval_ast (first (value ast)) env) *let*_mal*)
             (mal-eval_list ast env "let*"))
            (t 
              (let ((env_list (mal-eval_ast ast env)))
                (apply (car env_list) (cdr env_list)) )) ))
      (t (mal-eval_ast ast env)))
    (var-not-found (err)
                            (format *query-io* "~a not found.~%" (text err)))))

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
