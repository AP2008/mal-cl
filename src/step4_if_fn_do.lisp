(defpackage step4_if_fn_do
  (:use :cl :reader :printer :errors :types :env :core)
  (:export mal-READ mal-EVAL mal-PRINT rep main)
)

(in-package :step4_if_fn_do)

(defmacro sethash (h key value)
  `(setf (gethash ,key ,h) ,value))
(defmacro vals_hash (key h)
  `(multiple-value-bind (x y) (gethash ,key ,h) (list x y)))

(defvar *repl_env* (make-instance 'Env :outer nil))

(loop
  for k being the hash-key
  using (hash-value v) of *ns*
  do (env-set *repl_env* k v))

(defvar *def!_mal* (make-type-val 'mal-data-symbol "def!"))
(env-set *repl_env* "def!" *def!_mal*)

(defvar *let*_mal* (make-type-val 'mal-data-symbol "let*"))
(env-set *repl_env* "let*" *let*_mal*)

(defvar *do_mal* (make-type-val 'mal-data-symbol "do"))
(env-set *repl_env* "do" *do_mal*)

(defvar *if_mal* (make-type-val 'mal-data-symbol "if"))
(env-set *repl_env* "if" *if_mal*)

(defvar *fn*_mal* (make-type-val 'mal-data-symbol "fn*"))
(env-set *repl_env* "fn*" *fn*_mal*)

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
  (let ((str (value (mal-eval_ast (first (value lis)) env ) ) ) )
    (cond
      ((equal str "def!") (env-set env (value (second (value lis)))
                                       (mal-eval (third (value lis)) env)))
      ((equal str "let*")
       (let ((new_env) (vars (value (second (value lis)))))
         (setf new_env (make-instance 'Env :outer env))
         (loop for i from 0 to (1- (/ (length vars) 2))
                do (env-set new_env
                            (value (elt vars (* 2 i)))
                            (mal-eval (elt vars (1+ (* 2 i))) new_env)))
         (mal-eval (third (value lis)) new_env)))
      ((equal str "do") (car (last (loop for i in (cdr (value lis)) collecting (mal-EVAL i env)))))
      ((equal str "if") (if (value (mal-eval (second (value lis)) env)) 
                              (mal-eval (third (value lis)) env) 
                              (if (> (length (value lis)) 2) 
                                  (mal-eval (fourth (value lis)) env)
                                  (make-type-val 'mal-data-nil nil))))
      ((equal str "fn*")
                      (make-type-val 'mal-data-fn
                             #'(lambda (&rest ***argums***)
                                   (let ((***env*** (make-instance 'Env :outer env :binds (loop for i in (value (second (value lis))) collecting (value i))
                                                                                   :exprs ***argums***)))
                                      (mal-EVAL (third (value lis)) ***env***) ))))
      (t (mapcar #'(lambda (ast) (mal-eval ast env)) (value lis))))))

(defun mal-eval_ast (ast env)
  (cond
    ((typep ast 'mal-data-symbol) (env-get env (value ast)))
    ((typep ast 'mal-data-list) (mal-eval_list ast env))
    ((typep ast 'mal-data-vector) 
     (if (not (zerop (length (value ast))))
         (make-type-val 'mal-data-vector (mal-eval_list ast env))
         (make-type-val 'mal-data-vector nil)))
    ((typep ast 'mal-data-hash) 
     (if (not (zerop (length (value ast))))
         (make-type-val 'mal-data-hash (mal-eval_list ast env))
         (make-type-val 'mal-data-hash nil)))
    (t ast)))

(defun mal-EVAL (ast env)
  ;(handler-case
    (cond
      ((typep ast 'mal-data-list)
          (cond
            ((= (length (value ast)) 0) ast)
            ((not (zerop (count (mal-eval_ast (first (value ast)) env) 
                                (list *def!_mal* *do_mal* 
                                      *if_mal* *let*_mal* *fn*_mal*) 
                                      :test #'equal ))) (mal-eval_list ast env))
;            ((equal (mal-eval_ast (first (value ast)) env) *def!_mal*)
;             (mal-eval_list ast env "def!"))
;            ((equal (mal-eval_ast (first (value ast)) env) *do_mal*)
;             (mal-eval_list ast env "do"))
;            ((equal (mal-eval_ast (first (value ast)) env) *if_mal*)
;             (mal-eval_list ast env "if"))
;            ((equal (mal-eval_ast (first (value ast)) env) *let*_mal*)
;             (mal-eval_list ast env "let*"))
;            ((equal (mal-eval_ast (first (value ast)) env) *fn*_mal*)
;             (mal-eval_list ast env "fn*"))
            (t 
              (let ((env_list (mal-eval_ast ast env)))
                (apply (value (car env_list)) (cdr env_list)) )) ))
      (t (mal-eval_ast ast env)))
    )

(defun mal-PRINT (str)
  (if (not str) nil (format *query-io* "~a~%" (pr_str str)))
)
(defun rep (str)
  (mal-PRINT
    (handler-case
      (mal-EVAL
        (mal-READ str)
        *repl_env*)
      
      (var-not-found (err)
        (format *query-io* "~a not found.~%" (text err))))))

(defun main ()
(do ((inp 1))
  ((equal inp ""))
  (format *query-io* "user> ")
  (force-output *query-io*)
  (setf inp (read-line *query-io* nil nil))
  (unless (equal inp "")
    (rep inp)
    (force-output *query-io*))))
