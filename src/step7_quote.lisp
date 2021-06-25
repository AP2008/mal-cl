(defpackage step7_quote
  (:use :cl :reader :printer :errors :types :env :core)
  (:import-from :unix-opts :get-opts)
  (:export mal-READ mal-EVAL mal-PRINT rep main)
)

(in-package :step7_quote)


(defmacro sethash (h key value)
  `(setf (gethash ,key ,h) ,value))
(defmacro vals_hash (key h)
  `(multiple-value-bind (x y) (gethash ,key ,h) (list x y)))

(make-fn "eval"
       #'(lambda (ast) (mal-eval ast *repl_env*)))
(make-fn "swap!"
         #'(lambda (obj fun &rest args)
             (setf (value obj)
                   (if (typep fun 'fn*-obj)
                     (apply (value (getf-fn fun)) (cons (value obj) args))
                     (apply (value fun) (cons (value obj) args))))))



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

(defclass fn*-obj (mal-data-fn)
  ((ast :initarg :ast
        :accessor getf-ast)
   (params :initarg :params
           :accessor getf-params)
   (env :initarg :env
        :accessor getf-env)
   (fn :initarg :fn
       :accessor getf-fn)
   (value :initform "#<function>"
         :accessor value)))

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

(defun get-str (str)
  (if (typep str 'mal-data-symbol)
      (value str)
      nil))

(defun mal-EVAL (asta enva)
  (if (not asta) (return-from mal-eval nil))
  (let ((ast asta) (env enva))
  (loop
    (cond
      ((typep ast 'mal-data-list)
        (if (zerop (length (value ast)))
          (return-from mal-eval ast)
          (let ((str (get-str (first (value ast)))) (lis ast))
            (cond
              ((equal str "quasiquoteexpand") (return-from mal-eval (mal-quasi (second (value ast)))))
              ((equal str "quasiquote") (setf ast (mal-quasi (second (value ast)))))
              ((equal str "quote") (return-from mal-eval
                                      (elt (value lis) 1)))
              ((equal str "def!") (return-from mal-eval
                                      (env-set env (value (second (value lis)))
                                               (mal-eval (third (value lis)) env))))
              ((equal str "let*")
               (let ((new_env) (vars (value (second (value lis)))))
                 (setf new_env (make-instance 'Env :outer env))
                 (loop for i from 0 to (1- (/ (length vars) 2))
                        do (env-set new_env
                                    (value (elt vars (* 2 i)))
                                    (mal-eval (elt vars (1+ (* 2 i))) new_env)))
                 (setf env new_env)
                 (setf ast (third (value lis)))))
              ((equal str "do") (loop for i in (butlast (cdr (value lis))) do (mal-eval i env))
                                (setf ast (car (last (value lis)))));(car (last (loop for i in (cdr (value lis)) collecting (mal-EVAL i env)))))
              ((equal str "if") (if (value (mal-eval (second (value lis)) env)) 
                                      (setf ast (third (value lis))) 
                                      (if (> (length (value lis)) 2) 
                                          (setf ast (fourth (value lis)))
                                          (setf ast (make-type-val 'mal-data-nil nil)))))
              ((equal str "fn*")
               (return-from mal-eval
               (make-instance 'fn*-obj
                              :ast (third (value lis))
                              :params (second (value lis))
                              :env env
                              :fn (make-type-val 'mal-data-fn
                                     #'(lambda (&rest ***argums***)
                                           (let ((***env*** (make-instance 'Env :outer env :binds (loop for i in (value (second (value lis))) collecting (value i))
                                                                                           :exprs ***argums***)))
                                              (mal-EVAL (third (value lis)) ***env***)))))))
              (t 
                (let ((env_list (mal-eval_ast ast env)))
                  (cond
                    ((typep (first env_list) 'fn*-obj) 
                     (setf ast (getf-ast (first env_list)))
                     (setf env
                       (make-instance 'Env :outer (getf-env (first env_list))
                                      :binds (loop for i in (value (getf-params (first env_list))) collecting (value i))
                                      :exprs (cdr env_list))))
                  (t (return-from mal-eval (apply (value (car env_list)) (cdr env_list)) )) )))))))
      (t (return-from mal-eval (mal-eval_ast ast env)))
  ))))

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

(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))")
(rep "(def! not (fn* (a) (if a false true)))")

(defun main ()
  (defvar *ARGV* (cdr sb-ext:*posix-argv*))
  (env-set *repl_env* "*ARGV*"
         (make-type-val 'mal-data-list 
                        (mapcar #'(lambda (x) (make-type-val 'mal-data-string x))
                                *ARGV*)))
  (if *ARGV*
      (progn
        (setf inp (format nil "(load-file \"~a\")" (elt *ARGV* 0)))
        (rep inp))
      (do ((inp 1))
        ((equal inp ""))
        (format *query-io* "user> ")
        (force-output *query-io*)
        (setf inp (read-line *query-io* nil nil))
        (unless (equal inp "")
          (rep inp)
          (force-output *query-io*)))))
