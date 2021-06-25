(defpackage step9_try
  (:use :cl :reader :printer :errors :types :env :core)
  (:import-from :unix-opts :get-opts)
  (:export mal-READ mal-EVAL mal-PRINT rep main)
)

(in-package :step9_try)


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

(make-fn "apply"
         #'(lambda (fun &rest args)
             (apply (value fun)
             (let ((new_list))
               (setf new_list (reverse (value (first (last args)))))
               (loop for i in (butlast args)
                     do (push i new_list))
               (nreverse new_list)))))

(make-fn "map"
         #'(lambda (fun lis)
             (make-type-val (type-of lis) (mapcar (value fun) (value lis)))))


(defvar *repl_env* (make-instance 'Env :outer nil))
(loop
  for k being the hash-key
  using (hash-value v) of *ns*
  do (env-set *repl_env* k v))

(defmacro make_sp_func (name)
  `(env-set *repl_env* ,name (make-type-val 'mal-data-symbol ,name)))

(make_sp_func "def!")
(make_sp_func "do")
(make_sp_func "if")
(make_sp_func "let*")
(make_sp_func "fn*")
(make_sp_func "try*")
(make_sp_func "catch*")
(make_sp_func "defmacro!")
(make_sp_func "macroexpand")
(make_sp_func "quasiquote")
(make_sp_func "quote")
(make_sp_func "quasiquoteexpand")

(defclass fn*-obj (mal-data-fn)
  ((ast :initarg :ast
        :accessor getf-ast)
   (params :initarg :params
           :accessor getf-params)
   (env :initarg :env
        :accessor getf-env)
   (fn :initarg :rfn :accessor getf-fn)
   (value :initarg :value :accessor value)))

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

(defun is_macro_call (ast env)
  (and
    (typep ast 'mal-data-list)
    (typep (first (value ast)) 'mal-data-symbol)
    (let ((fni (mal-eval_ast (first (value ast)) env)))
      (and
        (typep fni 'fn*-obj)
        (is_macro fni)))))

(defun mal-macroexpand (asta enva)
  (do ((ast asta) (env enva))
      ((not (is_macro_call ast env)) ast)
      (setf ast
            (apply (value (getf-fn (mal-eval_ast (first (value ast)) env))) (cdr (value ast))))))

(defun get-str (str)
  (if (typep str 'mal-data-symbol)
      (value str)
      nil))

(defun mal-EVAL (asta enva)
  (if (not asta) (return-from mal-eval nil))
  (let ((ast asta) (env enva))
  (loop
    (setf ast (mal-macroexpand ast env))
    (if (not ast) (return-from mal-eval (make-type-val 'mal-data-nil nil)))
    (cond
      ((typep ast 'mal-data-list)
        (if (zerop (length (value ast)))
          (return-from mal-eval ast)
          (let ((str (get-str (first (value ast)))) (lis ast))
            (cond
              ((equal str "try*")
               (setf ast
               (handler-case (mal-eval (second (value lis)) env)
                 (t (err)
                   (let ((new_env (make-instance 'Env :outer env)))
                     (env-set new_env (value (second (value (third (value lis))))) (make-type-val 'mal-data-exception (format nil "~a" err)))
                    (mal-eval (third (value (third (value lis)))) new_env))))))
              ((equal str "macroexpand") (return-from mal-eval (mal-macroexpand (second (value lis)) env)))
              ((equal str "quasiquoteexpand") (return-from mal-eval (mal-quasi (second (value ast)))))
              ((equal str "quasiquote") (setf ast (mal-quasi (second (value ast)))))
              ((equal str "quote") (return-from mal-eval
                                      (elt (value lis) 1)))
              ((equal str "defmacro!") (return-from mal-eval
                                                    (env-set env (value (second (value lis)))
                                                             (let ((fni (mal-eval (third (value lis)) env)))
                                                               (setf (is_macro fni) t) fni))))
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
               (let ((t_fn #'(lambda (&rest ***argums***)
                                           (let ((***env*** (make-instance 'Env :outer env :binds (loop for i in (value (second (value lis))) collecting (value i))
                                                                                           :exprs ***argums***)))
                                              (mal-EVAL (third (value lis)) ***env***)))))
              (return-from mal-eval
               (make-instance 'fn*-obj
                              :ast (third (value lis))
                              :params (second (value lis))
                              :env env
                              :value t_fn
                              :rfn (make-instance 'mal-data-fn :value t_fn)))))
              (t 
                (let ((env_list (mal-eval_ast ast env)))
                  (cond
                    ((typep (first env_list) 'fn*-obj) 
                     (if (not (typep ast 'mal-data-list)) (return-from mal-eval ast))
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
;(rep "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1)) (cons 'cond (rest (rest xs)))))))")

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
        ((or (equal inp "") (equal inp nil) ))
        (format *query-io* "user> ")
        (force-output *query-io*)
        (setf inp (read-line *query-io* nil nil))
        (unless (equal inp "")
          (rep inp)
          (force-output *query-io*)))))
