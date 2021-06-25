(defpackage env
  (:use :cl :errors :types)
  (:export Env env-set env-find env-get outer data create-env))

(in-package env)

(defclass Env ()
  ((outer :initarg :outer
          :initform nil
          :accessor outer)
   (binds :initarg :binds
          :initform '()
          :accessor binds)
   (expressions :initarg :exprs
          :initform '()
          :accessor exprs)
   (data :initarg :data
;         :initform binds
;         :initform (make-hash-table :test 'equal)
         :accessor data)))

(defmethod initialize-instance :after ((envir Env) &key)
  (setf (data envir) (make-hash-table :test 'equal))
  (do ((i 0 (1+ i)) (j 0 (1+ j)))
      ((>= i (length (binds envir))))
      (if (string= (elt (binds envir) i) "&")
          (progn
            (setf (gethash (elt (binds envir) (1+ i)) (data envir)) (make-type-val 'mal-data-list (subseq (exprs envir) i)))
            (setf i (length (binds envir))))
          (setf (gethash (elt (binds envir) i) (data envir)) (elt (exprs envir) j)))))

(defmacro vals_hash (key h)
  `(multiple-value-bind (x y) (gethash ,key ,h) (list x y)))

(defmethod env-set ((envir Env) sym val)
  (setf (gethash sym (data envir)) val))
(defmethod env-find ((envir Env) sym)
  (if (second (vals_hash sym (data envir))) envir 
                                         (if (outer envir) (env-find (outer envir) sym) (error 'var-not-found :text sym)) ))
(defmethod env-get ((envir Env) sym)
  (car (vals_hash sym (data (env-find envir sym)))))
