(defpackage types
  (:use cl cl-ppcre)
  (:export mal-data-number mal-data-string mal-data-symbol
           value type-check print-obj make-type-val mal-data-false
           mal-data-true mal-data-nil mal-data-list mal-data-vector
           mal-data-struct mal-data-hash mal-data-kw mal-data-fn
           mal-data-quote mal-data-special mal-data-atom is_macro
           mal-data-exception getf-fn hash-val hash-key dat hash_from_lis
))
(in-package types)

(defmacro make-mal-type-class (name inherit)
  `(defclass ,name ,inherit
     ((value
       :initarg :value
       :accessor value))))
(defmacro make-mal-type-printer (name fmat)
  `(defmethod print-obj ((obj ,name) str &optional (pr-read t))
     (format str "~a" (funcall ,fmat (value obj) pr-read))))
(defmacro make-mal-type (name fmat &optional (inherit '()))
 `(progn
    (make-mal-type-class ,name ,inherit )
    (make-mal-type-printer ,name ,fmat)))

(defun type-check (var class)
  (eq (type-of var) class))

(defun make-type-val (obj val)
  (make-instance obj :value val))
(defmacro simple_formatter (fm)
  `(lambda (x &optional pr-read) (declare (ignore pr-read)) (format nil ,fm x)))

 (defun rev_str_fmat (str)
   (regex-replace-all (format nil "~%")
                     (regex-replace-all "\\"
                                        (regex-replace-all "\\\"" str "\\\"")
                                        "\\\\\\\\")
                     "\\n"))

(defun str_f_p (str read-p)
  (if read-p
      (format nil "\"~a\"" (rev_str_fmat str))
      (format nil "~a" str)
      )
  )

(make-mal-type mal-data-number (simple_formatter "~a"))
(make-mal-type mal-data-string #'str_f_p)
(make-mal-type mal-data-symbol (simple_formatter "~a"))
(make-mal-type mal-data-false (simple_formatter "false"))
(make-mal-type mal-data-true (simple_formatter "true"))
(make-mal-type mal-data-nil (simple_formatter "nil"))
(make-mal-type mal-data-kw (simple_formatter "~a"))
(defclass mal-data-fn ()
  ((value
       :initarg :value
       :accessor value)
   (is_macro
     :initarg :is_macro
     :initform nil
     :accessor is_macro)))
(make-mal-type-printer mal-data-fn (simple_formatter "#<function>"))
(defclass mal-data-struct () ())
(make-mal-type mal-data-list (simple_formatter "(~{~a~^ ~})") (mal-data-struct))
(make-mal-type mal-data-vector (simple_formatter "[~{~a~^ ~}]") (mal-data-struct))
(defclass mal-data-hash (mal-data-struct)
  ((value :initarg :value
          :accessor value)
   (data :accessor dat)
   )
  )

(defun hash_from_lis (lis)
  (let ((h (make-hash-table :test #'equal)))
    (loop for i from 0 to (1- (/ (length lis) 2))
            do (setf (gethash (elt lis (* 2 i)) h)  (elt lis (1+ (* 2 i)))) ) h))
(defmethod initialize-instance :after ((val mal-data-hash) &key)
    (setf (dat val) (hash_from_lis (value val))))
(make-mal-type-printer mal-data-hash (simple_formatter "{~{~a~^ ~}}"))
(defclass mal-data-special () ())
(make-mal-type mal-data-quote (simple_formatter "(quote ~a)") (mal-data-special))
(make-mal-type mal-data-atom (simple_formatter "atom ->> ~a"))
(make-mal-type mal-data-exception (simple_formatter "Exception caught: ~a"))
(defmethod print-obj ((obj mal-data-atom) str &optional (pr-read t))
  (declare (ignore pr-read))
  (format str "~a ->> ~a" obj (value obj)))
