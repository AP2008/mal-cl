(defpackage core
  (:use cl types printer reader uiop)
  (:export *ns* make-fn mal-quasi))

(in-package core)
(defmacro sethash (h key value)
  `(setf (gethash ,key ,h) ,value))

(defmacro make-fn (name fn)
  `(sethash *ns* ,name
            (make-instance 'mal-data-fn :value ,fn)))

(defvar *ns* (make-hash-table))
(defmacro calc-opers (sym oper)
  `(make-fn ,sym
            #'(lambda (a b) (make-type-val
                              'mal-data-number
                              (,oper (value a) (value b))))))
(calc-opers "+" +)
(calc-opers "-" -)
(calc-opers "*" *)
(calc-opers "/" /)

(make-fn "list"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-list args)))

(make-fn "empty?"
         #'(lambda (arg)
             (if (and (typep arg 'mal-data-list)
                      (zerop (length (value arg))))
                 (make-type-val 'mal-data-true t)
                 (make-type-val 'mal-data-false nil))))

(make-fn "count"
         #'(lambda (arg)
             (make-type-val 'mal-data-number (length (value arg)))))


(defun test-eq (arg1 arg2)
  (if (equal (type-of arg1) (type-of arg2))
      (progn
        (if (and (typep arg1 'mal-data-struct)
                 (every
                   (lambda (x) x)
                   (loop
                     for i in (value arg1)
                     for j in (value arg2)
                     collect (test-eq i j))))
            (return-from test-eq t))
      (return-from test-eq (equal (value arg1) (value arg2))))))

(make-fn "="
         #'(lambda (arg1 arg2)
             (if
                (test-eq arg1 arg2)
                (make-type-val 'mal-data-true t)
                (make-type-val 'mal-data-false nil))))

(defmacro numcomp (sym op)
         `(make-fn ,sym
            #'(lambda (arg1 arg2)
             (if (and (typep arg1 'mal-data-number)
                      (typep arg2 'mal-data-number))
                 (if (,op (value arg1) (value arg2))
                     (make-type-val 'mal-data-true t)
                     (make-type-val 'mal-data-false nil))
                 (make-type-val 'mal-data-nil nil)))))

(numcomp "<" <)
(numcomp ">" >)
(numcomp ">=" >=)
(numcomp "<=" <=)

(make-fn "pr-str"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-string
             (format nil "~{~a~^ ~}"
                     (loop for i in args collecting (pr_str i))))))
(make-fn "str"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-string
                            (format nil "~{~a~^~}"
                                    (loop for i in args
                                          collecting (pr_str i nil))))))
(make-fn "prn"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-nil
             (format *query-io* "~{~a~^ ~}~%"
                     (loop for i in args collecting (pr_str i))))))

(make-fn "println"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-nil
                            (format *query-io* "~{~a~^ ~}~%"
                                    (loop for i in args
                                          collecting (pr_str i nil))))))

(make-fn "read-string"
         #'(lambda (str)
             (read_str (value str))))
(make-fn "slurp"
         #'(lambda (str)
             (make-type-val 'mal-data-string
            (uiop:read-file-string (value str)))))

(make-fn "atom"
         #'(lambda (obj)
             (make-type-val 'mal-data-atom obj)))

(make-fn "deref"
         #'(lambda (obj)
             (value obj)))

(make-fn "reset!"
         #'(lambda (obj val)
             (setf (value obj) val)))

(make-fn "cons"
         #'(lambda (obj lis)
             (make-type-val 'mal-data-list (cons obj (value lis)))))

(make-fn "concat"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-list (apply #'append (loop for i in args collecting (value i))))))

(make-fn "vec"
         #'(lambda (arg)
             (make-type-val 'mal-data-vector (value arg))))

(make-fn "nth"
         #'(lambda (lis index)
             (elt (value lis) (value index))))

(make-fn "first"
         #'(lambda (lis)
             (let ((c (car (value lis))))
               (if c c (make-type-val 'mal-data-nil nil)))))

(make-fn "rest"
         #'(lambda (lis)
             (make-type-val (type-of lis) (cdr (value lis)))))

(make-fn "throw"
         #'(lambda (val)
             (let ((e (make-type-val 'mal-data-exception (value val))))
               (error (value e))
               e)))

(defmacro make-type-pr (name typ)
  `(make-fn ,name
            #'(lambda (val)
                (if (typep val ,typ)
                    (make-type-val 'mal-data-true t)
                    (make-type-val 'mal-data-false nil)))))

(make-type-pr "nil?" 'mal-data-nil)
(make-type-pr "true?" 'mal-data-true)
(make-type-pr "false?" 'mal-data-false)
(make-type-pr "symbol?" 'mal-data-symbol)
(make-type-pr "keyword?" 'mal-data-kw)
(make-type-pr "atom?" 'mal-data-vector)
(make-type-pr "list?" 'mal-data-vector)
(make-type-pr "vector?" 'mal-data-vector)
(make-type-pr "map?" 'mal-data-hash)
(make-type-pr "sequential?" 'mal-data-struct)

(make-fn "symbol"
         #'(lambda (str)
             (make-type-val 'mal-data-symbol (value str))))
(make-fn "keyword"
         #'(lambda (str)
             (make-type-val 'mal-data-kw
                            (if (char= (char (value str) 0) #\:)
                                (value str)
                                (concatenate 'string ":" (value str))))))
(make-fn "vector"
         #'(lambda (&rest args)
             (make-type-val 'mal-data-list args)))
(make-fn "hash-map"
         #'(lambda (&rest lis)
             (make-type-val 'mal-data-hash lis)))
(make-fn "assoc"
         #'(lambda (h &rest lis)
             (make-type-val 'mal-data-hash (append lis (value h)))))

(defun get-keys (h) (loop for i being the hash-key of h collect i))
(defun get-vals (h) (loop for i being the hash-value of h collect i))
(defun merge_list (l1 l2)
    (let ((n_l nil))
       (loop for i in l1 for j in l2 do (progn (push (value i) n_l) (push j n_l))) (nreverse n_l)))
(defun merge_list2 (l1 l2)
    (let ((n_l nil))
       (loop for i in l1 for j in l2 do (progn (push (make-type-val 'mal-data-string i) n_l) (push j n_l))) (nreverse n_l)))
(make-fn "dissoc"
         #'(lambda (h &rest lis)
             (let ((ha (hash_from_lis (merge_list (get-keys (copy-structure (dat h))) (get-vals (copy-structure (dat h)))))))
                  (loop for i in lis do (remhash (value i) ha))
                  (make-type-val 'mal-data-hash (merge_list2 (get-keys ha) (get-vals ha))))))

(make-fn "get"
         #'(lambda (h k)
             (let ((r (loop for i being the hash-key of (dat h) do (if (equal (value i) (value k)) (return (gethash i (dat h)))))))
                  (if r r (make-type-val 'mal-data-nil nil)))))
(make-fn "contains?"
         #'(lambda (h k)
             (if (loop for i being the hash-key of (dat h) do (if (equal (value i) (value k)) (return t)))
                 (make-type-val 'mal-data-true t)
                 (make-type-val 'mal-data-false nil))))
(make-fn "keys"
         #'(lambda (h)
             (make-type-val 'mal-data-list 
               (loop for i being the hash-keys of (dat h) collect i))))
(make-fn "vals"
         #'(lambda (h)
             (make-type-val 'mal-data-list
               (loop for i being the hash-values of (dat h) collect i))))

(defun mal-quasi (ast)
             (if
               (typep ast 'mal-data-list)
               (progn 
               (if
                 (and (not (zerop (length (value ast)))) (equal (value (elt (value ast) 0)) "unquote")) (return-from mal-quasi (elt (value ast) 1))
                 (let ((result (make-type-val 'mal-data-list '())))
                   (loop for elt in (reverse (value ast))
                         do (progn 
                              (if (and (typep elt 'mal-data-list) (not (zerop (length (value elt))))
                                     (equal (value (first (value elt))) "splice-unquote"))
                                (setf result (make-type-val 'mal-data-list
                                              (list
                                               (make-type-val 'mal-data-symbol "concat")
                                               (second (value elt)) result)))
                                 (setf result (make-type-val 'mal-data-list
                                               (list
                                               (make-type-val 'mal-data-symbol "cons")
                                               (mal-quasi elt) result))))
                              ))
                   (return-from mal-quasi result))))
               (if (or (typep ast 'mal-data-hash) (typep ast 'mal-data-symbol))
                   (return-from mal-quasi (make-type-val 'mal-data-list (list (make-type-val 'mal-data-symbol "quote") ast)))))
               (return-from mal-quasi ast))
                                
