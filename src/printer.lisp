(defpackage printer

  (:use cl types)
  (:export pr_str))

(in-package printer)
(defun pr_each_obj (lst print-readably)
  (let ((n nil))
    (loop for i in lst do (if (typep i 'mal-data-struct) 
                              (push (pr_str i) n) 
                              (push (print-obj i nil print-readably) n)))
  (nreverse n)))

(defun pr_str (obj &optional (print-readably t))
  (with-output-to-string (str)
    (cond
      ((typep obj 'mal-data-struct) 
          (print-obj
            (make-type-val (type-of obj) (pr_each_obj (value obj) print-readably)) str))
      ((typep obj 'mal-data-special)
          (setf (value obj) (pr_str (value obj)))
          (print-obj obj str))
      (t (print-obj obj str print-readably)))))


