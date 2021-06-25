(defpackage errors
  (:use cl)
  (:export check-brackets eof check-string text eof-quotes var-not-found))

(in-package errors)

(define-condition eof (error)
  ((text :initarg :text :reader text)))
(make-condition 'eof)

(defun check-brackets (lst open close)
  (if (= (count open lst :test #'string=)
         (count close lst :test #'string=))
      t (error 'eof :text lst)))

(define-condition eof-quotes (error)
  ((text :initarg :text :reader text)))
(make-condition 'eof-quotes)

(defun check-string (str)
  (if (and (> (length str) 1) (char= (aref str 0) (aref str (1- (length str)))))
      t (error 'eof-quotes :text str)))


(define-condition var-not-found (error)
  ((text :initarg :text :reader text)))
(make-condition 'var-not-found)