(defpackage reader
  (:use :cl :cl-ppcre :types :errors)
  (:export read_str peek next tokens))

(in-package reader)
(defclass reader ()
  ((pos :initarg :position
        :initform 0
        :accessor pos)
   (tokens :initarg :tokens
           :accessor tokens)))

(defmethod peek ((r reader))
  (elt (tokens r) (pos r)))

(defmethod next ((r reader))
  (let ((cur (peek r)))
    (incf (pos r))
    cur))

(defun read_str (str)
  (read_form (make-instance 'reader :tokens (tokenize str))))

(defvar *regex* (create-scanner
                 "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)"))
(defvar *number* (create-scanner "^-?\\d+$"))
(defvar *string* (create-scanner "^\"(?:\\\\.|[^\\\\\"])*\"?$"))
(defvar *nil* (create-scanner "^nil$"))
(defvar *true* (create-scanner "^true$"))
(defvar *false* (create-scanner "^false$"))
(defvar *keyword* (create-scanner "^:.+$"))
(defvar *whitespace-chars*
  '(#\Space #\Newline #\Backspace #\Tab
    #\Linefeed #\Page #\Return #\Rubout #\,))

(defun tokenize (str)
  (let ((tokens nil))
    (do-matches-as-strings (token *regex* str)
      (let ((tok (string-trim *whitespace-chars* token)))
        (unless
            (or
             (zerop (length tok))
             (char= (char tok 0) #\;))
          (push tok tokens))))
    (nreverse tokens)))

(defmacro create_struct_read (name charac)
  `(defun ,name (read_obj)
     (loop
       for i = (peek read_obj)
       while (not (char= (char i 0) ,charac))
       collect (read_form read_obj)
       finally (next read_obj))))

(create_struct_read read_list #\))
(create_struct_read read_vector #\])
(create_struct_read read_hash #\})

(defun str_fmat (str)
  (regex-replace-all "\\\\n" 
                     (regex-replace-all  "\\\\\\\\" 
                                         (regex-replace-all "\\\\\"" str "\"")
                                         "\\") (format nil "~%")))

(defun read_atom (read_obj)
  (let ((obj (next read_obj)))
    (cond
      ((scan *string* obj)
       (check-string obj)
       (make-type-val 'mal-data-string (str_fmat (subseq obj 1 (1- (length obj))))))
      ((scan *number* obj)
       (make-type-val 'mal-data-number (parse-integer obj)))
      ((scan *false* obj)
       (make-type-val 'mal-data-false nil))
      ((scan *true* obj)
       (make-type-val 'mal-data-true t))
      ((scan *nil* obj)
       (make-type-val 'mal-data-nil nil))
      ((scan *keyword* obj)
       (make-type-val 'mal-data-kw obj))
      (t (make-type-val 'mal-data-symbol obj)))))

(defmacro chareq_struct (name func ch)
  `((char= (char (peek read_obj) 0) ,ch)
    (next read_obj)
    (make-type-val ,name (,func read_obj))))

(defun read_form (read_obj)
  (if (not (> (length (tokens read_obj)) 0)) (return-from read_form nil))
  (cond 
      ((char= (char (peek read_obj) 0) #\()
        (check-brackets (tokens read_obj) #\( #\))
        (next read_obj)
        (make-type-val 'mal-data-list (read_list read_obj)))
      ((char= (char (peek read_obj) 0) #\[)
         (check-brackets (tokens read_obj) #\[ #\])
         (next read_obj)
         (make-type-val 'mal-data-vector (read_vector read_obj)))
      ((char= (char (peek read_obj) 0) #\{)
         (check-brackets (tokens read_obj) #\{ #\})
         (next read_obj)
         (make-type-val 'mal-data-hash (read_hash read_obj)))
      ((char= (char (peek read_obj) 0) #\@)
       (next read_obj)
       (make-type-val 'mal-data-list
                      (list 
                        (make-type-val 'mal-data-symbol "deref")
                        (read_form read_obj))))
      ((char= (char (peek read_obj) 0) #\')
       (next read_obj)
       (make-type-val 'mal-data-list
                      (list 
                        (make-type-val 'mal-data-symbol "quote")
                        (read_form read_obj))))
      ((char= (char (peek read_obj) 0) #\`)
       (next read_obj)
       (make-type-val 'mal-data-list
                      (list 
                        (make-type-val 'mal-data-symbol "quasiquote")
                        (read_form read_obj))))
      ((and (char= (char (peek read_obj) 0) #\~) (> (length (peek read_obj)) 1) (char= (char (peek read_obj) 1) #\@))
       (next read_obj)
       (make-type-val 'mal-data-list
                      (list 
                        (make-type-val 'mal-data-symbol "splice-unquote")
                        (read_form read_obj))))
      ((char= (char (peek read_obj) 0) #\~)
       (next read_obj)
       (make-type-val 'mal-data-list
                      (list 
                        (make-type-val 'mal-data-symbol "unquote")
                        (read_form read_obj))))
      (t (read_atom read_obj))))


