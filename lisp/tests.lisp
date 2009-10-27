;;; tests.lisp

(defpackage "TESTS" (:use "COMMON-LISP" #+(or abcl xcl) "EXTENSIONS"))

(in-package "TESTS")

#-(or abcl xcl)
(defun fixnump (x) (typep x 'fixnum))

#-(or abcl xcl)
(defmacro aver (&rest args)
  `(assert ,@args))

#+(or sbcl)
(defun bignump (x) (typep x 'bignum))

(aver (equal (multiple-value-list (unless (values t t))) '(nil)))
(aver (equal (multiple-value-list (unless (values nil t))) '(nil)))
(aver (equal (multiple-value-list (when (values t t))) '(nil)))
(aver (equal (multiple-value-list (when (values nil t))) '(nil)))
(aver (equal (multiple-value-list (if (values nil t) 42)) '(nil)))

(aver (eql (list-length '(1 2 3)) 3))
#+xcl
(aver (not (autoloadp 'list-length)))

(aver (eql (+ 1 2) 3))
(aver (eql (+ 1 2 3) 6))

(aver (zerop (+ 1 2 -3)))
(aver (zerop (+ +7 -7)))

(aver (eql (1+ 42) 43))
(aver (eql (1- 42) 41))

(aver (symbolp '+))
(aver (symbolp '-))
(aver (fixnump '7))

(aver (fixnump 1234.))

#-(or abcl clisp x86-64)
(progn
  (aver (fixnump 536870911))
  (aver (fixnump '536870911))
  (aver (not (fixnump 536870912)))
  (aver (not (fixnump '536870912)))

  (aver (fixnump -536870912))
  (aver (fixnump '-536870912))
  (aver (not (fixnump -536870913)))
  (aver (not (fixnump '-536870913)))

  (aver (bignump 2147483646))
  (aver (bignump 2147483647))
  (aver (bignump 2147483648))

  (aver (bignump -2147483647))
  (aver (bignump -2147483648))
  (aver (bignump -2147483649))

  (aver (eql (* most-positive-fixnum most-positive-fixnum) 288230375077969921))
  (aver (eql (* most-negative-fixnum most-negative-fixnum) 288230376151711744))
  (aver (eql (* most-negative-fixnum most-positive-fixnum) -288230375614840832))

  (aver (eql (1+ most-positive-fixnum) 536870912))
  (aver (eql (1+ (1+ most-positive-fixnum)) 536870913))

  (aver (eql (1- most-negative-fixnum) -536870913))
  (aver (eql (1- (1- most-negative-fixnum)) -536870914))

  (aver (fixnump (1- (1+ most-positive-fixnum))))
  (aver (eql (1- (1+ most-positive-fixnum)) most-positive-fixnum))

  (aver (fixnump (1+ (1- most-negative-fixnum))))
  (aver (eql (1+ (1- most-negative-fixnum)) most-negative-fixnum))

  (aver (eql (- most-negative-fixnum) 536870912))
  (aver (fixnump (- (1+ most-positive-fixnum))))
  (aver (eql (- (1+ most-positive-fixnum)) most-negative-fixnum))
  )

(aver (eq (car '(foo bar)) 'foo))
(aver (eq (cadr '(foo bar)) 'bar))

(aver (eql (length '(1 2 3)) 3))
(aver (eql (length "this is a test") 14))

(aver (eql (funcall 'length "test") 4))
(aver (eql (funcall '+ 1 2 3 4 5 6) 21))
(aver (eql (funcall (lambda (x) (+ x x)) 42) 84))

(aver (eql (apply 'length '("test")) 4))

(aver (eql (apply '+ '(1 2 3 4 5 6)) 21))
(aver (eql (apply '+ 1 '(2 3 4 5 6)) 21))
(aver (eql (apply '+ 1 2 '(3 4 5 6)) 21))
(aver (eql (apply '+ 1 2 3 '(4 5 6)) 21))
(aver (eql (apply '+ 1 2 3 4 '(5 6)) 21))
(aver (eql (apply '+ 1 2 3 4 5 '(6)) 21))

(aver (eql (if 42 3 17) 3))
(aver (eql (if nil 3 17) 17))

(aver (eq (if (listp '(1 2 3)) 'list 'atom) 'list))

(aver (stringp "test"))
(aver (consp '(1 2 3)))
(aver (listp '(1 2 3)))

(aver (special-operator-p 'progn))
(aver (not (special-operator-p 'car)))

;; EQUAL
(aver (equal nil nil))
(aver (equal 42 42))
(aver (equal '(a b) '(a b)))
(aver (equal '(1 2) '(1 2)))
(aver (not (equal '(a b) '(a b c))))
(aver (not (equal '(a b c) '(a b))))
(aver (equal "test" "test"))

#+xcl
(progn
  ;; LIST-DELETE-EQ
  (aver (equal (sys:list-delete-eq 'a '(a b c)) '(b c)))
  (aver (equal (sys:list-delete-eq 'b '(a b c)) '(a c)))
  (aver (equal (sys:list-delete-eq 'c '(a b c)) '(a b)))
  (aver (equal (sys:list-delete-eq 'b '(a c)) '(a c)))
  (aver (equal (sys:list-delete-eq 'c '(a b)) '(a b)))
  (aver (equal (sys:list-delete-eq 'c '(c)) nil))
  (aver (equal (sys:list-delete-eq 'c '()) nil))
  (aver (equal (sys:list-delete-eq '1.0 '(1.0 2.0 3.0)) '(1.0 2.0 3.0)))
  (aver (equal (sys:list-delete-eq '2.0 '(1.0 2.0 3.0)) '(1.0 2.0 3.0)))
  (aver (equal (sys:list-delete-eq '3.0 '(1.0 2.0 3.0)) '(1.0 2.0 3.0)))
  (aver (equal (sys:list-delete-eq '2.0 '(1.0 3.0)) '(1.0 3.0)))
  (aver (equal (sys:list-delete-eq '3.0 '(1.0 2.0)) '(1.0 2.0)))
  (aver (equal (sys:list-delete-eq '3.0 '(3.0)) '(3.0)))
  (aver (equal (sys:list-delete-eq '3.0 '()) nil))
  ;; LIST-DELETE-EQL
  (aver (equal (sys:list-delete-eql 'a '(a b c)) '(b c)))
  (aver (equal (sys:list-delete-eql 'b '(a b c)) '(a c)))
  (aver (equal (sys:list-delete-eql 'c '(a b c)) '(a b)))
  (aver (equal (sys:list-delete-eql 'b '(a c)) '(a c)))
  (aver (equal (sys:list-delete-eql 'c '(a b)) '(a b)))
  (aver (equal (sys:list-delete-eql 'c '(c)) nil))
  (aver (equal (sys:list-delete-eql 'c '()) nil))
  (aver (equal (sys:list-delete-eql '1.0 '(1.0 2.0 3.0)) '(2.0 3.0)))
  (aver (equal (sys:list-delete-eql '2.0 '(1.0 2.0 3.0)) '(1.0 3.0)))
  (aver (equal (sys:list-delete-eql '3.0 '(1.0 2.0 3.0)) '(1.0 2.0)))
  (aver (equal (sys:list-delete-eql '2.0 '(1.0 3.0)) '(1.0 3.0)))
  (aver (equal (sys:list-delete-eql '3.0 '(1.0 2.0)) '(1.0 2.0)))
  (aver (equal (sys:list-delete-eql '3.0 '(3.0)) nil))
  (aver (equal (sys:list-delete-eql '3.0 '()) nil))
  ;; SUBSETP-EQL
  (aver (eq (sys:subsetp-eql '(1.0 2.0) '(4.0 3.0 2.0 1.0)) t))
  (aver (eq (sys:subsetp-eql '(1.0 2.0 3.0 4.0) '(4.0 3.0 2.0 1.0)) t))
  (aver (eq (sys:subsetp-eql '(42.0 1.0 2.0) '(4.0 3.0 2.0 1.0)) nil))
  (aver (eq (sys:subsetp-eql '(1.0 2.0 42.0) '(4.0 3.0 2.0 1.0)) nil))
  (aver (eq (sys:subsetp-eql '(4.0 3.0 2.0 1.0) '(1.0 2.0)) nil))
  (aver (eq (sys:subsetp-eql '((1) (2)) '((1) (2))) nil))
  (aver (eq (sys:subsetp-eql '((1 2)) '(1 "a" (1 2))) nil))
  )

(let ((ht (make-hash-table)))
  (aver (hash-table-p ht))
  (aver (zerop (hash-table-count ht)))
  (aver (eq (type-of ht) 'hash-table))
  #+xcl
  (aver (eq (sys:puthash 'foo ht 'bar) 'bar))
  #-xcl
  (aver (eq (setf (gethash 'foo ht) 'bar) 'bar))
  (multiple-value-bind (value present-p) (gethash 'foo ht)
    (aver (eq value 'bar))
    (aver (eq present-p t)))
  (aver (eql (hash-table-count ht) 1)))

;; (defun cons-madly () (dotimes (i 10000 t) (setq *x* (make-string 100000))))

;; (aver (cons-madly))

;; (defun cons-madly-m-t (count)
;;   (dotimes (i count) (make-thread (lambda () (time (cons-madly))))))

(defparameter *x* 42)

(when (fboundp 'foo)
  (fmakunbound 'foo))
(aver (not (fboundp 'foo)))
(defun foo () *x*)
(aver (fboundp 'foo))

(aver (eql (foo) 42))
(aver (eql (let ((*x* 7)) *x*) 7))
(aver (eql (let ((*x* 7)) (foo)) 7))
(aver (eql (foo) 42))

(aver (eql (let ((*x* 7)) (setq *x* 12) (foo)) 12))
(aver (eql (foo) 42))

(fmakunbound 'foo)
(unintern '*x*)

(aver (eql (block nil 1 2 (return-from nil 42) 3 4) 42))
(aver (equal (multiple-value-list (block b1 (return-from b1 (values 1 2 3 4)) 1))
               '(1 2 3 4)))

(aver (equal (multiple-value-list (block foo (values 'a 'b) (values 'c 'd)))
               '(c d)))

(defun foo (x) (return-from foo 42) (+ x x))
(aver (eql (foo 'a) 42))
(fmakunbound 'foo)

#+xcl
(aver (eq (type-of (symbol-function 'setq)) 'special-operator))
#+xcl
(aver (not (functionp (symbol-function 'setq))))
#+xcl
(aver (eq (symbol-package 'special-operator) (find-package "EXT")))

;; Multiple values.
(aver (equal (multiple-value-list (values 1 2 3)) '(1 2 3)))
(aver (equal (values) nil))
(aver (eq (values) nil))
(aver (eql (values 42) 42))

(aver (eql (let* ((x 42) (y (1+ x))) (+ x y)) 85))

;; LIST
(aver (eq (list) nil))
(aver (equal (list 1) '(1)))
(aver (equal (list 1 2) '(1 2)))
(aver (equal (list 1 2 3) '(1 2 3)))
(aver (equal (list 1 2 3 4) '(1 2 3 4)))
(aver (equal (list 1 2 3 4 5) '(1 2 3 4 5)))
(aver (equal (list 1 2 3 4 5 6) '(1 2 3 4 5 6)))

(aver (equal (macroexpand '(fixnump 42)) '(fixnump 42)))
(aver (equal (multiple-value-list (macroexpand '(fixnump 42))) '((fixnump 42) nil)))

(aver (fboundp 'car))
(aver (boundp 'most-positive-fixnum))
(aver (not (fboundp 'most-positive-fixnum)))

;; NCONC
(aver (eq (nconc) nil))
(let ((list (list 'a 'b 'c))) (aver (eq (nconc list) list)))
(let ((x (list 'a 'b 'c))
      (y (list 'd 'e 'f)))
  (aver (equal (nconc x y) '(a b c d e f)))
  (aver (equal x '(a b c d e f)))
  (aver (eq (cdddr x) y)))

(aver (eq (append) nil))
(let ((list '(a b c))) (aver (eq (append list) list)))
(aver (equal (append '(a b c) '(d e f) '() '(g)) '(a b c d e f g)))
(aver (equal (append '(a b c) 'd) '(a b c . d)))
(let ((list '(a b c)))
  (aver (equal (append list '(d)) '(a b c d)))
  (aver (equal list '(a b c))))

;; M-V-B
(multiple-value-bind (x y z) (values 1 2 3)
  (aver (equal (list z y x) '(3 2 1))))

(multiple-value-bind (x y z) (values 1 2 3)
  (let ((x 4))
    (aver (equal (list x y z) '(4 2 3)))))

(multiple-value-bind (x y z) (values 1 2 3 4 5 6)
  (aver (equal (list x y z) '(1 2 3))))

(multiple-value-bind (x y z) (values 1 2)
  (aver (equal (list x y z) '(1 2 nil))))

(multiple-value-bind () (values 1 2)
  (aver (equal (values 'a 'b 'c) 'a)))

(multiple-value-bind (x y z) (values)
  (aver (equal (list x y z) '(nil nil nil))))

(multiple-value-bind (x y z) 'foo
  (aver (equal (list x y z) '(foo nil nil))))

(aver (eq (multiple-value-bind (x) 'foo x) 'foo))

(aver (eq (multiple-value-bind () 'foo) nil))

(aver (eq (multiple-value-bind () (values)) nil))

(aver (eq (multiple-value-bind () (values 1 2 3 4 5)) nil))

;; CHAR=
(aver (char= #\a))
(aver (char= #\a #\a))
(aver (char= #\a #\a #\a))
(aver (char= #\a #\a #\a #\a))
(aver (char= #\a #\a #\a #\a #\a))
(aver (char= #\a #\a #\a #\a #\a #\a))

(aver (not (char= #\a #\A)))
(aver (not (char= #\a #\a #\b)))

;; ATOM
(aver (atom nil))
(aver (atom 42))
(aver (atom 'foo))
(aver (not (atom '(1 2))))

;; DEFVAR
(when (boundp '*defvar-test-var*)
  (makunbound '*defvar-test-var*))
(aver (not (boundp '*defvar-test-var*)))
(aver (eq (defvar *defvar-test-var* 42) '*defvar-test-var*))
(aver (boundp '*defvar-test-var*))
(aver (eql *defvar-test-var* 42))
(aver (eq (defvar *defvar-test-var* 'foo) '*defvar-test-var*))
(aver (eql *defvar-test-var* 42))

;; COPY-LIST
(aver (equal (copy-list '(a b c d)) '(a b c d)))
(aver (not (eq (copy-list '(a b c d)) '(a b c d))))
(aver (equal (copy-list '(a . b)) '(a . b)))
(aver (equal (copy-list '(a b c . d)) '(a b c . d)))

;; NRECONC
(let* ((x (list 'a 'b 'c))
       (y (copy-list '(d e f)))
       (result (nreconc x y)))
  (aver (equal y '(d e f)))
  (aver (equal result '(c b a d e f))))

;; LOOP
(let ((n 0) (counter 10))
  (loop
    (setq n (1+ n))
    (setq counter (1- counter))
    (when (zerop counter)
      (return)))
  (aver (eql n 10)))

;; AND, OR
(aver (eql (and 1 2 3) 3))
(aver (eql (and 1 nil 3) nil))

(aver (eql (or 1 2 3) 1))
(aver (eql (or nil nil 42) 42))

;; TAGBODY
(aver (eq (tagbody) nil))
(aver (eq (tagbody 42) nil))
(aver (eq (tagbody 'foo) nil))
(aver (eq (tagbody (values)) nil))
(aver (eq (tagbody (values 1 2 3 4 5)) nil))

(let ((x 0))
  (tagbody
   (setq x 1)
   (go a)
   (setq x 2)
   a)
  (aver (eql x 1)))

(let ((x 0))
  (tagbody
   (setq x 1)
   (go a)
   b
   (setq x 2)
   (go c)
   a
   (setq x 3)
   (go b)
   c)
  (aver (eql x 2)))

(let (result)
  (tagbody
   (block a
     (setq result 10)
     (go a))
   (setq result 20)
   a)
  (aver (eql result 10)))

(let (result)
  (tagbody
   (block a
     (setq result 10)
     (return-from a nil))
   (setq result 20)
   a)
  (aver (eql result 20)))

(aver (eq (block done
              (tagbody
               (go around)
               10
               (return-from done 'good)
               around
               (go 10)))
            'good))

(let ((x 0))
  (tagbody
   (setq x 7)
   (funcall (lambda () (go end)))
   (setq x 42)
   end)
  (aver (eql x 7)))

;; packages
(aver (null (find-package "FOO")))

(aver (find-package "COMMON-LISP"))
(aver (find-package "COMMON-LISP-USER"))
#+xcl
(aver (find-package "EXTENSIONS"))
#+xcl
(aver (find-package "SYSTEM"))

(aver (eq (find-package "CL") (find-package "COMMON-LISP")))
(aver (eq (find-package "CL-USER") (find-package "COMMON-LISP-USER")))
#+xcl
(aver (eq (find-package "EXT") (find-package "EXTENSIONS")))
#+xcl
(aver (eq (find-package "SYS") (find-package "SYSTEM")))

(aver (eq (symbol-package 'car) (find-package "COMMON-LISP")))

#-(or allegro clisp)
(aver (equal (package-nicknames (find-package "CL")) '("CL")))
#-(or allegro clisp)
(aver (equal (package-nicknames (find-package "CL-USER")) '("CL-USER")))

;; keywords
(aver (eq (symbol-package ':foo) (find-package "KEYWORD")))
(aver (boundp ':foo))
(aver (eq :foo :foo))

;; INTERN
#-allegro
(progn
  (aver (eq *package* (find-package "TESTS")))
  (multiple-value-bind (symbol status) (intern "CAR")
    (aver (eq symbol 'car))
    (aver (eq status :inherited)))
  )

(aver (eq (fmakunbound 'foo) 'foo))
(defun foo () nil)
(aver (null (foo)))
(defun foo (a) (list a))
(aver (equal (foo 'bar) '(bar)))
(defun foo (a b) (list a b))
(aver (equal (foo 1 2) (list 1 2)))
(defun foo (a b c) (list a b c))
(aver (equal (foo 1 2 3) '(1 2 3)))
(defun foo (a b c d) (list a b c d))
(aver (equal (foo 1 2 3 4) '(1 2 3 4)))

;; &optional
(defun foo (a b &optional c) (list a b c))
(aver (equal (foo 1 2) '(1 2 nil)))
(aver (equal (foo 1 2 'bar) '(1 2 bar)))

(defun foo (a b &optional (c 42)) (list a b c))
(aver (equal (foo 1 2) '(1 2 42)))
(aver (equal (foo 1 2 'bar) '(1 2 bar)))

(defun foo (a b &optional (c 42) (d 'bar)) (list a b c d))
(aver (equal (foo 1 2) '(1 2 42 bar)))
(aver (equal (foo 1 2 3) '(1 2 3 bar)))
(aver (equal (foo 1 2 3 4) '(1 2 3 4)))

(defun foo (a b &optional (c nil c-supplied-p)) (list a b c c-supplied-p))
(aver (equal (foo 1 2) '(1 2 nil nil)))
(aver (equal (foo 1 2 3) '(1 2 3 t)))

;; &rest
(defun foo (&rest a) a)
(aver (eq (foo) nil))
(aver (equal (foo 1 2 3) '(1 2 3)))

;; &key
(defun foo (&key a b c) (list a b c))
(aver (equal (foo) '(nil nil nil)))
(aver (equal (foo :a 1 :b 2 :c 3) '(1 2 3)))
(aver (equal (foo :c 42) '(nil nil 42)))

(defun foo (a &key b c) (list a b c))
(aver (equal (foo 42) '(42 nil nil)))
(aver (equal (foo 42 :c 7) '(42 nil 7)))
(aver (equal (foo 42 :b 3) '(42 3 nil)))
(aver (equal (foo 42 :b 1 :c 2) '(42 1 2)))

(defun foo (&key (a 42 a-p)) (list a a-p))
(aver (equal (foo) '(42 nil)))
(aver (equal (foo :a 17) '(17 t)))

;; &aux
(defun foo (&aux (x 4) (y 3)) (+ x y))
(aver (eql (foo) 7))

;; CASE
(aver (eq (case 'a) nil))
(aver (eq (case 10 (10 'a)) 'a))
(aver (eq (case "abc" ("abc" 'a)) nil))
(aver (eql (case 'z ((a b c) 1)
               ((d e) 2)
               ((f z g) 3)
               (t 4))
             3))
(aver (eq (case nil (nil 'a) (t 'b)) 'b))
(aver (eql (case t ((t) 10) (t 20)) 10))

;; STRING
(aver (equal (string 'foo) "FOO"))
(aver (equal (string "foo") "foo"))
(aver (equal (string #\x) "x"))

;; DOTIMES
(aver (eql (dotimes (i 10 42)) 42))
(aver (eql (dotimes (i 10 i)) 10))
(aver (eql
         (let ((sum 0))
           (dotimes (i 10)
             (setq sum (+ sum i)))
           sum)
         45))
(aver (eql
         (let ((sum 0))
           (dotimes (i 10)
             (setq sum (+ sum i))
             (when (eql i 5) (return)))
           sum)
         15))

;; DOLIST
(aver (eql (dolist (i '(1 2 3) 42)) 42))
(aver (eql
         (let ((sum 0))
           (dolist (i '(1 2 3 4 5 6 7 8 9))
             (setq sum (+ sum i)))
           sum)
         45))
(aver (eql
         (let ((sum 0))
           (dolist (i '(1 2 3 4 5 6 7 8 9))
             (setq sum (+ sum i))
             (when (eql i 5) (return)))
           sum)
         15))

;; FORMAT
(aver (equal (format nil "this is a test") "this is a test"))
(aver (equal (format nil "~D" 42) "42"))
(aver (equalp (format nil "~X" 42) "2a"))
(aver (equal (format nil "~A" :foo) "FOO"))
(aver (equal (format nil "~S" :foo) ":FOO"))

(aver (eq (unintern 'foo) t))
(aver (equal (multiple-value-list (find-symbol "FOO")) '(nil nil)))
(aver (equal (multiple-value-list (intern "FOO")) '(foo :internal)))
(aver (equal (multiple-value-list (find-symbol "FOO")) '(foo :internal)))
(aver (eq (export 'foo) t))
(aver (equal (multiple-value-list (find-symbol "FOO")) '(foo :external)))

;; MAPCAR
(aver (equal (mapcar #'car '((1 a) (2 b) (3 c))) '(1 2 3)))
;; (aver (equal (mapcar #'abs '(3 -4 2 -5 -6)) '(3 4 2 5 6)))
(aver (equal (mapcar #'cons '(a b c) '(1 2 3)) '((a . 1) (b . 2) (c . 3))))

;; VECTORP
(aver (vectorp (vector 1 2 3 4)))
(aver (vectorp "test"))
(aver (not (vectorp 'foo)))
(aver (not (vectorp 42)))

(defparameter *v*
  (make-array 4 :initial-contents '(1 2 3 4) :fill-pointer 4))

(aver (eql (length *v*) 4))
(aver (eql (vector-pop *v*) 4))
(aver (eql (length *v*) 3))
#-xcl
(aver (equalp *v* #(1 2 3)))
(aver (eql (vector-pop *v*) 3))
(aver (eql (length *v*) 2))
(aver (eql (vector-push-extend 42 *v*) 2))
(aver (eql (length *v*) 3))
#-xcl
(aver (equalp *v* #(1 2 42)))
(aver (eql (vector-push 7 *v*) 3))
(aver (eql (length *v*) 4))
#-xcl
(aver (equalp *v* #(1 2 42 7)))

(unintern '*v*)

(aver (eql (logand -1 #xffffffff) 4294967295))
(aver (eql (logand #xffffffff -1) 4294967295))
#-abcl
(aver (eql (* 536870911 536870911) 288230375077969921))
(aver (eql (logand (* most-positive-fixnum most-positive-fixnum)
                     (* most-positive-fixnum most-positive-fixnum))
             (* most-positive-fixnum most-positive-fixnum)))

(aver (eql (ldb (byte 2 1) 10) 1))
(aver (eql (ldb (byte 8 0) #x12345678) #x78))
(aver (eql (ldb (byte 8 8) #x12345678) #x56))
(aver (eql (ldb (byte 8 16) #x12345678) #x34))
(aver (eql (ldb (byte 8 24) #x12345678) #x12))

(aver (equal (make-list 4) '(nil nil nil nil)))
(aver (equal (make-list 4 :initial-element 7) '(7 7 7 7)))

(aver (eql (elt '(1 2 3 4) 0) 1))
(aver (eql (elt '(1 2 3 4) 3) 4))
(aver (eql (elt "foo" 1) #\o))

#+xcl
(aver (equal (sys:concatenate-to-string '("foo" "bar")) "foobar"))
#+xcl
(aver (equal (sys:concatenate-to-string '((#\f #\o #\o))) "foo"))

(aver (equal (concatenate 'string "foo" "bar") "foobar"))
(aver (equal (concatenate 'string '(#\f #\o #\o) "bar") "foobar"))

(aver (eql #c(1 0) 1))
(aver (eql #c(1 2) #c(1 2)))
(aver (not (eql #c(1 2) #c(1 3))))
(aver (not (eql #c(1 2) #c(2 1))))

(aver (equalp "foo" "FOO"))

(defun (setf foo) (obj cons) (rplaca cons obj) cons)
(aver (equal (funcall #'(setf foo) 42 (list 1 2 3)) '(42 2 3)))
(aver (fboundp '(setf foo)))
(aver (equal (fmakunbound '(setf foo)) '(setf foo)))
(aver (not (fboundp '(setf foo))))

(aver (typep 42 '(and number real)))
(aver (not (typep 42 '(and number symbol))))

(aver (eql (rational 0.33d0) 5944751508129055/18014398509481984))

(aver (eql (+ 1/2 (1+ 536870911)) 1073741825/2))
(aver (= 1/2 0.5))

(aver (eql (char-code #\A) 65))
(aver (eql (code-char 65) #\A))

(aver (typep (make-array 10) 'vector))
#+xcl
(aver (equal (type-of (make-array 10 :element-type '(unsigned-byte 8)))
               '(simple-array (unsigned-byte 8) (10))))
#+xcl
(aver (eq (type-of #c(1 2)) 'complex))

(aver (equal (let ((plist '()))
                 (incf (getf plist 'count 0))
                 plist)
               '(count 1)))

(aver (equal (subseq "" 0 0) ""))
(aver (equal (subseq "test" 1 nil) "est"))

#+xcl
(unless (fboundp 'ext::precompile-system)
  (load "precompiler.lisp"))

(defun test1 ()
  (macrolet ((foo (x) `(+ ,x ,x))) (flet ((foo (x) 42)) (foo 3))))
(aver (= (test1) 42))
(fmakunbound 'test1)

(defun test2 ()
  (macrolet ((foo (x) `(+ ,x ,x))) (labels ((foo (x) 42)) (foo 3))))
(aver (= (test2) 42))
(fmakunbound 'test2)

(defun test3 ()
  (macrolet ((foo (x) `(+ ,x ,x))) (flet ((bar (x) 42)) (foo 3))))
(aver (= (test3) 6))
(fmakunbound 'test3)

(defun test4 ()
  (macrolet ((foo (x) `(+ ,x ,x))) (labels ((bar (x) 42)) (foo 3))))
(aver (= (test4) 6))
(fmakunbound 'test4)

(defun test5 ()
  (flet ((foo (x) 42)) (macrolet ((foo (x) `(+ ,x ,x))) (foo 3))))
(aver (= (test5) 6))
(fmakunbound 'test5)

(defun test6 ()
  (labels ((foo (x) 42)) (macrolet ((foo (x) `(+ ,x ,x))) (foo 3))))
(aver (= (test6) 6))
(fmakunbound 'test6)

(defun test7 ()
  (flet ((foo (x) 42)) (macrolet ((bar (x) `(+ ,x ,x))) (foo 3))))
(aver (= (test7) 42))
(fmakunbound 'test7)

(defun test8 ()
  (labels ((foo (x) 42)) (macrolet ((bar (x) `(+ ,x ,x))) (foo 3))))
(aver (= (test8) 42))
(fmakunbound 'test8)

(unintern '*x*)
(defun test9 () (declare (special *x*)) *x*)
(aver (= (let ((*x* 17)) (declare (special *x*)) (test9)) 17))
(fmakunbound 'test9)
(unintern '*x*)

#-(or abcl clisp)
(progn
  (unintern '*x*)
  (defun test10 ()
    (macrolet ((bar (x) `(+ ,x ,x))) (declare (special *x*)) *x*))
  (aver (= (let ((*x* 17)) (declare (special *x*)) (test10)) 17))
  (fmakunbound 'test10)
  (unintern '*x*)
  )

(progn
  (let ((x 2))
    (defun test11 () x))
  (aver (eql (test11) 2))
  (fmakunbound 'test11))

(progn
  (defun test12 ()
    (let ((x 2))
      (defun test13 () x)))
  (test12)
  (aver (eql (test13) 2))
  (fmakunbound 'test12)
  (fmakunbound 'test13))

;; ASH
(aver (= (ash -1 1) -2))
(aver (= (ash -1 2) -4))
(aver (= (ash -1 3) -8))
(aver (= (ash -1 4) -16))
(aver (= (ash -1 5) -32))

(aver (= (ash -1 -1) -1))
(aver (= (ash -2 -2) -1))
(aver (= (ash -3 -3) -1))
(aver (= (ash -4 -4) -1))
(aver (= (ash -5 -5) -1))

(aver (= (ash 1 1) 2))
(aver (= (ash 1 2) 4))
(aver (= (ash 1 3) 8))
(aver (= (ash 1 4) 16))
(aver (= (ash 1 5) 32))

(aver (= (ash 7 1) 14))
(aver (= (ash 7 2) 28))
(aver (= (ash 7 3) 56))
(aver (= (ash 7 4) 112))
(aver (= (ash 7 5) 224))

(aver (= (ash most-positive-fixnum 1) (+ most-positive-fixnum most-positive-fixnum)))
(aver (= (ash most-positive-fixnum 2) (* most-positive-fixnum 4)))
(aver (= (ash most-positive-fixnum 3) (* most-positive-fixnum 8)))

;; CHARPOS
#+xcl
(aver (= (progn (format t "this is a test~%") (charpos *standard-output*)) 0))
#+xcl
(aver (= (progn (format t "~%this is a test") (charpos *standard-output*)) 14))
#+xcl
(aver (= (progn
             (format t "~%this is a test")
             (setf (charpos *standard-output*) 42)
             (charpos *standard-output*))
           42))
#+xcl
(aver (= (progn
             (format t "this is a test")
             (terpri)
             (charpos *standard-output*))
           0))
#+xcl
(aver (= (progn
             (format t "this is a test")
             (fresh-line)
             (charpos *standard-output*))
           0))

#-clisp
(aver (complexp #c(0.0 0)))

(aver (= #b10 2))
(aver (= #-b10 -2))
(aver (= #b11111111 255))
(aver (= #b1111111111111111 65535))

(aver (pathnamep #p""))

(aver (null (multiple-value-list (unwind-protect (values) t))))

(let ((numbers '(42
                 (1+ most-positive-fixnum)
                 3/4
                 (/ (+ most-positive-fixnum 1) (+ most-positive-fixnum 2))
                 1.3
                 1.3d0
                 #c(3 4)))
      (ops '(+ - / *))
      (failures 0))
  (dolist (op ops)
    (dolist (n1 numbers)
      (dolist (n2 numbers)
        (let* ((form (list op n1 n2))
               (result (ignore-errors (eval form))))
          (unless (numberp result)
            (format t "unsupported case ~S~%" form)
            (incf failures))))))
  (unless (zerop failures)
    (format t "~D failures~%" failures)))

(aver (eql (string<= "test" "test") 4))
(aver (char< #\a #\b #\c #\d))
(aver (not (char< #\a #\b #\c #\c)))

;; see also MISC.273
(aver (eql (block foo (unwind-protect (return-from foo 42) (return-from foo 7)))
        7))

(aver (eql (/ 3/4 #c(3 4)) #c(9/100 -3/25)))
(aver (eql (/ (/ 536870912 536870913) #c(3 4))
             #C(536870912/4473924275 -2147483648/13421772825)))
(aver (eql (/ 1.3 #c(3 4)) #c(0.15599999 -0.20799999)))

#-xcl
(aver (eql (/ 1.3d0 #c(3 4))
             #+abcl    #c(0.15600000000000003d0 -0.20800000000000002d0)
             #+allegro #c(0.15600000000000003d0 -0.20800000000000002d0)
             #+clisp   #c(0.156d0               -0.20800000000000002d0)
             #+sbcl    #c(0.15600000000000003d0 -0.20800000000000002d0)
             #+xcl     #c(0.156d0               -0.208d0)
             ))

(aver (equal
         (let ((x (/ 1.3d0 #c(3 4))))
           (list (rational (realpart x)) (rational (imagpart x))))
         #+abcl
         '(1405123083739595/9007199254740992 -3746994889972253/18014398509481984)
         #+allegro
         '(1405123083739595/9007199254740992 -3746994889972253/18014398509481984)
         #+clisp
         '(5620492334958379/36028797018963968 -3746994889972253/18014398509481984)
         #+sbcl
         '(1405123083739595/9007199254740992 -3746994889972253/18014398509481984)
         #+xcl
         '(1405123083739595/9007199254740992 -3746994889972253/18014398509481984)
         ))

(aver (eql (let ((x (list 1 2 3))) (symbol-macrolet ((car-x (car x))) car-x))
             1))
(aver (equal (let ((x (list 1 2 3))) (symbol-macrolet ((car-x (car x))) (setq car-x 42) x))
               '(42 2 3)))

(aver (eq (class-of 'car) (find-class 'symbol)))
(aver (eq (class-of '(1 2)) (find-class 'cons)))
(aver (eq (class-of 42)
            #+(or abcl sbcl) (find-class 'fixnum)
            #-(or abcl sbcl) (find-class 'integer)
            ))
(aver (eq (class-of #\x) (find-class 'character)))

(aver (eql .5 0.5))

(aver (subtypep (type-of 42) (class-of 42)))

(aver (subtypep 'list '(or cons null)))
(aver (subtypep '(or cons null) 'list))
(aver (subtypep 'list '(or null cons)))
(aver (subtypep 'list '(or cons null)))

(aver (subtypep 'null '(and symbol list)))
(aver (subtypep '(and symbol list) 'null))
(aver (subtypep 'null '(and list symbol)))
(aver (subtypep '(and list symbol) 'null))

;; nil vectors
#-abcl
(progn
  (aver (not (array-has-fill-pointer-p (make-array 0 :element-type nil))))
  (aver (array-has-fill-pointer-p (make-array 0 :element-type nil :fill-pointer t)))
  (aver (not (simple-string-p (make-array 0 :element-type nil :fill-pointer t))))
  (aver (simple-string-p (make-array 0 :element-type nil)))
  (aver (equal (type-of (make-array 10 :element-type nil)) '(simple-array nil (10))))
  (aver (equal (type-of (make-array 10 :element-type nil :fill-pointer t)) '(vector nil 10))))

;; bit-vectors
(aver (equal (type-of #*0011) '(simple-bit-vector 4)))
(aver (simple-bit-vector-p #*0011))
(aver (typep #*0011 'simple-bit-vector))
(aver (typep #*0011 'simple-array))
(aver (bit-vector-p #*0011))

(aver (equal (type-of (make-array 4 :element-type 'bit :fill-pointer 0)) '(bit-vector 4)))
(aver (not (simple-bit-vector-p (make-array 4 :element-type 'bit :fill-pointer 0))))
(aver (not (typep (make-array 4 :element-type 'bit :fill-pointer 0) 'simple-bit-vector)))
(aver (not (typep (make-array 4 :element-type 'bit :fill-pointer 0) 'simple-array)))
(aver (bit-vector-p (make-array 4 :element-type 'bit :fill-pointer 0)))

(aver (not (array-has-fill-pointer-p (make-array 5
                                                   :initial-contents '(1 0 1 0 1)
                                                   :adjustable t
                                                   :element-type 'bit))))

(aver (equal (array-dimensions #2a()) '(0 0)))

(aver (subtypep '(member #\a #\b #\c #\d #\e #\f #\g #\h) 'character))

(aver (equal (loop repeat 5 for x = (list 1 2 3 4 5) then (cdr x) finally (return x)) '(5)))

(makunbound 'x)
#+clisp
(import 'ext:special-variable-p)
#+(or abcl xcl clisp)
(aver (not (special-variable-p 'x)))
(aver (locally (declare (special x)) (eql (progv '(x) '(1) x) 1)))
(aver (not (boundp 'x)))

(defun test14 () (declare (special x)) x)
(aver (locally (declare (special x)) (eql (progv '(x) '(42) (test14)) 42)))
(fmakunbound 'test14)

(aver (string= (let ((x (make-array 2))
                     (*print-circle* t)
                     (*print-array* t))
                 (setf (aref x 0) 42
                       (aref x 1) x)
                 (write-to-string x))
               "#1=#(42 #1#)"))

#+xcl
(import '(sys:single-float-bits sys:double-float-high-bits sys:double-float-low-bits))
#+sbcl
(import '(sb-kernel:single-float-bits sb-kernel:double-float-high-bits sb-kernel:double-float-low-bits))
#+(or sbcl xcl)
(progn
  (aver (eql (single-float-bits 42.0) 1109917696))
  (aver (eql (single-float-bits -42.0) -1037565952))
  (aver (eql (double-float-high-bits 42.0d0) 1078263808))
  (aver (eql (double-float-low-bits 42.0d0) 0))
  (aver (eql (double-float-high-bits -42.0d0) -1069219840))
  (aver (eql (double-float-low-bits -42.0d0) 0)))

#+(or abcl sbcl xcl)
(progn
  (aver (eql 0.0d0 0.0d0))
  (aver (eql -0.0d0 -0.0d0))
  (aver (not (eql 0.0d0 -0.0d0)))
  (aver (not (eql -0.0d0 0.0d0)))
  (aver (equal 0.0d0 0.0d0))
  (aver (equal -0.0d0 -0.0d0))
  (aver (not (equal 0.0d0 -0.0d0)))
  (aver (not (equal -0.0d0 0.0d0)))
  (aver (equalp 0.0d0 0.0d0))
  (aver (equalp -0.0d0 -0.0d0))
  (aver (equalp 0.0d0 -0.0d0))
  (aver (equalp -0.0d0 0.0d0))
  (aver (= 0.0d0 0.0d0))
  (aver (= -0.0d0 -0.0d0))
  (aver (= 0.0d0 -0.0d0))
  (aver (= -0.0d0 0.0d0))
  (aver (eql (- 0.0d0) -0.0d0))
  (aver (eql (- -0.0d0) 0.0d0))
  (aver (eql (- 0 0.0d0) 0.0d0))
  (aver (eql (- 0 -0.0d0) 0.0d0))
  (aver (eql (imagpart 1.0) 0.0))
  (aver (eql (imagpart -1.0) -0.0))
  (aver (eql (imagpart 1.0d0) 0.0d0))
  (aver (eql (imagpart -1.0d0) -0.0d0))
  )

(aver (equal (multiple-value-list (floor 7 2)) '(3 1)))
(aver (equal (multiple-value-list (floor -7 2)) '(-4 1)))
(aver (equal (multiple-value-list (floor 7 -2)) '(-4 -1)))
(aver (equal (multiple-value-list (floor -7 -2)) '(3 -1)))
(aver (equal (multiple-value-list (ceiling 7 2)) '(4 -1)))
(aver (equal (multiple-value-list (ceiling -7 2)) '(-3 -1)))
(aver (equal (multiple-value-list (ceiling 7 -2)) '(-3 1)))
(aver (equal (multiple-value-list (ceiling -7 -2)) '(4 1)))
(aver (equal (multiple-value-list (truncate 7 2)) '(3 1)))
(aver (equal (multiple-value-list (truncate -7 2)) '(-3 -1)))
(aver (equal (multiple-value-list (truncate 7 -2)) '(-3 1)))
(aver (equal (multiple-value-list (truncate -7 -2)) '(3 -1)))
(aver (equal (multiple-value-list (mod 7 2)) '(1)))
(aver (equal (multiple-value-list (mod -7 2)) '(1)))
(aver (equal (multiple-value-list (mod 7 -2)) '(-1)))
(aver (equal (multiple-value-list (mod -7 -2)) '(-1)))
(aver (equal (multiple-value-list (rem 7 2)) '(1)))
(aver (equal (multiple-value-list (rem -7 2)) '(-1)))
(aver (equal (multiple-value-list (rem 7 -2)) '(1)))
(aver (equal (multiple-value-list (rem -7 -2)) '(-1)))

#+abcl
(unintern 'x) ; FIXME why is this necessary?

(defun test15 (x &aux (y 42))
  (declare (special x y))
  (list x y))
(aver (equal (test15 3) '(3 42)))
(fmakunbound 'test15)

(defun test16 ()
  (block b4
    (unwind-protect
        (complex (return-from b4 0) 0)
      (block foo
        (+ 3 4)
        (return-from foo 3)))))
(aver (eql (test16) 0))
