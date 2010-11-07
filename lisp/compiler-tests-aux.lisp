;;; compiler-tests-aux.lisp

(defun-compile test1 () 42)
(aver (eql (test1) 42))

(defun-compile test2 () 'a)
(aver (eql (test2) 'a))

(defun-compile test2a () '(a b c))
(aver (equal (test2a) '(a b c)))

(defun-compile test3 () (+ 7 42))
(aver (eql (test3) 49))

(defun-compile test4 (x) x)
(aver (eql (test4 42) 42))
(aver (eql (test4 'a) 'a))

(defun-compile test5 (x) (1+ x))
(aver (eql (test5 7) 8))
(aver (eql (test5 (1+ most-positive-fixnum)) (+ most-positive-fixnum 2)))
(fmakunbound 'test5)

(defun-compile test6 (n) (< n 2))
(aver (eql (test6 1) t))
(aver (eql (test6 3) nil))
(aver (eql (test6 (1+ most-positive-fixnum)) nil))
(aver (eql (test6 (1- most-negative-fixnum)) t))
(fmakunbound 'test6)

(defun-compile test7 (n) (- n 4))
(aver (eql (test7 0) -4))
(aver (eql (test7 1) -3))
(aver (eql (test7 42) 38))
(aver (eql (test7 (1+ most-negative-fixnum)) (- most-negative-fixnum 3)))
(aver (eql (test7 (1- most-negative-fixnum)) (- most-negative-fixnum 5)))
(fmakunbound 'test7)

(defun-compile test8 () (let* ((x 3)) x))
(aver (eql (test8) 3))

(defun-compile test9 (x) (let* ((y x)) y))
(aver (eql (test9 42) 42))

(defun-compile test10 (x)
  (let ((y (oddp x))
        (z 42))
    (list y z)))
(aver (equal (test10 3) '(t 42)))
(aver (equal (test10 4) '(nil 42)))

(defun-compile test11 (x) (let ((y 42)) (setq y x) y))
(aver (eql (test11 3) 3))

(defun-compile test12 (x) (setq x 42) x)
(aver (eql (test12 3) 42))

(defun-compile test13 () (values 1 2 3) 42)
(aver (equal (multiple-value-list (test13)) '(42)))

(defun-compile test14 (x) (declare (ignore x)) (return-from test14 42))
(aver (eql (test14 3) 42))

(defun-compile test15 () (loop (return 42)))
(aver (eql (test15) 42))

(defun-compile test16 (x) (loop (return x)))
(aver (eql (test16 3) 3))

(defun-compile test17 () (loop (let ((x 42)) (return x))))
(aver (eql (test17) 42))

(defun-compile test18 (x) (let ((sum 0)) (dotimes (i x) (setq sum (+ sum i))) sum))
(aver (eql (test18 100) 4950))

(defun-compile test19 (x)
  (case x
    (42 'a)
    (7 'b)
    (t 'c)))
(aver (eq (test19 42) 'a))
(aver (eq (test19 7) 'b))
(aver (eq (test19 nil) 'c))

(defun-compile test20 () #'+)
(aver (eq (test20) #'+))

(defun-compile test21 () (lambda () 42))
(aver (functionp (test21)))
(aver (compiled-function-p (test21)))
(aver (eql (funcall (test21)) 42))

(defun-compile test22 (x) (lambda () x))
(aver (functionp (test22 3)))
(aver (compiled-function-p (test22 3)))
(aver (eql (funcall (test22 3)) 3))

(defun-compile test23 ()
  (multiple-value-bind (x y) (values 1 2 3)
    (list x y)))
(aver (equal (test23) '(1 2)))

(defun-compile test24a (a) (list a))
(aver (equal (test24a 1) '(1)))
(defun-compile test24b (a b) (list a b))
(aver (equal (test24b 1 2) '(1 2)))
(defun-compile test24c (a b c) (list a b c))
(aver (equal (test24c 1 2 3) '(1 2 3)))
(defun-compile test24d (a b c d) (list a b c d))
(aver (equal (test24d 1 2 3 4) '(1 2 3 4)))
(defun-compile test24e (a b c d e) (list a b c d e))
(aver (equal (test24e 1 2 3 4 5) '(1 2 3 4 5)))
(defun-compile test24f (a b c d e f) (list a b c d e f))
(aver (equal (test24f 1 2 3 4 5 6) '(1 2 3 4 5 6)))
(defun-compile test24g (a b c d e f g) (list a b c d e f g))
(aver (equal (test24g 1 2 3 4 5 6 7) '(1 2 3 4 5 6 7)))
(defun-compile test24h (a b c d e f g h) (list a b c d e f g h))
(aver (equal (test24h 1 2 3 4 5 6 7 8) '(1 2 3 4 5 6 7 8)))

(defparameter *x* 42)
(defun-compile test25 () (let ((*x* 7)) *x*))
(aver (eql *x* 42))
(aver (eql (test25) 7))
(aver (eql *x* 42))

(defun-compile test26 () (let ((*x* 7)) (list *x* (setq *x* 3) *x*)))
(aver (eql *x* 42))
(aver (equal (test26) '(7 3 3)))
(aver (eql *x* 42))

(defun-compile test27 () (tagbody (values 1 2 3)))
(aver (eq (test27) nil))

(defun-compile test28 () (let ((x (values 1 2 3))) x))
(aver (eql (test28) 1))

(defparameter *x* 42)
(defun-compile test29 () (multiple-value-bind (*x* y) (values 17 3) (list *x* y)))
(aver (eql *x* 42))
(aver (equal (test29) '(17 3)))
(aver (eql *x* 42))

(defun-compile test30a () (list 1 2 3 4 5 6))
(aver (equal (test30a) '(1 2 3 4 5 6)))

(defun-compile test30b () (list 1 2 3 4 5 6 7))
(aver (equal (test30b) '(1 2 3 4 5 6 7)))

(defun-compile test30c () (list 1 2 3 4 5 6 (values 7 42 17)))
(aver (equal (test30c) '(1 2 3 4 5 6 7)))
(aver (equal (multiple-value-list (test30c)) '((1 2 3 4 5 6 7))))

(defun-compile test30d ()
  (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33))
(aver (equal (test30d) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33)))

(defun-compile test31 () #.(rplaca (make-list 256 :initial-element 42) 'list))
(aver (eql (length (test31)) 255))
(aver (equal (test31) (make-list 255 :initial-element 42)))

(defun-compile test32 (a b c d e f g) (declare (ignore a b c d)) (list e f g))
(aver (equal (test32 1 2 3 4 5 6 7) '(5 6 7)))

(defun-compile test33 (&rest x) x)
(aver (equal (test33 1 2 3) '(1 2 3)))

(defun-compile test34 (a b &rest x) (list a b x))
(aver (equal (test34 1 2 3) '(1 2 (3))))

(defun-compile test35 (a b c d e f g &rest x) (list a b c d e f g x))
(aver (equal (test35 1 2 3 4 5 6 7 8) '(1 2 3 4 5 6 7 (8))))
(aver (equal (test35 1 2 3 4 5 6 7 8 9) '(1 2 3 4 5 6 7 (8 9))))

(defun-compile test36 () (not (values 1 2 3)))
(aver (equal (multiple-value-list (test36)) '(nil)))

(defparameter *x* 42)
(defun-compile test37 ()
  (block nil
    (let ((*x* 31))
      (return)))
  *x*)
(aver (eql (test37) 42))
(aver (eql *x* 42))

(defun-compile test38 (&optional x) x)
(aver (eql (test38) nil))
(aver (eql (test38 42) 42))
(aver (equal (test38 (list 1 2 3)) '(1 2 3)))

(defun-compile test39 (&optional (x 42)) x)
(aver (eql (test39) 42))
(aver (eql (test39 17) 17))
(aver (equal (test39 (list 1 2 3)) '(1 2 3)))

(defun-compile test40 (x &optional y) (list x y))
(aver (equal (test40 42) '(42 nil)))
(aver (equal (test40 42 7) '(42 7)))
(aver (equal (test40 42 (list 1 2 3)) '(42 (1 2 3))))

(defun-compile test41 () (let ((i 0)) (mapcar #'(lambda (v) (list v (incf i))) '(a b c d e f))))
(aver (equal (test41) '((a 1) (b 2) (c 3) (d 4) (e 5) (f 6))))

(defun-compile test42 () (symbolp (values 'x 42)))
(aver (equal (multiple-value-list (test42)) '(t)))

(defun-compile test43 () (if (zerop (values 1 2 3)) 42 7))
(aver (equal (multiple-value-list (test43)) '(7)))

(defun-compile test44 () (eq (values 1 2 3) 99))
(aver (equal (multiple-value-list (test44)) '(nil)))

(defun-compile test45 () (logior 1 (values 1 2 3)))
(aver (equal (multiple-value-list (test45)) '(1)))

(defun-compile test46 () (+ 42 (values 1 2 3)))
(aver (equal (multiple-value-list (test46)) '(43)))

(defun-compile test47 () #'(lambda () (let ((x 42)) (declare (ignore x)))))
(aver (functionp (test47)))

;; x86-64 stack alignment tests
(defun-compile test48 () (let ((x 1.0d0)) (prin1-to-string x)))
(aver (equal (test48) "1.0d0"))

(defun-compile test49 (x) (prin1-to-string x))
(aver (equal (test49 1.0d0) "1.0d0"))

;; this will crash on x86-64 if called with bad stack alignment
(defun crash () (prin1-to-string 1.0d0))

(defun-compile test50a () (list (crash)))
(aver (equal (test50a) '("1.0d0")))
(defun-compile test50b () (list (crash) (crash)))
(aver (equal (test50b) '("1.0d0" "1.0d0")))
(defun-compile test50c () (list (crash) (crash) (crash)))
(aver (equal (test50c) '("1.0d0" "1.0d0" "1.0d0")))
(defun-compile test50d () (list (crash) (crash) (crash) (crash)))
(aver (equal (test50d) '("1.0d0" "1.0d0" "1.0d0" "1.0d0")))
(defun-compile test50e () (list (crash) (crash) (crash) (crash) (crash)))
(aver (equal (test50e) '("1.0d0" "1.0d0" "1.0d0" "1.0d0" "1.0d0")))
(defun-compile test50f () (list (crash) (crash) (crash) (crash) (crash) (crash)))
(aver (equal (test50f) '("1.0d0" "1.0d0" "1.0d0" "1.0d0" "1.0d0" "1.0d0")))
;; (defun-compile test50g () (list (crash) (crash) (crash) (crash) (crash) (crash) (crash)))
;; (aver (equal (test50g) '("1.0d0" "1.0d0" "1.0d0" "1.0d0" "1.0d0" "1.0d0" "1.0d0")))

(fmakunbound 'crash)

;; 51a: flet, no body forms
(defun-compile test51a ()
  (flet ((foo () 42))))
(aver (null (test51a)))

;; 51b: flet, no body forms, declare ignore
(defun-compile test51b ()
  (flet ((foo () 42))
    (declare (ignore (function foo)))))
(aver (null (test51b)))

;; 52a: flet, 0 args to local function, no closure vars
(defun-compile test52a ()
  (flet ((foo () 42))
    (foo)))
(aver (eql (test52a) 42))

;; 52b: flet, 0 args to local function, arg to top-level function is closure var
(defun-compile test52b (a)
  (flet ((foo () a))
    (foo)))
(aver (eql (test52b 42) 42))

;; 52c: flet, 0 args to local function, lexical var is closure var
(defun-compile test52c ()
  (let ((counter 0))
    (flet ((foo () (incf counter)))
      (foo))
    counter))
(aver (eql (test52c) 1))

;; 53a: flet, 1 arg to local function, no closure vars
(defun-compile test53a ()
  (flet ((foo (x) x))
    (foo 42)))
(aver (eql (test53a) 42))

;; 53b: flet, 1 arg to local function, arg to top-level function is closure var
(defun-compile test53b (a)
  (flet ((foo (x) (list a x)))
    (foo 42)))
(aver (equal (test53b 3) '(3 42)))

;; 54a: flet, 2 args to local function, no closure vars
(defun-compile test54a ()
  (flet ((foo (x y) (list x y)))
    (foo 42 7)))
(aver (equal (test54a) '(42 7)))

;; 54b: flet, 2 args to local function, arg to top-level function is closure var
(defun-compile test54b (a)
  (flet ((foo (x y) (list a x y)))
    (foo 42 7)))
(aver (equal (test54b 3) '(3 42 7)))

;; 55a: flet, top-level &rest arg is closure var
(defun-compile test55a (&rest x)
  (flet ((foo () x))
    (foo)))
(aver (equal (test55a 1 2 3) '(1 2 3)))

;; 55b: flet, 1 required arg to top-level function, top-level &rest arg is closure var
(defun-compile test55b (a &rest x)
  (flet ((bar () x))
    (list a (bar))))
(aver (equal (test55b 1 2 3 4) '(1 (2 3 4))))

;; 55b: flet, required arg to top-level function is closure var, top-level &rest arg is closure var
(defun-compile test55c (a &rest x)
  (flet ((foo () (list a x)))
    (foo)))
(aver (equal (test55b 1 2 3) '(1 (2 3))))

(defun-compile test56 (a b c d e)
  (flet ((foo () (list a b c d e)))
    (foo)))
(aver (equal (test56 1 2 3 4 5) '(1 2 3 4 5)))

(defun-compile test57a ()
  (flet ((foo () (return-from test57a 42)))
      (foo)))
(aver (eql (test57a) 42))

(defun-compile test57b ()
  (flet ((foo () (return-from test57b (values))))
    (foo)))
(aver (equal (multiple-value-list (test57b)) nil))

(defun-compile test57c ()
  (flet ((foo () (return-from test57c (values 1 2 3))))
    (foo)))
(aver (equal (multiple-value-list (test57c)) '(1 2 3)))

(defun-compile test57d (a)
  (flet ((foo () (if a (return-from test57d 42) 0)))
    (foo)))
(aver (eql (test57d t) 42))
(aver (eql (test57d nil) 0))

(defun-compile test58 ()
  (funcall #'(lambda () 42)))
(aver (eql (test58) 42))

(defun-compile test59 (a &rest b)
  (funcall (lambda () (list a b))))
(aver (equal (test59 1 2 3 4) '(1 (2 3 4))))

(defun-compile test60 ()
  (let ((x 42))
    (declare (special x))
    (tagbody
     (let ((x 87))
       (declare (special x))
       (go a))
     a
     (return-from test60 x))))
(aver (eql (test60) 42))

(defun-compile test61 ()
  (let ((x nil))
    (unwind-protect
        (values 1 2 (push 1 x))
      (incf (car x)))))
(aver (equal (multiple-value-list (test61)) '(1 2 (2))))

(defun-compile test62 (a b c d)
  (declare (ignorable c d))
  (labels ((%f15 (f15-1 f15-2)
              (declare (ignorable f15-2))
              (return-from %f15 (logand f15-1
                                        (labels ((%f12 ()
                                                    (let ((v8 (prog2 f15-1 a)))
                                                      (declare (ignorable v8))
                                                      0)))
                                          b)))))
    (%f15 b 0)))
(aver (eql (test62 -109366908145349715 107072 50884377019 -751) 107072))

(defun-compile test63 (x)
  (declare (type list x))
  (car x))
(aver (eq (test63 nil) nil))
(aver (eql (test63 '(1 2 3)) 1))

(defun-compile test64 (x)
  (declare (type list x))
  (cdr x))
(aver (eq (test64 nil) nil))
(aver (equal (test64 '(1 2 3)) '(2 3)))

(defun-compile test65 () (values (values 1 2)))
(aver (equal (multiple-value-list (test65)) '(1)))

(defun-compile test66()
  (flet ((bar (x) (car x))
         ((setf bar) (new-value x) (setf (car x) new-value)))
    (let ((list (list 1 2 3)))
      (values (bar list)
              (setf (bar list) 42)
              list))))
(aver (equal (multiple-value-list (test66)) '(1 42 (42 2 3))))

(defun-compile test67 (x &aux (y 42))
  (declare (special x y))
  (list x y))
(aver (equal (test67 3) '(3 42)))

(defun-compile test68 (a b c d)
  (declare (ignore a c d))
  (unwind-protect
      (setq b -27566969)
    (block b1
      (multiple-value-prog1
        859663
        (return-from b1 b)
        -4))))
(aver (equal (multiple-value-list (test68 1 2 3 4)) '(-27566969)))

(defun-compile test69 (a b c d)
  (declare (ignorable a b c d))
  (catch 'ct1
    (throw 'ct1
           (prog2
             (progn
               (tagbody
                (catch 'ct1
                  (go 0))
                0)
               nil
               d)
             b))))

(aver (equal (multiple-value-list (test69 3176506326374 1696064078696799 48 153095689011423641))
             '(1696064078696799)))

(defun-compile test70 ()
  (unwind-protect
      2
    (tagbody
     (catch 'ct2
       (throw 'ct2 (go 5)))
     5)))
(aver (equal (multiple-value-list (test70)) '(2)))

(defun-compile test71 (a b c d)
  (declare (type (integer -2000121727 19660) c))
  (declare (ignorable a b c d))
  (unwind-protect
      (if (oddp (the integer (shiftf c -1721526753))) 0 0)
    b))
(aver (equal (multiple-value-list (test71 502599338405798365 6157215579643 -1423245011 8)) '(0)))

(defun-compile test72 ()
  (let ((x 42))
    (funcall (lambda () (setq x 17)))
    x))
(aver (equal (multiple-value-list (test72)) '(17)))

(defun-compile test73 ()
  (let ((vector (make-array 4)))
    (flet ((bar (i) (let ((index i))
                      (setf (aref vector i)
                            (lambda () index)))))
      (dotimes (x 4)
        (bar x)))
    (mapcar #'funcall (coerce vector 'LIST))))
(aver (equal (test73) '(0 1 2 3)))

(defun-compile test74 (x)
  (flet ((bar (a b c d e)
              (list a b c d e)))
    (bar 1 2 3 4 x)))
(aver (equal (test74 42) '(1 2 3 4 42)))

(defun-compile test75 (x)
  (flet ((bar (a b c d e)
              (list a b c d e x)))
    (bar 1 2 3 4 5)))
(aver (equal (test75 42) '(1 2 3 4 5 42)))

(defun-compile test76 (x)
  (declare (type (integer 0 #.most-positive-fixnum) x))
  (floor x 2))
(aver (equal (multiple-value-list (test76 most-positive-fixnum))
             (multiple-value-list (floor most-positive-fixnum 2))))

(defun-compile test77 (x)
  (declare (type (integer 0 #.most-positive-fixnum) x))
  (floor x 64))
(aver (equal (multiple-value-list (test77 most-positive-fixnum))
             (multiple-value-list (floor most-positive-fixnum 64))))

(defun-compile test78 (x)
  (declare (type fixnum x))
  (floor x 64))
(aver (equal (multiple-value-list (test78 most-negative-fixnum))
             (multiple-value-list (floor most-negative-fixnum 64))))

(defun-compile test79 ()
  (block b7
    (flet ((%f15 (&key (key1 (unwind-protect (return-from b7 17) 42)))
                 (+ key1 1)))
      (%f15))))
(aver (eq (test79) 17))

(defun-compile test80 (a b c d e f g)
  (labels ((%f10 ()
                 (catch 'ct8
                   (throw 'ct8
                          (return-from %f10 42)))))
    (catch 'ct8
      (%f10)
      (throw 'ct8 87))))
(aver (eq (test80 1 2 3 4 5 6 7) 87))

(defun-compile test81 (a b c d e f g)
  (declare (ignorable a b c d e f g))
  (reduce #'(lambda (lmv2 lmv6)
             (catch (quote ct5)
               (throw (quote ct5) (progn
                                    (ash (round 0 (min -92 0))
                                         (min 55 0))
                                    a))))
          (list 0 b a 0 0 0 0 0)
          :from-end t))
(aver (eql (test81 -69416315734252 -48704632 5626822493822 536 13 2564750168117 -118311591316) -69416315734252))

(defun-compile fact (n)
  (labels
    ((fact1 (n m)
            (if (zerop n)
                m
                (fact1 (1- n) (* m n)))))
    (fact1 n 1)))
(aver (eql (fact 6) 720))

(defun-compile fib (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
(aver (eql (fib 10) 89))

(defun-compile tak (x y z)
  (if (not (< y x))
      z
      (tak (tak (1- x) y z)
	   (tak (1- y) z x)
	   (tak (1- z) x y))))
(aver (eql (tak 18 12 6) 7))
