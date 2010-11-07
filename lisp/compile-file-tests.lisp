(in-package "CL-USER")

(require "DUMP-FORM")

(defparameter *compile-file-test-source-file*
  (merge-pathnames "compile-file-test-source.lisp" *load-truename*))

#+xcl
(defmacro defun-compile-file (name lambda-list &body body)
  `(progn
     (fmakunbound ',name)
     (with-open-file (stream *compile-file-test-source-file*
                             :direction :output
                             :if-exists :supersede)
       (c:dump-top-level-form
        '(defun ,name ,lambda-list ,@body)
        stream))
     (let ((c::*catch-errors* nil))
       (load (compile-file *compile-file-test-source-file*)))))

#-xcl
(defmacro defun-compile-file (name lambda-list &body body)
  `(progn
     (fmakunbound ',name)
     (with-open-file (stream *compile-file-test-source-file*
                             :direction :output
                             :if-exists :supersede)
       (with-standard-io-syntax
         (write
          '(defun ,name ,lambda-list ,@body)
          :stream stream)))
     (load (compile-file *compile-file-test-source-file*))))

(defun-compile-file test51a ()
  (flet ((foo () 42))
    (foo)))
(aver (eql (test51a) 42))

(defun-compile-file test51b (a)
  (flet ((foo () a))
    (foo)))
(aver (eql (test51b 42) 42))

(defun-compile-file test52a ()
  (flet ((foo (x) x))
    (foo 42)))
(aver (eql (test52a) 42))

(defun-compile-file test52b (a)
  (flet ((foo (x) (list a x)))
    (foo 42)))
(aver (equal (test52b 3) '(3 42)))

(defun-compile-file test53a ()
  (flet ((foo (x y) (list x y)))
    (foo 42 7)))
(aver (equal (test53a) '(42 7)))

(defun-compile-file test53b (a)
  (flet ((foo (x y) (list a x y)))
    (foo 42 7)))
(aver (equal (test53b 3) '(3 42 7)))

(defun-compile-file test57a ()
  (flet ((foo () (return-from test57a 42)))
    (foo)))
(aver (eql (test57a) 42))

(defun-compile-file test57b ()
  (flet ((foo () (return-from test57b (values))))
    (foo)))
(aver (equal (multiple-value-list (test57b)) nil))

(defun-compile-file test57c ()
  (flet ((foo () (return-from test57c (values 1 2 3))))
    (foo)))
(aver (equal (multiple-value-list (test57c)) '(1 2 3)))

(defun-compile-file test57d (a)
  (flet ((foo () (if a (return-from test57d 42) 0)))
    (foo)))
(aver (eql (test57d t) 42))
(aver (eql (test57d nil) 0))

(defun-compile-file test58 ()
  (funcall #'(lambda () 42)))
(aver (eql (test58) 42))

(defun-compile-file test59 (a &rest b)
  (funcall (lambda () (list a b))))
(aver (equal (test59 1 2 3 4) '(1 (2 3 4))))

(defun-compile-file test60 ()
  (let ((x 42))
    (declare (special x))
    (tagbody
     (let ((x 87))
       (declare (special x))
       (go a))
     a
     (return-from test60 x))))
(aver (eql (test60) 42))

(defun-compile-file test61 ()
  (let ((x nil))
    (unwind-protect
        (values 1 2 (push 1 x))
      (incf (car x)))))
(aver (equal (multiple-value-list (test61)) '(1 2 (2))))

(defun-compile-file test62 (a b c d)
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

(defun-compile-file test63 (x)
  (declare (type list x))
  (car x))
(aver (eq (test63 nil) nil))
(aver (eql (test63 '(1 2 3)) 1))

(defun-compile-file test64 (x)
  (declare (type list x))
  (cdr x))
(aver (eq (test64 nil) nil))
(aver (equal (test64 '(1 2 3)) '(2 3)))

(defun-compile-file test65 () (values (values 1 2)))
(aver (equal (multiple-value-list (test65)) '(1)))

(defun-compile-file test66()
  (flet ((bar (x) (car x))
         ((setf bar) (new-value x) (setf (car x) new-value)))
    (let ((list (list 1 2 3)))
      (values (bar list)
              (setf (bar list) 42)
              list))))
(aver (equal (multiple-value-list (test66)) '(1 42 (42 2 3))))

(defun-compile-file test67 (x &aux (y 42))
  (declare (special x y))
  (list x y))
(aver (equal (test67 3) '(3 42)))

(defun-compile-file test68 (a b c d)
  (declare (ignore a c d))
  (unwind-protect
      (setq b -27566969)
    (block b1
      (multiple-value-prog1
        859663
        (return-from b1 b)
        -4))))
(aver (equal (multiple-value-list (test68 1 2 3 4)) '(-27566969)))

(defun-compile-file test70 ()
  (unwind-protect
      2
    (tagbody
     (catch 'ct2
       (throw 'ct2 (go 5)))
     5)))
(aver (equal (multiple-value-list (test70)) '(2)))

(defun-compile-file test71 (a b c d)
  (declare (type (integer -2000121727 19660) c))
  (declare (ignorable a b c d))
  (unwind-protect
      (if (oddp (the integer (shiftf c -1721526753))) 0 0)
    b))
(aver (equal (multiple-value-list (test71 502599338405798365 6157215579643 -1423245011 8)) '(0)))

(defun-compile-file test72 ()
  (let ((x 42))
    (funcall (lambda () (setq x 17)))
    x))
(aver (equal (multiple-value-list (test72)) '(17)))

(defun-compile-file test73 ()
  (let ((vector (make-array 4)))
    (flet ((bar (i) (let ((index i))
                      (setf (aref vector i)
                            (lambda () index)))))
      (dotimes (x 4)
        (bar x)))
    (mapcar #'funcall (coerce vector 'LIST))))
(aver (equal (test73) '(0 1 2 3)))

(defun-compile-file test74 (x)
  (flet ((bar (a b c d e)
              (list a b c d e)))
    (bar 1 2 3 4 x)))
(aver (equal (test74 42) '(1 2 3 4 42)))

(defun-compile-file test75 (x)
  (flet ((bar (a b c d e)
              (list a b c d e x)))
    (bar 1 2 3 4 5)))
(aver (equal (test75 42) '(1 2 3 4 5 42)))

(defun-compile-file test76 (x)
  (declare (type (integer 0 #.most-positive-fixnum) x))
  (floor x 2))
(aver (equal (multiple-value-list (test76 most-positive-fixnum))
             (multiple-value-list (floor most-positive-fixnum 2))))

(defun-compile-file test77 (x)
  (declare (type (integer 0 #.most-positive-fixnum) x))
  (floor x 64))
(aver (equal (multiple-value-list (test77 most-positive-fixnum))
             (multiple-value-list (floor most-positive-fixnum 64))))

(defun-compile-file test78 (x)
  (declare (type fixnum x))
  (floor x 64))
(aver (equal (multiple-value-list (test78 most-negative-fixnum))
             (multiple-value-list (floor most-negative-fixnum 64))))

(defun-compile-file test79 ()
  (block b7
    (flet ((%f15 (&key (key1 (unwind-protect (return-from b7 17) 42)))
                 (+ key1 1)))
      (%f15))))
(aver (eq (test79) 17))

(defun-compile-file test80 (a b c d e f g)
  (labels ((%f10 ()
                 (catch 'ct8
                   (throw 'ct8
                          (return-from %f10 42)))))
    (catch 'ct8
      (%f10)
      (throw 'ct8 87))))
(aver (eq (test80 1 2 3 4 5 6 7) 87))

(defun-compile-file test81 (a b c d e f g)
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

(defun-compile-file test82 (a b c d e f g)
  (declare (ignorable a b c d e f g))
  (unwind-protect
      (progn
        (tagbody
         (logorc1 (multiple-value-prog1 d) (go tag6))
         tag6)
        0)
    0))
(aver (eq (test82 6947 231925617994586402 10 73602930704 13891693762 -22562281728870797 22) 0))

(defun-compile-file fact (n)
  (labels
    ((fact1 (n m)
            (if (zerop n)
                m
                (fact1 (1- n) (* m n)))))
    (fact1 n 1)))
(aver (eql (fact 6) 720))

(when (probe-file *compile-file-test-source-file*)
  (delete-file *compile-file-test-source-file*))

(let ((fasl (compile-file-pathname *compile-file-test-source-file*)))
  (when (probe-file fasl)
    (delete-file fasl)))
