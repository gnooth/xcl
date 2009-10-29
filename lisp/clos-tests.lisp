;;; clos-tests.lisp

#+xcl
(require "CLOS")

#+sbcl
(use-package "SB-MOP")

(aver (eq (class-of (find-class 'class))           (find-class 'standard-class)))
(aver (eq (type-of  (find-class 'class))           'standard-class))

(aver (eq (class-of (find-class 'built-in-class))  (find-class 'standard-class)))
(aver (eq (type-of  (find-class 'built-in-class))  'standard-class))

(aver (eq (class-of (find-class 'structure-class)) (find-class 'standard-class)))
(aver (eq (type-of (find-class 'structure-class))  'standard-class))

(aver (eq (class-of (find-class 'standard-class))  (find-class 'standard-class)))
(aver (eq (type-of  (find-class 'standard-class))  'standard-class))

(aver (eq (class-of (defclass test1 () ())) (find-class 'standard-class)))
(setf (find-class 'test1) nil)

(defclass test2 () ())
(aver (eq (class-of (make-instance 'test2)) (find-class 'test2)))
(aver (eq (type-of (make-instance 'test2)) 'test2))
(aver (typep (make-instance 'test2) (find-class 'test2)))
(aver (typep (make-instance 'test2) 'test2))
(setf (find-class 'test2) nil)

(defstruct test3 a b c)
(aver (eq (class-of (make-test3)) (find-class 'test3)))
(aver (typep (class-of (make-test3)) 'structure-class))
(aver (eq (class-of (class-of (make-test3))) (find-class 'structure-class)))

(defclass test4 () (a))
(let ((x (make-instance 'test4)))
  (aver (eql (length (class-slots (find-class 'test4))) 1))
  (defclass test4 () (a b))
  (aver (eql (length (class-slots (find-class 'test4))) 2))
  (aver (slot-exists-p x 'b))
  (aver (not (slot-boundp x 'b))))
(setf (find-class 'test4) nil)

(defmethod test5 ((x number)) (declare (optimize speed)) (+ x x))
(aver (typep #'test5 'standard-generic-function))
(aver (typep #'test5 'standard-object))
(aver (eql (test5 3) 6))
(fmakunbound 'test5)

(defgeneric test6 (x))
(defmethod test6 ((x number))
  (+ x x))
(defmethod test6 :around ((x integer))
  (declare (ignore x))
  (+ 42 (call-next-method)))
(aver (eql (test6 3.0) 6.0))
(aver (eql (test6 3) 48))
(fmakunbound 'test6)

(defgeneric test7 (x))
(defmethod test7 ((x t)) t)
(defmethod test7 ((x (eql 42))) 42)
(defmethod test7 ((x (eql (+ 42 3)))) 45)
(aver (eql (test7 3) t))
(aver (eql (test7 42) 42))
(aver (eql (test7 45) 45))
(aver (eql (test7 'a) t))
