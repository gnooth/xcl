;;; source-transforms.lisp
;;;
;;; Copyright (C) 2006-2008 Peter Graves <peter@armedbear.org>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(in-package "COMPILER")

(define-source-transform not (x)
  `(if ,x nil t))

(define-source-transform null (x)
  `(if ,x nil t))

(define-source-transform if (&whole form &rest args)
  (let ((test-form (first args))
        (consequent (second args))
        (alternate (third args)))
    (cond ((and (consp test-form)
                (length-eql test-form 2)
                (memq (%car test-form) '(NOT NULL)))
           `(if ,(%cadr test-form) ,alternate ,consequent))
          (t
           form))))

(define-source-transform logtest (x y)
  `(not (zerop (logand ,x ,y))))

(defconstant +type-predicates+ (make-hash-table :test 'equal))

(progn
  (clrhash +type-predicates+)
  (dolist (pair '((ARRAY              . arrayp)
                  (ATOM               . atom)
                  (BASE-CHAR          . characterp)
                  (BIT-VECTOR         . bit-vector-p)
                  (CHARACTER          . characterp)
                  (CLASS              . classp)
                  (COMPLEX            . complexp)
                  (CONDITION          . conditionp)
                  (CONS               . consp)
                  (DOUBLE-FLOAT       . double-float-p)
                  (FIXNUM             . fixnump)
                  (FLOAT              . floatp)
                  (FUNCTION           . functionp)
                  (HASH-TABLE         . hash-table-p)
                  (INTEGER            . integerp)
                  (KEYWORD            . keywordp)
                  (LIST               . listp)
                  (NULL               . null)
                  (NUMBER             . numberp)
                  (PACKAGE            . packagep)
                  (PATHNAME           . pathnamep)
                  (RATIO              . ratiop)
                  (RATIONAL           . rationalp)
                  (READTABLE          . readtablep)
                  (REAL               . realp)
                  (SEQUENCE           . sequencep)
                  (SIMPLE-ARRAY       . simple-array-p)
                  (SIMPLE-BASE-STRING . simple-string-p)
                  (SIMPLE-BIT-VECTOR  . simple-bit-vector-p)
                  (SIMPLE-STRING      . simple-string-p)
                  (SIMPLE-VECTOR      . simple-vector-p)
                  (SINGLE-FLOAT       . single-float-p)
                  (STANDARD-CHAR      . %standard-char-p)
                  (STANDARD-OBJECT    . standard-object-p)
                  (STREAM             . streamp)
                  (STRING             . stringp)
                  (STRUCTURE-OBJECT   . structure-object-p)
                  (SYMBOL             . symbolp)
                  (VECTOR             . vectorp)))
    (setf (gethash (canonicalize-type (%car pair)) +type-predicates+) (%cdr pair))))


(define-source-transform %typep (&whole form &rest args)
  (if (length-eql args 2) ; no environment arg
      (let* ((object (%car args))
             (type-specifier (%cadr args))
             (type (and (quoted-form-p type-specifier)
                        (canonicalize-type (%cadr type-specifier))))
             (predicate (and type (gethash type +type-predicates+))))
        (cond (predicate
               `(,predicate ,object))
              ((fixnum-type-p type)
               `(fixnum-typep ,object ,(second type) ,(third type)))
              ((and (consp type)
                    (eq (%car type) 'OR))
               (let ((types (%cdr type))
                     (obj (gensym)))
                 `(let ((,obj ,object))
                    (or ,@(mapcar (lambda (x) `(%typep ,obj ',(canonicalize-type x))) types)))))
              (t
               form)))
      form))

(define-source-transform two-arg-= (&whole form &rest args)
  (cond ((length-eql args 2)
         (cond ((eql (%car args) 0)
                `(zerop ,(%cadr args)))
               ((eql (%cadr args) 0)
                `(zerop ,(%car args)))
               (t
                form)))
        (t
         form)))

(define-source-transform + (&whole form &rest args)
  (let ((numargs (length args)))
    (cond ((> numargs 2)
           `(two-arg-+ ,(%car args) (+ ,@(%cdr args))))
          ((eql numargs 2)
           `(two-arg-+ ,(%car args) ,(%cadr args)))
          (t
           form))))

(define-source-transform - (&whole form &rest args)
  (let ((numargs (length args)))
    (cond ((> numargs 2)
           `(two-arg-- ,(%car args) (+ ,@(%cdr args))))
          ((eql numargs 2)
           `(two-arg-- ,(%car args) ,(%cadr args)))
          (t
           form))))

(define-source-transform / (&whole form &rest args)
  (let ((numargs (length args)))
    (case numargs
      (1
       `(two-arg-/ 1 ,(%car args)))
      (2
       `(two-arg-/ ,(%car args) ,(%cadr args)))
      (t
       form))))

(define-source-transform max (&whole form &rest args)
  (cond ((length-eql args 2)
         `(two-arg-max ,(%car args) ,(%cadr args)))
        (t
         form)))

(define-source-transform min (&whole form &rest args)
  (cond ((length-eql args 2)
         `(two-arg-min ,(%car args) ,(%cadr args)))
        (t
         form)))

(define-source-transform last (&whole form &rest args)
  (if (length-eql args 1)
      `(last1 ,(%car args))
      form))

(define-source-transform find-class (&whole form &rest args)
  (if (length-eql args 1)
      `(find-class-1 ,(%car args))
      form))

(define-source-transform logand (&whole form &rest args)
  (cond ((> (length args) 2)
         `(logand ,(%car args) (logand ,@(%cdr args))))
        (t
         form)))

(define-source-transform logior (&whole form &rest args)
  (cond ((> (length args) 2)
         `(logior ,(car args) (logior ,@(%cdr args))))
        (t
         form)))

(define-source-transform logxor (&whole form &rest args)
  (let ((numargs (length args)))
    (cond ((> numargs 2)
           `(two-arg-logxor ,(%car args) (logxor ,@(%cdr args))))
          ((eql numargs 2)
           `(two-arg-logxor ,(%car args) ,(%cadr args)))
          (t
           form))))

(define-source-transform mapcar2 (&whole form &rest args)
  (cond ((or (> *debug* *speed*)
             (> *space* *speed*))
         form)
        ((length-eql args 2)
         (let ((arg1 (%car args))
               (arg2 (%cadr args))
               (list (gensym))
               (result (gensym))
               (temp (gensym)))
           `(let* ((,list ,arg2)
                   (,result (list1 nil))
                   (,temp ,result))
              (loop
                (when (endp ,list)
                  (return (%cdr ,result)))
                (%rplacd ,temp (setq ,temp (list1 (funcall ,arg1 (%car ,list)))))
                (setq ,list (%cdr ,list))))))
        (t
         form)))

(define-source-transform funcall (&whole form &rest args)
  (if (< (length form) 2)
      form
      (let* ((operator-form (%cadr form))
             operator)
        (cond ((and (setq operator (and (quoted-form-p operator-form)
                                        (%cadr operator-form)))
                    (symbolp operator)
                    (kernel-function-p operator))
               `(,operator ,@(cdr args)))
              ((and (setq operator (and (consp operator-form)
                                        (length-eql operator-form 2)
                                        (eq (%car operator-form) 'FUNCTION)
                                        (%cadr operator-form)))
                    (symbolp operator)
                    (kernel-function-p operator))
               `(,operator ,@(cdr args)))
              (t
               form)))))

(define-source-transform assoc (&whole form &rest args)
  (case (length args)
    (2
     `(assql ,(%car args) ,(%cadr args)))
    (4
     (let ((arg3 (third args))
           (arg4 (fourth args)))
       (cond ((and (eq arg3 :test)
                   (member arg4 `(EQ #'EQ ,(list 'FUNCTION 'EQ)) :test #'equal))
              `(assq ,(%car args) ,(%cadr args)))
             ((and (eq arg3 :test)
                   (member arg4 `(EQL #'EQL ,(list 'FUNCTION 'EQL)) :test #'equal))
              `(assql ,(%car args) ,(%cadr args)))
             (t
              form))))
    (t
     form)))

(define-source-transform member (&whole form &rest args)
  (case (length args)
    (2
     `(memql ,(%car args) ,(%cadr args)))
    (4
     (let ((arg3 (third args))
           (arg4 (fourth args)))
       (cond ((and (eq arg3 :test)
                   (member arg4 `(EQ #'EQ ,(list 'FUNCTION 'EQ)) :test #'equal))
              `(memq ,(%car args) ,(%cadr args)))
             ((and (eq arg3 :test)
                   (member arg4 `(EQL #'EQL ,(list 'FUNCTION 'EQL)) :test #'equal))
              `(memql ,(%car args) ,(%cadr args)))
             (t
              form))))
    (t
     form)))

(define-source-transform position (&whole form item sequence &rest rest)
;;   (mumble "position source transform form = ~S~%" form)
  (let* ((test-form (ignore-errors (getf rest :test))))
    (when test-form
      (let (test op)
        (cond ((setq test (and (quoted-form-p test-form) (%cadr test-form)))
               (when (setq op (gethash test +two-arg-operators+))
;;                  (mumble "position source transform optimized case 1~%")
                 (setf (getf rest :test) (list 'QUOTE op))
;;                  (mumble "rest = ~S~%" rest)
                 (setq form `(position ,item ,sequence ,@rest))))
              ((setq test (and (consp test-form)
                               (length-eql test-form 2)
                               (eq (%car test-form) 'FUNCTION)
                               (%cadr test-form)))
               (when (setq op (gethash test +two-arg-operators+))
;;                  (mumble "position source transform optimized case 2~%")
                 (setf (getf rest :test) (list 'FUNCTION op))
                 (setq form `(position ,item ,sequence ,@rest))))))))
;;   (mumble "new form = ~S~%" form)
  form)

(define-source-transform sys::backq-list (&rest args)
  (case (length args)
    (0
     nil)
    (1
     `(list1 ,@args))
    (2
     `(list2 ,@args))
    (3
     `(list3 ,@args))
    (4
     `(list4 ,@args))
    (5
     `(list5 ,@args))
    (t
     `(list ,@args))))

(define-source-transform sys::backq-list* (&rest args)
  `(list* ,@args))

(define-source-transform sys::backq-append (&rest args)
  (cond ((length-eql args 2)
         `(two-arg-append ,@args))
        (t
         `(append ,@args))))

(define-source-transform sys::backq-nconc (&rest args)
  `(nconc ,@args))

(define-source-transform sys::backq-cons (&rest args)
  `(cons ,@args))

(define-source-transform sbit (&whole form simple-bit-array &rest subscripts)
  (mumble "sbit source transform~%")
  (cond ((length-eql subscripts 1)
         `(sbit1 ,simple-bit-array ,(%car subscripts)))
        (t
         form)))
