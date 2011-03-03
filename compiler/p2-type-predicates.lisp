;;; p2-type-predicates.lisp
;;;
;;; Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

(defun p2-test-widetag-bit (register widetag-bit label-if-false)
  (aver (eq register $ax))
  (inst :mov :al :dl)
  (clear-register-contents $dx)
  (inst :and +lowtag-mask+ :dl)
  (inst :cmp +typed-object-lowtag+ :dl)
  (emit-jmp-short :ne label-if-false)
  (aver (typep widetag-bit '(unsigned-byte 32)))
  (inst #+x86 :testl #+x86-64 :testq
        widetag-bit
        `(,(- +widetag-offset+ +typed-object-lowtag+) ,$ax))
  (emit-jmp-short :z label-if-false))

(defun p2-test-widetag (register widetag label-if-false)
  (aver (eq register $ax))
  (inst :mov :al :dl)
  (clear-register-contents $dx)
  (inst :and +lowtag-mask+ :dl)
  (inst :cmp +typed-object-lowtag+ :dl)
  (emit-jmp-short :ne label-if-false)
  (aver (typep widetag '(unsigned-byte 32)))
  (inst #+x86 :cmpl #+x86-64 :cmpq
        widetag
        `(,(- +widetag-offset+ +typed-object-lowtag+) ,$ax))
  (emit-jmp-short :nz label-if-false))

(defmacro define-type-predicate-handler (name type-predicate-test-handler)
  `(defun ,name (form target)
     (when (check-arg-count form 1)
       (let* ((NO (make-label))
              (EXIT (make-label)))
         (,type-predicate-test-handler form NO)
         (p2 t target)
         (emit-jmp-short t EXIT)
         (label NO)
         (p2 nil target)
         (label EXIT))
       t)))

(defknown p2-test-simple-vector-p (t t) t)
(defun p2-test-simple-vector-p (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (process-1-arg arg $ax t)
      (p2-test-widetag $ax +simple-vector-widetag+ label-if-false))
    t))

(define-type-predicate-handler p2-simple-vector-p p2-test-simple-vector-p)

(defknown p2-test-vectorp (t t) t)
(defun p2-test-vectorp (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (process-1-arg arg $ax t)
      (p2-test-widetag-bit $ax +widetag-vector-bit+ label-if-false))
    t))

(define-type-predicate-handler p2-vectorp p2-test-vectorp)

(defknown p2-test-functionp (t t) t)
(defun p2-test-functionp (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (process-1-arg arg $ax t)
      (p2-test-widetag-bit $ax +widetag-function-bit+ label-if-false))
    t))

(define-type-predicate-handler p2-functionp p2-test-functionp)

(defun p2-test-numberp (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form))
          (EXIT (make-label)))
      (process-1-arg arg $ax t)
      (inst :test +fixnum-tag-mask+ :al)
      (emit-jmp-short :e EXIT)
      (p2-test-widetag-bit $ax +widetag-number-bit+ label-if-false)
      (label EXIT))
    t))

(define-type-predicate-handler p2-numberp p2-test-numberp)

(defknown %p2-test-fixnump (t t t) t)
(defun %p2-test-fixnump (form label-if-true label-if-false)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (type (derive-type arg)))
      (cond ((fixnum-type-p type)
             (when label-if-true
               (emit-jmp-short t label-if-true)))
            (t
             (let* ((var (and (var-ref-p arg) (var-ref-var arg)))
                    (reg (and var (find-register-containing-var var))))
               (unless reg
                 (process-1-arg arg $ax t)
                 (setq reg $ax))
               (inst :test +fixnum-tag-mask+ (reg8 reg))
               (when label-if-true
                 (emit-jmp-short :e label-if-true))
               (when label-if-false
                 (emit-jmp-short :ne label-if-false))))))
    t))

(defknown p2-test-fixnump (t t) t)
(defun p2-test-fixnump (test-form label)
  (%p2-test-fixnump test-form nil label))

(defun p2-test-integerp (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form))
          (EXIT (make-label)))
      (process-1-arg arg $ax t)
      (inst :test +fixnum-tag-mask+ :al)
      (emit-jmp-short :e EXIT)
      (p2-test-widetag $ax +bignum-widetag+ label-if-false)
      (label EXIT))
    t))

(define-type-predicate-handler p2-integerp p2-test-integerp)

(defknown p2-test-stringp (t t) t)
(defun p2-test-stringp (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (process-1-arg arg $ax t)
      (p2-test-widetag-bit $ax +widetag-string-bit+ label-if-false))
    t))

(define-type-predicate-handler p2-stringp p2-test-stringp)
