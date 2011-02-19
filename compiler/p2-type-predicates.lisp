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

(defun p2-simple-vector-p (form target)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (NO (make-label))
           (EXIT (make-label)))
      (process-1-arg arg $ax t)
      (inst :mov :al :dl)
      (clear-register-contents $dx)
      (inst :and +lowtag-mask+ :dl)
      (inst :cmp +typed-object-lowtag+ :dl)
      (emit-jmp-short :ne NO)
      (aver (typep +simple-vector-widetag+ '(unsigned-byte 32)))
      (inst #+x86 :cmpl #+x86-64 :cmpq
            +simple-vector-widetag+
            `(,(- +widetag-offset+ +typed-object-lowtag+) ,$ax))
      (emit-jmp-short :ne NO)
      (p2 t target)
      (emit-jmp-short t EXIT)
      (label NO)
      (p2 nil target)
      (label EXIT))
    t))

(defknown p2-test-simple-vector-p (t t t) t)
(defun p2-test-simple-vector-p (form label-if-false)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (process-1-arg arg $ax t)
      (inst :mov :al :dl)
      (clear-register-contents $dx)
      (inst :and +lowtag-mask+ :dl)
      (inst :cmp +typed-object-lowtag+ :dl)
      (emit-jmp-short :ne label-if-false)
      (aver (typep +simple-vector-widetag+ '(unsigned-byte 32)))
      (inst #+x86 :cmpl #+x86-64 :cmpq
            +simple-vector-widetag+
            `(,(- +widetag-offset+ +typed-object-lowtag+) ,$ax))
      (emit-jmp-short :ne label-if-false))
    t))
