;;; p2-vector-ref.lisp
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

(defknown p2-vector-ref (t t) t)
(defun p2-vector-ref (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (type1 (derive-type arg1)))
      (cond ((eq type1 :unknown)
             (process-2-args args :default t)
             (emit-call-2 (if (zerop *safety*) '%vector-ref 'vector-ref) target))
            ((subtypep type1 'simple-vector)
             (p2-svref `(svref ,arg1 ,arg2) target))
            ((subtypep type1 'simple-bit-vector)
             (p2 `(sbit1 ,arg1 ,arg2) target))
            ((subtypep type1 'simple-string)
             (p2 `(schar ,arg1 ,arg2) target))
            ((subtypep type1 '(simple-array * (*)))
             (process-2-args args `(,$ax ,$dx) t) ; vector in rax, index in rdx
             (clear-register-contents $ax $dx)
             (let ((type2 (derive-type arg2))
                   (size (derive-vector-size type1))
                   ($dx-unboxed-p nil))
               (unless (zerop *safety*)
                 (unless (fixnum-type-p type2)
                   (inst :test +fixnum-tag-mask+ :dl)
                   (emit-jmp-short :nz (common-error-label 'error-not-fixnum $dx)))
                 (unless (and size (subtypep type2 (list 'INTEGER 0 (1- size))))
                   ;; check index against length
                   (unbox-fixnum $dx) ; unboxed index in $dx
                   (setq $dx-unboxed-p t)
                   (let ((ERROR-BAD-INDEX (make-label)))
                     (let ((*current-segment* :elsewhere)
                           (*register-contents* (copy-register-contents)))
                       (label ERROR-BAD-INDEX)
                       (box-fixnum $dx)
                       #+x86
                       (progn
                         (inst :push :edx)
                         (inst :push :eax))
                       #+x86-64
                       (progn
                         (inst :mov :rdx :rsi)
                         (inst :mov :rax :rdi))
                       (emit-call 'error-bad-index-for-vector)
                       (inst :exit))
                     (inst :cmp $dx `(,(- +vector-capacity-offset+ +typed-object-lowtag+) ,$ax))
                     (emit-jmp-short :le ERROR-BAD-INDEX))))
               ;; reaching here, index is in bounds (or safety is zero)
               (cond ((subtypep type1 '(simple-array (unsigned-byte 8) (*)))
                      (unless $dx-unboxed-p
                        (unbox-fixnum $dx))
                      (inst :movzbl `(,(- +simple-vector-data-offset+ +typed-object-lowtag+) ,$ax ,$dx) :eax)
                      (box-fixnum :eax)
                      (move-result-to-target target))
                     ((subtypep type1 '(simple-array (unsigned-byte 16) (*)))
                      (unless $dx-unboxed-p
                        (unbox-fixnum $dx))
                      (inst :movzwl `(,(- +simple-vector-data-offset+ +typed-object-lowtag+) ,$ax ,$dx 2) :eax)
                      (box-fixnum :eax)
                      (move-result-to-target target))
                     ((subtypep type1 '(simple-array (unsigned-byte 32) (*)))
                      (let (#+x86 (BIGNUM (make-label))
                            #+x86 (EXIT   (make-label)))
                        (unless $dx-unboxed-p
                          (unbox-fixnum $dx))
                        (inst :mov `(,(- +simple-vector-data-offset+ +typed-object-lowtag+) ,$ax ,$dx 4) :eax)
                        #+x86
                        (progn
                          (inst :cmp most-positive-fixnum :eax)
                          (emit-jmp-short :a BIGNUM))
                        (box-fixnum $ax)
                        #+x86 (label EXIT)
                        (move-result-to-target target)
                        #+x86
                        (let ((*current-segment* :elsewhere)
                              (*register-contents* (copy-register-contents)))
                          (label BIGNUM)
                          (inst :push :eax)
                          (emit-call-1 "RT_make_unsigned_bignum" :eax)
                          (emit-jmp-short t EXIT))))
                     (t
                      (when $dx-unboxed-p
                        (box-fixnum $dx))
                      #+x86
                      (progn
                        (inst :push $dx)
                        (inst :push $ax))
                      #+x86-64
                      (progn
                        (inst :mov $ax $di)
                        (inst :mov $dx $si))
                      (emit-call-2 '%vector-ref target)))))
            ((subtypep type1 'vector)
             ;; vector but not simple-array
             (process-2-args args :default t)
             (emit-call-2 '%vector-ref target))
            (t
             (process-2-args args :default t)
             (emit-call-2 (if (zerop *safety*) '%vector-ref 'vector-ref) target))))
    t))
