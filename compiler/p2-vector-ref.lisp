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
#+x86-64
(defun p2-vector-ref (form target)
  (mumble "p2-vector-ref new version~%")
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (type1 (derive-type arg1))
           type2
           size)
      (when (eq type1 :unknown)
        (process-2-args args :default t)
        (emit-call-2 (if (zerop *safety*) '%vector-ref 'vector-ref) target)
        (return-from p2-vector-ref t))
      (cond ((subtypep type1 'simple-vector)
             (p2-svref form target))
            ((subtypep type1 'simple-bit-vector)
             (p2 `(sbit1 ,arg1 ,arg2) target))
            ((subtypep type1 'simple-string)
             (p2 `(schar ,arg1 ,arg2) target))
            ((subtypep type1 'simple-array)
             (process-2-args args `(,$ax ,$dx) t) ; vector in rax, index in rdx
             (clear-register-contents $ax $dx)
             (cond ((subtypep type1 '(simple-array (unsigned-byte 8) (*)))
                    (cond ((or (zerop *safety*)
                               (and (neq (setq type2 (derive-type arg2)) :unknown)
                                    (integer-type-p type2)
                                    (setq size (derive-vector-size type1))
                                    (subtypep type2 (list 'INTEGER 0 (1- size)))))
                           (mumble "p2-vector-ref new code 1~%")
;;                            (process-2-args args '(:rax :rdx) t) ; vector in rax, index in rdx
;;                            (clear-register-contents :rax :rdx)
;;                            (inst :add (- +simple-vector-data-offset+ +typed-object-lowtag+) :rax)
;;                            ;; index is in rdx
;;                            ;; get rid of the fixnum shift
                           (unbox-fixnum $dx)
;;                            (inst :add :rdx :rax)
                           (inst :add :rax :rdx)
                           (inst :mov `(,(- +simple-vector-data-offset+ +typed-object-lowtag+) ,$dx) :al)
;;                            (emit-bytes #x48 #x0f #xb6 #x00) ; movzbq (%rax),%rax
                           (inst :movzbl :al :eax)
                           (box-fixnum :eax)
                           (move-result-to-target target))
                          (t
                           (mumble "p2-vector-ref %vector-ref case 1 type1 = ~S type2 = ~S~%" type1 type2)
                           (process-2-args args :default t)
                           (emit-call-2 '%vector-ref target))))
                   ((subtypep type1 '(simple-array (unsigned-byte 32) (*)))
                    (cond ((or (zerop *safety*)
                               (and (neq (setq type2 (derive-type arg2)) :unknown)
                                    (integer-type-p type2)
                                    (setq size (derive-vector-size type1))
                                    (subtypep type2 (list 'INTEGER 0 (1- size)))))
                           (mumble "p2-vector-ref new code 2~%")
;;                            (process-2-args args '(:rax :rdx) t) ; vector in rax, index in rdx
;;                            (clear-register-contents :rax)
;;                            (inst :add (- +simple-vector-data-offset+ +typed-object-lowtag+) :rax)
                           ;; index is in rdx
                           ;; get rid of the fixnum shift and multiply by 4 to get the offset in bytes
                           ;; (emit-bytes #x48 #xc1 #xfa +fixnum-shift+)         ; sar $0x2,%rdx
                           ;; (emit-bytes #x48 #xc1 #xe2 #x02)                   ; shl $0x2,%rdx
                           ;; nothing to do!
;;                            (inst :add :rdx :rax)
                           (inst :add :rax :rdx)
;;                            (emit-bytes #x8b #x00) ; mov (%rax),%eax
                           (inst :mov `(,(- +simple-vector-data-offset+ +typed-object-lowtag+) ,$dx) :eax)
                           (box-fixnum $ax)
                           (move-result-to-target target))
                          (t
                           (mumble "p2-vector-ref %vector-ref case 2 type1 = ~S type2 = ~S~%" type1 type2)
                           (process-2-args args :default t)
                           (emit-call-2 '%vector-ref target))))
                   (t
                    (mumble "p2-vector-ref unexpected situation type1 = ~S~%" type1)
                    (process-2-args args :default t)
                    (emit-call-2 (if (zerop *safety*) '%vector-ref 'vector-ref) target)
;;                     (aver nil)
                    )))
            ((subtypep type1 'vector)
             (mumble "p2-vector-ref %vector-ref case 3 type1 = ~S type2 = ~S~%" type1 type2)
             (process-2-args args :default t)
             (emit-call-2 '%vector-ref target))
            (t
             (process-2-args args :default t)
             (emit-call-2 (if (zerop *safety*) '%vector-ref 'vector-ref) target))))
    t))
