;;; p2-check-fixnum-bounds.lisp
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

#+x86
(defun p2-check-fixnum-bounds (form target)
  (when (check-arg-count form 3)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (arg3 (%caddr args))
           (derived-type (derive-type arg1)))
      (cond ((zerop *safety*)
             (p2 arg1 target))
            ((and (fixnump arg2)
                  (fixnump arg3)
                  (neq derived-type :unknown)
                  (subtypep derived-type (list 'INTEGER arg2 arg3)))
             (p2 arg1 target))
            ((and (fixnump arg2)
                  (fixnump arg3)
                  (typep (fixnumize arg2) '(signed-byte 32))
                  (typep (fixnumize arg3) '(signed-byte 32)))
             (let ((FAIL (make-label)))
               (process-1-arg arg1 :eax t)
               (inst :test +fixnum-tag-mask+ :al)
               (emit-jmp-short :nz FAIL)
               (inst :cmp (fixnumize arg2) :eax)
               (emit-jmp-short :l FAIL)
               (inst :cmp (fixnumize arg3) :eax)
               (emit-jmp-short :g FAIL)
               (when target
                 (move-result-to-target target))
               (let ((*current-segment* :elsewhere)
                     (*register-contents* nil))
                 (label FAIL)
                 (inst :push :eax)
                 (inst :push (fixnumize arg3))
                 (inst :push (fixnumize arg2))
                 (p2-symbol 'INTEGER :stack)
                 (emit-call-3 'LIST3 :eax)
                 (inst :pop :edx)
                 (inst :push :eax)
                 (inst :push :edx)
                 (emit-call-2 '%type-error nil)
                 (inst :exit) ; FIXME
                 )))
            (t
             (mumble "p2-check-fixnum-bounds full call~%")
             (process-3-args args :default t)
             (emit-call-3 'check-fixnum-bounds target))))
    t))

#+x86-64
(defun p2-check-fixnum-bounds (form target)
  (when (check-arg-count form 3)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (arg3 (%caddr args))
           (type1 (derive-type arg1)))
      (cond ((zerop *safety*)
             (p2 arg1 target))
            ((and (fixnump arg2)
                  (fixnump arg3)
                  (neq type1 :unknown)
                  (subtypep type1 (list 'INTEGER arg2 arg3)))
             (p2 arg1 target))
            ((and (zerop arg2)
                  (not (fixnump arg3))
                  (fixnum-type-p (derive-type arg3)))
             (mumble "p2-check-fixnum-bounds new code~%")
             (let ((ERROR (make-label)))
               (let ((*current-segment* :elsewhere)
                     (*register-contents* nil))
                 (label ERROR)
;;                  (inst :push :rax)
;;                  ;;                  (inst :mov (fixnumize arg3) :rdx)
;;                  (inst :xor :esi :esi)
;;                  (p2-symbol 'INTEGER :rdi)
;;                  (emit-call-3 'LIST3 :rsi)
;;                  (inst :pop :rdi)
;;                  (emit-call '%type-error)
                 (inst :mov :rax :rdi)
                 (inst :xor :esi :esi)
                 ;; arg3 is already in rdx
                 (emit-call 'error-integer-bounds)
                 (emit-exit)) ; FIXME
               ;;                (process-1-arg arg1 :rax t)
               (process-2-args (list arg1 arg3) '(:rax :rdx) t)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz ERROR))
               ;;                (inst :mov (fixnumize arg3) :rdx)
               ;;                (clear-register-contents :rdx)
               (inst :cmp :rdx :rax)
               (emit-jmp-short :a ERROR))
             (when target
               (move-result-to-target target)))
            ((and (eql arg2 0)
                  (eql arg3 1))
             (mumble "p2-check-fixnum-bounds bit case~%")
             (let ((ERROR (make-label)))
               (let ((*current-segment* :elsewhere))
                 (label ERROR)
                 (inst :mov :rax :rdi)
                 (emit-call 'error-not-bit)
                 (emit-exit))
               (process-1-arg arg1 :rax t)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz ERROR))
               (inst :cmp (fixnumize 1) :rax)
               (emit-jmp-short :a ERROR)
               (when target
                 (move-result-to-target target))))
            ((and (fixnump arg2)
                  (fixnump arg3))
             (let ((ERROR (make-label)))
               (let ((*current-segment* :elsewhere)
                     (*register-contents* nil))
                 (label ERROR)
                 (p2 `'(INTEGER ,arg2 ,arg3) :rsi)
                 (inst :mov :rax :rdi)
                 (emit-call '%type-error)
                 (emit-exit)) ; FIXME
               (process-1-arg arg1 :rax t)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz ERROR))
               (cond ((zerop arg2)
                      (cond ((typep (fixnumize arg3) '(signed-byte 32))
                             (inst :cmp (fixnumize arg3) :rax))
                            (t
                             (aver (typep (fixnumize arg3) '(signed-byte 64)))
                             (inst :mov (fixnumize arg3) :rdx)
                             (clear-register-contents :rdx)
                             (inst :cmp :rdx :rax)))
                      (emit-jmp-short :a ERROR))
                     (t
                      (cond ((typep (fixnumize arg2) '(signed-byte 32))
                             (inst :cmp (fixnumize arg2) :rax))
                            (t
                             (aver (typep (fixnumize arg2) '(signed-byte 64)))
                             (inst :mov (fixnumize arg2) :rdx)
                             (clear-register-contents :rdx)
                             (inst :cmp :rdx :rax)))
                      (emit-jmp-short :l ERROR)
                      (cond ((typep (fixnumize arg3) '(signed-byte 32))
                             (inst :cmp (fixnumize arg3) :rax))
                            (t
                             (aver (typep (fixnumize arg3) '(signed-byte 64)))
                             (inst :mov (fixnumize arg3) :rdx)
                             (clear-register-contents :rdx)
                             (inst :cmp :rdx :rax)))
                      (emit-jmp-short :g ERROR)))
               (when target
                 (move-result-to-target target))))
            (t
             (mumble "p2-check-fixnum-bounds full call~%")
             (process-3-args args :default t)
             (emit-call-3 'check-fixnum-bounds target))))
    t))

