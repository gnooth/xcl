;;; p2-ash.lisp
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

(defun p2-ash (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args)))
      (when (null target)
        (p2 `(require-integer ,arg1) nil)
        (p2 `(require-integer ,arg2) nil)
        (maybe-emit-clear-values arg1 arg2)
        (return-from p2-ash t))
      (let* ((type1 (derive-type arg1))
             (type2 (derive-type arg2))
             (result-type (derive-type form))
             (shift (integer-constant-value type2)))
        (unless (fixnump arg1)
          (when (and (integer-constant-value type1)
                     (flushable arg1))
            (setq arg1 (integer-constant-value type1))))
        (unless (fixnump arg2)
          (when (and (integer-constant-value type2)
                     (flushable arg2))
            (setq arg2 (integer-constant-value type2))))
        (when (and (integerp arg1) (integerp arg2))
          (p2-constant (ash arg1 arg2) target)
          (return-from p2-ash t))
        (cond ((and shift (integer-constant-value type1))
               ;; both args evaluate to constants
               (let ((must-clear-values nil))
                 (unless (flushable arg1)
                   (p2 arg1 nil)
                   (unless (single-valued-p arg1)
                     (setq must-clear-values t)))
                 (unless (flushable arg2)
                   (p2 arg2 nil)
                   (unless (single-valued-p arg2)
                     (setq must-clear-values t)))
                 (when must-clear-values
                   (emit-clear-values)))
               (p2-constant (ash (integer-constant-value type1) shift) target))
              ((eql shift 0)
               ;; zero shift
               (cond ((flushable arg2)
                      (cond ((or (integer-type-p type1) (zerop *safety*))
                             (process-1-arg arg1 target t))
                            (t
                             (process-1-arg arg1 :default t)
                             (emit-call-1 'require-integer target)))) ; require-integer returns its argument
                     (t ; arg2 not flushable
                      (process-2-args args '(:rdi :rcx) t) ; FIXME don't emit an instruction to trash $cx
                      (emit-call-1 'require-integer target)))) ; require-integer returns its argument
              ((and (eql (integer-constant-value type1) 0)
                    (or (integer-type-p type2) (zerop *safety*)))
               ;; integer arg evaluates to 0, so shift arg doesn't matter (as long as it's an integer)
               (unless (flushable arg1)
                 (p2 arg1 nil))
               (unless (flushable arg2)
                 (p2 arg2 nil))
               (p2-constant 0 target))
              ((and (fixnum-type-p type1)
                    (fixnum-type-p type2)
                    (fixnum-type-p result-type))
               ;; args and result are all fixnum-type-p
               (cond ((and shift (< (abs shift) +bits-per-word+))
                      (aver (neq shift 0))
                      (cond ((flushable arg2)
                             (let ((reg (if (register-p target) target $ax)))
                               (process-1-arg arg1 reg t)
                               (cond ((< shift 0)
                                      (inst :sar (- shift) reg)
                                      (inst :and #xfc (reg8 reg))) ; clear tag bits
                                     (t
                                      (inst :shl shift reg)))
                               (clear-register-contents reg)
                               (unless (eq reg target)
                                 (move-result-to-target target))))
                            (t ; arg2 not flushable
                             (process-2-args args `(,$ax ,$cx) t)
                             (unbox-fixnum $cx)
                             (cond ((< shift 0)
                                    (inst :neg $cx)
                                    (inst :sar :cl $ax)
                                    (inst :and #xfc :al)) ; clear tag bits
                                   (t
                                    (inst :shl :cl $ax)))
                             (move-result-to-target target))))
                     ((subtypep type2 `(integer ,(- (1- +bits-per-word+)) 0))
                      (process-2-args args `(,$ax ,$cx) t)
                      (unbox-fixnum $cx)
                      (inst :neg $cx)
                      (inst :sar :cl $ax)
                      (inst :and #xfc :al) ; clear tag bits
                      (clear-register-contents $ax $cx)
                      (move-result-to-target target))
                     ((subtypep type2 `(integer 0 ,(1- +bits-per-word+)))
                      (process-2-args args `(,$ax ,$cx) t)
                      (unbox-fixnum $cx)
                      (inst :shl :cl $ax)
                      (clear-register-contents $ax $cx)
                      (move-result-to-target target))
                     (t
                      (process-2-args args :default t)
                      (emit-call-2 'ash target))))
              ((and shift (< (- +bits-per-word+) shift 0))
               ;; negative shift, arg1 and/or result not known to be fixnum-type-p
               (let ((FULL-CALL (make-label))
                     (EXIT (make-label)))
                 (cond ((flushable arg2)
                        (process-1-arg arg1 $ax t)
                        (unless (fixnum-type-p type1)
                          (inst :test +fixnum-tag-mask+ :al)
                          (emit-jmp-short :nz FULL-CALL))
                        (inst :sar (- shift) $ax)
                        (inst :and #xfc :al) ; clear tag bits
                        (clear-register-contents $ax)
                        (unless (fixnum-type-p type1)
                          (emit-jmp-short t EXIT)
                          (let ((*current-segment* :elsewhere))
                            (label FULL-CALL)
                            #+x86    (progn
                                       (inst :push (fixnumize shift))
                                       (inst :push :eax))
                            #+x86-64 (progn
                                       (inst :mov :rax :rdi)
                                       (inst :mov (fixnumize shift) :rsi))
                            (emit-call 'ash)
                            #+x86    (inst :add (* 2 +bytes-per-word+) :esp)
                            (emit-jmp-short t EXIT))
                          (label EXIT)))
                       (t
                        (process-2-args args `(,$ax ,$cx) t)
                        (unless (fixnum-type-p type1)
                          (inst :test +fixnum-tag-mask+ :al)
                          (emit-jmp-short :nz FULL-CALL))
                        (inst :sar (- shift) $ax)
                        (inst :and #xfc :al) ; clear tag bits
                        (clear-register-contents $ax)
                        (unless (fixnum-type-p type1)
                          (emit-jmp-short t EXIT)
                          (let ((*current-segment* :elsewhere))
                            (label FULL-CALL)
                            #+x86    (progn
                                       (inst :push :ecx)
                                       (inst :push :eax))
                            #+x86-64 (progn
                                       (inst :mov :rax :rdi)
                                       (inst :mov :rcx :rsi))
                            (emit-call 'ash)
                            #+x86    (inst :add (* 2 +bytes-per-word+) :esp)
                            (emit-jmp-short t EXIT))
                          (label EXIT))))
                 (move-result-to-target target)))
              ((and shift (< 0 shift +bits-per-word+)
                    (fixnum-type-p type1)
                    (subtypep result-type `(unsigned-byte ,+bits-per-word+)))
               (process-2-args args `(,$ax ,$cx) t)
               (unbox-fixnum $ax)
               (unbox-fixnum $cx)
               (inst :shl :cl $ax)
               #+x86    (inst :push :eax)
               #+x86-64 (inst :mov :rax :rdi)
               (emit-call "RT_make_unsigned_integer")
               #+x86    (inst :add +bytes-per-word+ :esp)
               (move-result-to-target target))
              ((and (subtypep type1 'unsigned-byte)
                    (subtypep type2 `(integer ,(- (1- +bits-per-word+)) ,(1- +bits-per-word+)))
                    (subtypep result-type `(unsigned-byte ,+bits-per-word+)))
               (let ((FULL-CALL (make-label))
                     (RIGHT-SHIFT (make-label))
                     (BIGNUM (make-label))
                     (EXIT (make-label)))
                 (let ((*current-segment* :elsewhere))
                   (label BIGNUM)
                   #+x86    (inst :push :eax)
                   #+x86-64 (inst :mov :rax :rdi)
                   (emit-call "RT_make_unsigned_bignum")
                   #+x86    (inst :add +bytes-per-word+ :esp)
                   (emit-jmp-short t EXIT)
                   (unless (fixnum-type-p type1)
                     (label FULL-CALL)
                     #+x86    (progn
                                (inst :push :ecx)
                                (inst :push :eax))
                     #+x86-64 (progn
                                (inst :mov :rcx :rsi)
                                (inst :mov :rax :rdi))
                     (emit-call 'ash)
                     #+x86    (inst :add (* 2 +bytes-per-word+) :esp)
                     (emit-jmp-short t EXIT)))
                 (process-2-args args `(,$ax ,$cx) t)
                 (clear-register-contents)
                 (unless (fixnum-type-p type1)
                   (inst :test +fixnum-tag-mask+ :al)
                   (emit-jmp-short :nz FULL-CALL))
                 (unbox-fixnum $cx)
                 (unbox-fixnum $ax)
                 (inst :test $cx $cx)
                 (emit-jmp-short :s RIGHT-SHIFT)
                 ;; left shift
                 (inst :shl :cl $ax)
                 (inst :mov (ldb (byte +bits-per-word+ 0) (lognot most-positive-fixnum)) $dx)
                 (inst :test $ax $dx)
                 (emit-jmp-short :nz BIGNUM)
                 (box-fixnum $ax)
                 (emit-jmp-short t EXIT)
                 (label RIGHT-SHIFT)
                 (inst :neg $cx)
                 (inst :sar :cl $ax)
                 ;; if number was a fixnum and shift is negative, result must be a fixnum
                 (box-fixnum $ax)
                 (label EXIT)
                 (move-result-to-target target)))
              (t
               (process-2-args args :default t)
               (emit-call-2 'ash target)))))
    t))
