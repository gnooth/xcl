;;; p2-arith.lisp
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

(defknown p2-two-arg-+ (t t) t)

;; #+x86-64
(defun p2-two-arg-+ (form target)
  (mumble "p2-two-arg-+ new version~%")
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (type1 (derive-type arg1))
           (type2 (derive-type arg2))
           (result-type (derive-type form)))
      (unless (fixnump arg1)
        (when (and (integer-constant-value type1)
                   (flushable arg1))
          (setq arg1 (integer-constant-value type1))))
      (unless (fixnump arg2)
        (when (and (integer-constant-value type2)
                   (flushable arg2))
          (setq arg2 (integer-constant-value type2))))
      (when (and (numberp arg1)
                 (numberp arg2))
        (p2-constant (two-arg-+ arg1 arg2) target)
        (return-from p2-two-arg-+ t))
      (when (fixnump arg1)
        (mumble "p2-two-arg-+ calling psetq~%")
        (psetq arg1  arg2
               arg2  arg1
               type1 type2
               type2 type1)
        (setq args (list arg1 arg2)))
      (cond ((eql arg2 0)
             (process-1-arg arg1 $ax t)
             (move-result-to-target target))
;;             ((fixnump arg2)
;;              )

            #+nil
            ((and (fixnump arg2)
                  (typep (fixnumize arg2) '(signed-byte 32)))
             (mumble "p2-two-arg-+ case 1~%")
             (let* ((FULL-CALL (make-label))
                    (FIX-OVERFLOW (make-label))
                    (EXIT (make-label))
                    (var (and (var-ref-p arg1) (var-ref-var arg1)))
                    (reg (and var (find-register-containing-var var)))
;;                     reg2
                    )
               (unless reg
                 (setq reg $ax)
                 (process-1-arg arg1 reg t))
;;                (setq reg2 (if (eql reg $ax) $dx $ax))
;;                (inst :mov reg reg2)
;;                (clear-register-contents reg2)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ (reg8 reg))
                 (emit-jmp-short :nz FULL-CALL))
               ;; falling through, both args are fixnums
               (cond ((typep (fixnumize arg2) '(signed-byte 8))
                      (inst :add (fixnumize arg2) reg)
                      (clear-register-contents reg))
                     (t
                      (inst :add (fixnumize arg2) reg)
                      (clear-register-contents reg)))
;;                (emit-jmp-short :o FULL-CALL)
               (emit-jmp-short :o FIX-OVERFLOW)
               (label EXIT)
               (move-result-to-target target)
               (unless (and (fixnum-type-p type1)
                            (fixnum-type-p result-type))
                 (clear-register-contents)
                 (let ((*current-segment* :elsewhere)
                       ;(*register-contents* (copy-register-contents))
                       )
                   (label FULL-CALL)
                   (clear-register-contents)
                   (inst :mov reg :rdi)
                   (inst :mov (fixnumize arg2) :rsi)
                   (emit-call 'two-arg-+)
                   #+x86 (inst :add (* +bytes-per-word+ 2) :esp)
                   (emit-jmp-short t EXIT))
                 (let ((*current-segment* :elsewhere)
                       ;;                      (*register-contents* (copy-register-contents))
                       )
                   (label FIX-OVERFLOW)
                   (unbox-fixnum $ax)
                   (inst :mov :rax :rdi)
                   (emit-call "RT_fix_overflow")
                   (emit-jmp-short t EXIT))
                 )))

            #+nil
            ((and (fixnum-type-p type1) (fixnum-type-p type2) (fixnum-type-p type3))
             )

            (t
             (let ((FULL-CALL (make-label))
                   (FIX-OVERFLOW (make-label))
                   (EXIT (make-label)))
               (mumble "p2-two-arg-+ case 2~%")
               (process-2-args args `(,$ax ,$dx) t)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz FULL-CALL))
               (unless (fixnum-type-p type2)
                 (inst :test +fixnum-tag-mask+ :dl)
                 (emit-jmp-short :nz FULL-CALL))
               ;; falling through, both args are fixnums
               (inst :add $dx $ax)
               (clear-register-contents $ax)
               (unless (fixnum-type-p result-type)
                 (emit-jmp-short :o FIX-OVERFLOW))
               (label EXIT)
               (move-result-to-target target)
               (unless (and (fixnum-type-p type1) (fixnum-type-p type2))
                 (clear-register-contents)
                 (let ((*current-segment* :elsewhere))
                   (label FULL-CALL)
                   #+x86    (progn
                              (inst :push :edx)
                              (inst :push :eax))
                   #+x86-64 (progn
                              (inst :mov :rdx :rsi)
                              (inst :mov :rax :rdi))
                   (emit-call 'two-arg-+)
                   #+x86    (inst :add (* +bytes-per-word+ 2) :esp)
                   (emit-jmp-short t EXIT)))
               (unless (fixnum-type-p result-type)
                 (clear-register-contents)
                 (let ((*current-segment* :elsewhere))
                   (label FIX-OVERFLOW)
                   (unbox-fixnum $ax)
                   #+x86    (inst :push :eax)
                   #+x86-64 (inst :mov :rax :rdi)
                   (emit-call "RT_fix_overflow")
                   #+x86    (inst :add +bytes-per-word+ :esp)
                   (emit-jmp-short t EXIT)))
               ))))
    t))

;; #+x86-64
#+nil
(defun p2-two-arg-+ (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           type1
           type2
           result-type)
      (when (and (numberp arg1)
                 (numberp arg2))
        (p2-constant (two-arg-+ arg1 arg2) target)
        (return-from p2-two-arg-+ t))
      (when (fixnump arg1)
        (let ((temp arg1))
          (setq arg1 arg2
                arg2 temp))
        (setq args (list arg1 arg2)))
      (setq type1 (derive-type arg1)
            type2 (derive-type arg2)
            result-type (derive-type form))
      (unless (fixnump arg2)
        (when (and (integer-constant-value type2)
                   (flushable arg2))
          (setq arg2 (integer-constant-value type2))))
      (cond ((eql arg2 0)
             (process-1-arg arg1 :rax t)
             (move-result-to-target target))
            ((and (fixnum-type-p type1)
                  (fixnum-type-p type2)
                  (fixnum-type-p result-type))
             (cond ((and (fixnump arg2)
                         (typep (fixnumize arg2) '(signed-byte 32)))
                    (cond ((reg64-p target)
                           (process-1-arg arg1 target t)
                           (inst :add (fixnumize arg2) target)
                           (clear-register-contents target))
                          (t
                           (process-1-arg arg1 :rax t)
                           (inst :add (fixnumize arg2) :rax)
                           (clear-register-contents :rax)
                           (move-result-to-target target))))
                   (t
                    (process-2-args args '(:rax :rdx) t)
                    (inst :add :rdx :rax)
                    (clear-register-contents :rax)
                    (move-result-to-target target))))
            ((and (fixnump arg2)
                  (typep (fixnumize arg2) '(signed-byte 32)))
             (let ((FULL-CALL (make-label))
                   (EXIT (make-label)))
               (process-1-arg arg1 :rax t)
               (unless (constant-or-local-var-ref-p arg1)
                 (inst :mov :rax :rdi))
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz FULL-CALL))
               ;; falling through, both args are fixnums
               (cond ((typep (fixnumize arg2) '(signed-byte 8))
                      (inst :add (fixnumize arg2) :rax)
                      (clear-register-contents :rax))
                     (t
                      (inst :mov (fixnumize arg2) :rdx)
                      (inst :add :rdx :rax)
                      (clear-register-contents :rax :rdx)))
               (emit-jmp-short :no EXIT)
               (label FULL-CALL)
               (clear-register-contents)
               (when (constant-or-local-var-ref-p arg1)
                 (process-1-arg arg1 :rdi nil))
               (inst :mov (fixnumize arg2) :rsi)
               (emit-call 'two-arg-+)
               (label EXIT)
               (move-result-to-target target)))
            ((or (float-type-p type1)
                 (float-type-p type2))
             (cond ((and (subtypep type1 'DOUBLE-FLOAT)
                         (subtypep type2 'DOUBLE-FLOAT))
                    (mumble "p2-two-arg-+ double-float case~%")
                    (process-2-args args '(:rdi :rsi) t)
                    (emit-call '%double-float-+)
                    (move-result-to-target target))
                   (t
                    ;; full call
                    (mumble "p2-two-arg-+ float case~%")
                    (process-2-args args '(:rdi :rsi) t)
                    (emit-call 'two-arg-+)
                    (move-result-to-target target))))
            (t
             (let ((FULL-CALL (make-label))
                   (EXIT (make-label)))
               (process-2-args args '(:rax :rdx) t)
               (inst :mov :rax :rdi)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz FULL-CALL))
               (unless (fixnum-type-p type2)
                 (inst :test +fixnum-tag-mask+ :dl)
                 (emit-jmp-short :nz FULL-CALL))
               ;; falling through, both args are fixnums
               (inst :add :rdx :rax)
               (emit-jmp-short :no EXIT)
               (label FULL-CALL)
               (inst :mov :rdx :rsi)
               (emit-call 'two-arg-+)
               (label EXIT)
               (move-result-to-target target)))))
    t))

;; #+x86
#+nil
(defun p2-two-arg-+ (form target)
  (let ((args (cdr form)))
    (when (eql (length args) 2)
      (let* ((arg1 (car args))
             (arg2 (cadr args))
             (type1 (derive-type arg1))
             (type2 (derive-type arg2))
             (result-type (derive-type form))
             (OVERFLOW (make-label))
             (FULL-CALL (make-label))
             (EXIT (make-label)))
        ;;       (mumble "type1 = ~S type2 = ~S result-type = ~S~%"
        ;;               type1 type2 result-type)
        (cond ((and (integer-constant-value type1)
                    (integer-constant-value type2)
                    (flushable arg1)
                    (flushable arg2))
               (p2-constant (two-arg-+ (integer-constant-value type1) (integer-constant-value type2))
                            target))
              ((and (fixnum-type-p type1)
                    (fixnump arg2)
                    (fixnum-type-p result-type))
               ;; a very common special case: (INCF X) where X is of type INDEX
               (p2 arg1 :eax)
               (unless (single-valued-p arg1)
                 (emit-clear-values :preserve :eax))
               (emit-byte #x05)                     ; add immediate dword from eax
               (emit-dword arg2)                    ; arg2 is a fixnum literal
               (clear-register-contents :eax)
               (move-result-to-target target))
              ((fixnump arg2)
               ;; as in (+ n 2), for example
               (p2 arg1 :eax)
               (unless (single-valued-p arg1)
                 (emit-clear-values :preserve :eax))
               (inst :mov :eax :edx)
               (clear-register-contents :edx)
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz FULL-CALL))
               ;; falling through, arg1 is a fixnum
               (let ((n (ash arg2 +fixnum-shift+)))
                 (cond ((typep n '(signed-byte 8))
                        (inst :add n :eax))
                       (t
                        (emit-byte #x05) ; add immediate dword from eax
                        (emit-dword arg2) ; arg2 is a fixnum literal
                        )))
               (clear-register-contents :eax)
               (emit-jmp-short :o FULL-CALL)
               (label EXIT)
               (move-result-to-target target)
               (let ((*current-segment* :elsewhere))
                 (label FULL-CALL)
                 ;; push arg2 first
                 (inst :push (ash arg2 +fixnum-shift+))
                 ;; arg1 is already in edx
                 (inst :push :edx)
                 (emit-call 'two-arg-+)
                 (emit-adjust-stack-after-call 2)
                 (emit-jmp-short t EXIT)))
              ((or (float-type-p type1)
                   (float-type-p type2))
               (cond ((and (subtypep type1 'DOUBLE-FLOAT)
                           (subtypep type2 'DOUBLE-FLOAT))
                      (process-2-args args :stack t)
                      (emit-call-2 '%double-float-+ target))
                     (t
                      ;; full call
                      (process-2-args args :stack t)
                      (emit-call-2 'two-arg-+ target))))
              (t
               (process-2-args args '(:eax :edx) t)
               ;; arg1 in eax, arg2 in edx
               (unless (fixnum-type-p type1)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz FULL-CALL))
               (unless (fixnum-type-p type2)
                 (inst :test +fixnum-tag-mask+ :dl)
                 (emit-jmp-short :nz FULL-CALL))
               ;; falling through, both args are fixnums
               (unless (fixnum-type-p result-type)
                 (inst :mov :eax :ecx) ; we're about to trash eax
                 (clear-register-contents :ecx))
               (inst :add :edx :eax)
               (clear-register-contents :eax)
               (case target
                 (:return
                  (emit-jmp-short :o OVERFLOW)
                  ;; falling through: no overflow, we're done
                  (inst :exit)
                  (label OVERFLOW))
                 (t
                  ;; if no overflow, we're done
                  (emit-jmp-short :no EXIT)))
               (inst :mov :ecx :eax)
               (label FULL-CALL)
               (inst :push :edx)
               (inst :push :eax)
               (emit-call-2 'two-arg-+ :eax)
               (label EXIT)
               (move-result-to-target target))))
      t)))
