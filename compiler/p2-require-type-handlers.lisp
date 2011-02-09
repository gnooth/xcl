;;; p2-require-type-handlers.lisp
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

(defun common-label (compiland error-function register)
  (let* ((common-labels (compiland-common-labels compiland))
         (key (concatenate 'string (symbol-name error-function) "-" (symbol-name register)))
         (label (gethash key common-labels)))
    (unless label
      (setq label (make-label))
      (let ((*current-segment* :elsewhere))
        (label label)
        #+x86
        (inst :push register)
        #+x86-64
        (unless (eq register :rdi)
          (inst :mov register :rdi))
        (inst :call error-function) ; don't clear register contents!
        (inst :exit)
        (setf (gethash key common-labels) label)))
    label))

(defun p2-require-cons (form target)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((zerop *safety*)
             (p2 arg target))
            ((cons-type-p (derive-type arg))
             (p2 arg target))
            (t
             (let ((ERROR (common-label *current-compiland* 'error-not-cons $ax)))
               (process-1-arg arg $ax t)
               (inst :compare-immediate nil $ax)
               (emit-jmp-short :e ERROR)
               (inst :mov :al :dl)
               (clear-register-contents $dx)
               (inst :and +lowtag-mask+ :dl)
               (inst :cmp +list-lowtag+ :dl)
               (emit-jmp-short :ne ERROR)
               (move-result-to-target target)
               (when (var-ref-p arg)
                 (let ((var (var-ref-var arg)))
                   (set-register-contents $ax var)
                   (add-type-constraint var 'CONS)))))))
    t))

(defun p2-require-fixnum (form target)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((zerop *safety*)
             (p2 arg target))
            ((fixnum-type-p (derive-type arg))
             (p2 arg target))
            (t
             (let ((ERROR (common-label *current-compiland* 'error-not-fixnum $ax)))
               (process-1-arg arg $ax t)
               (inst :test +fixnum-tag-mask+ :al)
               (emit-jmp-short :nz ERROR)
               (move-result-to-target target)
               (when (var-ref-p arg)
                 (add-type-constraint (var-ref-var arg) 'FIXNUM))))))
    t))

(defun p2-require-widetag-bit (register widetag-bit error-function)
  (aver (eq register $ax))
  (let ((ERROR (common-label *current-compiland* error-function register)))
    (inst :mov :al :dl)
    (clear-register-contents $dx)
    (inst :and +lowtag-mask+ :dl)
    (inst :cmp +typed-object-lowtag+ :dl)
    (emit-jmp-short :ne ERROR)
    (aver (typep widetag-bit '(unsigned-byte 32)))
    (inst #+x86 :testl #+x86-64 :testq
          widetag-bit
          `(,(- +widetag-offset+ +typed-object-lowtag+) ,$ax))
    (emit-jmp-short :z ERROR)))

(defun p2-require-widetag (register widetag error-function)
  (aver (eq register $ax))
  (let ((ERROR (common-label *current-compiland* error-function register)))
    (inst :mov :al :dl)
    (clear-register-contents $dx)
    (inst :and +lowtag-mask+ :dl)
    (inst :cmp +typed-object-lowtag+ :dl)
    (emit-jmp-short :ne ERROR)
    (aver (typep widetag '(unsigned-byte 32)))
    (inst #+x86 :cmpl #+x86-64 :cmpq
          widetag
          `(,(- +widetag-offset+ +typed-object-lowtag+) ,$ax))
    (emit-jmp-short :nz ERROR)))

(defmacro define-require-type-handler (type)
  (let* ((type-string (string type))
         (name (intern (concatenate 'string "P2-REQUIRE-" type-string)))
         (widetag (intern (concatenate 'string "+" type-string "-WIDETAG+") +system-package+))
         (widetag-bit (intern (concatenate 'string "+WIDETAG-" type-string "-BIT+") +system-package+))
         (error-function (intern (concatenate 'string "ERROR-NOT-" type-string) +system-package+))
         require-widetag-function
         widetag-arg)
    (aver (not (and (boundp widetag) (boundp widetag-bit))))
    (cond ((boundp widetag)
           (setq widetag-arg widetag
                 require-widetag-function 'p2-require-widetag))
          ((boundp widetag-bit)
           (setq widetag-arg widetag-bit
                 require-widetag-function 'p2-require-widetag-bit))
          (t
           (error "Neither %S nor %S is bound." widetag widetag-bit)))
    `(defun ,name (form target)
       (when (length-eql form 2)
         (let* ((arg (%cadr form)))
           (cond ((zerop *safety*)
                  (p2 arg target))
                 ((subtypep (derive-type arg) ',type)
                  (p2 arg target))
                 (t
                  (mumble "~A~%" ',name)
                  (process-1-arg arg $ax t)
                  (,require-widetag-function $ax ,widetag-arg ',error-function)
                  (when target
                    (move-result-to-target target))
                  (when (var-ref-p arg)
                    (add-type-constraint (var-ref-var arg) ',type)))))
         t))))

(define-require-type-handler function)
(define-require-type-handler vector)
(define-require-type-handler simple-bit-vector)
(define-require-type-handler simple-string)
(define-require-type-handler simple-vector)

(defun %p2-require-type (form target required-type)
  (declare (type symbol required-type))
  (when (check-arg-count form 1)
    (let ((op (%car form))
          (arg (%cadr form))
          type)
      (cond ((zerop *safety*)
             (process-1-arg arg target t))
            ((and (neq (setq type (derive-type arg)) :unknown)
                  (subtypep type required-type))
             (process-1-arg arg target t))
            (t
             (if (var-ref-p arg)
                 (mumble "%p2-require-type full call to ~S var = ~S~%" op (var-name (var-ref-var arg)))
                 (mumble "%p2-require-type full call to ~S~%" op))
             (process-1-arg arg :default t)
             (emit-call-1 op target)
             (when (var-ref-p arg)
               (set-register-contents $ax (var-ref-var arg))))))
    t))

(defun p2-require-hash-table (form target)
  (%p2-require-type form target 'hash-table))

(defun p2-require-integer (form target)
  (%p2-require-type form target 'integer))

(defun p2-require-keyword (form target)
  (%p2-require-type form target 'keyword))

(defun p2-require-number (form target)
  (%p2-require-type form target 'number))

(defun p2-require-stream (form target)
  (%p2-require-type form target 'stream))

(defun p2-require-string (form target)
  (%p2-require-type form target 'string))

(defun p2-require-structure-type (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           derived-type)
      (cond ((zerop *safety*)
             (p2 arg1 target))
            ((and (quoted-form-p arg2)
                  (neq (setq derived-type (derive-type arg1)) :unknown)
                  (subtypep derived-type (%cadr arg2)))
             (p2 arg1 target))
            (t
             (process-2-args args :default t)
             (emit-call-2 'require-structure-type target)
             (when (var-ref-p arg1)
               (let ((var (var-ref-var arg1)))
                 (set-register-contents $ax var)
                 (when (quoted-form-p arg2)
                   (add-type-constraint var (%cadr arg2))))))))
    t))

(defun p2-require-ub32 (form target)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form))
          type)
      (cond ((zerop *safety*)
             (p2 arg target))
            ((and (neq (setq type (derive-type arg)) :unknown)
                  (subtypep type '(INTEGER 0 4294967295)))
             (p2 arg target))
            (t
             (process-1-arg arg :default t)
             (mumble "p2-require-ub32 emitting full call to require-ub32~%")
             (emit-call-1 'require-ub32 target)
             (when (var-ref-p arg)
               (let ((var (var-ref-var arg)))
                 (set-register-contents $ax var)
                 (add-type-constraint var '(INTEGER 0 4294967295)))))))
    t))

(defun p2-require-unsigned-byte (form target)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form))
          type)
      (cond ((zerop *safety*)
             (p2 arg target))
            ((and (neq (setq type (derive-type arg)) :unknown)
                  (subtypep type 'UNSIGNED-BYTE))
             (p2 arg target))
            (t
             ;;              (process-1-arg arg :default t)
             ;;              (emit-call-1 'require-unsigned-byte target)
             (mumble "p2-require-unsigned-byte new code~%")
             (process-1-arg arg $ax t)
             (let ((FULL-CALL (make-label))
                   (EXIT (make-label)))
               (unless (fixnum-type-p type)
                 (inst :test +fixnum-tag-mask+ :al)
                 (emit-jmp-short :nz FULL-CALL))
               (inst :test $ax $ax)
               (emit-jmp-short :l FULL-CALL)
               (label EXIT)
               (move-result-to-target target)
               (let ((*current-segment* :elsewhere))
                 (label FULL-CALL)
                 #+x86
                 (inst :push :eax)
                 #+x86-64
                 (inst :mov :rax :rdi)
                 (emit-call 'require-unsigned-byte)
                 #+x86
                 (inst :add +bytes-per-word+ :esp)
                 (emit-jmp-short t EXIT)))
             (when (var-ref-p arg)
               (let ((var (var-ref-var arg)))
                 (set-register-contents $ax var)
                 (add-type-constraint var '(INTEGER 0 *)))))))
    t))
