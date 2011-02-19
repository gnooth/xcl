;;; p2-cxr.lisp
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

(defknown p2-%car (t t) t)
(defun p2-%car (form target)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (reg (and (var-ref-p arg)
                     (find-register-containing-var (var-ref-var arg)))))
      (cond ((register-p target)
             (cond (reg
                    (mumble "p2-%car case 1a~%")
                    (inst :mov `(-1 ,reg) target))
                   (t
                    (mumble "p2-%car case 1b~%")
                    (process-1-arg arg target t)
                    (inst :mov `(-1 ,target) target)))
             (clear-register-contents target))
            ((eql target :stack)
             (cond (reg
                    (mumble "p2-%car case 2a~%")
                    (inst :push `(-1 ,reg)))
                   (t
                    (mumble "p2-%car case 2b~%")
                    (process-1-arg arg $ax t)
                    (inst :push `(-1 ,$ax)))))
            ((null target)
             (mumble "p2-%car null target case~%")
             )
            (t
             (mumble "p2-%car default case target = ~S~%" target)
             (cond (reg
                    (mumble "p2-%car case 3a~%")
                    (inst :mov `(-1 ,reg) $ax))
                   (t
                    (mumble "p2-%car case 3b~%")
                    (process-1-arg arg $ax t)
                    (inst :mov `(-1 ,$ax) $ax)))
             (clear-register-contents $ax)
             (move-result-to-target target))))
    t))

;; #+x86
#+nil
(defun p2-%car (form target)
  (when (check-arg-count form 1)
    (process-1-arg (%cadr form) :eax t)
    (cond ((reg32-p target)
           (inst :mov '(-1 :eax) target)
           (clear-register-contents target))
          (t
           (inst :mov '(-1 :eax) :eax)
           (clear-register-contents :eax)
           (move-result-to-target target)))
    t))
;; #+x86-64
#+nil
(defun p2-%car (form target)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (reg (and (var-ref-p arg)
                     (find-register-containing-var (var-ref-var arg)))))
      (cond ((reg64-p target)
             (cond (reg
                    (inst :mov `(-1 ,reg) target))
                   (t
                    (process-1-arg arg target t)
                    (inst :mov `(-1 ,target) target)))
             (clear-register-contents target))
            (reg
             (inst :mov `(-1 ,reg) :rax)
             (clear-register-contents :rax)
             (move-result-to-target target))
            (t
             (process-1-arg arg :rax t)
             (inst :mov '(-1 :rax) :rax)
             (clear-register-contents :rax)
             (move-result-to-target target)))
      t)))

(defknown p2-car (t t) t)
#+x86
(defun p2-car (form target)
  (when (zerop *safety*)
    (return-from p2-car (p2-%car form target)))
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (type (derive-type arg)))
      (cond ((eq type 'LIST)
             (p2-%car form target))
            ((cons-type-p type)
             (p2-%car form target))
            (t
             (process-1-arg arg :edx t)
             (let* (;(common-labels (compiland-common-labels *current-compiland*))
                    ;(ERROR (gethash :error-not-list common-labels))
                    (ERROR (common-label-error-not-list *current-compiland* :edx))
                    )
               ;;                (unless ERROR
               ;;                  (setq ERROR (make-label))
               ;;                  (let ((*current-segment* :elsewhere))
               ;;                    (label ERROR)
               ;;                    (p2-symbol 'LIST :stack)
               ;;                    (inst :push :eax)
               ;;                    (emit-call-2 '%type-error nil)
               ;;                    (inst :exit) ; FIXME
               ;;                    (setf (gethash :error-not-list common-labels) ERROR)))
               (inst :mov :edx :eax)
               (inst :and +lowtag-mask+ :al)
               (inst :cmp +list-lowtag+ :al)
               (emit-jmp-short :ne ERROR)
               (cond ((reg32-p target)
                      (inst :mov '(-1 :edx) target)
                      (clear-register-contents target))
                     (t
                      (inst :mov '(-1 :edx) :eax)
                      (clear-register-contents :eax)
                      (move-result-to-target target)))
               (when (var-ref-p arg)
                 (add-type-constraint (var-ref-var arg) 'LIST))))))
    t))
#+x86-64
(defun p2-car (form target)
  (when (zerop *safety*)
    (return-from p2-car (p2-%car form target)))
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (type (derive-type arg)))
      (cond ((eq type 'LIST)
             (p2-%car form target))
            ((cons-type-p type)
             (p2-%car form target))
            (t
             (process-1-arg arg :rdi t)
             (let ((ERROR-NOT-LIST (common-label-error-not-list *current-compiland* :rdi)))
               (inst :mov :edi :eax)
               (clear-register-contents :rax)
               (inst :and +lowtag-mask+ :al)
               (inst :cmp +list-lowtag+ :al)
               (emit-jmp-short :ne ERROR-NOT-LIST)
               (cond ((reg64-p target)
                      (inst :mov '(-1 :rdi) target)
                      (clear-register-contents target))
                     (t
                      (inst :mov '(-1 :rdi) :rax)
                      (move-result-to-target target)))
               (when (var-ref-p arg)
                 (add-type-constraint (var-ref-var arg) 'LIST))))))
    t))
