;;; p3-x86-64.lisp
;;;
;;; Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

;; (defun generate-function-prolog ()
;;   (let* ((compiland *current-compiland*)
;;          (arity (compiland-arity compiland))
;;          prolog)
;;     (when (and arity
;;                (<= arity 6)
;;                (null *closure-vars*))
;;       (let ((code *code*)
;;             (r12-used-p nil)
;;             (leaf-p t))
;;         (declare (type simple-vector code))
;;         (dotimes (i (length code) nil)
;;           (let ((instruction (svref code i)))
;;             (when (listp instruction)
;;               (case (first instruction)
;;                 ((:mov :movb)
;;                  (unless r12-used-p
;;                    (let ((operand1 (second instruction))
;;                          (operand2 (third instruction)))
;;                      (when (or (eq operand1 :r12)
;;                                (eq operand2 :r12)
;;                                (and (listp operand1) (memq :r12 operand1))
;;                                (and (listp operand2) (memq :r12 operand2)))
;;                        (setq r12-used-p t)))))
;;                 ((:call :tail-call)
;;                  (setq leaf-p nil))))))
;;         (setf (compiland-needs-thread-var-p compiland) r12-used-p)
;;         (setf (compiland-leaf-p compiland) leaf-p)
;;         (when leaf-p
;;           (mumble "~S leaf-p = ~S r12-used-p = ~S numargs = ~S numvars = ~S~%"
;;                   (compiland-name compiland)
;;                   leaf-p
;;                   r12-used-p
;;                   (length (compiland-arg-vars compiland))
;;                   (length (compiland-local-vars compiland)))))
;;       (let ((*code* nil)
;;             (*main* nil)
;;             (*elsewhere* nil))
;;         (if (and (compiland-leaf-p compiland)
;;                  (not (compiland-needs-thread-var-p compiland))
;;                  (<= (length (compiland-arg-vars compiland)) 2)
;;                  (zerop (length (compiland-local-vars compiland))))
;;             (p2-trivial-leaf-function-prolog compiland)
;;             (p2-trivial-function-prolog compiland))
;;         (if *elsewhere*
;;             (setq prolog (concatenate 'simple-vector *main* *elsewhere*))
;;             (setq prolog (concatenate 'simple-vector *main*))))
;;       (setq *code* (concatenate 'simple-vector prolog *code*)))))

;; converts IR2 into bytes
(defun p3 ()
;;   (generate-function-prolog)
  (let* ((compiland *current-compiland*))
    (let* ((code *code*)
           (len (length code))
           ;; make initial size big enough to avoid having to resize the output vector
           (initial-size (max (logand (+ len (ash len -1) 16) (lognot 15)) 64))
           (new-code (make-array initial-size :fill-pointer 0)))
      (declare (type simple-vector code))
      (dotimes (i (length code))
        (let ((instruction (svref code i)))
          (unless (consp instruction)
            (format t "p3 non-cons instruction = ~S~%" instruction))
          (if (consp instruction)
              (let ((mnemonic (first instruction))
                    (operand1 (second instruction))
                    (operand2 (third  instruction)))
                (case mnemonic
                  (:mov
                   (cond ((var-p operand1)
                          ;; var ref
                          (cond ((var-index operand1)
                                 (setf (second instruction)
                                       (list (index-displacement (var-index operand1)) :rbp)))
                                ((var-register operand1)
                                 (setf (second instruction) (var-register operand1)))
                                (t
                                 (mumble "p3 no var-index for var ~S~%" (var-name operand1))
                                 (unsupported))))
                         ((var-p operand2)
                          ;; setq
                          (cond ((var-index operand2)
                                 (setf (third instruction)
                                       (list (index-displacement (var-index operand2)) :rbp)))
                                ((var-register operand2)
                                 (setf (third instruction) (var-register operand2)))
                                (t
                                 (mumble "p3 no var-index for var ~S~%" (var-name operand2))
                                 (unsupported))))
                         (t
                          ;; nothing to do
                          ))
                   (vector-push-extend (assemble-instruction instruction) new-code))
                  (:push
                   (cond ((var-p operand1)
                          (cond ((var-index operand1)
                                 (setf (second instruction)
                                       (list (index-displacement (var-index operand1)) :rbp)))
                                ((var-register operand1)
                                 (setf (second instruction) (var-register operand1)))
                                (t
                                 (mumble "p3 :push unsupported case~%")
                                 (unsupported))))
                         (t
                          ;; nothing to do
                          ))
                   (vector-push-extend (assemble-instruction instruction) new-code))
                  (:exit
                   (let ((instructions nil))
                     (unless (compiland-omit-frame-pointer compiland)
                       (push '(:leave) instructions))
                     (dolist (reg (reverse (compiland-registers-to-be-saved compiland)))
                       (push `(:pop ,reg) instructions))
                     (when (compiland-needs-thread-var-p compiland)
                       (push '(:pop :r12) instructions))
                     (push '(:ret) instructions)
                     (setq instructions (nreverse instructions))
                     (let ((bytes (assemble instructions)))
                       (setq instruction
                             (make-instruction :exit (length bytes) (coerce bytes 'list)))))
                   (vector-push-extend instruction new-code))
                  (:tail-call
                   (let ((target operand1)
                         (instructions nil))
                     (unless (compiland-omit-frame-pointer compiland)
                       (push '(:leave) instructions))
                     (dolist (reg (reverse (compiland-registers-to-be-saved compiland)))
                       (push `(:pop ,reg) instructions))
                     (when (compiland-needs-thread-var-p compiland)
                       (push '(:pop :r12) instructions))
                     (when instructions
                       (setq instructions (nreverse instructions))
                       (let ((bytes (assemble instructions)))
                         (setq instruction
                               (make-instruction :exit (length bytes) (coerce bytes 'list))))
                       (vector-push-extend instruction new-code))
                     (vector-push-extend (make-instruction :jmp 5 (list t target)) new-code)))
                  (:call
                   (setq instruction (make-instruction :call 5 operand1))
                   (vector-push-extend instruction new-code))
                  (:move-immediate
                   (cond ((and (consp operand1)
                               (eq (%car operand1) :function))
                          (aver (length-eql operand1 2))
                          (let ((symbol (%cadr operand1))
                                (register operand2))
                            ;; mov imm32, reg
                            (vector-push-extend
                             (make-instruction :byte 1 (+ #xb8 (register-number register)))
                             new-code)
                            (vector-push-extend
                             (make-instruction :function 4 symbol)
                             new-code)))
                         ((and (consp operand1)
                               (eq (%car operand1) :constant-32))
                          (aver (length-eql operand1 2))
                          (let ((form (%cadr operand1))
                                (register operand2))
                            (cond ((memq register '(:eax :ebx :ecx :edx :esi :edi))
                                   (vector-push-extend
                                    (make-instruction :byte 1 (+ #xb8 (register-number register)))
                                    new-code))
                                  ((memq register '(:r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))
                                   (vector-push-extend
                                    (make-instruction :bytes 3
                                                      (list #x49 #xc7 (+ #xc0 (register-number register))))
                                    new-code))
                                  (t
                                   (mumble "p3 :move-immediate :constant-32 unsupported case register = ~S~%"
                                              register)
                                   (unsupported)))
                            (vector-push-extend
                             (make-instruction :constant-32 4 form)
                             new-code)))
                         (t
                          (mumble "p3 :move-immediate unsupported case~%")
                          (unsupported))))
                  (:cmp-immediate
                   ;;                    (mumble "p3 :cmp-immediate case~%")
                   (cond ((and (consp operand1)
                               (eq (%car operand1) :constant-32))
                          (aver (length-eql operand1 2))
                          (let ((form (%cadr operand1))
                                (register operand2))
                            (aver (reg64-p register))
                            (aver (memq register '(:rax :rbx :rcx :rdx :rsi :rdi)))
                            ;;                             (mumble "p3 :cmp-immediate register = ~S~%" register)
                            (if (eq register :rax)
                                (vector-push-extend
                                 (make-instruction :bytes 2 (list #x48 #x3d))
                                 new-code)
                                (vector-push-extend
                                 (make-instruction :bytes 3
                                                   (list #x48 #x81 (+ #xf8 (register-number register))))
                                 new-code))
                            (vector-push-extend
                             (make-instruction :constant-32 4 form)
                             new-code)))
                         (t
                          (mumble "p3 :cmp-immediate unsupported case~%")
                          (unsupported))))
                  (:byte
                   (vector-push-extend (make-instruction :byte 1 operand1) new-code))
                  (:bytes
                   (let* ((bytes (cdr instruction))
                          (length (length bytes)))
                     (vector-push-extend (make-instruction :bytes length bytes) new-code)))
                  (:recurse
                   (vector-push-extend (make-instruction :recurse 5 nil) new-code))
                  (t
                   (vector-push-extend (assemble-instruction instruction) new-code))))
              (vector-push-extend instruction new-code))))
      (setq *code* (coerce new-code 'simple-vector)))))
