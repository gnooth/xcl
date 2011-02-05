;;; p3-x86-64.lisp
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

(defun finalize-ir2 ()
  (let* ((compiland *current-compiland*)
         (code *code*)
         (len (length code))
         ;; make initial size big enough to avoid having to resize the output vector
         (initial-size (max (logand (+ len (ash len -1) 16) (lognot 15)) 64))
         (new-code (make-array initial-size :fill-pointer 0))
         (index 0)
         (local-var-count 0)
         (locals-allocated-p nil)
         (stack-used 0))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
        (aver (ir2-instruction-p instruction))
        (if (ir2-instruction-p instruction)
            (let ((operator (operator instruction)))
              (case operator
                (:exit
                 (unless (compiland-omit-frame-pointer compiland)
                   (add-ir2-instruction (make-ir2-instruction :leave nil nil) new-code))
                 (dolist (reg (reverse (compiland-registers-to-be-saved compiland)))
                   (add-ir2-instruction (make-ir2-instruction :pop reg nil) new-code))
                 (when (compiland-needs-thread-var-p compiland)
                   (add-ir2-instruction (make-ir2-instruction :pop :r12 nil) new-code))
                 (add-ir2-instruction (make-ir2-instruction :ret nil nil) new-code))
                (:align-stack
                 ;; "The end of the input argument area shall be aligned on a
                 ;; 16 byte boundary. In other words, the value (%rsp - 8) is
                 ;; always a multiple of 16 when control is transferred to the
                 ;; function entry point."
                 (aver (not locals-allocated-p))
                 (unless (compiland-omit-frame-pointer compiland)
                   (let ((numwords local-var-count))
;;                      (unless (compiland-leaf-p compiland)
;;                        (when (evenp stack-used)
;;                          (incf numwords)))
                     (unless (zerop numwords)
                       (add-ir2-instruction (make-ir2-instruction :sub (* numwords +bytes-per-word+) :rsp)
                                            new-code))))
                 (setq locals-allocated-p t))
                (:allocate-local
                 (aver (not locals-allocated-p))
                 (let ((var (operand1 instruction)))
                   (aver (var-p var))
                   (aver (null (var-index var)))
                   (unless (var-register var)
                     (setf (var-index var) index)
                     (incf index)
                     (incf local-var-count)
                     (incf stack-used))))
                (:initialize-arg-var
                 ;; FIXME move this case to FINALIZE-VARS
                 (unless (compiland-omit-frame-pointer compiland)
                   (let ((var (operand1 instruction)))
                     (aver (var-p var))
                     (aver (null (var-index var)))
                     (aver (null (var-register var)))
                     (aver (var-arg-register var))
                     (add-ir2-instruction (make-ir2-instruction :push (var-arg-register var) nil) new-code)
                     (incf stack-used)
                     (setf (var-index var) index)
                     (incf index))))
                (:enter-frame
                 (unless (compiland-omit-frame-pointer compiland)
                   (add-ir2-instruction (make-ir2-instruction :push :rbp nil) new-code)
                   (incf stack-used)
                   (add-ir2-instruction (make-ir2-instruction :mov :rsp :rbp) new-code)))
                (:initialize-thread-register
                 (when (compiland-needs-thread-var-p compiland)
                   (add-ir2-instruction (make-ir2-instruction :call "RT_current_thread" nil) new-code)
                   (add-ir2-instruction (make-ir2-instruction :mov :rax :r12) new-code)))
                (:save-registers
                 (dolist (reg (compiland-registers-to-be-saved compiland))
                   (add-ir2-instruction (make-ir2-instruction :push reg nil) new-code)
                   (incf stack-used)))
                (:save-thread-register
                 (when (compiland-needs-thread-var-p compiland)
                   (add-ir2-instruction (make-ir2-instruction :push :r12 nil) new-code)
                   (incf stack-used)))
                (t
                 (add-ir2-instruction instruction new-code)))))))
    (setq *code* (coerce new-code 'simple-vector))))

;; converts IR2 into bytes
(defun assemble-ir2 ()
  (let* ((compiland *current-compiland*)
         (code *code*)
         (len (length code))
         ;; make initial size big enough to avoid having to resize the output vector
         (initial-size (max (logand (+ len (ash len -1) 16) (lognot 15)) 64))
         (new-code (make-array initial-size :fill-pointer 0))
         (compile-file-p (compile-file-p)))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
        (declare (type ir2-instruction instruction))
        (let ((operator (operator instruction))
              (operand1 (operand1 instruction))
              (operand2 (operand2 instruction)))
          (case operator
            (:mov
             (cond ((var-p operand1)
                    ;; var ref
                    (cond ((var-index operand1)
                           (setf (operand1 instruction)
                                 (list (index-displacement (var-index operand1)) :rbp)))
                          ((var-register operand1)
                           (setf (operand1 instruction) (var-register operand1)))
                          (t
                           (mumble "p3 no var-index for var ~S~%" (var-name operand1))
                           (unsupported))))
                   ((var-p operand2)
                    ;; setq
                    (cond ((var-index operand2)
                           (setf (operand2 instruction)
                                 (list (index-displacement (var-index operand2)) :rbp)))
                          ((var-register operand2)
                           (setf (operand2 instruction) (var-register operand2)))
                          (t
                           (mumble "p3 no var-index for var ~S~%" (var-name operand2))
                           (unsupported))))
                   (t
                    ;; nothing to do
                    ))
             (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))
            (:push
             (cond ((var-p operand1)
                    (cond ((var-index operand1)
                           (setf (operand1 instruction)
                                 (list (index-displacement (var-index operand1)) :rbp)))
                          ((var-register operand1)
                           (setf (operand1 instruction) (var-register operand1)))
                          (t
                           (mumble "p3 :push unsupported case~%")
                           (unsupported))))
                   (t
                    ;; nothing to do
                    ))
             (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))
            (:exit
             (aver nil))
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
                   (add-instruction (make-instruction :exit (length bytes) (coerce bytes 'list))
                                    new-code compile-file-p)))
               (add-instruction (make-instruction :jmp 5 (list t target)) new-code compile-file-p)))
            (:call
             (add-instruction (make-instruction :call 5 operand1) new-code compile-file-p))
            (:move-immediate
             (cond ((and (consp operand1)
                         (eq (%car operand1) :function))
                    (aver (length-eql operand1 2))
                    (let ((symbol (%cadr operand1))
                          (register operand2))
                      ;; mov imm32, reg
                      (add-instruction (make-instruction :bytes 1 (list (+ #xb8 (register-number register))))
                                       new-code compile-file-p)
                      (add-instruction (make-instruction :function 4 symbol)
                                       new-code compile-file-p)))
                   ((and (consp operand1)
                         (eq (%car operand1) :constant-32))
                    (aver (length-eql operand1 2))
                    (let ((form (%cadr operand1))
                          (register operand2))
                      (cond ((memq register '(:eax :ebx :ecx :edx :esi :edi))
                             (add-instruction (make-instruction :bytes 1 (list (+ #xb8 (register-number register))))
                                              new-code compile-file-p))
                            ((memq register '(:r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))
                             (add-instruction (make-instruction :bytes 3
                                                                (list #x49 #xc7
                                                                      (+ #xc0 (register-number register))))
                                              new-code compile-file-p))
                            (t
                             (mumble "p3 :move-immediate :constant-32 unsupported case register = ~S~%"
                                     register)
                             (unsupported)))
                      (add-instruction (make-instruction :constant-32 4 form)
                                       new-code compile-file-p)))
                   (t
                    (mumble "p3 :move-immediate unsupported case~%")
                    (unsupported))))
            (:compare-immediate
             (let (form register)
               (cond ((memq operand1 '(nil t))
                      (setq form     operand1
                            register operand2))
                     ((and (consp operand1)
                           (eq (%car operand1) :constant-32))
                      (aver (length-eql operand1 2))
                      (setq form     (%cadr operand1)
                            register operand2))
                     (t
                      (mumble "p3 :compare-immediate unsupported case~%")
                      (unsupported)))
               (aver (memq register '(:rax :rbx :rcx :rdx :rsi :rdi)))
               (if (eq register :rax)
                   (add-instruction (make-instruction :bytes 2 '(#x48 #x3d)) new-code compile-file-p)
                   (add-instruction (make-instruction :bytes 3
                                                      (list #x48 #x81 (+ #xf8 (register-number register))))
                                    new-code compile-file-p))
               (add-instruction (make-instruction :constant-32 4 form) new-code compile-file-p)))
            (:byte
             (add-instruction (make-instruction :bytes 1 (list operand1)) new-code compile-file-p))
            (:bytes
             (let* ((bytes (operand1 instruction))
                    (length (length bytes)))
               (add-instruction (make-instruction :bytes length bytes) new-code compile-file-p)))
            (:recurse
             (add-instruction (make-instruction :recurse 5 nil) new-code compile-file-p))
            (t
             (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))))))
    (setq *code* (coerce new-code 'simple-vector))))
