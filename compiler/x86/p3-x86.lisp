;;; p3-x86.lisp
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

(defun finalize-ir2 ()
  (let* ((compiland *current-compiland*)
         (code *code*)
         (len (length code))
         ;; make initial size big enough to avoid having to resize the output vector
         (initial-size (max (logand (+ len (ash len -1) 16) (lognot 15)) 64))
         (new-code (make-array initial-size :fill-pointer 0))
         (index -1) ; first local is at -4(%ebp)
         (local-var-count 0)
         (locals-allocated-p nil))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
        (declare (type ir2-instruction instruction))
        (let ((operator (operator instruction)))
          (case operator
            (:exit
             (unless (compiland-omit-frame-pointer compiland)
               (add-ir2-instruction (make-ir2-instruction :leave nil nil) new-code))
             (dolist (reg (reverse (compiland-registers-to-be-saved compiland)))
               (add-ir2-instruction (make-ir2-instruction :pop reg nil) new-code))
             (add-ir2-instruction (make-ir2-instruction :ret nil nil) new-code))
            (:align-stack
             (aver (not locals-allocated-p))
             (unless (zerop local-var-count)
               (add-ir2-instruction (make-ir2-instruction :sub (* local-var-count +bytes-per-word+) :esp)
                                    new-code))
             (setq locals-allocated-p t))
            (:allocate-local
             (aver (not locals-allocated-p))
             (let ((var (operand1 instruction)))
               (aver (var-p var))
               (aver (null (var-index var)))
               (unless (var-register var)
                 (setf (var-index var) index)
                 (decf index)
                 (incf local-var-count))))
            (:enter-frame
             (unless (compiland-omit-frame-pointer compiland)
               (add-ir2-instruction (make-ir2-instruction :push :ebp nil) new-code)
               (add-ir2-instruction (make-ir2-instruction :mov :esp :ebp) new-code)))
            (:initialize-thread-var
             (when (compiland-thread-var compiland)
               (add-ir2-instruction (make-ir2-instruction :call "RT_current_thread" nil) new-code)
               (add-ir2-instruction (make-ir2-instruction :mov :eax (compiland-thread-var compiland)) new-code)))
            (:save-registers
             (dolist (reg (compiland-registers-to-be-saved compiland))
               (add-ir2-instruction (make-ir2-instruction :push reg nil) new-code)))
            (t
             (add-ir2-instruction instruction new-code))))))
    (setq *code* (coerce new-code 'simple-vector))))

(defun assemble-ir2 ()
  (let* ((code *code*)
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
                                 (list (index-displacement (var-index operand1)) :ebp)))
                          ((var-register operand1)
                           (setf (operand1 instruction) (var-register operand1)))
                          (t
                           (mumble "p3 :mov no var-index for var ~S~%" (var-name operand1))
                           (unsupported))))
                   ((var-p operand2)
                    ;; setq
                    (cond ((var-index operand2)
                           (setf (operand2 instruction)
                                 (list (index-displacement (var-index operand2)) :ebp)))
                          ((var-register operand2)
                           (setf (operand2 instruction) (var-register operand2)))
                          (t
                           (mumble "p3 :mov no var-index for var ~S~%" (var-name operand2))
                           (unsupported))))
                   (t
                    ;; nothing to do
                    ))
             (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))
            (:push
             (cond ((var-p operand1)
                    (cond ((var-index operand1)
                           (setf (operand1 instruction)
                                 (list (index-displacement (var-index operand1)) :ebp))
                           (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))
                          ((var-register operand1)
                           (setf (operand1 instruction) (var-register operand1))
                           (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))
                          (t
                           (mumble "p3 :push no var-index for var ~S~%" (var-name operand1))
                           (unsupported))))
                   ((and (consp operand1)
                         (length-eql operand1 2)
                         (eq (%car operand1) :constant))
                    (add-instruction (make-instruction :bytes 1 '(#x68)) new-code compile-file-p)
                    (add-instruction (make-instruction :constant 4 (%cadr operand1)) new-code compile-file-p))
                   (t
                    ;; nothing to do
                    (add-instruction (assemble-ir2-instruction instruction) new-code compile-file-p))))
            (:push-immediate
             (cond ((and (consp operand1)
                         (eq (%car operand1) :constant))
                    (aver (length-eql operand1 2))
                    (add-instruction (make-instruction :bytes 1 '(#x68)) new-code compile-file-p)
                    (add-instruction
                     (make-instruction :constant 4 (%cadr operand1))
                     new-code compile-file-p))
                   (t
                    (mumble "push-immediate unsupported case~%")
                    (unsupported))))
            (:exit
             (aver nil))
            (:call
             (add-instruction (make-instruction :call 5 operand1) new-code compile-file-p))
            (:move-immediate
             (cond ((and (consp operand1)
                         (eq (%car operand1) :function))
                    (aver (length-eql operand1 2))
                    (let ((symbol (%cadr operand1))
                          (register operand2))
                      ;; mov imm32, reg
                      (add-instruction
                       (make-instruction :bytes 1 (list (+ #xb8 (register-number register))))
                       new-code compile-file-p)
                      (add-instruction
                       (make-instruction :function 4 symbol)
                       new-code compile-file-p)))
                   ((and (consp operand1)
                         (eq (%car operand1) :constant))
                    (aver (length-eql operand1 2))
                    (let ((form (%cadr operand1))
                          (register operand2))
                      (cond ((memq register '(:eax :ebx :ecx :edx :esi :edi))
                             (add-instruction
                              (make-instruction :bytes 1 (list (+ #xb8 (register-number register))))
                              new-code compile-file-p))
                            (t
                             (mumble "p3 :move-immediate :constant unsupported case register = ~S~%"
                                     register)
                             (unsupported)))
                      (add-instruction
                       (make-instruction :constant 4 form)
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
                           (eq (%car operand1) :constant))
                      (aver (length-eql operand1 2))
                      (setq form     (%cadr operand1)
                            register operand2))
                     (t
                      (mumble "p3 :compare-immediate unsupported case~%")
                      (unsupported)))
               (aver (memq register '(:eax :ebx :ecx :edx :esi :edi)))
               (if (eq register :eax)
                   (add-instruction (make-instruction :bytes 1 '(#x3d)) new-code compile-file-p)
                   (add-instruction (make-instruction :bytes 2
                                                      (list #x81 (+ #xf8 (register-number register))))
                                    new-code compile-file-p))
               (add-instruction (make-instruction :constant 4 form) new-code compile-file-p)))
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
    (when (> (length new-code) initial-size)
      (mumble "p3 initial-size = ~D (length new-code) = ~D~%"
              initial-size
              (length new-code)))
    (setq *code* (coerce new-code 'simple-vector))))
