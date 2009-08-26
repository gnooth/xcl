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

(defun generate-function-prolog ()
  (let* ((compiland *current-compiland*)
         (arity (compiland-arity compiland))
         prolog)
    (when (and arity
               (<= arity 6)
               (null *closure-vars*)
;;                (not (compiland-child-p compiland))
               )
      (let ((*code* nil)
            (*main* nil)
            (*elsewhere* nil))
        (p2-trivial-function-prolog compiland)
        (if *elsewhere*
            (setq prolog (concatenate 'simple-vector *main* *elsewhere*))
            (setq prolog (concatenate 'simple-vector *main*))))
      (setq *code* (concatenate 'simple-vector prolog *code*)))))

(defun p3 ()
  (generate-function-prolog)
  (let* ((compiland *current-compiland*)
         (code *code*)
         (len (length code))
         ;; make initial size big enough to avoid having to resize the output vector
         (initial-size (max (logand (+ len (ash len -1) 16) (lognot 15)) 64))
         (new-code (make-array initial-size :fill-pointer 0))
         (leaf-p t)
         (var-ref-count 0))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
        ;;           (mumble "p3 instruction = ~S~%" instruction)
        ;;           (unless (consp instruction)
        ;;             (format t "p3 non-cons instruction = ~S~%" instruction))
        (if (consp instruction)
            (let ((mnemonic (first instruction))
                  (operand1 (second instruction))
                  (operand2 (third  instruction)))
              (case mnemonic
                (:mov
                 (cond ((var-p operand1)
                        ;; var ref
                        (incf var-ref-count)
                        (cond ((var-index operand1)
                               (setf (second instruction)
                                     (list (index-displacement (var-index operand1)) :ebp)))
                              ((var-register operand1)
                               (setf (second instruction) (var-register operand1)))
                              (t
                               (mumble "p3 :mov no var-index for var ~S~%" (var-name operand1))
                               (unsupported))))
                       ((var-p operand2)
                        ;; setq
                        (incf var-ref-count)
                        (cond ((var-index operand2)
                               (setf (third instruction)
                                     (list (index-displacement (var-index operand2)) :ebp)))
                              ((var-register operand2)
                               (setf (third instruction) (var-register operand2)))
                              (t
                               (mumble "p3 :mov no var-index for var ~S~%" (var-name operand2))
                               (unsupported))))
                       (t
                        ;; nothing to do
                        ))
                 (vector-push-extend (assemble-instruction instruction) new-code))
                (:push
                 (cond ((var-p operand1)
                        (cond ((var-index operand1)
                               (setf (second instruction)
                                     (list (index-displacement (var-index operand1)) :ebp))
                               (vector-push-extend (assemble-instruction instruction) new-code))
                              ((var-register operand1)
                               (setf (second instruction) (var-register operand1))
                               (vector-push-extend (assemble-instruction instruction) new-code))
                              (t
                               (mumble "p3 :push no var-index for var ~S~%" (var-name operand1))
                               (unsupported))))
                       ((and (consp operand1)
                             (length-eql operand1 2)
                             (eq (%car operand1) :constant))
                        (vector-push-extend (make-instruction :byte 1 #x68) new-code)
                        (vector-push-extend (make-instruction :constant 4 (%cadr operand1)) new-code))
                       (t
                        ;; nothing to do
                        (vector-push-extend (assemble-instruction instruction) new-code))))
                (:exit
                 (let ((instructions nil))
                   (unless (compiland-omit-frame-pointer compiland)
                     (push '(:leave) instructions))
                   (dolist (reg (reverse (compiland-registers-to-be-saved compiland)))
                     (push `(:pop ,reg) instructions))
                   (push '(:ret) instructions)
                   (setq instructions (nreverse instructions))
                   (let ((bytes (assemble instructions)))
                     (setq instruction
                           (make-instruction :exit (length bytes) (coerce bytes 'list)))
                     ;;                        (setf (svref code i) instruction)
                     ))
                 (vector-push-extend instruction new-code))
                (:call
                 (setq leaf-p nil)
                 (setq instruction (make-instruction :call 5 operand1))
                 (vector-push-extend instruction new-code))
                (:mov-immediate
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
                       ;;                          ((and (consp operand1)
                       ;;                                (eq (%car operand1) :constant-32))
                       ;;                           (aver (length-eql operand1 2))
                       ;;                           (let ((form (%cadr operand1))
                       ;;                                 (register operand2))
                       ;;                             ;; mov imm32, reg
                       ;;                             (vector-push-extend
                       ;;                              (make-instruction :byte 1 (+ #xb8 (register-number register)))
                       ;;                              new-code)
                       ;;                             (vector-push-extend
                       ;;                              (make-instruction :constant-32 4 form)
                       ;;                              new-code)))
                       ((and (consp operand1)
                             (eq (%car operand1) :constant))
                        (aver (length-eql operand1 2))
                        (let ((form (%cadr operand1))
                              (register operand2))
                          (cond ((memq register '(:eax :ebx :ecx :edx :esi :edi))
                                 (vector-push-extend
                                  (make-instruction :byte 1 (+ #xb8 (register-number register)))
                                  new-code))
                                (t
                                 (mumble "p3 :mov-immediate :constant unsupported case register = ~S~%"
                                         register)
                                 (unsupported)))
                          (vector-push-extend
                           (make-instruction :constant 4 form)
                           new-code)))
                       (t
                        (mumble "p3 :mov-immediate unsupported case~%")
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
            ;;             (when (consp instruction)
            ;;               (let ((assembled-instruction (assemble-instruction instruction)))
            ;;                 (setf (svref code i) assembled-instruction)))
            (vector-push-extend instruction new-code))))
    (when (> (length new-code) initial-size)
      (mumble "p3 initial-size = ~D (length new-code) = ~D~%"
              initial-size
              (length new-code)))
    (setq *code* (coerce new-code 'simple-vector))
    ;;       (when leaf-p
    ;;         (mumble "~S leaf-p = ~S var-ref-count = ~S~%" (compiland-name compiland) leaf-p var-ref-count))
    ))
