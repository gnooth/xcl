;;; disassemble.lisp
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

(in-package "DISASSEMBLER")

(require #+x86 "X86" #+x86-64 "X86-64")

(defvar *lambda-name* nil)

(defvar *blocks* nil)

(defvar *labels* nil)

(defvar *labels-hash-table* nil)

(defvar *locals* nil)

(defvar *instructions* nil)

(defvar *start-address* nil)

(defvar *end-address* nil)

(load-system-file "lisp/disasm")

(defun register-string (reg)
  (ecase reg
    (:al  "%al")
    (:bl  "%bl")
    (:cl  "%cl")
    (:dl  "%dl")
    (:spl "%spl")
    (:bpl "%bpl")
    (:sil "%sil")
    (:dil "%dil")
    (:ax  "%ax")
    (:cx  "%cx")
    (:dx  "%dx")
    (:bx  "%bx")
    (:sp  "%sp")
    (:bp  "%bp")
    (:si  "%si")
    (:di  "%di")
    (:eax "%eax")
    (:ecx "%ecx")
    (:edx "%edx")
    (:ebx "%ebx")
    (:esp "%esp")
    (:ebp "%ebp")
    (:esi "%esi")
    (:edi "%edi")
    (:eax "%eax")
    (:ecx "%ecx")
    (:edx "%edx")
    (:ebx "%ebx")
    (:esp "%esp")
    (:ebp "%ebp")
    (:esi "%esi")
    (:edi "%edi")
    (:rax "%rax")
    (:rcx "%rcx")
    (:rdx "%rdx")
    (:rbx "%rbx")
    (:rsp "%rsp")
    (:rbp "%rbp")
    (:rsi "%rsi")
    (:rdi "%rdi")
    (:rip "%rip")
    (:r8  "%r8")
    (:r9  "%r9")
    (:r10 "%r10")
    (:r11 "%r11")
    (:r12 "%r12")
    (:r13 "%r13")
    (:r14 "%r14")
    (:r15 "%r15")))

(defparameter *runtime-addresses* nil)

(defun get-runtime-addresses (function)
  (let ((ht (make-hash-table)))
    (maphash #'(lambda (key value) (setf (gethash (ldb (byte 32 0) value) ht) key))
             *runtime-names*)
    (flet ((process (thing)
             #+x86
             (setf (gethash (value-to-ub32 thing) ht) thing)
             #+x86-64
             (setf (gethash (value-to-ub64 thing) ht) thing)
             (when (and (symbolp thing)
                        (fboundp thing)
                        (not (autoloadp thing)))
               (let* ((function (symbol-function thing))
                      (code-address (function-code-address function)))
                 #+x86
                 (setf (gethash (value-to-ub32 function) ht) function)
                 #+x86-64
                 (setf (gethash (value-to-ub64 function) ht) function)
                 (when code-address
                   (setf (gethash code-address ht) thing))))))
      (dolist (package (list-all-packages))
        (dolist (symbol (package-external-symbols package))
          (process symbol))
        (dolist (symbol (package-internal-symbols package))
          (process symbol)))
      (when (compiled-function-p function)
        (let ((constants (compiled-function-constants function)))
          (dolist (constant constants)
            (when (or (symbolp constant) (functionp constant) (listp constant))
              (process constant))))))
    ht))

(defun print-operand (op)
  (declare (type operand op))
  (let ((string (case (operand-kind op)
                  (:register
                   (princ-to-string (register-string (operand-register op))))
                  (:indirect
                   (format nil "(~A)" (register-string (operand-register op))))
                  (:immediate
                   (format nil "$0x~X" (operand-data op)))
                  (:relative
                   (cond ((zerop (operand-data op))
                          (format nil "(~A)" (register-string (operand-register op))))
                         ((minusp (operand-data op))
                          (format nil "-0x~X(~A)" (- (operand-data op)) (register-string (operand-register op))))
                         (t
                          (format nil "0x~X(~A)" (operand-data op) (register-string (operand-register op))))))
                  (:indexed
                   (format nil "0x~X(~A,~A,~D)"
                           (operand-data op)
                           (register-string (operand-register op))
                           (register-string (operand-index op))
                           (ecase (operand-scale op)
                             (0 1)
                             (1 2)
                             (2 4)
                             (3 8))))
                  (:absolute
                   (format nil "0x~X" (operand-data op))))))
    (format t "~A" (nstring-downcase string))))

(defun fill-to-pos (pos stream)
  (loop
    (write-char #\space stream)
    (when (>= (charpos stream) pos)
      (return))))

(defun print-instruction (instruction)
  (fresh-line)
  (let ((start (instruction-start instruction)))
    (let ((label (and *labels-hash-table* (gethash start *labels-hash-table*))))
      (when label
        (format t "~A:~%" label)))
    (format t "  ~X: " start)
    (dotimes (i (instruction-length instruction))
      (format t "~2,'0X " (mref-8 start i))))
  (fill-to-pos 40 *standard-output*)
  (let ((mnemonic (instruction-mnemonic instruction))
        (op1 (instruction-operand1 instruction))
        (op2 (instruction-operand2 instruction))
        (annotation (instruction-annotation instruction)))
    (let* ((s (string mnemonic)))
      (princ (string-downcase s)))
    (when op1
      (fill-to-pos 48 *standard-output*)
      (if (memq mnemonic '(:jmp :jmpq :je :jz :jne :jge :jle :jnz :jo :jno :jl :jnl :jg :jng :js :ja :jae :call :callq))
          (let* ((address (operand-data op1))
                 (label (and *labels-hash-table* (gethash address *labels-hash-table*))))
            (cond (label
                   (format t "~A" label))
                  (t
                   (print-operand op1))))
          (print-operand op1)))
    (when op2
      (write-char #\,)
      (print-operand op2))
    (let ((annotation-pos #+x86-64 80 #-x86-64 72))
      (cond ((eql annotation *start-address*)
             (fill-to-pos annotation-pos *standard-output*)
             (if *lambda-name*
                 (format t "; ~S" *lambda-name*)
                 (format t "; recursive call")))
            ((integerp annotation)
             (multiple-value-bind (value present-p)
                 (gethash annotation *runtime-addresses*)
               (when present-p
                 (fill-to-pos annotation-pos *standard-output*)
                 (cond ((consp value)
                        (format t "; '~A" value))
                       ((memq value '(t nil))
                        (format t "; ~S" value))
                       ((keywordp value)
                        (format t "; ~S" value))
                       ((symbolp value)
                        (cond ((memq mnemonic '(:jmpq :callq :call))
                               (format t "; ~S" value))
                              (t
                               (format t "; '~S" value))))
                       (t
                        (format t "; ~A" value))))))
            ((and annotation (symbolp annotation))
             (fill-to-pos (if (< (charpos *standard-output*) annotation-pos)
                              annotation-pos
                              (+ annotation-pos 8))
                          *standard-output*)
             (format t "; ~S" annotation))))))

(defun process-block (block)
  (let ((start (block-start-address block)))
    (loop
      (let ((byte1 (mref-8 start 0))
            prefix-byte)
        #+x86-64
        (when (<= #x40 byte1 #x4f)
          (setq prefix-byte byte1)
          (setq byte1 (mref-8 (incf start) 0)))
        (let (disassembler instruction)
          (cond ((eq byte1 #x0f)
                 (let ((byte2 (mref-8 start 1)))
                   (setq disassembler (find-two-byte-disassembler byte1 byte2))
                   (unless disassembler
                     (error "No disassembler for two-byte opcode #x~2,'0x #x~2,'0x at #x~X" byte1 byte2 start))
                   (setq instruction (funcall disassembler byte1 byte2 start prefix-byte))))
                (t
                 (setq disassembler (find-disassembler byte1))
                 (unless disassembler
                   (error "No disassembler for opcode #x~2,'0x at #x~X" byte1 start))
                 (setq instruction (funcall disassembler byte1 start prefix-byte))))
          #+nil
          (print-instruction instruction)
          (push instruction *instructions*)
          (when (memq byte1 '(#xc3 #xe9 #xeb))
            (setf (block-end-address block)
                  (+ (instruction-start instruction) (instruction-length instruction)))
            (return))
          (setq start (+ (instruction-start instruction) (instruction-length instruction))))))))

(defun process-blocks ()
  (loop
    (when (null *blocks*)
      (return))
    (let ((block (car *blocks*)))
      (setq *blocks* (cdr *blocks*))
      ;; REVIEW
      (cond ((eql (block-start-address block) *end-address*)
             (process-block block)
             (setq *end-address* (max *end-address* (block-end-address block))))
            ((> (block-start-address block) *end-address*)
             (push block *blocks*)
             (push (make-disassembly-block :start-address *end-address*) *blocks*))))))

(defun collect-labels ()
  (setq *labels* (sort *labels* #'<))
  (setq *labels-hash-table* (make-hash-table))
  (let ((i 0))
    (dolist (label *labels*)
      (when (and (>= label *start-address*)
                 (< label *end-address*))
        (unless (gethash label *labels-hash-table*)
          (setf (gethash label *labels-hash-table*) (format nil "L~D" i))
          (incf i))))))

(defun disassemble-function (function)
  (let* ((*locals* (local-variable-information function))
         (*start-address* (function-code-address function))
         (*end-address* *start-address*)
         (block (make-disassembly-block :start-address *start-address*))
         (*blocks* (list block))
         (*labels* nil)
         (*instructions* nil))
    (process-blocks)
    (collect-labels)
    (format t "; Disassembly of ~S~%" function)
    (when (compiled-function-p function)
      (format t "; Constants:~%")
      (dolist (constant  (compiled-function-constants function))
        (format t "  ~S~%" constant)))
    (format t "; Code:~%")
    (dolist (instruction (nreverse *instructions*))
      (print-instruction instruction))
    (fresh-line)
    (format t "; ~D bytes~%" (- *end-address* *start-address*)))
  (when (compiled-function-p function)
    (let ((constants (compiled-function-constants function)))
      (dolist (constant constants)
        (cond ((and (compiled-function-p constant)
                    (not (kernel-function-p constant)))
               (terpri)
               (disassemble-function constant))
              ((and (symbolp constant)
                    (null (symbol-package constant))
                    (fboundp constant))
               (terpri)
               (disassemble-function (symbol-function constant))))))))

(defun disassemble (arg)
  (unless (or (functionp arg)
              (lambda-expression-p arg)
              (valid-function-name-p arg))
    (error 'type-error
           :datum arg
           :expected-type
           '(or symbol function (satisfies setf-function-name-p) (satisfies lambda-expression-p))))
  (setq *lambda-name* (if (symbolp arg) arg nil))
  (when (and (symbolp arg) (autoloadp arg))
    (resolve arg))
  (let ((function (cond ((functionp arg)
                         arg)
                        ((lambda-expression-p arg)
                         (coerce-to-function arg))
                        (t
                         (fdefinition arg)))))
    (when (typep function 'standard-generic-function)
      (setq function (funcallable-instance-function function)))
    (unless (compiled-function-p function)
      (setq function (compile nil function)))
    (let ((*runtime-addresses* (get-runtime-addresses function)))
      (disassemble-function function)))
  nil)

(load-system-file #+x86 "lisp/disasm-x86" #+x86-64 "lisp/disasm-x86-64")
