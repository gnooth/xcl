;;; disassemble.lisp
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

(in-package "EXTENSIONS")

(export '(d x p/x))

(in-package "SYSTEM")

#+x86
(require "X86")
#+x86-64
(require "X86-64")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "DISASSEMBLER")
    (make-package "DISASSEMBLER"
                  :nicknames '("DIS")
                  :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM"
                         #+x86    "X86"
                         #+x86-64 "X86-64"))))

(in-package "DISASSEMBLER")

(defvar *lambda-name* nil)

(defvar *blocks* nil)

(defvar *labels* nil)

(defvar *labels-hash-table* nil)

(defvar *locals* nil)

(defvar *instructions* nil)

(defvar *start-address* nil)

(defvar *end-address* nil)

(defun unsupported ()
  (error "unsupported"))

;; #+x86
;; (load-system-file "disasm-x86.lisp")
;; #+x86-64
;; (load-system-file "disasm-x86-64.lisp")

(defstruct (disassembly-block (:conc-name "BLOCK-"))
  start-address
  end-address
  instructions
  )

(defstruct operand
  kind ; :register, :indirect, :immediate, :relative, :absolute
  register
  data
  )

(defknown make-register-operand (t) operand)
(defun make-register-operand (reg)
  (make-operand :kind :register
                :register reg))

(defknown make-indirect-operand (t) operand)
(defun make-indirect-operand (reg)
  (make-operand :kind :indirect
                :register reg))

(defknown make-immediate-operand (t) operand)
(defun make-immediate-operand (data)
  (make-operand :kind :immediate
                :data data))

(defknown make-absolute-operand (t) operand)
(defun make-absolute-operand (data)
  (make-operand :kind :absolute
                :data data))

(defstruct instruction
  start
  length
  mnemonic
  operand1
  operand2
  annotation
  )

(defun register-string (reg)
  (case reg
    (:al  "%al")
    (:bl  "%bl")
    (:cl  "%cl")
    (:dl  "%dl")
    (:spl "%spl")
    (:bpl "%bpl")
    (:sil "%sil")
    (:dil "%dil")
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
    (:r15 "%r15")
    ))

(defparameter *runtime-addresses* nil)

(defun print-operand (op)
  (declare (type operand op))
  (case (operand-kind op)
    (:register
     (princ (register-string (operand-register op))))
    (:indirect
     (format t "(~A)" (register-string (operand-register op))))
    (:immediate
     (format t "$0x~X" (operand-data op)))
    (:relative
     (cond ((zerop (operand-data op))
            (format t "(~A)" (register-string (operand-register op))))
           #+x86-64
           ((eq (operand-register op) :rip)
            ;; gdb prints the displacement in decimal if the register is %rip
            (format t "~D(~A)" (operand-data op) (register-string (operand-register op))))
           ((minusp (operand-data op))
;;             (format t "0x~X(~A)" (ldb (byte #+x86 32 #+x86-64 64 0) (operand-data op))
;;                     (register-string (operand-register op)))
            (format t "~D(~A)" (operand-data op) (register-string (operand-register op)))
            )
           (t
;;             (format t "0x~X(~A)" (operand-data op) (register-string (operand-register op)))
            (format t "~D(~A)" (operand-data op) (register-string (operand-register op)))
            )))
    (:absolute
     (format t "0x~X" (operand-data op)))))

(defun fill-to-pos (pos stream)
;;   (when (< (charpos stream) pos)
;;     (dotimes (i (- pos (charpos stream)))
;;       (write-char #\space stream))))
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
      (format t "~2,'0X " (mref-8 start i)))
;;     (let ((label (and *labels-hash-table* (gethash start *labels-hash-table*))))
;;       (when label
;;         (fill-to-pos 32 *standard-output*)
;;         (format t "~A:" label)))
    )
  (fill-to-pos 40 *standard-output*)
  (let ((mnemonic (instruction-mnemonic instruction))
        (op1 (instruction-operand1 instruction))
        (op2 (instruction-operand2 instruction))
        (annotation (instruction-annotation instruction)))
    (let* ((s (string mnemonic)))
      (princ (string-downcase s)))
    (when op1
      (fill-to-pos 48 *standard-output*)
      (if (memq mnemonic '(:jmp :jmpq :je :jz :jne :jge :jle :jnz :jo :jno :jl :jnl :jg :jng :js :call :callq))
          (let* ((address (operand-data op1))
                 (label (and *labels-hash-table* (gethash address *labels-hash-table*))))
            (cond (label
                   (format t "~A" label)
;;                    (fill-to-pos 72 *standard-output*)
;;                    (format t "; 0x~X" address)
                   )
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
                 (cond ((or (symbolp value)
                            (functionp value))
                        (format t "; ~S" value))
                       (t
                        (format t "; ~A" value))))))
            ((and annotation (symbolp annotation))
             (fill-to-pos (if (< (charpos *standard-output*) annotation-pos)
                              annotation-pos
                              (+ annotation-pos 8))
                          *standard-output*)
             (format t "; ~S" annotation))))
    ))

(defun print-inst (inst)
  (let* ((s (princ-to-string inst))
         (len (length s)))
    (princ s)
    (when (< len 7)
      (dotimes (i (- 7 len))
        (write-char #\space)))))

(defun print-arg (arg)
  (cond ((numberp arg)
         (format t "$~X" arg))
        ((stringp arg)
         (princ arg))
        ((keywordp arg)
         (princ (ecase arg
                  (:eax "%eax")
                  (:ecx "%ecs")
                  (:edx "%edx")
                  (:ebx "%ebx")
                  (:esp "%esp")
                  (:ebp "%ebp")
                  (:esi "%esi")
                  (:edi "%edi"))))))

;; EAX is 0, ECX is 1, EDX is 2, EBX is 3, ESP is 4, EBP is 5, ESI is 6, and EDI is 7

(defun print-args (args)
  (cond ((null args))
        ((atom args)
         (print-arg args))
        (t
         (print-arg (car args))
         (write-char #\,)
         (print-arg (cadr args)))))

(defun print-line-header (code start-index nbytes)
  (fresh-line)
  (format t "~8X: " (+ code start-index))
  (dotimes (i nbytes)
    (format t "~2,'0X " (mref-8 code (+ start-index i))))
  (let ((pos (charpos *standard-output*)))
    (when (< pos 32)
      (dotimes (i (- 32 pos)) (write-char #\space)))))

(defun print-line (code start-index nbytes inst args)
  (print-line-header code start-index nbytes)
  (print-inst inst)
  (print-args args)
  (terpri))

(defmacro with-modrm-byte (byte &body body)
  `(let ((modrm-byte ,byte))
     (declare (type (unsigned-byte 8) modrm-byte))
     (let ((mod (ldb (byte 2 6) modrm-byte))
           (reg (ldb (byte 3 3) modrm-byte))
           (rm  (ldb (byte 3 0) modrm-byte)))
       (declare (ignorable mod reg rm))
       ,@body)))

(defmacro with-sib (byte &body body)
  `(let* ((sib ,byte)
          (ss    (ldb (byte 2 6) sib))
          (index (ldb (byte 3 3) sib))
          (base  (ldb (byte 3 0) sib)))
     (declare (ignorable ss index base))
     ,@body))

(defun process-jcc (instruction-start-address)
  (aver (eql (mref-8 instruction-start-address 0) #x0f))
  (let* ((byte2 (mref-8 instruction-start-address 1))
         (mnemonic (case (ldb (byte 4 0) byte2)
                     (#x0 :jo)
                     (#x1 :jno)
                     (#x2 :jb)
                     (#x3 :jae)
                     (#x4 :je)
                     (#x5 :jne)
                     (#x6 :jbe)
                     (#x7 :ja)
                     (#x8 :js)
                     (#x9 :jns)
                     (#xa :jpe)
                     (#xb :jpo)
                     (#xc :jl)
                     (#xd :jge)
                     (#xe :jle)
                     (#xf :jg)))
         (displacement (mref-32 instruction-start-address 2))
         (absolute-address (ldb (byte 32 0) (+ instruction-start-address 6 displacement))))
    (push (make-disassembly-block :start-address absolute-address) *blocks*)
    (push absolute-address *labels*)
    (make-instruction :start instruction-start-address
                      :length 6
                      :mnemonic mnemonic
                      :operand1 (make-absolute-operand absolute-address))))

(defun process-blocks ()
  (loop
    (when (null *blocks*)
      (return))
    (let ((block (car *blocks*)))
      (setq *blocks* (cdr *blocks*))
      ;; REVIEW
      (cond ((eql (block-start-address block) *end-address*)
             (process-block block)
             (setq *end-address* (max *end-address* (block-end-address block)))
             )
            ((> (block-start-address block) *end-address*)
             (push block *blocks*)
             (push (make-disassembly-block :start-address *end-address*) *blocks*)
             )
            ))))

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
  (let* (;(*locals* (getf (function-plist function) 'sys::locals))
         (*locals* (local-variable-information function))
         (*start-address* (function-code function))
         (*end-address* *start-address*)
         (block (make-disassembly-block :start-address *start-address*))
         (*blocks* (list block))
         (*labels* nil)
         (*instructions* nil))
    (process-blocks)
    (collect-labels)
    (format t "; Disassembly of ~S~%" function)
    (when (compiled-function-p function)
      (format t "; Constants:~%  ~S~%" (compiled-function-constants function)))
    (format t "; Code:~%")
    (dolist (instruction (nreverse *instructions*))
      (print-instruction instruction))
    (fresh-line)
    (format t "; ~D bytes~%" (- *end-address* *start-address*)))

  (when (compiled-function-p function)
    (let ((constants (compiled-function-constants function)))
      (dolist (constant constants)
        (when (and (compiled-function-p constant)
                   (not (kernel-function-p constant)))
          (terpri)
          (disassemble-function constant))))))

(defun disassemble (arg)
  (setq *lambda-name* (if (symbolp arg) arg nil))
  (when (and (symbolp arg) (autoloadp arg))
    (resolve arg))
  (let ((function (if (symbolp arg)
                      (fdefinition arg)
                      (coerce-to-function arg))))
    (when (typep function 'standard-generic-function)
      (setq function (funcallable-instance-function function)))
    (unless (compiled-function-p function)
      (setq function (compile nil function)))
    (let ((*runtime-addresses* (make-hash-table)))
      (maphash #'(lambda (key value) (setf (gethash (ldb (byte 32 0) value) *runtime-addresses*) key))
               *runtime-names*)
      (flet ((process (thing)
                      #+x86
                      (setf (gethash (value-to-ub32 thing) *runtime-addresses*) thing)
                      #+x86-64
                      (setf (gethash (value-to-ub64 thing) *runtime-addresses*) thing)
                      (when (and (symbolp thing)
                                 (fboundp thing)
                                 (not (autoloadp thing)))
                        (let* ((function (symbol-function thing))
                               (code (function-code function)))
                          #+x86
                          (setf (gethash (value-to-ub32 function) *runtime-addresses*) function)
                          #+x86-64
                          (setf (gethash (value-to-ub64 function) *runtime-addresses*) function)
                          (when code
                            (setf (gethash code *runtime-addresses*) thing))))))
        (dolist (package (list-all-packages))
          (dolist (symbol (package-external-symbols package))
            (process symbol))
          (dolist (symbol (package-internal-symbols package))
            (process symbol)))
        (when (compiled-function-p function)
          (let ((constants (compiled-function-constants function)))
            (dolist (constant constants)
              (when (or (symbolp constant) (functionp constant))
                (process constant)))))
        )
      (disassemble-function function)))
  nil)

(defun d (addr)
  (let* ((*locals* nil)
         (*start-address* addr)
         (*end-address* *start-address*)
         (block (make-disassembly-block :start-address *start-address*))
         (*blocks* (list block))
         (*labels* nil)
         (*instructions* nil))
    (process-blocks)
    (collect-labels)
    (dolist (instruction (nreverse *instructions*))
      (print-instruction instruction))))

(defun x (addr &optional (nbytes 16))
  (let ((offset 0))
    (when (< offset nbytes)
      (loop
        (fresh-line)
        (format t "~X: " (+ addr offset))
        (dotimes (i 16)
          (let ((byte (mref-8 addr offset)))
            (format t "~2,'0x " byte))
          (incf offset)
          (when (eql offset nbytes)
            (terpri)
            (return-from x)))
        (terpri)))))

(defun p/x (n)
  (let ((*print-base* 16)) (format t "#x~X" n)))

#+x86
(load-system-file "disasm-x86.lisp")
#+x86-64
(load-system-file "disasm-x86-64.lisp")
