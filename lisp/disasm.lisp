;;; disasm.lisp
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

(defstruct (disassembly-block (:conc-name "BLOCK-"))
  start-address
  end-address
  instructions)

(defstruct operand
  kind ; :register, :indirect, :immediate, :relative, :absolute
  register ; base register
  index
  scale
  data)

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
  annotation)

(defparameter *disassemblers* (make-array 256 :initial-element nil))

(declaim (type (simple-array t (256)) *disassemblers*))

(defun install-disassembler (byte disassembler)
  (declare (type (integer 0 255) byte))
  (declare (type symbol disassembler))
  (setf (svref *disassemblers* byte) disassembler))

(defun find-disassembler (byte)
  (declare (type (integer 0 255) byte))
  (svref *disassemblers* byte))

(defmacro define-disassembler (byte-or-bytes &body body)
  (let* ((bytes (designator-list byte-or-bytes))
         (name (intern (format nil "DIS~{-~2,'0X~}" bytes)))
         (args '(byte1 start prefix-byte)))
    `(progn
       (defun ,name ,args
         (declare (ignorable ,@args))
         (let (mnemonic length operand1 operand2 annotation)
           ,@body
           (when prefix-byte
             (decf start)
             (incf length))
           (make-instruction :start start
                             :length length
                             :mnemonic mnemonic
                             :operand1 operand1
                             :operand2 operand2
                             :annotation annotation)))
       (dolist (byte ',bytes)
         (install-disassembler byte ',name)))))

(defparameter *two-byte-disassemblers* (make-hash-table :test 'equal))

(declaim (type hash-table *two-byte-disassemblers*))

(defun install-two-byte-disassembler (bytes disassembler)
  (setf (gethash bytes *two-byte-disassemblers*) disassembler))

(defun find-two-byte-disassembler (byte1 byte2)
  (gethash (list byte1 byte2) *two-byte-disassemblers*))

(defmacro define-two-byte-disassembler (first-byte second-byte-or-bytes &body body)
  (let* ((second-bytes (designator-list second-byte-or-bytes))
         (name (intern (format nil "DIS-~2,'0X~{-~2,'0X~}" first-byte second-bytes)))
         (args '(byte1 byte2 start prefix-byte)))
    `(progn
       (defun ,name ,args
         (declare (ignorable ,@args))
         (let (mnemonic length operand1 operand2 annotation)
           ,@body
           (when prefix-byte
             (decf start)
             (incf length))
           (make-instruction :start start
                             :length length
                             :mnemonic mnemonic
                             :operand1 operand1
                             :operand2 operand2
                             :annotation annotation)))
       (dolist (second-byte ',second-bytes)
         (install-two-byte-disassembler (list ,first-byte second-byte) ',name)))))

(defun unsupported ()
  (error "unsupported"))

(defun unsupported-byte-sequence (&rest bytes)
  (if (length-eql bytes 1)
      (error "unsupported opcode #x~2,'0x" (%car bytes))
      (error "unsupported byte sequence~{ #x~2,'0x~}" bytes)))

(defmacro with-modrm-byte (byte &body body)
  `(let ((modrm-byte ,byte))
     (declare (type (unsigned-byte 8) modrm-byte))
     (let ((mod (ldb (byte 2 6) modrm-byte))
           (reg (ldb (byte 3 3) modrm-byte))
           (rm  (ldb (byte 3 0) modrm-byte)))
       (declare (ignorable mod reg rm))
       ,@body)))

(defmacro with-sib-byte (byte &body body)
  `(let ((sib-byte ,byte))
     (declare (type (unsigned-byte 8) modrm-byte))
     (let ((scale (ldb (byte 2 6) sib-byte))
           (index (ldb (byte 3 3) sib-byte))
           (base  (ldb (byte 3 0) sib-byte)))
       (declare (ignorable scale index base))
       ,@body)))
