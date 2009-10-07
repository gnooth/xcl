;;; assembler.lisp
;;;
;;; Copyright (C) 2007-2008 Peter Graves <peter@armedbear.org>
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

(in-package "SYSTEM")

#+x86
(require "X86")
#+x86-64
(require "X86-64")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "ASSEMBLER")
    (make-package "ASSEMBLER"
                  :nicknames '("ASM")
                  :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM"
                         #+x86    "X86"
                         #+x86-64 "X86-64"))))

(in-package "EXTENSIONS")

(export '(assemble))

(in-package "ASSEMBLER")

(export '(*output*))

(defvar *output* nil)

(defun unsupported ()
  (error "unsupported"))

(defun emit-byte (byte)
  (vector-push-extend byte *output*))

(defun emit-bytes (&rest bytes)
  (dolist (byte bytes)
    (vector-push-extend byte *output*)))

(defun emit-raw-dword (n)
  (let ((bytes (list (ldb (byte 8 0)  n)
                     (ldb (byte 8 8)  n)
                     (ldb (byte 8 16) n)
                     (ldb (byte 8 24) n))))
    (dolist (byte bytes)
      (vector-push-extend byte *output*))))

(unless (boundp '+assemblers+)
  (defconstant +assemblers+ (make-hash-table :test 'eq)))

(defun install-assembler (mnemonic name)
  (puthash3 mnemonic +assemblers+ name))

(defmacro define-assembler (mnemonic &body body)
  (let* ((name (intern (format nil "ASSEMBLE-~A" mnemonic)))
         (args '(operand1 operand2)))
    `(progn
       (defun ,name ,args
         (declare (ignorable ,@args))
         ,@body)
       (install-assembler ,mnemonic ',name))))

(defun assemble-instruction (instruction)
  (let* ((mnemonic (first instruction))
         (operand1 (second instruction))
         (operand2 (third instruction))
         (assembler (gethash2-1 mnemonic +assemblers+)))
    (unless assembler
      (error "no assembler for ~S" mnemonic))
    (funcall assembler operand1 operand2)))

(defun assemble (instruction-list)
  (when (atom (first instruction-list))
    (setq instruction-list (list instruction-list)))
  (let ((*output* (or ;;*output*
                      (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0))))
    (dolist (instruction instruction-list)
      (assemble-instruction instruction))
;;     (let ((*print-base* 16))
;;       (format t "~S~%" *output*))
    *output*))

;; (defmacro asm (&rest instructions)
;;   (list 'QUOTE instructions))

#+x86
(load-system-file "x/asm-x86")
#+x86-64
(load-system-file "x/asm-x86-64")

(provide "ASSEMBLER")
