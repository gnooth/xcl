;;; x86-64.lisp
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

(in-package "SYSTEM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "X86-64")
    (make-package "X86-64"
                  :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM"))))

(in-package "X86-64")

(export '(rex.w rex.r rex.x rex.b
          register register-reg register-rm
          byte-register register-number register-bit-size
          +call-argument-registers+
          +call-return-register+
          +extended-registers+
          $ax $cx $dx $bx $sp $bp $si $di
          reg8-p reg16-p reg32-p reg64-p
          reg8 reg32 reg64
          extended-register-p
          make-modrm-byte
          make-sib-byte))

;; "0 = default operand size   1 = 64-bit operand size"
(defconstant rex.w 8)

;; "1-bit (high) extension of the ModRM reg field, thus permitting access to 16 registers."
(defconstant rex.r 4)

;; "1-bit (high) extension of the SIB index field, thus permitting access to 16 registers."
(defconstant rex.x 2)

;; "1-bit (high) extension of the ModRM r/m field, SIB base field1, or opcode reg field,
;; thus permitting access to 16 registers."
(defconstant rex.b 1)

(defun register (n &optional prefix-byte)
  (declare (type (integer 0 7) n))
  (cond ((and prefix-byte (not (zerop (logand prefix-byte rex.b))))
         (case n
           (0 :r8)
           (1 :r9)
           (2 :r10)
           (3 :r11)
           (4 :r12)
           (5 :r13)
           (6 :r14)
           (7 :r15)))
        ((and prefix-byte (not (zerop (logand prefix-byte rex.w))))
         (case n
           (0 :rax)
           (1 :rcx)
           (2 :rdx)
           (3 :rbx)
           (4 :rsp)
           (5 :rbp)
           (6 :rsi)
           (7 :rdi)))
        (t
         (case n
           (0 :eax)
           (1 :ecx)
           (2 :edx)
           (3 :ebx)
           (4 :esp)
           (5 :ebp)
           (6 :esi)
           (7 :edi)))))

(defun register-reg (n &optional prefix-byte)
  (cond ((and prefix-byte (not (zerop (logand prefix-byte rex.r))))
         (case n
           (0 :r8)
           (1 :r9)
           (2 :r10)
           (3 :r11)
           (4 :r12)
           (5 :r13)
           (6 :r14)
           (7 :r15)))
        ((and prefix-byte (not (zerop (logand prefix-byte rex.w))))
         (case n
           (0 :rax)
           (1 :rcx)
           (2 :rdx)
           (3 :rbx)
           (4 :rsp)
           (5 :rbp)
           (6 :rsi)
           (7 :rdi)))
        (t
         (case n
           (0 :eax)
           (1 :ecx)
           (2 :edx)
           (3 :ebx)
           (4 :esp)
           (5 :ebp)
           (6 :esi)
           (7 :edi)))))

(defun register-rm (n &optional prefix-byte)
  (cond ((and prefix-byte (not (zerop (logand prefix-byte rex.b))))
         (case n
           (0 :r8)
           (1 :r9)
           (2 :r10)
           (3 :r11)
           (4 :r12)
           (5 :r13)
           (6 :r14)
           (7 :r15)))
        ((and prefix-byte (not (zerop (logand prefix-byte rex.w))))
         (case n
           (0 :rax)
           (1 :rcx)
           (2 :rdx)
           (3 :rbx)
           (4 :rsp)
           (5 :rbp)
           (6 :rsi)
           (7 :rdi)))
        (t
         (case n
           (0 :eax)
           (1 :ecx)
           (2 :edx)
           (3 :ebx)
           (4 :esp)
           (5 :ebp)
           (6 :esi)
           (7 :edi)))))

(defknown byte-register (t) t)
(defun byte-register (n)
  (declare (type (integer 0 7) n))
  (case n
    (0 :al)
    (1 :cl)
    (2 :dl)
    (3 :bl)
    (4 :spl)
    (5 :bpl)
    (6 :sil)
    (7 :dil)))

(defknown register-number (t) t)
(defun register-number (reg)
  (cond ((and (fixnump reg) (<= 0 reg 7))
         reg)
        (t
         (case reg
           (:rax 0)
           (:rcx 1)
           (:rdx 2)
           (:rbx 3)
           (:rsp 4)
           (:rbp 5)
           (:rsi 6)
           (:rdi 7)
           (:r8  0)
           (:r9  1)
           (:r10 2)
           (:r11 3)
           (:r12 4)
           (:r13 5)
           (:r14 6)
           (:r15 7)
           (:eax 0)
           (:ecx 1)
           (:edx 2)
           (:ebx 3)
           (:esp 4)
           (:ebp 5)
           (:esi 6)
           (:edi 7)
           (:ax 0)
           (:cx 1)
           (:dx 2)
           (:bx 3)
           (:sp 4)
           (:bp 5)
           (:si 6)
           (:di 7)
           (:al  0)
           (:cl  1)
           (:dl  2)
           (:bl  3)
           (:spl 4)
           (:bpl 5)
           (:sil 6)
           (:dil 7)))))

(defun register-bit-size (thing)
  (case thing
    ((:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
     64)
    ((:eax :ecx :edx :ebx :esp :ebp :esi :edi)
     32)
    (t
     nil)))

(defconstant +call-argument-registers+ '(:rdi :rsi :rdx :rcx :r8 :r9))

(defconstant +call-return-register+ :rax)

(defconstant +extended-registers+ '(:r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))

(defun reg8-p (x)
  (and (keywordp x)
       (memq x '(:al :cl :dl :bl :spl :bpl :sil :dil))))

(defun reg16-p (x)
  (and (keywordp x)
       (memq x '(:ax :cx :dx :bx :sp :bp :si :di))))

(defun reg32-p (x)
  (and (keywordp x)
       (memq x '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))))

(defun reg64-p (x)
  (and (keywordp x)
       (memq x '(:rax :rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15))))

(defun reg8 (reg)
  (ecase reg
    (:rax :al)
    (:rcx :cl)
    (:rdx :dl)
    (:rbx :bl)
    (:rsp :spl)
    (:rbp :bpl)
    (:rsi :sil)
    (:rdi :dil)
    (:eax :al)
    (:ecx :cl)
    (:edx :dl)
    (:ebx :bl)
    (:esp :spl)
    (:ebp :bpl)
    (:esi :sil)
    (:edi :dil)))

(defun reg32 (reg)
  (ecase reg
    (:rax :eax)
    (:rcx :ecx)
    (:rdx :edx)
    (:rbx :ebx)
    (:rsp :esp)
    (:rbp :ebp)
    (:rsi :esi)
    (:rdi :edi)))

(defun reg64 (reg)
  (ecase reg
    (:eax :rax)
    (:ecx :rcx)
    (:edx :rdx)
    (:ebx :rbx)
    (:esp :rsp)
    (:ebp :rbp)
    (:esi :rsi)
    (:edi :rdi)))


(defun extended-register-p (thing)
  (memq thing +extended-registers+))

(defknown make-modrm-byte (t t t) t)
(defun make-modrm-byte (mod reg rm)
  (declare (type (unsigned-byte 8) mod reg rm))
  (let ((result 0))
    (setf (ldb (byte 3 0) result) rm)
    (setf (ldb (byte 3 3) result) reg)
    (setf (ldb (byte 2 6) result) mod)
    result))

(defknown make-sib-byte (t t t) t)
(defun make-sib-byte (scale index base)
  (declare (type (unsigned-byte 8) scale index base))
  (let ((result 0))
    (setf (ldb (byte 3 0) result) base)
    (setf (ldb (byte 3 3) result) index)
    (setf (ldb (byte 2 6) result) scale)
    result))

(defconstant $ax :rax)
(defconstant $cx :rcx)
(defconstant $dx :rdx)
(defconstant $bx :rbx)
(defconstant $sp :rsp)
(defconstant $bp :rbp)
(defconstant $si :rsi)
(defconstant $di :rdi)

(provide "X86-64")
