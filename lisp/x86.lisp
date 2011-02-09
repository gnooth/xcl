;;; x86.lisp
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
  (unless (find-package "X86")
    (make-package "X86"
                  :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM"))))

(in-package "X86")

(export '(reg8-p reg32-p
          register
          byte-register register-number register-bit-size
          +call-return-register+
          $ax
          reg8
          make-modrm-byte))

(defun reg8-p (x)
  (and (keywordp x)
       (memq x '(:al :cl :dl :bl))))

(defun reg32-p (x)
  (and (keywordp x)
       (memq x '(:eax :ecx :edx :ebx :esp :ebp :esi :edi))))

(defun register (n)
  (case n
    (0 :eax)
    (1 :ecx)
    (2 :edx)
    (3 :ebx)
    (4 :esp)
    (5 :ebp)
    (6 :esi)
    (7 :edi)))

(defun byte-register (n)
  (aver (<= 0 n 3))
  (case n
    (0 :al)
    (1 :cl)
    (2 :dl)
    (3 :bl)))

(defun register-number (reg)
  (cond ((and (fixnump reg) (<= 0 reg 7))
         reg)
        (t
         (case reg
           (:eax 0)
           (:ecx 1)
           (:edx 2)
           (:ebx 3)
           (:esp 4)
           (:ebp 5)
           (:esi 6)
           (:edi 7)
           (:al  0)
           (:cl  1)
           (:dl  2)
           (:bl  3)
           ))))

(defun register-bit-size (thing)
  (case thing
    ((:eax :ecx :edx :ebx :esp :ebp :esi :edi)
     32)
    (t
     nil)))

(defconstant +call-return-register+ :eax)

(defconstant $ax :eax)

(defconstant $dx :edx)

(defun reg8 (reg)
  (ecase reg
    (:eax :al)
    (:ecx :cl)
    (:edx :dl)
    (:ebx :bl)))

(defknown make-modrm-byte (t t t) t)
(defun make-modrm-byte (mod reg rm)
  (let ((result 0))
    (setf (ldb (byte 3 0) result) rm)
    (setf (ldb (byte 3 3) result) reg)
    (setf (ldb (byte 2 6) result) mod)
    result))

(provide "X86")
