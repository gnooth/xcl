;;; asm-x86.lisp
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

(in-package "ASSEMBLER")

(defun emit-raw (x)
  (let ((bytes (list (ldb (byte 8  0) x)
                     (ldb (byte 8  8) x)
                     (ldb (byte 8 16) x)
                     (ldb (byte 8 24) x))))
    (dolist (byte bytes)
      (vector-push-extend byte *output*))))

(define-assembler :add
  (cond ((and (reg32-p operand1)
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg (register-number operand1))
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x01 modrm-byte)))
        ((and (typep operand1 '(signed-byte 8))
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg 0)
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x83 modrm-byte (ldb (byte 8 0) operand1))))
        ((and (typep operand1 '(signed-byte 32))
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg 0)
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x81 modrm-byte)
           (emit-raw-dword operand1)))
        (t
         (unsupported))))

(define-assembler :and
  (cond ((and (typep operand1 '(unsigned-byte 8))
              (eq operand2 :al))
         (emit-bytes #x24 operand1))
        ((and (typep operand1 '(unsigned-byte 32))
              (eq operand2 :eax))
         (emit-byte #x25)
         (emit-raw operand1))
        (t
         (unsupported))))

(define-assembler :cmp
  (cond ((and (reg32-p operand1)
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg (register-number operand1))
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x39 modrm-byte)))
        ((and (typep operand1 '(unsigned-byte 8))
              (eq operand2 :al))
         (emit-bytes #x3c operand1))
        ((and (typep operand1 '(signed-byte 8))
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg 7)
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x83 modrm-byte (ldb (byte 8 0) operand1))))
        ((and (typep operand1 '(signed-byte 32))
              (eq operand2 :eax))
         (emit-bytes #x3d)
         (emit-raw operand1))
        (t
         (unsupported))))

(define-assembler :leave
  (emit-byte #xc9))

(define-assembler :mov
  (cond ((and (reg32-p operand1)
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg (register-number operand1))
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x89 modrm-byte)))
        ((and (reg32-p operand2)
              (integerp operand1)
              (typep operand1 '(unsigned-byte 32)))
         (emit-byte (+ #xb8 (register-number operand2)))
         (emit-raw operand1))
        ((and (consp operand1)
              (length-eql operand1 1)
              (reg32-p (%car operand1))
              (reg32-p operand2))
         (let* ((reg1 (%car operand1))
                (reg2 operand2)
                (modrm-byte (make-modrm-byte #xb00
                                             (register-number reg2)
                                             (register-number reg1))))
           (emit-bytes #x8b modrm-byte)))
        ((and (consp operand1)
              (length-eql operand1 2)
              (integerp (first operand1))
              (reg32-p (second operand1))
              (reg32-p operand2))
         (let ((displacement (first operand1))
               (reg1 (second operand1))
               (reg2 operand2))
           (cond ((typep displacement '(signed-byte 8))
                  (let* ((displacement-byte (ldb (byte 8 0) displacement))
                         (mod #b01)
                         (reg (register-number reg2))
                         (rm  (register-number reg1))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                    (emit-bytes #x8b modrm-byte displacement-byte)))
                 (t
                  (let* ((mod #b10)
                         (reg (register-number reg2))
                         (rm  (register-number reg1))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                    (emit-bytes #x8b modrm-byte)
                    (emit-raw displacement))))))
        ((and (consp operand2)
              (length-eql operand2 2)
              (integerp (first operand2))
              (eq (second operand2) :esp)
              (reg32-p operand1))
         (let ((reg1 operand1)
               (displacement (first operand2))
               (reg2 (second operand2)))
           (cond ((zerop displacement)
                  (let* ((mod #xb00)
                         (reg (register-number reg1))
                         (rm  (register-number reg2))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                  (emit-bytes #x89 modrm-byte #x24)))
                 ((typep displacement '(signed-byte 8))
                  (let* ((mod #xb01)
                         (reg (register-number reg1))
                         (rm  (register-number reg2))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                  (emit-bytes #x89 modrm-byte #x24 (ldb (byte 8 0) displacement))))
                 (t
                  (let* ((mod #b10)
                         (reg (register-number reg1))
                         (rm  (register-number reg2))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                    (emit-bytes #x89 modrm-byte #x24)
                    (emit-raw displacement))))))
        ((and (consp operand2)
              (length-eql operand2 2)
              (integerp (first operand2))
              (reg32-p (second operand2))
              (reg32-p operand1))
         (let ((reg1 operand1)
               (displacement (first operand2))
               (reg2 (second operand2)))
           (cond ((typep displacement '(signed-byte 8))
                  ;; displacement fits in a byte
                  (let* ((displacement-byte (ldb (byte 8 0) displacement))
                         (mod #b01)
                         (reg (register-number reg1))
                         (rm  (register-number reg2))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                    (emit-bytes #x89 modrm-byte displacement-byte)))
                 (t
                  (let* ((mod #b10)
                         (reg (register-number reg1))
                         (rm  (register-number reg2))
                         (modrm-byte (make-modrm-byte mod reg rm)))
                    (emit-bytes #x89 modrm-byte)
                    (emit-raw displacement))))))
        (t
         (unsupported))))

(define-assembler :movb
  (cond ((and (typep operand1 '(unsigned-byte 8))
              (consp operand2)
              (length-eql operand2 2)
              (integerp (first operand2))
              (reg32-p (second operand2)))
         (let* ((mod #b01)
                (reg 0)
                (rm (register-number (second operand2)))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #xc6 modrm-byte (first operand2) operand1)))
        (t
         (unsupported))))

(define-assembler :pop
  (cond ((reg32-p operand1)
         (emit-byte (ecase operand1
                      (:eax #x58)
                      (:ecx #x59)
                      (:edx #x5a)
                      (:ebx #x5b)
                      (:esp #x5c)
                      (:ebp #x5d)
                      (:esi #x5e)
                      (:edi #x5f))))
        ((and (consp operand1)
              (length-eql operand1 2)
              (typep (%car operand1) '(signed-byte 8))
              (reg32-p (%cadr operand1)))
         (let* ((mod #xb01)
                (reg 0)
                (rm (register-number (%cadr operand1)))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x8f modrm-byte (ldb (byte 8 0) (%car operand1)))))
        ((and (consp operand1)
              (length-eql operand1 1)
              (reg32-p (%car operand1)))
         (let* ((mod #xb00)
                (reg 0)
                (rm (register-number (%car operand1)))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x8f modrm-byte)))))

(define-assembler :push
  (cond ((reg32-p operand1)
;;          (emit-byte (ecase operand1
;;                       (:eax #x50)
;;                       (:ecx #x51)
;;                       (:edx #x52)
;;                       (:ebx #x53)
;;                       (:esp #x54)
;;                       (:ebp #x55)
;;                       (:esi #x56)
;;                       (:edi #x57)))
         (emit-byte (+ (register-number operand1) #x50))
         )
        ((typep operand1 '(signed-byte 8))
         (emit-bytes #x6a (ldb (byte 8 0) operand1)))
        ((typep operand1 '(signed-byte 32))
         (emit-byte #x68)
         (emit-raw operand1))
        ((and (consp operand1)
              (typep (car operand1) '(signed-byte 8))
              (reg32-p (cadr operand1)))
         (let* ((mod #b01)
                (reg 6)
                (rm (register-number (cadr operand1)))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #xff modrm-byte (ldb (byte 8 0) (car operand1)))))
        ((and (consp operand1)
              (typep (car operand1) '(signed-byte 32))
              (reg32-p (cadr operand1)))
         (let* ((mod #b10)
                (reg 6)
                (rm (register-number (cadr operand1)))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #xff modrm-byte)
           (emit-raw (car operand1))))
        (t
         (unsupported))))

(define-assembler :sar
  (cond ((and (typep operand1 '(unsigned-byte 8))
              (reg32-p operand2))
         (let ((modrm-byte (make-modrm-byte #b11 7 (register-number operand2))))
           (emit-bytes #xc1 modrm-byte operand1)))
        (t
         (unsupported))))

(define-assembler :shl
  (cond ((and (typep operand1 '(unsigned-byte 8))
              (reg32-p operand2))
         (let ((modrm-byte (make-modrm-byte #b11 4 (register-number operand2))))
           (emit-bytes #xc1 modrm-byte operand1)))
        (t
         (unsupported))))

(define-assembler :shr
  (cond ((and (typep operand1 '(unsigned-byte 8))
              (reg32-p operand2))
         (let ((modrm-byte (make-modrm-byte #b11 5 (register-number operand2))))
           (emit-bytes #xc1 modrm-byte operand1)))
        (t
         (unsupported))))

(define-assembler :sub
  (cond ((and (integerp operand1)
              (reg32-p operand2))
         (cond ((typep operand1 '(signed-byte 8))
                (let ((modrm-byte (make-modrm-byte #b11 5 (register-number operand2))))
                  (emit-bytes #x83 modrm-byte (ldb (byte 8 0) operand1))))
               ((typep operand1 '(signed-byte 32))
                (let ((modrm-byte (make-modrm-byte #b11 5 (register-number operand2))))
                  (emit-bytes #x81 modrm-byte)
                  (emit-raw-dword operand1)))
               (t
                (unsupported))))))

(define-assembler :ret
  (emit-byte #xc3))

(define-assembler :test
  (cond ((and (reg32-p operand1)
              (reg32-p operand2))
         (let* ((mod #b11)
                (reg (register-number operand1))
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x85 modrm-byte)))
        ((and (reg8-p operand1)
              (reg8-p operand2))
         (let* ((mod #b11)
                (reg (register-number operand1))
                (rm  (register-number operand2))
                (modrm-byte (make-modrm-byte mod reg rm)))
           (emit-bytes #x84 modrm-byte)))
        ((and (typep operand1 '(unsigned-byte 8))
              (eq operand2 :al))
         (emit-bytes #xa8 operand1))
        ((and (typep operand1 '(unsigned-byte 8))
              (memq operand2 '(:bl :cl :dl)))
         (let ((modrm-byte (make-modrm-byte #b11 0 (register-number operand2))))
           (emit-bytes #xf6 modrm-byte operand1)))
        (t
         (unsupported))))

(define-assembler :xor
  (cond ((and (reg32-p operand1)
              (reg32-p operand2))
         (let ((modrm-byte (make-modrm-byte #b11
                                            (register-number operand1)
                                            (register-number operand2))))
           (emit-bytes #x31 modrm-byte)))
        (t
         (unsupported))))
