;;; asm.lisp
;;;
;;; Copyright (C) 2007-2011 Peter Graves <gnooth@gmail.com>
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

(defknown make-modrm-byte (t t t) t)
(defun make-modrm-byte (mod reg rm)
  (declare (type (unsigned-byte 2) mod)
           (type (unsigned-byte 3) reg rm))
  (let ((result 0))
    (setf (ldb (byte 3 0) result) rm)
    (setf (ldb (byte 3 3) result) reg)
    (setf (ldb (byte 2 6) result) mod)
    result))

(defknown make-sib-byte (t t t) t)
(defun make-sib-byte (scale index base)
  (declare (type (unsigned-byte 2) scale)
           (type (unsigned-byte 3) index base))
  (let ((result 0))
    (setf (ldb (byte 3 0) result) base)
    (setf (ldb (byte 3 3) result) index)
    (setf (ldb (byte 2 6) result) scale)
    result))

(declaim (inline address-operand-p))
(defun address-operand-p (operand)
  (consp operand))

(defun parse-address-operand (operand)
  (declare (type cons operand))
  (let ((length (length operand))
        displacement
        base
        index
        (scale 1))
    (ecase length
      (4
       (setq displacement (%car operand))
       (let ((rest (%cdr operand)))
         (setq base (%car rest)
               rest (%cdr rest))
         (setq index (%car rest)
               rest (%cdr rest))
         (setq scale (%car rest))))
      (3
       (let ((thing (%car operand))
             (rest (%cdr operand)))
         (cond ((integerp thing)
                (setq displacement thing
                      base (%car rest)
                      index (%cadr rest)))
               (t
                (setq base thing
                      index (%car rest)
                      scale (%cadr rest))))))
      (2
       (let ((thing (%car operand)))
         (cond ((integerp thing)
                (setq displacement thing
                      base (%cadr operand)))
               (t
                (setq base thing
                      index (%cadr operand))))))
      (1
       (setq base (%car operand))))
    (setq scale (ecase scale
                  (1 0)
                  (2 1)
                  (4 2)
                  (8 3)))
    (values displacement base index scale)))

(defmacro with-address-operand (operand &body body)
  `(multiple-value-bind (displacement base index scale)
       (parse-address-operand ,operand)
     (declare (type scale '(integer 0 3)))
     (declare (ignorable displacement base index scale))
     ,@body))

