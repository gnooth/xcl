;;; sbit.lisp
;;;
;;; Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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

(defun sbit (simple-bit-array &rest subscripts)
  (if (length-eql subscripts 1)
      (sbit1 simple-bit-array (%car subscripts))
      (row-major-aref simple-bit-array
                      (apply 'array-row-major-index simple-bit-array subscripts))))

(defun set-sbit (simple-bit-array &rest subscripts-and-new-value)
  (if (length-eql subscripts-and-new-value 2)
      (set-sbit1 simple-bit-array (%car subscripts-and-new-value) (%cadr subscripts-and-new-value))
      (apply 'aset simple-bit-array subscripts-and-new-value)))
