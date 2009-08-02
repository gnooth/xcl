;;; make-sequence-of-type.lisp
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

(defmacro type-specifier-atom (type)
  `(if (atom ,type) ,type (%car ,type)))

(defun make-sequence-of-type (type length)
  (case (type-specifier-atom type)
    (list
     (make-list length))
    ((bit-vector simple-bit-vector)
     (make-array length :element-type 'bit))
    ((simple-base-string simple-string string)
     (make-string length))
    (simple-vector
     (make-simple-vector length))
    (vector
     (if (cadr type)
         (make-array length :element-type (%cadr type))
         (make-array length)))
    (nil-vector
     (make-array length :element-type nil))
    (simple-array
     (if (cadr type)
         (make-array length :element-type (%cadr type))
         (make-array length)))
    (t
     (error "MAKE-SEQUENCE-OF-TYPE: unsupported case ~S" type))))
