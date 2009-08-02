;;; acos.lisp
;;;
;;; Copyright (C) 2007-2009 Peter Graves <peter@armedbear.org>
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

;;; Z may be any NUMBER, but the result is always a COMPLEX.
(defun complex-acos (z)
  (let ((sqrt-1+z (sqrt (+ 1 z)))
        (sqrt-1-z (sqrt (- 1 z))))
    (complex (* 2 (atan (realpart sqrt-1-z)
                        (realpart sqrt-1+z)))
             (asinh (imagpart (* (conjugate sqrt-1+z)
                                 sqrt-1-z))))))

(defun acos (number)
  (if (or (complexp number)
          (> number 1)
          (< number -1))
      (complex-acos number)
      (real-acos number)))
