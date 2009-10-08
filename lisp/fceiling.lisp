;;; fceiling.lisp
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

;;; Adapted from OpenMCL.

(in-package "SYSTEM")

(defun fceiling (number &optional (divisor 1))
  "Same as CEILING, but returns first value as a float."
  (multiple-value-bind (q r) (ceiling number divisor)
    (values (float q (if (floatp r) r 0.0)) r)))
