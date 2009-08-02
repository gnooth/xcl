;;; phase.lisp
;;;
;;; Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

(defun phase (number)
  (etypecase number
    (rational
     (if (minusp number)
         (coerce pi 'single-float)
         0.0f0))
    (single-float
     (if (minusp (float-sign number))
         (coerce pi 'single-float)
         0.0f0))
    (double-float
     (if (minusp (float-sign number))
         (coerce pi 'double-float)
         0.0d0))
    (complex
     (atan (imagpart number) (realpart number)))))
