;;; asinh.lisp
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

(in-package "SYSTEM")

;;; Compute asinh z = log(z + sqrt(1 + z*z)).
;;;
;;; Z may be any number, but the result is always a complex.
(defun complex-asinh (z)
  (declare (type (or rational complex) z))
  ;; asinh z = -i * asin (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
         (result (complex-asin iz)))
    (complex (imagpart result)
             (- (realpart result)))))

(defun asinh (number)
  (cond ((realp number)
         (real-asinh number))
        ((complexp number)
         (complex-asinh number))
        (t
         (error 'type-error :datum number :expected-type 'number))))
