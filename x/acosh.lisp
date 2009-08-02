;;; acosh.lisp
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

;;; Compute acosh z = 2 * log(sqrt((z+1)/2) + sqrt((z-1)/2))
;;;
;;; Z may be any NUMBER, but the result is always a COMPLEX.
(defun complex-acosh (z)
  (declare (type (or rational complex) z))
  (let ((sqrt-z-1 (sqrt (- z 1)))
        (sqrt-z+1 (sqrt (+ z 1))))
      (complex (asinh (realpart (* (conjugate sqrt-z-1)
                                   sqrt-z+1)))
               (* 2 (atan (imagpart sqrt-z-1)
                          (realpart sqrt-z+1))))))

;; (defun acosh (number)
;;   #!+sb-doc
;;   "Return the hyperbolic arc cosine of NUMBER."
;;   (number-dispatch ((number number))
;;     ((rational)
;;      ;; acosh is complex if number < 1
;;      (if (< number 1)
;;          (complex-acosh number)
;;          (coerce (%acosh (coerce number 'double-float)) 'single-float)))
;;     (((foreach single-float double-float))
;;      (if (< number (coerce 1 '(dispatch-type number)))
;;          (complex-acosh (complex number))
;;          (coerce (%acosh (coerce number 'double-float))
;;                  '(dispatch-type number))))
;;     ((complex)
;;      (complex-acosh number))))

(defun acosh (number)
  ;; acosh is complex if number < 1
  (if (or (complexp number)
          (< number 1))
      (complex-acosh number)
      (real-acosh number)))
