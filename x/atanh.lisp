;;; atanh.lisp
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

;;; atanh z = (log(1+z) - log(1-z))/2
(defun complex-atanh (number)
  (/ (- (log (+ 1 number)) (log (- 1 number))) 2))

;; (defun atanh (number)
;;   #!+sb-doc
;;   "Return the hyperbolic arc tangent of NUMBER."
;;   (number-dispatch ((number number))
;;     ((rational)
;;      ;; atanh is complex if |number| > 1
;;      (if (or (> number 1) (< number -1))
;;          (complex-atanh number)
;;          (coerce (%atanh (coerce number 'double-float)) 'single-float)))
;;     (((foreach single-float double-float))
;;      (if (or (> number (coerce 1 '(dispatch-type number)))
;;              (< number (coerce -1 '(dispatch-type number))))
;;          (complex-atanh (complex number))
;;          (coerce (%atanh (coerce number 'double-float))
;;                  '(dispatch-type number))))
;;     ((complex)
;;      (complex-atanh number))))

(defun atanh (number)
  ;; atanh is complex if |number| > 1
  (if (or (complexp number)
          (> number 1)
          (< number -1))
      (complex-atanh number)
      (real-atanh number)))
