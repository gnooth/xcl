;;; coerce.lisp
;;;
;;; Copyright (C) 2004-2009 Peter Graves <peter@armedbear.org>
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

(defun coerce-list-to-vector (list)
  (let* ((length (length list))
         (result (make-simple-vector length)))
    (dotimes (i length)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (aref result i) (%car list))
      (setq list (%cdr list)))
    result))

(defun coerce-vector-to-list (vector)
  (declare (type vector vector))
  (let* ((len (length vector))
         (i len)
         (result nil))
    (declare (optimize speed (safety 0)))
    (declare (type index i))
    (loop
      (when (zerop i)
        (return-from coerce-vector-to-list result))
      (decf i)
      (push (aref vector i) result))))

(defun coerce-error (object result-type)
  (error 'type-error
         :format-control "~S cannot be converted to type ~S."
         :format-arguments (list object result-type)
         :datum object
         :expected-type result-type))

(defun coerce (object result-type)
  (cond ((typep object result-type)
         object)
        ((and (listp object)
              (memq result-type '(VECTOR SIMPLE-VECTOR)))
         (coerce-list-to-vector object))
        ((and (vectorp object)
              (eq result-type 'LIST))
         (coerce-vector-to-list object))
        ((and (stringp object) ; a string, but not a simple-string
              (eq result-type 'simple-string))
         (copy-string object))
        ((eq result-type 'character)
         (cond ((and (stringp object)
                     (length-eql object 1))
                (char object 0))
               ((and (symbolp object)
                     (length-eql (symbol-name object) 1))
                (char (symbol-name object) 0))
               (t
                (coerce-error object result-type))))
        ((memq result-type '(float single-float short-float))
         (when (realp object)
           (float object 1.0s0)))
        ((memq result-type '(double-float long-float))
         (when (realp object)
           (float object 1.0d0)))
        ((eq result-type 'complex)
         (cond ((floatp object)
                (complex object 0.0))
               ((numberp object)
                object)
               (t
                (coerce-error object result-type))))
        ((eq result-type 'function)
         (coerce-to-function object))
        ((and (consp result-type)
              (eq (%car result-type) 'complex))
         (if (memq (cadr result-type)
                   '(float single-float double-float short-float long-float))
             (complex object 0.0)
             object))
        ((and (typep object 'sequence)
              (subtypep result-type 'sequence))
         (concatenate result-type object))
        (t
         (let ((expanded-type (expand-deftype result-type)))
           (unless (eq expanded-type result-type)
             (return-from coerce (coerce object expanded-type))))
         (coerce-error object result-type))))
