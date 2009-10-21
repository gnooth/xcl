;;; define-method-combination.lisp
;;;
;;; Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

(defstruct method-combination
  name
  operator
  identity-with-one-argument
  documentation)

(defun expand-short-defcombin (whole)
  (let* ((name (cadr whole))
         (documentation
          (getf (cddr whole) :documentation ""))
         (identity-with-one-arg
          (getf (cddr whole) :identity-with-one-argument nil))
         (operator
          (getf (cddr whole) :operator name)))
    `(progn
       (setf (get ',name 'method-combination-object)
             (make-method-combination :name ',name
                                      :operator ',operator
                                      :identity-with-one-argument ',identity-with-one-arg
                                      :documentation ',documentation))
       ',name)))

(defun expand-long-defcombin (whole)
  (declare (ignore whole))
  (error "The long form of DEFINE-METHOD-COMBINATION is not implemented."))

(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  (if (and (cddr form)
           (listp (caddr form)))
      (expand-long-defcombin form)
      (expand-short-defcombin form)))
