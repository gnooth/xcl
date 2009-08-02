;;; check-type.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves <peter@armedbear.org>
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

;;; Adapted from CMUCL.

(in-package "SYSTEM")

(defmacro check-type (place type &optional type-string)
  (let ((place-value (gensym)))
    `(loop
       (let ((,place-value ,place))
         (when (typep ,place-value ',type)
           (return nil))
         (setf ,place
               (check-type-error ',place ,place-value ',type ,type-string))))))

(defun check-type-error (place place-value type type-string)
  (let ((cond (if type-string
                  (make-condition 'type-error
                                  :datum place-value :expected-type type
                                  :format-control
                                  "The value of ~S is ~S, which is not ~A."
                                  :format-arguments
                                  (list place place-value type-string))
		  (make-condition 'type-error
				  :datum place-value :expected-type type
				  :format-control
                                  "The value of ~S is ~S, which is not of type ~S."
				  :format-arguments
				  (list place place-value type)))))
    (restart-case (error cond)
      (store-value (value)
        :report (lambda (stream) (store-value-report stream place))
        :interactive store-value-interactive
        value))))
