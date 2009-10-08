;;; defknown.lisp
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

(export '(operator-single-valued-p function-result-type function-attributes defknown))

(defun operator-single-valued-p (name)
  (get name 'single-valued))

(defun set-operator-single-valued-p (name single-valued-p)
  (put name 'single-valued single-valued-p))

(assign-setf-inverse 'operator-single-valued-p 'set-operator-single-valued-p)

(defparameter *function-result-types* (make-hash-table :test 'equal))

(defun result-type-single-valued-p (result-type)
  (and result-type
       (neq result-type '*)
       (or (atom result-type)
           (neq (%car result-type) 'values))))

(defun function-result-type (name)
  (if (symbolp name)
      (get name 'function-result-type :unknown)
      (values (gethash name *function-result-types* :unknown))))

(defun set-function-result-type (name result-type)
  (if (symbolp name)
      (put name 'function-result-type result-type)
      (puthash name *function-result-types* result-type))
  (set-operator-single-valued-p name (result-type-single-valued-p result-type)))

(assign-setf-inverse 'function-result-type 'set-function-result-type)

(defparameter *function-attributes* (make-hash-table :test 'equal))

(defun function-attributes (name)
  (if (symbolp name)
      (get name 'function-attributes)
      (values (gethash name *function-attributes*))))

(defun set-function-attributes (name attributes)
  (if (symbolp name)
      (put name 'function-attributes attributes)
      (puthash name *function-attributes* attributes)))

(assign-setf-inverse 'function-attributes 'set-function-attributes)

(defun %defknown (name-or-names argument-types result-type attributes)
  (declare (ignore argument-types))
  (cond ((or (symbolp name-or-names)
             (setf-function-name-p name-or-names))
         (set-function-result-type name-or-names result-type)
         (set-function-attributes name-or-names attributes))
        (t
         (dolist (name name-or-names)
           (set-function-result-type name result-type)
           (set-function-attributes name attributes))))
  name-or-names)

(defmacro defknown (name-or-names argument-types result-type &optional attributes)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%defknown ',name-or-names ',argument-types ',result-type ',attributes)))
