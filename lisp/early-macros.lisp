;;; early-macros.lisp
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

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (sys:%in-package "SYSTEM"))

(defmacro in-package (name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%in-package ,(string name))))

(defmacro prog1 (first-form &rest forms)
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

(defmacro prog2 (first-form second-form &rest forms)
  `(prog1 (progn ,first-form ,second-form) ,@forms))

(defmacro when (test-form &rest body)
  (if (cdr body)
      `(if ,test-form (progn ,@body))
      `(if ,test-form ,(car body))))

(defmacro unless (test-form &rest body)
  (if (cdr body)
      `(if (not ,test-form) (progn ,@body))
      `(if (not ,test-form) ,(car body))))

(defmacro return (&optional result)
  `(return-from nil ,result))

(defmacro multiple-value-setq (vars value-form)
  (unless (and (listp vars) (every #'symbolp vars))
    (error "~S is not a list of symbols." vars))
  (if vars
      `(values (setf (values ,@vars) ,value-form))
      `(values ,value-form)))

(defmacro lambda (lambda-list &rest body)
  (list 'FUNCTION (list* 'LAMBDA lambda-list body)))

(defmacro nth-value (n form)
  `(nth ,n (multiple-value-list ,form)))
