;;; define-source-transform.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves <peter@armedbear.org>
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

;;; Adapted from ABCL.

(in-package "SYSTEM")

(export '(source-transform define-source-transform expand-source-transform))

(defvar *source-transforms* (make-hash-table :test #'equal))

(defun source-transform (name)
  (gethash2-1 name *source-transforms*))

(defun set-source-transform (name transform)
  (puthash name *source-transforms* transform))

(assign-setf-inverse 'source-transform 'set-source-transform)

(defmacro define-source-transform (name lambda-list &rest body)
  (let* ((form (gensym))
         (env (gensym))
         (body (parse-defmacro lambda-list form body name 'defmacro
                               :environment env))
         (expander
          (if (symbolp name)
              `(lambda (,form) (block ,name ,body))
              `(lambda (,form) (block ,(cadr name) ,body)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (source-transform ',name) ,expander)
       ',name)))

(defun expand-source-transform-1 (form)
  (let ((expander nil)
        (new-form nil))
    (cond ((atom form)
           (values form nil))
          ((and (consp (%car form))
                (eq (caar form) 'SETF)
                (setq expander (source-transform (%car form))))
           (values (setq new-form (funcall expander form))
                   (neq new-form form)))
          ((and (symbolp (%car form))
                (setq expander (source-transform (%car form))))
           (values (setq new-form (funcall expander form))
                   (neq new-form form)))
          (t
           (values form nil)))))

(defun expand-source-transform (form)
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expansion exp-p) (expand-source-transform-1 form)
        (if exp-p
            (setq form expansion
                  expanded-p t)
            (return))))
    (values form expanded-p)))
