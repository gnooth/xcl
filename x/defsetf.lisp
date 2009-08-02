;;; defsetf.lisp
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

(defun %defsetf (orig-access-form num-store-vars expander)
  (let (subforms
        subform-vars
        subform-exprs
        store-vars)
    (dolist (subform (cdr orig-access-form))
      (if (constantp subform)
          (push subform subforms)
          (let ((var (gensym)))
            (push var     subforms)
            (push var     subform-vars)
            (push subform subform-exprs))))
    (dotimes (i num-store-vars)
      (push (gensym) store-vars))
    (setq subforms      (nreverse subforms)
          subform-vars  (nreverse subform-vars)
          subform-exprs (nreverse subform-exprs)
          store-vars    (nreverse store-vars))
    (values subform-vars
            subform-exprs
            store-vars
            (funcall expander (cons subforms store-vars))
            `(,(car orig-access-form) ,@subforms))))

(defun assign-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put name 'setf-inverse inverse))
  (when expander
    (put name 'setf-expander expander))
  name)

(defmacro defsetf (access-fn &rest rest)
  (cond ((not (listp (car rest)))
	 `(eval-when (:load-toplevel :compile-toplevel :execute)
	    (assign-setf-macro ',access-fn
                                nil
                                ',(car rest)
				,(when (and (car rest) (stringp (cadr rest)))
				   `',(cadr rest)))))
	((and (cdr rest) (listp (cadr rest)))
	 (destructuring-bind (lambda-list (&rest store-variables) &body body)
             rest
           (let ((arglist-var     (gensym "ARGS-"))
                 (access-form-var (gensym "ACCESS-FORM-"))
                 (env-var         (gensym "ENVIRONMENT-")))
             (multiple-value-bind
                 (body doc)
                 (parse-defmacro `(,lambda-list ,@store-variables)
                                 arglist-var body access-fn 'defsetf
                                 :anonymousp t)
               `(eval-when (:load-toplevel :compile-toplevel :execute)
                  (assign-setf-macro
                   ',access-fn
                   #'(lambda (,access-form-var ,env-var)
                      (declare (ignore ,env-var))
                      (%defsetf ,access-form-var ,(length store-variables)
                                #'(lambda (,arglist-var)
                                   (block ,access-fn
                                     ,body))))
                   nil
                   ',doc))))))
	(t
	 (error "Ill-formed DEFSETF for ~S." access-fn))))
