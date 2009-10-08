;;; late-setf.lisp
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

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (require-type access-fn 'symbol)
  (let ((whole       (gensym "WHOLE-"))
	(environment (gensym "ENV-")))
    (multiple-value-bind (body local-decs doc)
        (parse-defmacro lambda-list whole body access-fn
                        'define-setf-expander
                        :environment environment)
      (declare (ignore doc)) ; FIXME
      `(setf (get ',access-fn 'setf-expander)
             #'(lambda (,whole ,environment)
                ,@local-decs
                (block ,access-fn ,body))))))

(define-setf-expander getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemp (gensym))
          (def-temp (if default (gensym))))
      (values `(,@temps ,ptemp ,@(if default `(,def-temp)))
              `(,@values ,prop ,@(if default `(,default)))
              `(,newval)
              `(let ((,(car stores) (putf ,get ,ptemp ,newval)))
                 ,set
                 ,newval)
              `(getf ,get ,ptemp ,@(if default `(,def-temp)))))))

(define-setf-expander values (&rest places &environment env)
  (let ((setters ())
        (getters ())
        (all-dummies ())
        (all-vals ())
        (newvals ()))
    (dolist (place places)
      (multiple-value-bind (dummies vals newval setter getter)
          (get-setf-expansion place env)
        (setf all-dummies (append all-dummies dummies (cdr newval))
              all-vals (append all-vals vals
                               (mapcar (constantly nil) (cdr newval)))
              newvals (append newvals (list (car newval))))
        (push setter setters)
        (push getter getters)))
    (values all-dummies all-vals newvals
            `(values ,@(reverse setters)) `(values ,@(reverse getters)))))

;; From SBCL.
(define-setf-expander ldb (bytespec place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (if (and (consp bytespec) (eq (car bytespec) 'byte))
	(let ((n-size (gensym))
	      (n-pos (gensym))
	      (n-new (gensym)))
	  (values (list* n-size n-pos dummies)
		  (list* (second bytespec) (third bytespec) vals)
		  (list n-new)
		  `(let ((,(car newval) (dpb ,n-new (byte ,n-size ,n-pos)
					     ,getter)))
		     ,setter
		     ,n-new)
		  `(ldb (byte ,n-size ,n-pos) ,getter)))
	(let ((btemp (gensym))
	      (gnuval (gensym)))
	  (values (cons btemp dummies)
		  (cons bytespec vals)
		  (list gnuval)
		  `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
		     ,setter
		     ,gnuval)
		  `(ldb ,btemp ,getter))))))

(defun make-gensym-list (n)
  (let ((list ()))
    (dotimes (i n list)
      (push (gensym) list))))

(define-setf-expander apply (functionoid &rest args)
  (unless (and (listp functionoid)
               (= (length functionoid) 2)
               (eq (first functionoid) 'function)
               (memq (second functionoid) '(aref bit sbit)))
    (error "SETF of APPLY is only defined for #'AREF, #'BIT and #'SBIT."))
  (let ((function (second functionoid))
        (new-var (gensym))
        (vars (make-gensym-list (length args))))
    (values vars args (list new-var)
            `(apply #'(setf ,function) ,new-var ,@vars)
            `(apply #',function ,@vars))))

;; (SETF (APPLY #'AREF ...
(defun (setf aref) (new-value array &rest subscripts)
  (setf (row-major-aref array (apply #'array-row-major-index array subscripts)) new-value))

;; (SETF (APPLY #'BIT ...
(defun (setf bit) (new-value array &rest subscripts)
  (setf (row-major-aref array (apply #'array-row-major-index array subscripts)) new-value))

;; (SETF (APPLY #'SBIT ...
(defun (setf sbit) (new-value array &rest subscripts)
  (setf (row-major-aref array (apply #'array-row-major-index array subscripts)) new-value))

(define-setf-expander the (type place &environment env)
  (multiple-value-bind (temps subforms store-vars setter getter)
      (get-setf-expansion place env)
    (values temps subforms store-vars
            `(multiple-value-bind ,store-vars
                 (the ,type (values ,@store-vars))
               ,setter)
            `(the ,type ,getter))))
