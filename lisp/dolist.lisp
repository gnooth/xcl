;;; dolist.lisp
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

(defmacro dolist ((var list-form &optional (result-form nil)) &body body)
  ;; We repeatedly bind the var instead of setting it so that we never
  ;; have to give the var an arbitrary value such as NIL (which might
  ;; conflict with a declaration). If there is a result form, we
  ;; introduce a gratuitous binding of the variable to NIL without the
  ;; declarations, then evaluate the result form in that
  ;; environment. We spuriously reference the gratuitous variable,
  ;; since we don't want to use IGNORABLE on what might be a special
  ;; var.
  (multiple-value-bind (forms declarations)
      (parse-body body nil)
    (let ((list (gensym "LIST-"))
          (top (gensym "TOP-")))
      `(block nil
         (let ((,list ,list-form))
           (tagbody
            ,top
            (unless (endp ,list)
              (let ((,var (%car ,list)))
                ,@declarations
                (setq ,list (%cdr ,list))
                (tagbody ,@forms))
              (go ,top))))
         ,(if (constantp result-form)
              `,result-form
              `(let ((,var nil))
                 ;; Filter out TYPE declarations (VAR gets bound to NIL,
                 ;; and might have a conflicting type declaration) and
                 ;; IGNORE (VAR might be ignored in the loop body, but
                 ;; it's used in the result form).
                 ,@(filter-dolist-declarations declarations)
                 ,var
                 ,result-form))))))

;; (defun filter-dolist-declarations (decls)
;;   (mapcar (lambda (decl)
;;             `(declare ,@(remove-if
;;                          (lambda (clause)
;;                            (and (consp clause)
;;                                 (memq (%car clause) '(TYPE IGNORE))))
;;                          (cdr decl))))
;;           decls))

;; filter out (declare (fixnum x)) as well as (declare (type fixnum x))
(defun filter-dolist-declarations (decls)
  (mapcar (lambda (decl)
            `(declare ,@(remove-if-not
                         (lambda (clause)
                           (or (atom clause)
                               ;; REVIEW
                               (memq (%car clause)
                                     '(FTYPE IGNORABLE INLINE NOTINLINE OPTIMIZE SPECIAL))))
                         (cdr decl))))
          decls))
