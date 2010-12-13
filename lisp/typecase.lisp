;;; typecase.lisp
;;;
;;; Copyright (C) 2006-10 Peter Graves <gnooth@gmail.com>
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

(defmacro typecase (keyform &rest clauses)
  (let ((keyvar (gensym))
        (cond-body ())
        otherwise-seen-p)
    (dolist (clause clauses)
      (when (atom clause)
        (error "Invalid clause in TYPECASE."))
      (when otherwise-seen-p
        (error "OTHERWISE must be final clause in TYPECASE." ))
      (let ((typespec (car clause))
            (consequents (cdr clause)))
        (cond ((eq typespec 'otherwise)
               (setq otherwise-seen-p typespec
                     typespec t))
              ((eq typespec t)
               (setq typespec t))
              (t
               (setq typespec `(typep ,keyvar ',typespec))))
        (push `(,typespec ,@(or consequents '(nil))) cond-body)))
    `(let ((,keyvar ,keyform))
       (declare (ignorable ,keyvar))
       (cond ,@(nreverse cond-body)))))
