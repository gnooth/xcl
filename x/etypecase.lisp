;;; etypecase.lisp
;;;
;;; Copyright (C) 2007 Peter Graves
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

(defmacro etypecase (keyform &rest clauses)
  (let ((keyvar (gensym))
        (cond-body ())
        t-seen-p)
    (dolist (clause clauses)
      (when (atom clause)
        (error "Invalid clause in ETYPECASE."))
      (let ((typespec (car clause))
            (consequents (cdr clause)))
        (cond ((eq typespec t)
               (setq t-seen-p t))
              (t
               (setq typespec `(typep ,keyvar ',typespec))))
        (push `(,typespec ,@(or consequents '(nil))) cond-body)))
    (unless t-seen-p
      (push `(t (error 'type-error
                       :datum ,keyvar :expected-type '(or ,@(mapcar #'car clauses))))
            cond-body))
    `(let ((,keyvar ,keyform))
       (cond ,@(nreverse cond-body)))))
