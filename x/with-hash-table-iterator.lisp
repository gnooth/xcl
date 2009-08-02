;;; with-hash-table-iterator.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package "SYSTEM")

(defun hash-table-iterator-function (hash-table)
  (let ((entries (hash-table-entries hash-table)))
    #'(lambda () (let ((entry (car entries)))
                   (setq entries (cdr entries))
                   (if entry
                       (values t (car entry) (cdr entry))
                       nil)))))

(defmacro with-hash-table-iterator ((name hash-table) &body body)
  (let ((iter (gensym)))
    `(let ((,iter (hash-table-iterator-function ,hash-table)))
       (macrolet ((,name () '(funcall ,iter)))
                 ,@body))))
