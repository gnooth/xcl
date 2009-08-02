;;; do-all-symbols.lisp
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

(defmacro do-all-symbols ((var &optional result-form) &body body)
  (multiple-value-bind (forms decls) (parse-body body nil)
    (let ((flet-name (gensym "DO-SYMBOLS-")))
      `(block nil
         (flet ((,flet-name (,var)
                 ,@decls
                 (tagbody ,@forms)))
           (dolist (package (list-all-packages))
             (flet ((iterate-over-symbols (symbols)
                      (dolist (symbol symbols)
                        (,flet-name symbol))))
               (iterate-over-symbols (package-internal-symbols package))
               (iterate-over-symbols (package-external-symbols package)))))
         (let ((,var nil))
           (declare (ignorable ,var))
           ,@decls
           ,result-form)))))
