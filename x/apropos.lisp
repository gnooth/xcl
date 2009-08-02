;;; apropos.lisp
;;;
;;; Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

(defun apropos (string-designator &optional package-designator)
  (let ((*print-array* nil)
        (*print-structure* nil))
    (dolist (symbol (apropos-list string-designator package-designator))
      (fresh-line)
      (prin1 symbol)
      (when (boundp symbol)
        (format t "~40Tvalue: ~S~%" (symbol-value symbol)))
      (when (fboundp symbol)
        (cond ((macro-function symbol)
               (format t "~40Tmacro~%"))
              ((special-operator-p symbol)
               (format t "~40Tspecial operator~%"))
              ((typep (fdefinition symbol) 'generic-function)
               (format t "~40Tgeneric function~%"))
              (t
               (format t "~40Tfunction~%"))))))
  (values))
