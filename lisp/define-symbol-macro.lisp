;;; define-symbol-macro.lisp
;;;
;;; Copyright (C) 2010 Peter Graves <gnooth@gmail.com>
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

(defun %define-symbol-macro (symbol expansion)
  (setf (symbol-value symbol) (make-symbol-macro expansion))
  symbol)

(defmacro define-symbol-macro (symbol expansion)
  (when (special-variable-p symbol)
    (error 'program-error "~S has already been defined as a global variable." symbol))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%define-symbol-macro ',symbol ',expansion)))
