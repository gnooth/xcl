;;; destructuring-bind.lisp
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

;;;; Adapted from CMUCL/SBCL.

(in-package "SYSTEM")

(defmacro destructuring-bind (lambda-list arg-list &rest body)
  (let* ((arg-list-name (gensym "ARG-LIST-")))
    (multiple-value-bind (body local-decls)
	(parse-defmacro lambda-list arg-list-name body nil 'destructuring-bind
			:anonymousp t
                        :doc-string-allowed nil
                        :wrap-block nil)
      `(let ((,arg-list-name ,arg-list))
	 ,@local-decls
	 ,body))))
