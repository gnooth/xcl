;;; print-unreadable-object.lisp
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

(defun %print-unreadable-object (object stream type identity body)
  (setq stream (designator-output-stream stream))
  (when *print-readably*
    (error 'print-not-readable :object object))
  (format stream "#<")
  (when type
    (format stream "~S" (type-of object))
      (format stream " "))
  (when body
    (funcall body))
  (when identity
    (when (or body (not type))
      (format stream " "))
    (format stream "{~X}" (address-of object))) ; REVIEW
  (format stream ">")
  nil)

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  `(%print-unreadable-object ,object ,stream ,type ,identity
			     ,(if body
				  `(lambda () ,@body)
				  nil)))
