;;; compiler-error.lisp
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

(in-package "SYSTEM")

(export '(*compiler-error-context*
          *last-error-context*
          compiler-style-warn
          compiler-warn
          compiler-error
          compiler-unsupported
          *defined-functions*
          *undefined-functions*
          note-name-defined
          *undefined-function-warnings*
          note-undefined-function))

(defvar *compiler-error-context* nil)

(defvar *last-error-context* nil)

(defun compiler-style-warn (format-control &rest format-arguments)
  (warn 'style-warning
        :format-control format-control
        :format-arguments format-arguments))

(defun compiler-warn (format-control &rest format-arguments)
  (warn 'warning
        :format-control format-control
        :format-arguments format-arguments))

(defun compiler-error (format-control &rest format-arguments)
  (error 'compiler-error
         :format-control format-control
         :format-arguments format-arguments))

(defun compiler-unsupported (format-control &rest format-arguments)
  (error 'compiler-unsupported-feature-error
         :format-control format-control
         :format-arguments format-arguments))

(defvar *defined-functions*)

(defvar *undefined-functions*)

(defun note-name-defined (name)
  (when (boundp '*defined-functions*)
    (push name *defined-functions*))
  (when (and (boundp '*undefined-functions*) (not (null *undefined-functions*)))
    (setq *undefined-functions* (remove name *undefined-functions*))))

(defvar *undefined-function-warnings*)

(defun note-undefined-function (name context)
  (unless (memq name *undefined-functions*)
    (push name *undefined-functions*))
  (push (cons name context) *undefined-function-warnings*))
