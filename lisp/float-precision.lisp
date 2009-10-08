;;; float-precision.lisp
;;;
;;; Copyright (C) 2009 Peter Graves <peter@armedbear.org>
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

(defun float-precision (float)
  (if (floatp float)
      (cond ((zerop float)
             0)
            ((typep float 'single-float)
             24)
            ((typep float 'double-float)
             53)
            (t
             ;; shouldn't get here!
             (aver nil)))
      (error 'simple-type-error
             :format-control "~S is not of type FLOAT."
             :format-arguments (list float))))
