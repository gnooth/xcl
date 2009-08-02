;;; describe-compiler-policy.lisp
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

(defun describe-compiler-policy ()
  (format t "~&; Compiler policy: safety ~D, space ~D, speed ~D, debug ~D~%"
          *safety* *space* *speed* *debug*)
  (format t "; ~S is ~S~%" '*force-full-calls* *force-full-calls*)
  (format t "; ~S is ~S~%" '*force-type-checks* *force-type-checks*)
  (values))
