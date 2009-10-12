;;; p3-common.lisp
;;;
;;; Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

(in-package "COMPILER")

(defun convert-binary-data ()
  (let ((code *code*))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (when (eq (instruction-kind instruction) :bytes)
          (setf (aref code i)
                (coerce (the list (instruction-data instruction)) '(simple-array (unsigned-byte 8) 1))))))))

(defun p3 ()
  (finalize-ir2)
  (assemble-ir2)
  (convert-binary-data))
