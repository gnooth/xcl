;;; concatenate.lisp
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

(in-package "SYSTEM")

(defun concatenate (result-type &rest sequences)
  (case result-type
    (LIST
     (let ((result nil))
       (dolist (seq sequences (nreverse result))
         (dotimes (i (length seq))
           (declare (type index i))
           (declare (optimize (safety 0)))
           (push (elt seq i) result)))))
    ((STRING SIMPLE-STRING)
     (concatenate-to-string sequences))
    (t
     (let* ((length (apply '+ (mapcar 'length sequences)))
            (result (make-sequence result-type length))
            (i 0))
       (declare (type index i))
       (dolist (seq sequences result)
         (dotimes (j (length seq))
           (setf (elt result i) (elt seq j))
           (incf i)))))))
