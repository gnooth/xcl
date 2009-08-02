;;; copy-seq.lisp
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

;; Adapted from CMUCL.

(defun vector-copy-seq (sequence)
  (declare (type vector sequence))
  (let* ((length (length sequence))
         (copy (make-array length :element-type (array-element-type sequence))))
    (dotimes (index length copy)
      (declare (type index index))
      (declare (optimize speed (safety 0)))
      (vector-set copy index (vector-ref sequence index)))))

(defun list-copy-seq (list)
  (if (atom list)
      nil
      (let ((result (cons (%car list) nil)))
        (do ((x (%cdr list) (%cdr x))
             (splice result (%cdr (%rplacd splice (cons (car x) nil)))))
            ((atom x)
             (unless (null x)
               (%rplacd splice x))
             result)))))

(defun copy-seq (sequence)
  (cond ((listp sequence)
         (list-copy-seq sequence))
        ((stringp sequence)
         (copy-string sequence))
        (t
         (vector-copy-seq sequence))))
