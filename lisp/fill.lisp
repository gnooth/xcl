;;; fill.lisp
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

;;; Adapted from CMUCL.

(in-package "SYSTEM")

(defun list-fill (sequence item start end)
  (declare (type list sequence))
  (do ((current (nthcdr start sequence) (cdr current))
       (index start (1+ index)))
      ((or (atom current) (and end (eql index end)))
       sequence)
    (declare (type fixnum index))
    (rplaca current item)))

(defun vector-fill (sequence item start end)
  (declare (type vector sequence))
  (unless end
    (setq end (length sequence)))
  (do ((index start (1+ index)))
      ((eql index end) sequence)
    (declare (type index index))
    (declare (optimize speed (safety 0)))
    (setf (aref sequence index) item)))

(defun fill (sequence item &key (start 0) end)
  (cond ((listp sequence)
         (list-fill sequence item start end))
        (t
         (vector-fill sequence item start end))))
