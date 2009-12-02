;;; fill.lisp
;;;
;;; Copyright (C) 2007-2009 Peter Graves <peter@armedbear.org>
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

(defun list-fill-range (sequence item start end)
  (declare (type list sequence))
  (do ((current (nthcdr start sequence) (cdr current))
       (index start (1+ index)))
      ((or (atom current) (and end (eql index end)))
       sequence)
    (declare (type fixnum index))
    (rplaca current item)))

(defun simple-string-fill (sequence item)
  (declare (type simple-string sequence))
  (declare (type character item))
  (dotimes (i (length sequence) sequence)
    (declare (optimize speed (safety 0)))
    (declare (type index i))
    (setf (schar sequence i) item)))

(defun simple-string-fill-range (sequence item start end)
  (declare (type simple-string sequence))
  (declare (type index start))
  (if end
      (require-type end 'index)
      (setq end (length sequence)))
  (do ((index start (1+ index)))
      ((eql index end) sequence)
    (declare (type index index))
    (declare (optimize speed (safety 0)))
    (setf (aref sequence index) item)))

(defun simple-vector-fill (sequence item)
  (declare (type simple-vector sequence))
  (dotimes (i (length sequence) sequence)
    (declare (optimize speed (safety 0)))
    (declare (type index i))
    (setf (svref sequence i) item)))

(defun simple-vector-fill-range (sequence item start end)
  (declare (type simple-vector sequence))
  (declare (type index start))
  (if end
      (require-type end 'index)
      (setq end (length sequence)))
  (do ((index start (1+ index)))
      ((eql index end) sequence)
    (declare (type index index))
    (declare (optimize speed (safety 0)))
    (setf (aref sequence index) item)))

(defun vector-fill-range (sequence item start end)
  (declare (type vector sequence))
  (declare (type index start))
  (if end
      (require-type end 'index)
      (setq end (length sequence)))
  (do ((index start (1+ index)))
      ((eql index end) sequence)
    (declare (type index index))
    (declare (optimize speed (safety 0)))
    (setf (aref sequence index) item)))

(defun fill (sequence item &key (start 0) end)
  (cond ((listp sequence)
         (list-fill-range sequence item start end))
        ((simple-string-p sequence)
         (simple-string-fill-range sequence item start end))
        ((simple-vector-p sequence)
         (simple-vector-fill-range sequence item start end))
        (t
         (vector-fill-range sequence item start end))))
