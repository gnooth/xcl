;;; replace.lisp
;;;
;;; Copyright (C) 2010 Peter Graves <peter@armedbear.org>
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

(defun check-sequence-bounds (start end length)
  (declare (type index start end length))
  (unless (<= 0 start end length)
    (error "The bounding indices ~S and ~S are bad for a sequence of length ~S."
           start end length)))

(defun replace (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  (declare (type index start1 start2))
  (let* ((length1 (length sequence1))
	 (end1    (or end1 length1))
         (length2 (length sequence2))
	 (end2    (or end2 length2)))
    (declare (type index end1 end2))
    (check-sequence-bounds start1 end1 length1)
    (check-sequence-bounds start2 end2 length2)
    (when (eq sequence1 sequence2)
      (setq sequence2 (subseq sequence2 start2 end2))
      (setq start2 0)
      (setq length2 (length sequence2))
      (setq end2 length2))
    (let* ((width1 (- end1 start1))
           (width2 (- end2 start2))
           (width  (min width1 width2)))
      (declare (type index width))
      (cond ((and (simple-string-p sequence1)
                  (simple-string-p sequence2))
             (let ((source sequence2)
                   (target sequence1)
                   (source-index start2)
                   (target-index start1))
               (declare (optimize speed (safety 0)))
               (declare (type simple-string source target))
               (declare (type index source-index target-index))
               (dotimes (i width)
                 (declare (type index i))
                 (setf (schar target target-index) (schar source source-index))
                 (incf source-index)
                 (incf target-index))))
            ((and (vectorp sequence1)
                  (vectorp sequence2))
             (let ((source sequence2)
                   (target sequence1)
                   (source-index start2)
                   (target-index start1))
               (declare (optimize speed (safety 0)))
               (declare (type vector source target))
               (declare (type index source-index target-index))
               (dotimes (i width)
                 (declare (type index i))
                 (setf (aref target target-index) (aref source source-index))
                 (incf source-index)
                 (incf target-index))))
            (t
             (dotimes (i width)
               (let ((x (elt sequence2 (+ start2 i))))
                 (setf (elt sequence1 (+ start1 i)) x)))))))
  sequence1)
