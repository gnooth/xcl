;;; write-sequence.lisp
;;;
;;; Copyright (C) 2004-2010 Peter Graves <gnooth@gmail.com>
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

(defun write-sequence (sequence stream &key (start 0) end)
  (declare (type sequence sequence))
  (declare (type stream stream))
  (declare (type index start))
  (let* ((length (length sequence))
         (end (or end length))
         (stream-element-type (stream-element-type stream)))
    (declare (type index end))
    (check-sequence-bounds start end length)
    (cond ((eq stream-element-type 'character)
           (cond ((stringp sequence)
                  (%stream-write-string stream sequence start end))
                 ((typep sequence '(array (unsigned-byte 8) (*)))
                  ;; bivalent stream
                  (do* ((i start (1+ i)))
                       ((>= i end) sequence)
                    (declare (optimize speed (safety 0)))
                    (declare (type index i))
                    (%write-8-bits (aref sequence i) stream)))
                 (t
                  (do* ((i start (1+ i)))
                       ((>= i end) sequence)
                    (declare (type index i))
                    (%stream-write-char stream (elt sequence i))))))
          ((equal stream-element-type '(unsigned-byte 8))
           (cond ((typep sequence '(simple-array (unsigned-byte 8) (*)))
                  (locally
                    (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
                    (do* ((i start (1+ i)))
                         ((>= i end) sequence)
                      (declare (optimize speed (safety 0)))
                      (declare (type index i))
                      (%write-8-bits (aref sequence i) stream))))
                 (t
                  (do* ((i start (1+ i)))
                       ((>= i end) sequence)
                    (declare (type index i))
                    (write-8-bits (elt sequence i) stream)))))
          (t
           (do* ((i start (1+ i)))
                ((>= i end) sequence)
             (declare (type index i))
             (write-byte (elt sequence i) stream))))))
