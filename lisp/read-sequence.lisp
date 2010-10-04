;;; read-sequence.lisp
;;;
;;; Copyright (C) 2004-2010 Peter Graves <peter@armedbear.org>
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

(defun read-sequence (sequence stream &key (start 0) end)
  (declare (type stream stream))
  (locally (declare (optimize safety))
    (require-type start '(integer 0))
    (if end
        (require-type end '(or (integer 0) null))
        (setq end (length sequence))))

  (let ((read-byte-function (stream-read-byte-function stream)))
    (when read-byte-function
      (return-from read-sequence
                   (do ((pos start (1+ pos)))
                       ((>= pos end) pos)
                     (let ((element (funcall read-byte-function stream nil :eof)))
                       (when (eq element :eof)
                         (return pos))
                       (setf (elt sequence pos) element))))))

  (let* ((element-type (stream-element-type stream)))
    (cond ((eq element-type 'character)
           (do ((pos start (1+ pos)))
               ((>= pos end) pos)
             (let ((element (read-char stream nil :eof)))
               (when (eq element :eof)
                 (return pos))
               (setf (elt sequence pos) element))))
;;           ((equal element-type '(unsigned-byte 8))
;;            (if (and (vectorp sequence)
;;                     (equal (array-element-type sequence) '(unsigned-byte 8)))
;;                (read-vector-unsigned-byte-8 sequence stream start end)
;;                (do ((pos start (1+ pos)))
;;                    ((>= pos end) pos)
;;                  (let ((element (read-8-bits stream nil :eof)))
;;                    (when (eq element :eof)
;;                      (return pos))
;;                    (setf (elt sequence pos) element)))))
          (t
           (do ((pos start (1+ pos)))
               ((>= pos end) pos)
             (let ((element (read-byte stream nil :eof)))
               (when (eq element :eof)
                 (return pos))
               (setf (elt sequence pos) element)))))))
