;;; read-byte.lisp
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

(defun %read-byte (stream eof-error-p eof-value)
  (locally (declare (optimize speed))
    (let ((read-byte-function (stream-read-byte-function stream)))
      (when read-byte-function
        (return-from %read-byte (funcall read-byte-function stream eof-error-p eof-value)))))
  (let* ((element-type (stream-element-type stream)))
    (unless element-type
      (if eof-error-p
          (error 'end-of-file :stream stream)
          (return-from %read-byte eof-value)))
    (unless (consp element-type)
      (error 'type-error
             :format-control "READ-BYTE: unsupported element type ~S."
             :format-arguments (list element-type)))
    (let ((width (cadr element-type)))
      (if (eql width 8)
          (read-8-bits stream eof-error-p eof-value)
          (let ((result 0))
            (dotimes (i (/ width 8))
              (let ((byte (read-8-bits stream eof-error-p eof-value)))
                (when (eq byte eof-value)
                  (return-from %read-byte eof-value))
                (setf result (ash result 8))
                (setf result (+ result byte))))
            (if (and (eq (car element-type) 'signed-byte)
                     (not (zerop (logand result (expt 2 (1- width))))))
                (- result (expt 2 width))
                result))))))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (%read-byte stream eof-error-p eof-value))
