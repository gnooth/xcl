;;; reader.lisp
;;;
;;; Copyright (C) 2010 Peter Graves <gnooth@gmail.com>
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

(defun read-line (&optional stream (eof-error-p t) eof-value recursive-p)
  (declare (ignore recursive-p))
  (let ((stream (designator-input-stream stream)))
    (cond ((ansi-stream-p stream)
           (ansi-stream-read-line stream eof-error-p eof-value))
          (t
           (multiple-value-bind (line eof) (stream-read-line stream)
             (cond ((and eof (zerop (length line)))
                    (if eof-error-p
                        (error 'end-of-file :stream stream)
                        (values eof-value t)))
                   (t
                    (values line eof))))))))
