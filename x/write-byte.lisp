;;; write-byte.lisp
;;;
;;; Copyright (C) 2004-2009 Peter Graves <peter@armedbear.org>
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

(defun write-byte (byte stream)
  (let ((element-type (stream-element-type stream)))
    (cond ((equal element-type '(unsigned-byte 8))
           ;; the most common case
           (write-8-bits byte stream))
          (t
           (require-type byte element-type)
           (let ((width (cadr element-type)))
             (if (eql width 8)
                 (write-8-bits byte stream)
                 (let ((bytes nil))
                   (dotimes (i (/ width 8))
                     (push (logand byte #xff) bytes)
                     (setq byte (ash byte -8)))
                   (dolist (b bytes)
                     (write-8-bits b stream)))))))
    byte))
