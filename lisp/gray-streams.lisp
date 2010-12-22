;;; gray-streams.lisp
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

(defclass fundamental-stream (standard-object stream) ())

(defclass fundamental-input-stream (fundamental-stream) ())

(defclass fundamental-output-stream (fundamental-stream) ())

(defclass fundamental-character-stream (fundamental-stream) ())

(defclass fundamental-binary-stream (fundamental-stream) ())

(defclass fundamental-character-input-stream
  (fundamental-input-stream fundamental-character-stream) ())

(defclass fundamental-character-output-stream
  (fundamental-output-stream fundamental-character-stream) ())

(defclass fundamental-binary-input-stream
  (fundamental-input-stream fundamental-binary-stream) ())

(defclass fundamental-binary-output-stream
  (fundamental-output-stream fundamental-binary-stream) ())

;; character input

(defgeneric stream-read-char (stream))

(defgeneric stream-unread-char (stream character))

(defgeneric stream-read-char-no-hang (stream))

(defgeneric stream-peek-char (stream))

(defgeneric stream-listen (stream))

(defgeneric stream-read-line (stream))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (let ((line (make-array 64 :element-type 'character :fill-pointer 0)))
    (loop
      (let ((character (stream-read-char stream)))
        (if (eq character :eof)
            (return (values line t))
            (if (eql character #\newline)
                (return (values line nil))
                (vector-push-extend character line)))))))

(defgeneric stream-clear-input (stream))

;; character output

(defgeneric stream-write-char (stream character))

(defgeneric stream-line-column (stream))

(defgeneric stream-start-line-p (stream))

(defgeneric stream-write-string (stream string &optional start end))

(defgeneric stream-terpri (stream))

(defgeneric stream-fresh-line (stream))

(defgeneric stream-finish-output (stream))

(defgeneric stream-force-output (stream))

(defgeneric stream-clear-output (stream))

(defgeneric stream-advance-to-column (stream column))

;; other functions

(fmakunbound 'close)

(defgeneric close (stream &key abort))

(defmethod close ((stream ansi-stream) &key abort)
  (ansi-stream-close stream))

(fmakunbound 'open-stream-p)

(defgeneric open-stream-p (stream))

(defmethod open-stream-p ((stream ansi-stream))
  (ansi-stream-open-stream-p stream))

(fmakunbound 'streamp)

(defgeneric streamp (object)) ; optional

(defmethod streamp ((stream ansi-stream))
  (ansi-stream-p stream))

(defmethod streamp ((non-stream t))
  nil)

(fmakunbound 'input-stream-p)

(defgeneric input-stream-p (stream)) ; optional

(defmethod input-stream-p ((stream ansi-stream))
  (ansi-stream-input-stream-p stream))

(fmakunbound 'output-stream-p)

(defgeneric output-stream-p (stream)) ; optional

(defmethod output-stream-p ((stream ansi-stream))
  (ansi-stream-output-stream-p stream))

(fmakunbound 'stream-element-type)

(defgeneric stream-element-type (stream))

(defmethod stream-element-type ((stream ansi-stream))
  (ansi-stream-element-type stream))

;; binary streams

(defgeneric stream-read-byte (stream))

(defgeneric stream-write-byte (stream))

#+(or)
(progn

;; CMUCL example: a character input stream encapsulating a lisp-stream

(defclass character-input-stream (fundamental-character-input-stream)
  ((lisp-stream
    :initarg :lisp-stream
    :accessor character-input-stream-lisp-stream)))

(defun make-character-input-stream (lisp-stream)
  (declare (type lisp-stream lisp-stream))
  (make-instance 'character-input-stream :lisp-stream lisp-stream))

(defmethod open-stream-p ((stream character-input-stream))
  (open-stream-p (character-input-stream-lisp-stream stream)))

(defmethod close ((stream character-input-stream) &key abort)
  (close (character-input-stream-lisp-stream stream) :abort abort))

(defmethod input-stream-p ((stream character-input-stream))
  (input-stream-p (character-input-stream-lisp-stream stream)))

(defmethod output-stream-p ((stream character-input-stream))
  (output-stream-p (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char ((stream character-input-stream))
  (read-char (character-input-stream-lisp-stream stream)))

(defmethod stream-unread-char ((stream character-input-stream) character)
  (unread-char character (character-input-stream-lisp-stream stream)))

(defmethod stream-read-char-no-hang ((stream character-input-stream))
  (read-char-no-hang (character-input-stream-lisp-stream stream) nil :eof))

#+(or)
(defmethod stream-peek-char ((stream character-input-stream))
  (peek-char nil (character-input-stream-lisp-stream stream) nil :eof))

#+(or)
(defmethod stream-listen ((stream character-input-stream))
  (listen (character-input-stream-lisp-stream stream)))

(defmethod stream-clear-input ((stream character-input-stream))
  (clear-input (character-input-stream-lisp-stream stream)))
)
