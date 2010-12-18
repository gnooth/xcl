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

;; (defgeneric close (stream &key abort))

;; (defgeneric open-stream-p (stream))

;; (defgeneric streamp (object)) ; optional

;; (defgeneric input-stream-p (stream)) ; optional

;; (defgeneric output-stream-p (stream)) ; optional

;; (defgeneric stream-element-type (stream))

;; binary streams

(defgeneric stream-read-byte (stream))

(defgeneric stream-write-byte (stream))
