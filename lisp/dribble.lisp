;;; dribble.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves <peter@armedbear.org>
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

;;; Each time we start dribbling to a new stream, we put it in
;;; *DRIBBLE-STREAM*, and push a list of *DRIBBLE-STREAM*, *STANDARD-INPUT*,
;;; *STANDARD-OUTPUT* and *ERROR-OUTPUT* in *PREVIOUS-DRIBBLE-STREAMS*.
;;; *STANDARD-OUTPUT* and *ERROR-OUTPUT* is changed to a broadcast stream that
;;; broadcasts to *DRIBBLE-STREAM* and to the old values of the variables.
;;; *STANDARD-INPUT* is changed to an echo stream that echos input from the old
;;; value of standard input to *DRIBBLE-STREAM*.
;;;
;;; When dribble is called with no arguments, *DRIBBLE-STREAM* is closed,
;;; and the values of *DRIBBLE-STREAM*, *STANDARD-INPUT*, and
;;; *STANDARD-OUTPUT* are popped from *PREVIOUS-DRIBBLE-STREAMS*.

(defvar *previous-dribble-streams* nil)

(defvar *dribble-stream* nil)

(defun dribble (&optional pathname &key (if-exists :append))
  "With a file name as an argument, dribble opens the file and sends a
  record of further I/O to that file. Without an argument, it closes
  the dribble file, and quits logging."
  (cond (pathname
	 (let* ((new-dribble-stream
		 (open pathname
		       :direction :output
		       :if-exists if-exists
		       :if-does-not-exist :create))
		(new-standard-output
		 (make-broadcast-stream *standard-output* new-dribble-stream))
		(new-error-output
		 (make-broadcast-stream *error-output* new-dribble-stream))
		(new-standard-input
		 (make-echo-stream *standard-input* new-dribble-stream)))
	   (push (list *dribble-stream* *standard-input* *standard-output*
		       *error-output*)
		 *previous-dribble-streams*)
	   (setq *dribble-stream* new-dribble-stream)
	   (setq *standard-input* new-standard-input)
	   (setq *standard-output* new-standard-output)
	   (setq *error-output* new-error-output)))
	((null *dribble-stream*)
	 (error "Not currently dribbling."))
	(t
	 (let ((old-streams (pop *previous-dribble-streams*)))
	   (close *dribble-stream*)
	   (setq *dribble-stream* (first old-streams))
	   (setq *standard-input* (second old-streams))
	   (setq *standard-output* (third old-streams))
	   (setq *error-output* (fourth old-streams)))))
  (values))
