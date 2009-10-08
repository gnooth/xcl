;;; query.lisp
;;;
;;; Copyright (C) 2003-2009 Peter Graves <peter@armedbear.org>
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

(defun query-readline ()
  (force-output *query-io*)
  (string-trim '(#\space #\tab) (read-line *query-io*)))

(defun y-or-n-p (&optional format-string &rest arguments)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (loop
    (let* ((line (query-readline))
	   (ans (if (string= line "")
		    #\? ;Force CASE below to issue instruction.
		    (schar line 0))))
      (unless (whitespacep ans)
	(case ans
	  ((#\y #\Y) (return t))
	  ((#\n #\N) (return nil))
	  (t
	   (write-line "Type \"y\" for yes or \"n\" for no. " *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))
	   (force-output *query-io*)))))))

(defun yes-or-no-p (&optional format-string &rest arguments)
  (clear-input *query-io*)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (cond ((string-equal ans "YES") (return t))
	  ((string-equal ans "NO") (return nil))
	  (t
	   (write-line "Type \"yes\" for yes or \"no\" for no. " *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))))))
