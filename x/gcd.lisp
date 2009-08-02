;;; gcd.lisp
;;;
;;; Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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

(defun gcd (&rest integers)
  (cond ((null integers)
         0)
	((null (cdr integers))
         (let ((n (car integers)))
           (if (integerp n)
               (abs n)
               (error 'type-error :datum n :expected-type 'integer))))
	(t
	 (do ((gcd (car integers) (two-arg-gcd gcd (car rest)))
	      (rest (cdr integers) (cdr rest)))
	     ((null rest) gcd)))))
