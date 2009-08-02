;;; lcm.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves <peter@armedbear.org>
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

;;; Adapted from ABCL.

(in-package "SYSTEM")

(defun two-arg-lcm (n m)
  (declare (type integer n m))
  (cond ((zerop n)
         0)
        ((zerop m)
         0)
        (t
         (/ (abs (* n m)) (gcd n m)))))

(defun lcm (&rest integers)
  (unless (every #'integerp integers)
    (error 'type-error :datum (find-if-not #'integerp integers) :expected-type 'integer))
  (case (length integers)
    (0 1)
    (1
     (abs (%car integers)))
    (2
     (two-arg-lcm (%car integers) (%cadr integers)))
    (t
     (do ((result (car integers) (two-arg-lcm result (car rest)))
          (rest (cdr integers) (cdr rest)))
         ((null rest) result)))))
