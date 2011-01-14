;;; epsilons.lisp
;;;
;;; Copyright (C) 2009-2011 Peter Graves <peter@armedbear.org>
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

;; Adapted from ECL.

(in-package "SYSTEM")

(defun compute-epsilon (type)
  (let* ((one (coerce 1 type))
         (min (coerce 0 type))
         (max one))
    (do ((new (/ (+ min max) 2) (/ (+ min max) 2)))
        ((>= min max) max)
      (if (/= one (+ one new))
          (if (= new max)
              (return max)
              (setq max new))
          (if (= new min)
              (return max)
              (setq min new))))))

(defun compute-negative-epsilon (type)
  (let* ((one (coerce 1 type))
         (min (coerce 0 type))
         (max one))
    (do ((new (/ (+ min max) 2) (/ (+ min max) 2)))
        ((>= min max) max)
      (if (/= one (- one new))
          (if (= new max)
              (return max)
              (setq max new))
          (if (= new min)
              (return max)
              (setq min new))))))

(%defconstant 'single-float-epsilon (compute-epsilon 'single-float))
(%defconstant 'short-float-epsilon single-float-epsilon)
(%defconstant 'double-float-epsilon (compute-epsilon 'double-float))
(%defconstant 'long-float-epsilon double-float-epsilon)

(%defconstant 'single-float-negative-epsilon (compute-negative-epsilon 'single-float))
(%defconstant 'short-float-negative-epsilon single-float-negative-epsilon)
(%defconstant 'double-float-negative-epsilon (compute-negative-epsilon 'double-float))
(%defconstant 'long-float-negative-epsilon double-float-negative-epsilon)

(fmakunbound 'compute-epsilon)
(fmakunbound 'compute-negative-epsilon)
