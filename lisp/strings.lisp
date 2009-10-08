;;; strings.lisp
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

;;; Adapted from OpenMCL.

(in-package "SYSTEM")

(defun string= (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (zerop (string-cmp string1 string2 start1 end1 start2 end2)))

(defun string/= (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-cmp string1 string2 start1 end1 start2 end2)
    (if (eql result 0) nil pos)))

(defun string< (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-cmp string1 string2 start1 end1 start2 end2)
    (if (eql result -1) pos nil)))

(defun string<= (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-cmp string1 string2 start1 end1 start2 end2)
    (if (eql result 1) nil pos)))

(defun string> (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-cmp string1 string2 start1 end1 start2 end2)
    (if (eql result 1) pos nil)))

(defun string>= (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-cmp string1 string2 start1 end1 start2 end2)
    (if (eql result -1) nil pos)))

(defun string-equal (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (zerop (string-compare string1 string2 start1 end1 start2 end2)))

(defun string-not-equal (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-compare string1 string2 start1 end1 start2 end2)
    (if (eql result 0) nil pos)))

(defun string-greaterp (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-compare string1 string2 start1 end1 start2 end2)
    (if (eql result 1) pos nil)))

(defun string-not-greaterp (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-compare string1 string2 start1 end1 start2 end2)
    (if (eql result 1) nil pos)))

(defun string-lessp (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-compare string1 string2 start1 end1 start2 end2)
    (if (eql result -1) pos nil)))

(defun string-not-lessp (string1 string2 &key start1 end1 start2 end2)
  (declare (optimize speed))
  (multiple-value-bind (result pos) (string-compare string1 string2 start1 end1 start2 end2)
    (if (eql result -1) nil pos)))
