;;; check-sequence-bounds.lisp
;;;
;;; Copyright (C) 2010 Peter Graves <peter@armedbear.org>
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

(defun check-sequence-bounds (start end length)
  (declare (type index start end length))
;;   (unless (<= 0 start end length)
  (unless (and (<= start end)
               (<= end length))
    (error "The bounding indices ~S and ~S are bad for a sequence of length ~S."
           start end length)))
