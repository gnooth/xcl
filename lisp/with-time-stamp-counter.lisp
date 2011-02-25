;;; with-time-stamp-counter.lisp
;;;
;;; Copyright (C) 2011 Peter Graves <gnooth@gmail.com>
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

(defmacro with-time-stamp-counter (&body body)
  (let ((t1 (gensym))
        (t2 (gensym)))
    `(let (,t1 ,t2)
       ;; "A strange quirk of the CPUID instruction is that it can take longer
       ;; to complete the first couple of times it is called. Thus, the best
       ;; policy is to call the instruction three times, measure the elapsed
       ;; time on the third call, then subtract this measurement from all
       ;; future measurements."
       (sys:%cpuid)
       (sys:%cpuid)
       (sys:%cpuid)
       (setq ,t1 (sys:rdtsc))
       (values (progn ,@body)
               (progn
                 (sys:%cpuid)
                 (setq ,t2 (sys:rdtsc))
                 (- ,t2 ,t1))))))
