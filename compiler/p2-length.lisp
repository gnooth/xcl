;;; p2-length.lisp
;;;
;;; Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

(in-package "COMPILER")

(defun p2-length (form target)
  (mumble "p2-length~%")
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form))
           (type (derive-type arg)))
      (cond ((eq type :unknown)
             (p2-function-call form target)
             (when (var-ref-p arg)
               (add-type-constraint (var-ref-var arg) 'SEQUENCE)))
            ((vectorp arg)
             (p2-constant (length arg) target))
            ((and (quoted-form-p arg)
                  (sequencep (%cadr arg)))
             (p2-constant (length (%cadr arg)) target))
            ((subtypep type '(SIMPLE-ARRAY * (*)))
             (let ((reg (if (register-p target) target $ax))
                   (displacement (- +vector-capacity-offset+ +typed-object-lowtag+)))
               (process-1-arg arg reg t)
               (cond ((register-p target)
                      (inst :mov `(,displacement ,reg) target)
                      (clear-register-contents target)
                      (box-fixnum target))
                     (t
                      (inst :mov `(,displacement ,reg) $ax)
                      (clear-register-contents $ax)
                      (box-fixnum $ax)
                      (move-result-to-target target)))))
            ((subtypep type 'VECTOR)
             (process-1-arg arg :default t)
             (emit-call-1 '%vector-length target))
            ((subtypep type 'LIST)
             (process-1-arg arg :default t)
             (emit-call-1 '%list-length target))
            (t
             (p2-function-call form target)
             (when (var-ref-p arg)
               (add-type-constraint (var-ref-var arg) 'SEQUENCE)))))
    t))
