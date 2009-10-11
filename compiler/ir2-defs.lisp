;;; ir2-defs.lisp
;;;
;;; Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

(progn
  (deftype ir2-instruction () 'cons)

  (declaim (inline ir2-instruction-p))
  (defun ir2-instruction-p (thing)
    (and (consp thing)
         (length-eql thing 3)))

  (declaim (inline make-ir2-instruction))
  (defun make-ir2-instruction (operator operand1 operand2)
    (list operator operand1 operand2))

  (declaim (inline operator))
  (defun operator (instruction)
    (first instruction))

  (declaim (inline operatand1))
  (defun operand1 (instruction)
    (second instruction))

  (declaim (inline operatand2))
  (defun operand2 (instruction)
    (third instruction))

  (declaim (inline set-operator))
  (defun set-operator (instruction operator)
    (setf (first instruction) operator))

  (declaim (inline set-operand1))
  (defun set-operand1 (instruction operand1)
    (setf (second instruction) operand1))

  (declaim (inline set-operand2))
  (defun set-operand2 (instruction operand2)
    (setf (third instruction) operand2))

  (assign-setf-inverse 'operator 'set-operator)
  (assign-setf-inverse 'operand1 'set-operand1)
  (assign-setf-inverse 'operand2 'set-operand2)
  )

#+nil
(defstruct (ir2-instruction
            (:conc-name nil)
            (:constructor make-ir2-instruction (operator operand1 operand2)))
  operator
  operand1
  operand2)
