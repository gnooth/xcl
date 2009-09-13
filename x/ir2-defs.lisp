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

;; (declaim (inline make-ir2-instruction))
(defknown make-ir2-instruction (t t t) t)
(defun make-ir2-instruction (operator operand1 operand2)
  (list operator operand1 operand2))

;; (declaim (inline ir2-instruction-p))
(defknown ir2-instruction-p (t) t)
(defun ir2-instruction-p (instruction)
  (consp instruction))

;; (declaim (inline operator))
(defknown operator (t) t)
(defun operator (instruction)
  (first instruction))

;; (declaim (inline operand1))
(defknown operand1 (t) t)
(defun operand1 (instruction)
  (second instruction))

;; (declaim (inline set-operand1))
(defknown set-operand1 (t t) t)
(defun set-operand1 (instruction operand)
  (setf (second instruction) operand))

;; (declaim (inline operand2))
(defknown operand2 (t) t)
(defun operand2 (instruction)
  (third instruction))

;; (declaim (inline set-operand2))
(defknown set-operand2 (t t) t)
(defun set-operand2 (instruction operand)
  (setf (third instruction) operand))
