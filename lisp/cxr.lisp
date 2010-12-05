;;; cxr.lisp
;;;
;;; Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

(defun caaaar (list) (car (car (car (car list)))))
(defun caaadr (list) (car (car (car (cdr list)))))
(defun caaddr (list) (car (car (cdr (cdr list)))))
(defun cadddr (list) (car (cdr (cdr (cdr list)))))
(defun cddddr (list) (cdr (cdr (cdr (cdr list)))))
(defun cdaaar (list) (cdr (car (car (car list)))))
(defun cddaar (list) (cdr (cdr (car (car list)))))
(defun cdddar (list) (cdr (cdr (cdr (car list)))))
(defun caadar (list) (car (car (cdr (car list)))))
(defun cadaar (list) (car (cdr (car (car list)))))
(defun cadadr (list) (car (cdr (car (cdr list)))))
(defun caddar (list) (car (cdr (cdr (car list)))))
(defun cdaadr (list) (cdr (car (car (cdr list)))))
(defun cdadar (list) (cdr (car (cdr (car list)))))
(defun cdaddr (list) (cdr (car (cdr (cdr list)))))
(defun cddadr (list) (cdr (cdr (car (cdr list)))))

(defun fifth   (list) (car (cddddr list)))
(defun sixth   (list) (cadr (cddddr list)))
(defun seventh (list) (caddr (cddddr list)))
(defun eighth  (list) (cadddr (cddddr list)))
(defun ninth   (list) (car (cddddr (cddddr list))))
(defun tenth   (list) (cadr (cddddr (cddddr list))))
