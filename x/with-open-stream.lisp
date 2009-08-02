;;; with-open-stream.lisp
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

(in-package "SYSTEM")

(defmacro with-open-stream (&rest args)
  (let ((var (caar args))
        (stream (cadar args))
        (decls-and-forms (cdr args))
        (abortp (gensym)))
    (multiple-value-bind (forms decls) (parse-body decls-and-forms nil)
      `(let ((,var ,stream)
             (,abortp t))
         ,@decls
         (unwind-protect
             (multiple-value-prog1
              (progn ,@forms)
              (setq ,abortp nil))
           (when ,var
             (close ,var :abort ,abortp)))))))
