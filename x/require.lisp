;;; require.lisp
;;;
;;; Copyright (C) 2003-2009 Peter Graves <peter@armedbear.org>
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

(defun require (module &optional pathnames)
  (let ((module-name (string module)))
    (unless (member module-name *modules* :test #'string=)
      (let ((saved-modules (copy-list *modules*)))
        (cond (pathnames
               (unless (listp pathnames)
                 (setq pathnames (list pathnames)))
               (dolist (x pathnames)
                 (load x)))
              (t
               (load-system-file (concatenate 'string "x/" (string-downcase module-name)))))
        (set-difference *modules* saved-modules)))))
