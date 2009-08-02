;;; with-package-iterator.lisp
;;;
;;; Copyright (C) 2003-2008 Peter Graves <peter@armedbear.org>
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

(defun package-iterator-function (package-list symbol-types)
  (let ((results nil))
    (dolist (package (designator-list package-list))
      (unless (packagep package)
        (setq package (find-package package))
        (unless package
          (error 'package-error
                 :package (string package)
                 :format-control "~S does not designate a package."
                 :format-arguments (list package))))
      (when (memq :internal symbol-types)
        (dolist (sym (package-internal-symbols package))
          (push (list sym :internal package) results)))
      (when (memq :external symbol-types)
        (dolist (sym (package-external-symbols package))
          (push (list sym :external package) results)))
      (when (memq :inherited symbol-types)
        (dolist (sym (package-inherited-symbols package))
          (push (list sym :inherited package) results))))
    #'(lambda ()
       (let ((item (car results)))
         (setq results (cdr results))
         (if item
             (values t (first item) (second item) (third item))
             nil)))))

(defmacro with-package-iterator ((name package-list &rest symbol-types)
                                 &body body)
  (unless symbol-types
    (error 'program-error
           :format-control "~S: no symbol types specified."
           :format-arguments (list 'with-package-iterator)))
  (dolist (symbol-type symbol-types)
    (unless (memq symbol-type '(:internal :external :inherited))
      (error 'program-error
             :format-control "~S: invalid symbol type: %S"
             :format-arguments (list 'with-package-iterator symbol-type))))
  (let ((iter (gensym)))
    `(let ((,iter (package-iterator-function ,package-list ',(remove-duplicates symbol-types))))
       (macrolet ((,name () '(funcall ,iter)))
                 ,@body))))
