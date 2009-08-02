;;; proclaim.lisp
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

(export '(proclaimed-type proclaimed-ftype ftype-result-type))

(defconstant *proclaimed-ftypes* (make-hash-table :test 'equal))

(defknown proclaim-type (*) t)
(defun proclaim-type (type &rest names)
  (declare (optimize speed))
  (dolist (name names)
    (setf (get name 'proclaimed-type) type)))

(defknown proclaimed-type (symbol) t)
(defun proclaimed-type (name)
  (get3 name 'proclaimed-type :none))

(defknown proclaim-ftype (*) t)
(defun proclaim-ftype (ftype &rest names)
  (declare (optimize speed))
  (when (and (consp ftype)
             (neq (%car ftype) 'FUNCTION))
    (setq ftype (expand-deftype ftype)))
  (dolist (name names)
    (if (symbolp name)
        (setf (get name 'proclaimed-ftype) ftype)
        (setf (gethash name *proclaimed-ftypes*) ftype))))

(defknown proclaimed-ftype (t) t)
(defun proclaimed-ftype (name)
  (if (symbolp name)
      (get2 name 'proclaimed-ftype)
      (gethash2-1 name *proclaimed-ftypes*)))

(defknown ftype-result-type (t) t)
(defun ftype-result-type (ftype)
  (cond ((and (consp ftype)
              (length-eql ftype 3)
              (eq (%car ftype) 'FUNCTION))
         (let ((result-type (%caddr ftype)))
           (if result-type
               result-type
               '*)))
        (t
         '*)))

;; proclaim declaration-specifier => implementation-dependent
(defun proclaim (declaration-specifier)
  (declare (type cons declaration-specifier))
  (unless (symbolp (%car declaration-specifier))
    (error 'type-error :datum (%car declaration-specifier) :expected-type 'symbol))
  ;; (cdr declaration-specifier) must be a proper list.
  (unless (listp (cddr declaration-specifier))
    (error 'type-error :datum (cddr declaration-specifier) :expected-type 'list))
  (case (%car declaration-specifier)
    (SPECIAL
     (dolist (name (%cdr declaration-specifier))
       (proclaim-special name)))
    (OPTIMIZE
     (dolist (spec (%cdr declaration-specifier))
       (let ((val 3)
             (quality spec))
         (when (consp spec)
           (setq quality (%car spec)
                 val     (cadr spec)))
         (when (and (fixnump val)
                    (<= 0 val 3))
           (case quality
             (SPEED
              (setq *speed* val))
             (SPACE
              (setq *space* val))
             (SAFETY
              (setq *safety* val))
             (DEBUG
              (setq *debug* val)))))))
    ((INLINE NOTINLINE)
     (dolist (name (%cdr declaration-specifier))
       (when (symbolp name) ; FIXME
         (put name '%inline (%car declaration-specifier)))))
    (TYPE
     (unless (%cdr declaration-specifier)
       (error "No type specified in TYPE declaration: ~S" declaration-specifier))
     (apply #'proclaim-type (%cdr declaration-specifier)))
    (FTYPE
     (unless (%cdr declaration-specifier)
       (error "No type specified in FTYPE declaration: ~S" declaration-specifier))
     (apply #'proclaim-ftype (%cdr declaration-specifier))))
  t)
