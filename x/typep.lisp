;;; typep.lisp
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

(in-package "SYSTEM")

(export '(integer-typep))

(defun in-interval-p (x interval)
  (if (endp interval)
      t
      (let ((low (%car interval))
            (high (if (endp (%cdr interval)) '* (%cadr interval))))
        (cond ((eq low '*))
              ((consp low)
               (when (<= x (%car low))
                 (return-from in-interval-p nil)))
              ((when (< x low)
                 (return-from in-interval-p nil))))
        (cond ((eq high '*))
              ((consp high)
               (when (>= x (%car high))
                 (return-from in-interval-p nil)))
              ((when (> x high)
                 (return-from in-interval-p nil))))
        t)))

(defun integer-typep (object type-specifier)
  (declare (type cons type-specifier))
  (and (integerp object) (in-interval-p object (%cdr type-specifier))))

;; (defun monitor (x) (declare (ignore x)))

(defun %typep (object type-specifier)
  (setq type-specifier (canonicalize-type type-specifier))
;;   (monitor type-specifier)
  (if (atom type-specifier)
      (case type-specifier
        (list
         (listp object))
        (boolean
         (or (eq object t) (null object)))
        (keyword
         (keywordp object))
        (symbol
         (symbolp object))
        (single-float
         (single-float-p object))
        (double-float
         (double-float-p object))
        (character
         (characterp object))
        ((t)
         t)
        ((nil)
         nil)
        (t
         (builtin-typep object type-specifier)))
      (case (%car type-specifier)
        (INTEGER
         (integer-typep object type-specifier))
        (OR
         (dolist (type (%cdr type-specifier) nil)
           (when (%typep object type)
             (return t))))
        (RATIONAL
         (and (rationalp object) (in-interval-p object (%cdr type-specifier))))
        (FLOAT
         (and (floatp object) (in-interval-p object (%cdr type-specifier))))
        ((SINGLE-FLOAT SHORT-FLOAT)
         (and (single-float-p object) (in-interval-p object (%cdr type-specifier))))
        ((DOUBLE-FLOAT LONG-FLOAT)
         (and (double-float-p object) (in-interval-p object (%cdr type-specifier))))
        (REAL
         (and (realp object) (in-interval-p object (%cdr type-specifier))))
        (AND
         (dolist (type (%cdr type-specifier) t)
           (unless (%typep object type)
             (return nil))))
        (NOT
         (not (%typep object (cadr type-specifier))))
        (EQL
         (eql object (cadr type-specifier)))
        (MEMBER
         (dolist (thing (%cdr type-specifier) nil)
           (when (eql object thing)
             (return t))))
        (CONS
         (if (consp object)
             (let ((types (cdr type-specifier))
                   (type1 '*)
                   (type2 '*))
               (case (length types)
                 (1
                  (setq type1 (%car types)))
                 (2
                  (setq type1 (%car types))
                  (setq type2 (%cadr types))))
               (and (or (eq type1 '*) (%typep (%car object) type1))
                    (or (eq type2 '*) (%typep (%cdr object) type2))))
             nil))
        (SATISFIES
         (let ((predicate (cadr type-specifier)))
           (unless (symbolp predicate)
             (error 'type-error
                    :datum predicate
                    :expected-type 'SYMBOL))
           (and (funcall (coerce-to-function predicate) object) t)))
        (COMPLEX
         (case (length type-specifier)
           (1
            (complexp object))
           (2
            (if (complexp object)
                (let ((type (%cadr type-specifier)))
                  (or (eq type '*)
                      (and (%typep (realpart object) type)
                           (%typep (imagpart object) type))))
                nil))
           (t
            (error "Invalid type specifier: ~S" type-specifier))))
        ((VALUES FUNCTION)
         (error "~A types are not a legal argument to ~S." (%car type-specifier) 'TYPEP))
        (t
         (builtin-typep object type-specifier)))))

(defun typep (object type-specifier &optional environment)
  (declare (ignore environment))
  (%typep object type-specifier))
