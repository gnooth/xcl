;;; make-sequence.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves <peter@armedbear.org>
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

;;; Adapted from ECL.

(in-package "SYSTEM")

(defun size-mismatch-error (type size)
  (error 'type-error
         :format-control "The requested length (~D) does not match the specified type ~A."
         :format-arguments (list size type)))

(defun make-sequence (type size	&key (initial-element nil iesp))
  (let (element-type sequence)
    (setq type (expand-deftype type))
    (cond ((atom type)
           (when (classp type)
             (setq type (%class-name type)))
           (cond ((memq type '(LIST CONS))
                  (when (zerop size)
                    (if (eq type 'CONS)
                        (size-mismatch-error type size)
                        (return-from make-sequence nil)))
                  (return-from make-sequence
                               (if iesp
                                   (make-list size :initial-element initial-element)
                                   (make-list size))))
                 ((memq type '(STRING SIMPLE-STRING BASE-STRING SIMPLE-BASE-STRING))
                  (return-from make-sequence
                               (if iesp
                                   (make-string size :initial-element initial-element)
                                   (make-string size))))
                 ((eq type 'NULL)
                  (if (zerop size)
                      (return-from make-sequence nil)
                      (size-mismatch-error type size)))
                 (t
                  (setq element-type
                        (cond ((memq type '(BIT-VECTOR SIMPLE-BIT-VECTOR)) 'BIT)
                              ((memq type '(VECTOR SIMPLE-VECTOR)) t)
                              (t
                               (error 'type-error
                                      :format-control "~S is not a sequence type."
                                      :format-arguments (list type))))))))
          (t
           (let ((name (%car type))
                 (args (%cdr type)))
             (when (eq name 'LIST)
               (return-from make-sequence
                            (if iesp
                                (make-list size :initial-element initial-element)
                                (make-list size))))
             (when (eq name 'CONS)
               (unless (plusp size)
                 (size-mismatch-error name size))
               (return-from make-sequence
                            (if iesp
                                (make-list size :initial-element initial-element)
                                (make-list size))))
             (unless (memq name '(ARRAY SIMPLE-ARRAY VECTOR SIMPLE-VECTOR
                                  BIT-VECTOR SIMPLE-BIT-VECTOR STRING SIMPLE-STRING
                                  BASE-STRING SIMPLE-BASE-STRING))
               (error 'type-error
                      :format-control "~S is not a sequence type."
                      :format-arguments (list type)))
             (let ((len nil))
               (cond ((memq name '(STRING SIMPLE-STRING BASE-STRING SIMPLE-BASE-STRING))
                      (setq element-type 'character
                            len (car args)))
                     ((memq name '(ARRAY SIMPLE-ARRAY))
                      (setq element-type (or (car args) t)
                            len (if (consp (cadr args)) (caadr args) '*)))
                     ((memq name '(BIT-VECTOR SIMPLE-BIT-VECTOR))
                      (setq element-type 'bit
                            len (car args)))
                     ((eq name 'SIMPLE-VECTOR)
                      (setq element-type t
                            len (car args)))
                     (t
                      (setq element-type (or (car args) t)
                            len (cadr args))))
               (unless (or (null len) (eq len '*) (equal len '(*)))
                 (when (/= size len)
                   (size-mismatch-error type size)))))))
    (setq sequence
          (if iesp
              (make-array size :element-type element-type :initial-element initial-element)
              (make-array size :element-type element-type)))
    sequence))
