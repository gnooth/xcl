;;; defclass.lisp
;;;
;;; Copyright (C) 2006-2011 Peter Graves <peter@armedbear.org>
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

;;; Originally based on Closette.

;;; Closette Version 1.0 (February 10, 1991)
;;;
;;; Copyright (c) 1990, 1991 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;;
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;;
;;; Closette is an implementation of a subset of CLOS with a metaobject
;;; protocol as described in "The Art of The Metaobject Protocol",
;;; MIT Press, 1991.

(in-package "SYSTEM")

(defmacro defclass (&whole form name direct-superclasses direct-slots &rest options)
  (unless (>= (length form) 4)
    (error 'program-error
           :format-control
           "Wrong number of arguments for DEFCLASS (expected at least 3, but received only ~D)."
           :format-arguments
           (list (length (cdr form)))))
  `(ensure-class ',name
                 :direct-superclasses
;;                  (canonicalize-direct-superclasses ',direct-superclasses)
                 ',direct-superclasses
                 :direct-slots
                 ,(canonicalize-direct-slots direct-slots)
                 ,@(canonicalize-defclass-options options)))

(defun canonicalize-direct-slots (direct-slots)
   `(list ,@(mapcar #'canonicalize-direct-slot direct-slots)))

(defun canonicalize-direct-slot (spec)
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
            (initfunction nil)
            (initform nil)
            (initform-supplied-p nil)
            (initargs nil)
            (allocation nil)
            (type nil)
            (type-supplied-p nil)
            (documentation nil)
            (documentation-supplied-p nil)
            (readers nil)
            (writers nil)
            (other-options nil))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform
             (when initform-supplied-p
               (error 'program-error
                      :format-control "Duplicate :INITFORM slot option for slot ~S."
                      :format-arguments (list name)))
             (setq initfunction
                   `(function (lambda () ,(cadr olist))))
             (setq initform `',(cadr olist))
             (setq initform-supplied-p t))
            (:initarg
             (push (cadr olist) initargs))
            (:allocation
             (when allocation
               (error 'program-error
                      :format-control "Duplicate :ALLOCATION slot option for slot ~S."
                      :format-arguments (list name)))
             (setf allocation (cadr olist))
             (push (car olist) other-options)
             (push (cadr olist) other-options))
            (:type
             (when type-supplied-p
               (error 'program-error
                      :format-control "Duplicate :TYPE slot option for slot ~S."
                      :format-arguments (list name)))
             (setq type (cadr olist)) ;; FIXME type is ignored
             (setq type-supplied-p t))
            (:documentation
             (when documentation-supplied-p
               (error 'program-error
                      :format-control "Duplicate :DOCUMENTATION slot option for slot ~S."
                      :format-arguments (list name)))
             (setq documentation (cadr olist)) ;; FIXME documentation is ignored
             (setq documentation-supplied-p t))
            (:reader
             (note-name-defined (cadr olist))
             (push (cadr olist) readers))
            (:writer
             (note-name-defined (cadr olist))
             (push (cadr olist) writers))
            (:accessor
             (note-name-defined (cadr olist))
             (push (cadr olist) readers)
             (push `(setf ,(cadr olist)) writers))
            (t
             ;; "The options to DEFCLASS can be extended. It is required that
             ;; all implementations signal an error if they observe a class
             ;; option or a slot option that is not implemented locally."
             (error 'program-error
                    :format-control "Unknown slot option ~S for slot ~S."
                    :format-arguments (list (car olist) name)))))
        (setq initargs      (nreverse initargs)
              readers       (nreverse readers)
              writers       (nreverse writers)
              other-options (nreverse other-options))
        `(list
           :name ',name
           ,@(when initfunction
               `(:initform     ,initform
                 :initfunction ,initfunction))
           ,@(when initargs `(:initargs ',initargs))
           ,@(when readers  `(:readers  ',readers))
           ,@(when writers  `(:writers  ',writers))
           ,@other-options))))

;; (defun canonicalize-direct-superclasses (direct-superclasses)
;; ;;   `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses)))
;;   (let ((classes nil))
;;     (dolist (class-specifier direct-superclasses)
;;       (if (classp class-specifier)
;;           (push class-specifier classes)
;;           (let ((class (find-class class-specifier nil)))
;;             (unless class
;;               (setq class (make-instance 'forward-referenced-class))
;;               (setf (class.name class) class-specifier
;;                     (class.direct-subclasses class) nil
;;                     (class.direct-superclasses class) nil
;;                     (find-class class-specifier) class
;;                     ))
;;             (push class classes))))
;;     (nreverse classes)))

;; (defun canonicalize-direct-superclass (class-name)
;;   `(find-class ',class-name))

(defun canonicalize-defclass-options (options)
  (mappend #'canonicalize-defclass-option options))

(defun canonicalize-defclass-option (option)
  (case (car option)
    (:metaclass
     (list ':metaclass
           `(find-class ',(cadr option))))
    (:default-initargs
     (let ((default-initargs (cdr option)))
       `(:direct-default-initargs
         (list ,@(do ((initargs default-initargs (cddr initargs))
                      (result nil))
                     ((not initargs) (reverse result))
                   (push `(list ',(car initargs) ',(cadr initargs)
                                #'(lambda () ,(cadr initargs)))
                         result))))))
    ((:documentation :report)
     (list (car option) `',(cadr option)))
    (t
     (error 'program-error
            :format-control "Unknown DEFCLASS option: ~S"
            :format-arguments (list (car option))))))
