;;; dump-form.lisp
;;;
;;; Copyright (C) 2004-2011 Peter Graves <gnooth@gmail.com>
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

(export '(dump-form dump-top-level-form))

(defknown dump-cons (cons stream) t)
(defun dump-cons (object stream)
  (declare (optimize speed))
  (declare (type cons object))
  (declare (type stream stream))
  (cond ((quoted-form-p object)
         (%stream-write-char stream #\')
         (dump-object (%cadr object) stream))
        (t
         (%stream-write-char stream #\()
         (loop
           (dump-object (%car object) stream)
           (let ((tail (%cdr object)))
             (when (null tail)
               (return))
             (when (> (%stream-charpos stream) 80)
               (%stream-terpri stream))
             (%stream-write-char stream #\space)
             (when (atom tail)
               (%stream-write-char stream #\.)
               (%stream-write-char stream #\space)
               (dump-object tail stream)
               (return))
             (setq object tail)))
         (%stream-write-char stream #\)))))

(defknown dump-vector (vector stream) t)
(defun dump-vector (vector stream)
  (declare (type vector vector))
  (let ((length (length vector)))
    (cond ((equal (array-element-type vector) '(unsigned-byte 8)) ; FIXME type=
           (%stream-write-char stream #\#)
           (%stream-write-object stream (length vector))
           (%stream-write-char stream #\%)
           (dotimes (i (length vector))
             (%stream-write-char stream (code-char (aref vector i)))))
          (t
           (%stream-write-char stream #\#)
           (%stream-write-object stream length)
           (%stream-write-char stream #\()
           (when (> length 0)
             (dotimes (i (1- length))
               (declare (type index i))
               (dump-object (aref vector i) stream)
               (when (> (%stream-charpos stream) 80)
                 (%stream-terpri stream))
               (%stream-write-char stream #\space))
             (dump-object (aref vector (1- length)) stream))
           (%stream-write-char stream #\))))))

(defknown dump-instance (t stream) t)
(defun dump-instance (object stream)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form object)
    (write-string "#." stream)
    (if initialization-form
        (let* ((instance (gensym))
               load-form)
          (setq initialization-form
                (subst instance object initialization-form))
          (setq initialization-form
                (subst instance (list 'quote instance) initialization-form
                       :test #'equal))
          (setq load-form `(progn
                             (let ((,instance ,creation-form))
                               ,initialization-form
                               ,instance)))
          (dump-object load-form stream))
        (dump-object creation-form stream))))

(defknown dump-object (t stream) t)
(defun dump-object (object stream)
  (declare (optimize speed))
  (cond ((consp object)
         (dump-cons object stream))
        ((stringp object)
         (%stream-write-object stream object))
        ((bit-vector-p object)
         (%stream-write-object stream object))
        ((vectorp object)
         (dump-vector object stream))
;;         ((or (structure-object-p object) ;; FIXME instance-p
;;              (standard-object-p object))
;;          (dump-instance object stream))
        ((or (typep object 'structure-object)
             (typep object 'standard-object))
         (dump-instance object stream))
        ((functionp object)
         (multiple-value-bind (lambda-expression closure-p name)
             (function-lambda-expression object)
           (declare (ignore closure-p)) ; REVIEW
           (cond (name
                  (%stream-write-object stream object))
                 (lambda-expression
                  (write-string "#." stream)
                  (%stream-write-object stream (list 'coerce-to-function lambda-expression)))
                 (t
                  (compiler-unsupported "DUMP-FORM unsupported situation")))))
        (t
         (%stream-write-object stream object))))

(defknown dump-form (t stream) t)
(defun dump-form (form stream)
  (declare (optimize speed))
  (let ((*print-fasl* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-circle* nil)
        (*print-readably* t)
        (*print-structure* t))
    (dump-object form stream)))

(defknown dump-top-level-form (t stream) t)
(defun dump-top-level-form (form stream)
  (declare (optimize speed (safety 0)))
  (declare (type stream stream))
  (dump-form form stream)
  (%stream-terpri stream)
  (%stream-terpri stream))

(provide "DUMP-FORM")
