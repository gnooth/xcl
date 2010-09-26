;;; map.lisp
;;;
;;; Copyright (C) 2005-2010 Peter Graves <peter@armedbear.org>
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

(defun map (result-type function sequence &rest more-sequences)
  (cond (more-sequences
         (let* ((sequences (cons sequence more-sequences))
                (limit (apply #'min (mapcar #'length sequences))))
           (declare (type index limit))
           (cond ((null result-type)
                  (dotimes (i limit nil)
                    (apply function (mapcar #'(lambda (z) (elt z i)) sequences))))
                 ((eq result-type 'LIST)
                  (let (result)
                    (dotimes (i limit (nreverse result))
                      (push (apply function (mapcar #'(lambda (z) (elt z i)) sequences))
                            result))))
                 (t
                  (let ((result (case result-type
                                  (STRING
                                   (make-string limit))
                                  (VECTOR
                                   (make-array limit))
                                  (t
                                   (make-sequence result-type limit)))))
                    (dotimes (i limit result)
                      (setf (elt result i)
                            (apply function (mapcar #'(lambda (z) (elt z i)) sequences)))))))))
        ((listp sequence)
         (let ((limit (length sequence)))
           (declare (type index limit))
           (cond ((memq result-type '(NULL NIL))
                  (dolist (element sequence nil)
                    (funcall function element)))
                 ((memq result-type '(LIST CONS))
                  (let (result)
                    (dolist (element sequence (nreverse result))
                      (push (funcall function element) result))))
                 (t
                  (let ((result (case result-type
                                  (STRING
                                   (make-string limit))
                                  (VECTOR
                                   (make-array limit))
                                  (t
                                   (make-sequence result-type limit))))
                        (i 0))
                    (declare (type vector result))
                    (declare (type index i))
                    (dolist (element sequence result)
                      (setf (aref result i) (funcall function element))
                      (incf i)))))))
        (t
         (locally
           (declare (type vector sequence))
           (let ((limit (length sequence)))
             (declare (type index limit))
             (cond ((memq result-type '(NULL NIL))
                    (dotimes (i limit nil)
                      (funcall function (aref sequence i))))
                   ((memq result-type '(LIST CONS))
                    (let (result)
                      (dotimes (i limit (nreverse result))
                        (push (funcall function (aref sequence i))
                              result))))
                   (t
                    (let ((result (case result-type
                                    (STRING
                                     (make-string limit))
                                    (VECTOR
                                     (make-array limit))
                                    (t
                                     (make-sequence result-type limit)))))
                      (dotimes (i limit result)
                        (setf (elt result i)
                              (funcall function (aref sequence i))))))))))))
