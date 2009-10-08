;;; some.lisp
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

;; Adapted from ECL.

(in-package "SYSTEM")

(defun some (predicate sequence &rest more-sequences)
  (cond ((null more-sequences)
         (cond ((listp sequence)
                (dolist (x sequence nil)
                  (let ((result (funcall predicate x)))
                    (when result
                      (return result)))))
               (t
                (dotimes (i (length sequence) nil)
                  (let ((result (funcall predicate (elt sequence i))))
                    (when result
                      (return result)))))))
        (t
         (setq more-sequences (cons sequence more-sequences))
         (do ((i 0 (1+ i))
              (l (apply #'min (mapcar #'length more-sequences))))
             ((>= i l) nil)
           (let ((result
                  (apply predicate
                         (mapcar #'(lambda (z) (elt z i)) more-sequences))))
             (when result
               (return result)))))))

(defun notany (predicate sequence &rest more-sequences)
  (not (apply #'some predicate sequence more-sequences)))
