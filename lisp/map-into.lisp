;;; map-into.lisp
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

;;; Adapted from OpenMCL.

(in-package "SYSTEM")

(defun map-into (result-sequence function &rest sequences)
  (let* ((nseqs (list-length sequences))
         (temp  (make-list nseqs))
         (limit (if (listp result-sequence)
                    (length result-sequence)
                    (array-total-size result-sequence)))
         (rseq  result-sequence))
    (dolist (seq sequences)
      (let ((len (length seq)))
        (if (< len limit)
            (setq limit len))))
    (dotimes (i limit)
      (let ((args temp)
            (seqs sequences))
        (dotimes (j nseqs)
          (let ((seq (%car seqs)))
            (cond ((listp seq)
                   (%rplaca seqs (%cdr seq))
                   (%rplaca args (%car seq)))
                  (t
                   (%rplaca args (aref seq i)))))
          (setq args (%cdr args))
          (setq seqs (%cdr seqs))))
      (let ((res (apply function temp)))
        (cond ((consp rseq)
               (%rplaca rseq res)
               (setq rseq (%cdr rseq)))
              (t
               (setf (aref result-sequence i) res)))))
    (when (and (not (listp result-sequence))
               (array-has-fill-pointer-p result-sequence))
      (setf (fill-pointer result-sequence) limit))
    result-sequence))
