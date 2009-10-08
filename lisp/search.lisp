;;; search.lisp
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

;;; Adapted from CMUCL.

(in-package "SYSTEM")

(eval-when (:compile-toplevel :execute)

  (defmacro compare-elements (elt1 elt2)
    `(if test-not
         (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
             (return nil)
             t)
         (if (not (funcall test (apply-key key ,elt1) (apply-key key ,elt2)))
             (return nil)
             t)))


  (defmacro search-compare-list-list (main sub)
    `(do ((main ,main (cdr main))
          (jndex start1 (1+ jndex))
          (sub (nthcdr start1 ,sub) (cdr sub)))
         ((or (null main) (null sub) (eql end1 jndex))
          t)
       (compare-elements (car sub) (car main))))


  (defmacro search-compare-list-vector (main sub)
    `(do ((main ,main (cdr main))
          (index start1 (1+ index)))
         ((or (null main) (eql index end1)) t)
       (compare-elements (aref ,sub index) (car main))))


  (defmacro search-compare-vector-list (main sub index)
    `(do ((sub (nthcdr start1 ,sub) (cdr sub))
          (jndex start1 (1+ jndex))
          (index ,index (1+ index)))
         ((or (eql end1 jndex) (null sub)) t)
       (compare-elements (car sub) (aref ,main index))))


  (defmacro search-compare-vector-vector (main sub index)
    `(do ((index ,index (1+ index))
          (sub-index start1 (1+ sub-index)))
         ((eql sub-index end1) t)
       (compare-elements (aref ,sub sub-index) (aref ,main index))))


  (defmacro search-compare (main-type main sub index)
    (if (eq main-type 'list)
        `(if (listp ,sub)
             (search-compare-list-list ,main ,sub)
             (search-compare-list-vector ,main ,sub))
        `(if (listp ,sub)
             (search-compare-vector-list ,main ,sub ,index)
             (search-compare-vector-vector ,main ,sub ,index))))


  (defmacro list-search (main sub)
    `(do ((main (nthcdr start2 ,main) (cdr main))
          (index2 start2 (1+ index2))
          (terminus (- end2 (- end1 start1)))
          (last-match ()))
         ((> index2 terminus) last-match)
       (if (search-compare list main ,sub index2)
           (if from-end
               (setq last-match index2)
               (return index2)))))


  (defmacro vector-search (main sub)
    `(do ((index2 start2 (1+ index2))
          (terminus (- end2 (- end1 start1)))
          (last-match ()))
         ((> index2 terminus) last-match)
       (if (search-compare vector ,main ,sub index2)
           (if from-end
               (setq last-match index2)
               (return index2)))))

  ) ; eval-when

(defun search (sequence1 sequence2 &key from-end (test #'eql) test-not
                         (start1 0) end1 (start2 0) end2 key)
  (let ((end1 (or end1 (length sequence1)))
	(end2 (or end2 (length sequence2))))
    (when key
      (setq key (coerce-to-function key)))
    (if (listp sequence2)
        (list-search sequence2 sequence1)
        (vector-search sequence2 sequence1))))

;; (defun simple-search (sequence1 sequence2)
;;   (cond ((and (stringp sequence1) (stringp sequence2))
;;          (simple-string-search sequence1 sequence2))
;;         ((vectorp sequence2)
;;          (simple-vector-search sequence1 sequence2))
;;         (t
;;          (search sequence1 sequence2 :from-end nil))))
