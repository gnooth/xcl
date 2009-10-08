;;; delete-duplicates.lisp

;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: sequence.lisp,v 1.42 2004/02/20 07:23:42 yuji Exp $
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "SYSTEM")

(defun list-delete-duplicates (test list start end key)
  (let* ((head (cons nil list))
	 (splice head)
	 (tail (when end (nthcdr end list))))
    (flet ((list-member (list)
                        (do ((x (cdr list) (cdr x))
                             (item (car list)))
                            ((eq x tail) nil)
                          (when (funcall test (apply-key key item) (apply-key key (car x)))
                            (return t)))))
      (do ((i 0 (1+ i))
	   (x list (cdr x)))
	  ((endp x) (%rplacd splice nil) (cdr head))
	(unless (and (<= start i) (or (null end) (< i end)) (list-member x))
	  (setq splice (%cdr (%rplacd splice x))))))))

(defun vector-delete-duplicates (test vector start end key)
  (let* ((length (length vector))
	 (end (or end length))
	 (i 0))
    (flet ((vector-member (item j)
                          (do ((k (1+ j) (1+ k)))
                              ((>= k end) nil)
                            (when (funcall test (apply-key key item)
                                           (apply-key key (aref vector k)))
                              (return t)))))
      (do* ((j 0 (1+ j))
	    element)
           ((>= j length))
	(setq element (aref vector j))
	(unless (and (<= start j) (< j end) (vector-member element j))
	  (setf (aref vector i) element)
	  (incf i)))
      (cond ((array-has-fill-pointer-p vector)
             (setf (fill-pointer vector) i)
             vector)
            ((adjustable-array-p vector)
             (adjust-array vector i))
            (t
             (subseq vector 0 i))))))

(defun delete-duplicates (sequence &key from-end (test #'eql) test-not
				   (start 0) end key)
  "Modify SEQUENCE deleting redundant elements."
  (when test-not (setq test (complement test-not)))
  (if from-end
      (let ((length (length sequence)))
	(nreverse (delete-duplicates (nreverse sequence) :test test :key key
				     :start (- length (or end length))
				     :end (- length start))))
      (etypecase sequence
        (null nil)
        (cons (list-delete-duplicates test sequence start end key))
        (vector (vector-delete-duplicates test sequence start end key)))))
