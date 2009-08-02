;;; stable-sort.lisp

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

(defun mergesort-list (list predicate key)
  (labels ((mergesort (list length)
	      (if (<= length 1)
		  list
		(let* ((length1 (floor (/ length 2)))
		       (length2 (- length length1))
		       (list1 list)
		       (last1 (nthcdr (1- length1) list))
		       (list2 (cdr last1)))
		  (rplacd last1 nil)
		  (merge 'list
			 (mergesort list1 length1) (mergesort list2 length2)
			 predicate :key key)))))
    (mergesort list (length list))))

(defun stable-sort (sequence predicate &key key)
  "Sort SEQUENCE destructively guaranteeing the stability of equal elements' order."
  (if (listp sequence)
      (mergesort-list sequence predicate key)
      (let ((list (mergesort-list (coerce sequence 'list) predicate key)))
        (do ((x list (cdr x))
             (i 0 (1+ i)))
            ((endp x) sequence)
          (setf (aref sequence i) (car x))))))
