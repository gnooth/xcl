;;; do-subsequence.lisp

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

(defmacro do-sublist ((var list start end from-end result) &body body)
  (let ((rev (gensym))
	(i   (gensym))
	(x   (gensym)))
    `(symbol-macrolet ((,var (car ,x)))
       (if ,from-end
	   (let ((,rev nil))
	     (do ((x (nthcdr ,start ,list) (cdr x))
		  (i ,start (1+ i)))
		 ((>= i ,end))
	       (setq ,rev (cons x ,rev)))
	     (do* ((,rev ,rev (cdr ,rev))
		   (,x (car ,rev) (car ,rev)))
                  ((null ,rev) ,result)
	       ,@body))
           (do ((,x (nthcdr ,start ,list) (cdr ,x))
                (,i ,start (1+ ,i)))
               ((>= ,i ,end) ,result)
             ,@body)))))

(defmacro do-subvector ((var vector start end from-end result) &body body)
  (let ((i     (gensym))
	(step  (gensym))
	(limit (gensym)))
    `(symbol-macrolet ((,var (aref ,vector ,i)))
       (let ((,step (if ,from-end -1 1))
	     (,limit (if ,from-end (1- ,start) ,end)))
	 (do ((,i (if ,from-end (1- ,end) ,start) (+ ,i ,step)))
	     ((= ,i ,limit) ,result)
           ,@body)))))

(defmacro do-subsequence ((var sequence-form start-form &optional end-form
			       from-end-form result-form) &body body)
  (let ((sequence (gensym))
	(start    (gensym))
	(end      (gensym)))
    `(let* ((,sequence ,sequence-form)
	    (,start ,start-form)
	    (,end (or ,end-form (length ,sequence))))
       (check-subsequence ,sequence ,start ,end)
       (etypecase ,sequence
	 (list
	  (do-sublist (,var ,sequence ,start ,end ,from-end-form ,result-form)
                      ,@body))
	 (vector
	  (do-subvector (,var ,sequence ,start ,end ,from-end-form ,result-form)
                        ,@body))))))

