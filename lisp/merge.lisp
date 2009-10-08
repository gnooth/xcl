;;; merge.lisp

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

(defun merge-lists (list1 list2 predicate key)
  (let* ((list   (list nil))
	 (splice list))
    (do ((x1 list1)
	 (x2 list2))
	((or (endp x1) (endp x2))
         (rplacd splice (or x1 x2))
         (cdr list))
      (if (funcall predicate (apply-key key (car x2)) (apply-key key (car x1)))
	  (setq splice (cdr (rplacd splice x2))
		x2     (cdr x2))
          (setq splice (cdr (rplacd splice x1))
                x1     (cdr x1))))))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Merge SEQUENCE1 with SEQUENCE2 destructively according to an order determined by the PREDICATE."
  (let ((merged-list (merge-lists (coerce sequence1 'list)
				  (coerce sequence2 'list) predicate key)))
    (cond ((eq result-type 'null)
           (if (null merged-list)
               merged-list
               (error 'type-error
                      :datum 6
                      :expected-type '(eql 0))))
          ((subtypep result-type 'list)
           merged-list)
	  ((subtypep result-type 'vector)
           (coerce merged-list result-type))
	  (t
           (error 'simple-error
                  :format-control "~S is not a sequence type."
                  :format-arguments (list result-type))))))
