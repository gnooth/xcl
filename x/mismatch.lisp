;;; mismatch.lisp

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

(defun mismatch (sequence1 sequence2 &key from-end (test #'eql) test-not key
			   (start1 0) (start2 0) end1 end2)
  "Return the first position where SEQUENCE1 and SEQUENCE2 differ."
  (when test-not (setq test (complement test-not)))
  (let* ((length1 (length sequence1))
	 (length2 (length sequence2))
	 (end1 (or end1 length1))
	 (end2 (or end2 length2))
	 (width1 (- end1 start1))
	 (width2 (- end2 start2))
	 (width (min width1 width2))
	 (s1 (if from-end (- end1 width) start1))
	 (e1 (if from-end end1 (+ start1 width))))
    (multiple-value-bind (get2 reset2)
	(make-iterator sequence2 start2 end2 length2 from-end)
      (declare (ignore reset2))
      (let ((i1 (if from-end (1- end1) start1))
	    (step (if from-end -1 1)))
	(do-subsequence (element1 sequence1 s1 e1 from-end
                                  (cond ((= width1 width2) nil)
                                        ((< width1 width2) (if from-end 0 end1))
                                        (t (if from-end
                                               (- end1 width2)
                                               (+ start1 width2)))))
                        (unless (funcall test (apply-key key element1)
                                         (apply-key key (funcall get2)))
                          (return (if from-end (1+ i1) i1)))
                        (incf i1 step))))))
