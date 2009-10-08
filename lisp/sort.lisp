;;; sort.lisp

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

(defun quicksort-vector (vector predicate key)
  (declare (optimize speed (safety 0)))
  (declare (type vector vector))
  (labels ((quicksort (left right)
             (if (<= right left)
                 vector
                 (let ((v (partition left right)))
                   (quicksort left (1- v))
                   (quicksort (1+ v) right))))
	   (partition (left right)
             (let ((pivot (apply-key key (aref vector right)))
                   (l left)
                   (r (1- right)))
               (loop
                 (loop
                   (unless (and (<= l r)
                                (funcall predicate
                                         (apply-key key (aref vector l))
                                         pivot))
                     (return))
                   (incf l))
                 (loop
                   (when (or (>= l r)
                             (funcall predicate
                                      (apply-key key (aref vector r))
                                      pivot))
                     (return))
                   (decf r))
                 (when (>= l r)
                   (return))
                 (rotatef (aref vector l) (aref vector r)))
               (rotatef (aref vector l) (aref vector right))
               l)))
    (quicksort 0 (1- (length vector)))))

(defun sort (sequence predicate &key key)
  "Sort SEQUENCE destructively according to the order determined by PREDICATE."
  (setq predicate (coerce-to-function predicate))
  (if (vectorp sequence)
      (quicksort-vector sequence predicate key)
      (let ((vector (quicksort-vector (make-array (length sequence)
                                                  :initial-contents sequence)
                                      predicate key)))
        (do ((x sequence (cdr x))
             (i 0 (1+ i)))
            ((endp x) sequence)
          (%rplaca x (aref vector i))))))
