;;; p2-cxr.lisp
;;;
;;; Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

(in-package "COMPILER")

;; REVIEW move these to kernel?
(defconstant +car-offset+ 0)
(defconstant +cdr-offset+ +bytes-per-word+)

(defknown p2-%cxr (t t) t)
(defun p2-%cxr (form target)
  (when (check-arg-count form 1)
    (let ((arg (%cadr form)))
      (cond ((null target)
             (unless (flushable arg)
               (p2 arg nil)))
            (t
             (let* ((reg (and (var-ref-p arg)
                              (find-register-containing-var (var-ref-var arg))))
                    (op (%car form))
                    (offset (ecase op
                              ((%car car first) +car-offset+)
                              ((%cdr cdr rest)  +cdr-offset+)))
                    (displacement (- offset +list-lowtag+)))
               (unless reg
                 (process-1-arg arg $ax t)
                 (setq reg $ax))
               (cond ((register-p target)
                      (inst :mov `(,displacement ,reg) target)
                      (clear-register-contents target))
                     ((eql target :stack)
                      (inst :push `(,displacement ,reg)))
                     (t
                      (inst :mov `(,displacement ,reg) $ax)
                      (clear-register-contents $ax)
                      (move-result-to-target target)))))))
    t))

(defknown p2-cxr (t t) t)
(defun p2-cxr (form target)
  (when (check-arg-count form 1)
    (let* ((arg (%cadr form)))
      (cond ((or (zerop *safety*)
                 (subtypep (derive-type arg) 'LIST))
             (p2-%cxr form target))
            (t
             (let* ((var (and (var-ref-p arg) (var-ref-var arg)))
                    (reg (and var (find-register-containing-var var)))
                    (op (%car form))
                    (offset (ecase op
                              ((car first) +car-offset+)
                              ((cdr rest)  +cdr-offset+)))
                    (displacement (- offset +list-lowtag+)))
               (unless reg
                 (process-1-arg arg $ax t)
                 (setq reg $ax))
               (let* ((ERROR-NOT-LIST (common-error-label 'error-not-list reg))
                      (reg2 (if (eql reg $ax) $dx $ax))
                      (test-reg (reg8 reg2)))
                 (inst :mov (reg8 reg) test-reg)
                 (clear-register-contents reg2)
                 (inst :and +lowtag-mask+ test-reg)
                 (inst :cmp +list-lowtag+ test-reg)
                 (emit-jmp-short :ne ERROR-NOT-LIST))
               (cond ((null target))
                     ((register-p target)
                      (inst :mov `(,displacement ,reg) target)
                      (clear-register-contents target))
                     ((eql target :stack)
                      (inst :push `(,displacement ,reg)))
                     (t
                      (inst :mov `(,displacement ,reg) $ax)
                      (clear-register-contents $ax)
                      (move-result-to-target target)))
               (when var
                 (add-type-constraint var 'LIST))))))
    t))
