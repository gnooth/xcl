;;; p2-multiple-value-prog1.lisp
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

;; #+x86-64
(defun p2-multiple-value-prog1 (form target)
  (let* ((block (cadr form))
         (subforms (cdr (block-form block)))
         #+x86    (thread-var      (compiland-thread-var      *current-compiland*))
         #+x86-64 (thread-register (compiland-thread-register *current-compiland*)))
    (when (null subforms)
      (compiler-error
       "Wrong number of arguments for ~A (expected at least 1, but received 0)."
       'MULTIPLE-VALUE-PROG1))
    (let ((first-form (first subforms))
          (rest-forms (rest subforms)))
      (cond ((single-valued-p first-form)
             (cond (target
                    (p2 first-form $ax)
                    (inst :mov $ax (block-values-var block)))
                   (t
                    (p2 first-form nil)))
             (dolist (subform rest-forms)
               (p2 subform nil))
             (unless (every 'single-valued-p rest-forms)
               (emit-clear-values))
             (cond ((null target))
                   ((register-p target)
                    (inst :mov (block-values-var block) target)
                    (clear-register-contents target))
                   ((eq target :stack)
                    (inst :mov (block-values-var block) $ax)
                    (clear-register-contents $ax)
                    (inst :push $ax))
                   ((eq target :return)
                    (inst :mov (block-values-var block) $ax)
                    (inst :exit))
                   (t
                    (compiler-unsupported "p2-multiple-value-prog1 unsupported situation"))))
            (t
             #+x86    (progn
                        (p2 first-form :stack)
                        (inst :push thread-var))
             #+x86-64 (progn
                        (p2 first-form :rsi)
                        (inst :mov thread-register :rdi))
             (emit-call-2 "RT_thread_copy_values" $ax)
             (inst :mov $ax (block-values-var block))
             (dolist (subform (rest subforms))
               (p2 subform nil))
             #+x86    (progn
                        (inst :mov (block-values-var block) :eax)
                        (inst :push :eax)
                        (inst :push thread-var))
             #+x86-64 (progn
                        (inst :mov (block-values-var block) :rsi)
                        (inst :mov thread-register :rdi))
             (emit-call-2 "RT_thread_set_values" target))))))
