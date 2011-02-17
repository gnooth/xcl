;;; p2-call.lisp
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

(defknown p2-function-call-0 (t t) t)

#+x86
(defun p2-function-call-0 (op target)
  (let ((compiland *current-compiland*)
        (kernel-function-p (kernel-function-p op))
        (use-fast-call-p (use-fast-call-p))
        thread-var)
    (declare (type compiland compiland))
    (cond (use-fast-call-p
           (cond ((and kernel-function-p
                       (eql (function-arity op) 0)
                       (function-code-address (symbol-function op)))
                  (emit-call-0 op target))
                 (kernel-function-p
                  (cond ((and (eql (function-arity op) -1)
                              (verify-call op 0))
                         (inst :push :esp)
                         (inst :push 0)
                         (emit-call-2 op target))
                        (t
                         (emit-move-function-to-register op :eax)
                         (inst :push :eax)
                         (emit-call-1 "RT_fast_call_function_0" target))))
                 ((and (use-direct-call-p)
                   *functions-defined-in-current-file*
                   (eql (gethash op *functions-defined-in-current-file*) 0))
                  (mumble "emitting direct call to ~S (0) defined in current file ~A~%"
                          op *compile-file-truename*)
                  (emit-call-0 op target))
                 ((and (eq op (compiland-name compiland))
                       (eql (compiland-arity compiland) 0))
                  (emit-recurse)
                  (move-result-to-target target))
                 (t
                  (p2-symbol op :stack)
                  (emit-call-1 "RT_fast_call_symbol_0" target))))
          ;; not use-fast-call-p
          ((setq thread-var (compiland-thread-var compiland))
           (cond (kernel-function-p
                  (emit-move-function-to-register op :eax)
                  (inst :push :eax)
                  (inst :push thread-var)
                  (emit-call-2 "RT_thread_call_function_0" target))
                 (t
                  (p2-symbol op :stack)
                  (inst :push thread-var)
                  (emit-call-2 "RT_thread_call_symbol_0" target))))
          (t
           (p2-symbol op :stack)
           (emit-call-1 "RT_current_thread_call_symbol_0" target)))))

#+x86-64
(defun p2-function-call-0 (op target)
  (let ((compiland *current-compiland*)
        (kernel-function-p (kernel-function-p op))
        thread-register)
    (declare (type compiland compiland))
    (cond ((use-fast-call-p)
           (cond ((and kernel-function-p
                       (eql (function-arity op) 0)
                       (function-code-address (symbol-function op)))
                  (emit-call op))
                 (kernel-function-p
                  (cond ((and (eql (function-arity op) -1)
                              (verify-call op 0))
                         (inst :mov :rsp :rsi)
                         (inst :xor :rdi :rdi)
                         (emit-call op))
                        (t
                         (inst :move-immediate `(:function ,op) :rdi)
                         (emit-call "RT_fast_call_function_0"))))
                 ((and (use-direct-call-p)
                   *functions-defined-in-current-file*
                   (eql (gethash op *functions-defined-in-current-file*) 0))
                  (mumble "emitting direct call to ~S (0) defined in current file ~A~%"
                          op *compile-file-truename*)
                  (emit-call op))
                 ((and (eq op (compiland-name compiland))
                       (eql (compiland-arity compiland) 0))
                  (emit-recurse))
                 (t
                  (p2-symbol op :rdi)
                  (emit-call "RT_fast_call_symbol_0"))))
          ;; not use-fast-call-p
          ((setq thread-register (compiland-thread-register compiland))
           (cond (kernel-function-p
                  (inst :move-immediate `(:function ,op) :rsi)
                  (inst :mov thread-register :rdi)
                  (emit-call "RT_thread_call_function_0"))
                 (t
                  (p2-symbol op :rsi)
                  (inst :mov thread-register :rdi)
                  (emit-call "RT_thread_call_symbol_0"))))
          (t
           (cond (kernel-function-p
                  (inst :move-immediate `(:function ,op) :rdi)
                  (emit-call "RT_current_thread_call_function_0"))
                 (t
                  (p2-symbol op :rdi)
                  (emit-call "RT_current_thread_call_symbol_0"))))))
  (move-result-to-target target))

