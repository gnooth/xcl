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

(defknown move-result-to-target (t) t)
(defun move-result-to-target (target)
  (ecase target
    #+x86
    ((:eax nil)
     ;; nothing to do
     )
    #+x86-64
    ((:rax nil)
     ;; nothing to do
     )
    #+x86
    ((:ecx :edx :ebx :esp :ebp :esi :edi)
     (inst :mov :eax target)
     (clear-register-contents target))
    #+x86-64
    ((:rcx :rdx :rbx :rsp :rbp :rsi :rdi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)
     (inst :mov :rax target)
     (clear-register-contents target))
    (:stack
     (inst :push $ax))
    (:return
     (inst :exit))))

(defknown emit-call-n (t t t) t)
(defun emit-call-n (address target n)
  #+x86    (declare (type (integer 0 #.call-arguments-limit) n))
  #+x86-64 (declare (ignore n))
  (emit-call address)
  #+x86
  (unless (and (eq target :return)
               (not (compiland-omit-frame-pointer *current-compiland*)))
    (emit-adjust-stack-after-call n))
  (move-result-to-target target))

#.(loop for i from 0 to 8
    collect (let ((name (intern (format nil "EMIT-CALL-~D" i))))
              `(progn
                 (defknown ,name (t t) t)
                 (defun ,name (address target)
                   #+x86 (emit-call-n address target ,i)
                   #+x86-64
                   (progn
                     (emit-call address)
                     (move-result-to-target target))))) into forms
    finally (return `(progn ,@forms)))

(defknown p2-function-call-0 (t t) t)
(defun p2-function-call-0 (op target)
  (let* ((compiland *current-compiland*)
         (kernel-function-p (kernel-function-p op))
         arity)
    (declare (type compiland compiland))
    (cond ((use-fast-call-p)
           (cond (kernel-function-p
                  (setq arity (function-arity op))
                  (cond ((eql arity 0)
                         (emit-call-0 op target))
                        ((and (eql arity -1)
                              (verify-call op 0))
                         #+x86
                         (progn
                           (inst :push :esp)
                           (inst :push 0))
                         #+x86-64
                         (progn
                           (inst :mov :rsp :rsi)
                           (inst :xor :rdi :rdi))
                         (emit-call-2 op target))
                        (t
                         #+x86
                         (progn
                           (inst :move-immediate `(:function ,op) :eax)
                           (inst :push :eax))
                         #+x86-64
                         (inst :move-immediate `(:function ,op) :rdi)
                         (emit-call-1 "RT_fast_call_function_0" target))))
                 ((and (eq op (compiland-name compiland))
                       (eql (compiland-arity compiland) 0))
                  (emit-recurse)
                  (move-result-to-target target))
                 ((and (use-direct-call-p)
                       *functions-defined-in-current-file*
                       (setq arity (gethash op *functions-defined-in-current-file*))
                       (memq arity '(0 -1)))
                  (case arity
                    (0
;;                      (mumble "emitting direct call to ~S (0) defined in current file ~A~%"
;;                              op *compile-file-truename*)
                     (emit-call-0 op target))
                    (-1
;;                      (mumble "p2-function-call-0 emitting direct call to ~S (-1) defined in current file~%" op)
                     #+x86
                     (progn
                       (inst :push :esp)
                       (inst :push 0))
                     #+x86-64
                     (progn
                       (inst :mov :rsp :rsi)
                       (inst :xor :rdi :rdi))
                     (emit-call-2 op target))))
                 (t
                  #+x86    (p2-symbol op :stack)
                  #+x86-64 (p2-symbol op :rdi)
                  (emit-call-1 "RT_fast_call_symbol_0" target))))
          ;; not use-fast-call-p
          #+x86
          (t
           (let ((thread-var (compiland-thread-var compiland)))
             (declare (type var thread-var))
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :eax)
                    (inst :push :eax)
                    (inst :push thread-var)
                    (emit-call-2 "RT_thread_call_function_0" target))
                   (t
                    (p2-symbol op :stack)
                    (inst :push thread-var)
                    (emit-call-2 "RT_thread_call_symbol_0" target)))))
          #+x86-64
          (t
           (let ((thread-register (compiland-thread-register compiland)))
             (aver (reg64-p thread-register))
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-2 "RT_thread_call_function_0" target))
                   (t
                    (p2-symbol op :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-2 "RT_thread_call_symbol_0" target))))))))

(defknown p2-function-call-1 (t t t) t)
(defun p2-function-call-1 (op args target)
  (let ((compiland *current-compiland*)
        (kernel-function-p (kernel-function-p op))
        (arg (car args))
        arity)
    (declare (type compiland compiland))
    (cond ((use-fast-call-p)
           (cond (kernel-function-p
                  (setq arity (function-arity op))
                  (cond ((eql arity 1)
                         (process-1-arg arg :default t)
                         (emit-call-1 op target))
                        ((and (eql arity -1)
                              (verify-call op 1))
                         (call-with-vectorized-args op args)
                         (move-result-to-target target))
                        (t
                         #+x86
                         (progn
                           (process-1-arg arg :stack t)
                           (inst :move-immediate `(:function ,op) :eax)
                           (inst :push :eax))
                         #+x86-64
                         (progn
                           (process-1-arg arg :rsi t)
                           (inst :move-immediate `(:function ,op) :rdi))
                         (emit-call-2 "RT_fast_call_function_1" target))))
                 ;; not kernel-function-p
                 ((and (eq op (compiland-name compiland))
                       (eql (compiland-arity compiland) 1))
                  (process-1-arg arg :default t)
                  (emit-recurse)
                  #+x86 (emit-adjust-stack-after-call 1)
                  (move-result-to-target target))
                 ((and (use-direct-call-p)
                       *functions-defined-in-current-file*
                       (setq arity (gethash op *functions-defined-in-current-file*))
                       (memq arity '(1 -1)))
                  (case arity
                    (1
;;                      (mumble "emitting direct call to ~S (1) defined in current file~%" op)
                     (process-1-arg arg :default t)
                     (emit-call-1 op target))
                    (-1
;;                      (mumble "p2-function-call-1 emitting direct call to ~S (-1) defined in current file~%" op)
                     (call-with-vectorized-args op args)
                     (move-result-to-target target))))
                 (t
                  #+x86
                  (progn
                    (process-1-arg arg :stack t)
                    (p2-symbol op :stack))
                  #+x86-64
                  (progn
                    (process-1-arg arg :rsi t)
                    (p2-symbol op :rdi))
                  (emit-call-2 "RT_fast_call_symbol_1" target))))
          ;; not use-fast-call-p
          #+x86
          (t
           (let ((thread-var (compiland-thread-var compiland)))
             (aver thread-var)
             ;; runtime calls thread->clear_values()
             (process-1-arg arg :stack nil)
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :eax)
                    (inst :push :eax)
                    (inst :push thread-var)
                    (emit-call-3 "RT_thread_call_function_1" target))
                   (t
                    (p2-symbol op :stack)
                    (inst :push thread-var)
                    (emit-call-3 "RT_thread_call_symbol_1" target)))))
          #+x86-64
          (t
           (let ((thread-register (compiland-thread-register compiland)))
             (aver thread-register)
             ;; runtime calls thread->clear_values()
             (process-1-arg arg :rdx nil)
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-3 "RT_thread_call_function_1" target))
                   (t
                    (p2-symbol op :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-3 "RT_thread_call_symbol_1" target))))))))

(defknown p2-function-call-2 (t t t) t)
(defun p2-function-call-2 (op args target)
  (let ((compiland *current-compiland*)
        (kernel-function-p (kernel-function-p op))
        arity)
    (declare (type compiland compiland))
    (cond ((use-fast-call-p)
           (cond (kernel-function-p
                  (setq arity (function-arity op))
                  (cond ((eql arity 2)
                         (process-2-args args :default t)
                         (emit-call-2 op target))
                        ((and (eql arity -1)
                              (verify-call op 1))
                         (call-with-vectorized-args op args)
                         (move-result-to-target target))
                        (t
                         #+x86
                         (progn
                           (process-2-args args :stack t)
                           (inst :move-immediate `(:function ,op) :eax)
                           (inst :push :eax))
                         #+x86-64
                         (progn
                           (process-2-args args '(:rsi :rdx) t)
                           (inst :move-immediate `(:function ,op) :rdi))
                         (emit-call-3 "RT_fast_call_function_2" target))))
                 ;; not kernel-function-p
                 ((and (eq op (compiland-name compiland))
                       (eql (compiland-arity compiland) 2))
                  (process-2-args args :default t)
                  (emit-recurse)
                  #+x86 (emit-adjust-stack-after-call 2)
                  (move-result-to-target target))
                 ((and (use-direct-call-p)
                       *functions-defined-in-current-file*
                       (setq arity (gethash op *functions-defined-in-current-file*))
                       (memq arity '(2 -1)))
                  (case arity
                    (2
                     (process-2-args args :default t)
                     (emit-call-2 op target))
                    (-1
                     (call-with-vectorized-args op args)
                     (move-result-to-target target))))
                 (t
                  #+x86
                  (progn
                    (process-2-args args :stack t)
                    (p2-symbol op :stack))
                  #+x86-64
                  (progn
                    (process-2-args args '(:rsi :rdx) t)
                    (p2-symbol op :rdi))
                  (emit-call-3 "RT_fast_call_symbol_2" target))))
          ;; not use-fast-call-p
          #+x86
          (t
           (let ((thread-var (compiland-thread-var compiland)))
             (aver thread-var)
             ;; runtime calls thread->clear_values()
             (process-2-args args :stack nil)
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :eax)
                    (inst :push :eax)
                    (inst :push thread-var)
                    (emit-call-4 "RT_thread_call_function_2" target))
                   (t
                    (p2-symbol op :stack)
                    (inst :push thread-var)
                    (emit-call-4 "RT_thread_call_symbol_2" target)))))
          #+x86-64
          (t
           (let ((thread-register (compiland-thread-register compiland)))
             (aver thread-register)
             ;; runtime calls thread->clear_values()
             (process-2-args args '(:rdx :rcx :r8) nil)
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-4 "RT_thread_call_function_2" target))
                   (t
                    (p2-symbol op :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-4 "RT_thread_call_symbol_2" target))))))))

(defknown p2-function-call-4 (t t t) t)
(defun p2-function-call-4 (op args target)
  (let ((compiland *current-compiland*)
        (kernel-function-p (kernel-function-p op))
        arity)
    (declare (type compiland compiland))
    (cond ((use-fast-call-p)
           (cond (kernel-function-p
                  (setq arity (function-arity op))
                  (cond ((eql arity 4)
                         (process-4-args args :default t)
                         (emit-call-4 op target))
                        ((and (eql arity -1)
                              (verify-call op 4))
                         (call-with-vectorized-args op args)
                         (move-result-to-target target))
                        (t
                         #+x86
                         (progn
                           (process-4-args args :stack t)
                           (inst :move-immediate `(:function ,op) :eax)
                           (inst :push :eax))
                         #+x86-64
                         (progn
                           (process-4-args args '(:rsi :rdx :rcx :r8) t)
                           (inst :move-immediate `(:function ,op) :rdi))
                         (emit-call-5 "RT_fast_call_function_4" target))))
                 ;; not kernel-function-p
                 ((and (eq op (compiland-name compiland))
                       (eql (compiland-arity compiland) 4))
                  (process-4-args args :default t)
                  (emit-recurse)
                  #+x86 (emit-adjust-stack-after-call 4)
                  (move-result-to-target target))
                 ((and (use-direct-call-p)
                       *functions-defined-in-current-file*
                       (setq arity (gethash op *functions-defined-in-current-file*))
                       (memq arity '(4 -1)))
                  (case arity
                    (4
;;                      (mumble "emitting direct call to ~S (4) defined in current file ~A~%"
;;                              op *compile-file-truename*)
                     (process-4-args args :default t)
                     (emit-call-4 op target))
                    (-1
;;                      (mumble "p2-function-call-4 emitting direct call to ~S (-1) defined in current file~%" op)
                     (call-with-vectorized-args op args)
                     (move-result-to-target target))))
                 (t
                  #+x86
                  (progn
                    (process-4-args args :stack t)
                    (p2-symbol op :stack))
                  #+x86-64
                  (progn
                    (process-4-args args '(:rsi :rdx :rcx :r8) t)
                    (p2-symbol op :rdi))
                  (emit-call-5 "RT_fast_call_symbol_4" target))))
          ;; not use-fast-call-p
          #+x86
          (t
           (let ((thread-var (compiland-thread-var compiland)))
             (aver thread-var)
             ;; runtime calls thread->clear_values()
             (process-4-args args :stack nil)
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :eax)
                    (inst :push :eax)
                    (inst :push thread-var)
                    (emit-call-6 "RT_thread_call_function_4" target))
                   (t
                    (p2-symbol op :stack)
                    (inst :push thread-var)
                    (emit-call-6 "RT_thread_call_symbol_4" target)))))
          #+x86-64
          (t
           (let ((thread-register (compiland-thread-register compiland)))
             (aver thread-register)
             ;; runtime calls thread->clear_values()
             (process-4-args args '(:rdx :rcx :r8 :r9) nil)
             (cond (kernel-function-p
                    (inst :move-immediate `(:function ,op) :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-6 "RT_thread_call_function_4" target))
                   (t
                    (p2-symbol op :rsi)
                    (inst :mov thread-register :rdi)
                    (emit-call-6 "RT_thread_call_symbol_4" target))))))))
