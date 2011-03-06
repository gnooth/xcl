;;; p2-function-prolog.lisp
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

(defknown p2-trivial-function-prolog (compiland) t)
#+x86
(defun p2-trivial-function-prolog (compiland)
  (declare (type compiland compiland))
  (clear-register-contents)
  (inst :save-registers)
  (inst :enter-frame)

  ;;   (trivial-allocate-locals compiland)
  (inst :allocate-thread-var)
  (inst :allocate-locals)

  (inst :align-stack)
  (inst :initialize-thread-var)
  (clear-register-contents)
  t)

#+x86-64
(defun p2-trivial-function-prolog (compiland)
  (declare (type compiland compiland))
  (clear-register-contents)
  (inst :save-thread-register)
  (inst :save-registers)
  (inst :enter-frame)
  (dolist (var (compiland-arg-vars compiland))
    (inst :initialize-arg-var var))
  (trivial-allocate-locals compiland)
  (inst :align-stack)
  (inst :initialize-thread-register)
  ;;   (clear-register-contents)
  (cond ((compiland-needs-thread-var-p compiland)
         (clear-register-contents))
        (t
         (dolist (var (compiland-arg-vars compiland))
           (set-register-contents (var-arg-register var) var))))
  t)

(defun p2-function-prolog-&optional-only (compiland)
  (declare (type compiland compiland))
  #+x86-64 (inst :save-thread-register)
  (inst :enter-frame)
  (let* ((lambda-list (second (compiland-lambda-expression compiland)))
         (numreq (position '&optional lambda-list))
         (numopt (- (length lambda-list) numreq 1))
         (minargs numreq)
         (maxargs (+ numreq numopt))
         (NOT-SUPPLIED (make-label))
         (EXIT (make-label))
         (ERROR (make-label))
         (reg1 #+x86 :eax #+x86-64 :rdi)
         (reg2 #+x86 :edx #+x86-64 :rsi))
    (mumble "p2-function-prolog-&optional-only numreq = ~D numopt = ~D~%" numreq numopt)
    #+x86 (inst :mov `(,(* 2 +bytes-per-word+) :ebp) reg1) ; numargs
    (cond ((zerop minargs))
          ((eql minargs 1)
           (inst :test reg1 reg1)
           (emit-jmp-short :z ERROR))
          (t
           (inst :cmp minargs reg1)
           (emit-jmp-short :b ERROR)))
    (inst :cmp maxargs reg1)
    (emit-jmp-short :a ERROR)
    #+x86 (inst :mov `(,(* 3 +bytes-per-word+) :ebp) reg2) ; args
    (dotimes (index numreq)
      (inst :push `(,(* index +bytes-per-word+) ,reg2))
      (setf (var-index (find-var (elt lambda-list index) (compiland-arg-vars compiland)))
            #+x86    (- -1 index)
            #+x86-64 index))
    ;; flags remain set from :cmp maxargs reg1 above
    (emit-jmp-short :nz NOT-SUPPLIED)
    (inst :push `(,(* numreq +bytes-per-word+) ,reg2))
    (emit-jmp-short t EXIT)
    (label NOT-SUPPLIED)
    (p2-constant nil :stack)
    (label EXIT)
    (setf (var-index (find-var (elt lambda-list (+ numreq 1)) (compiland-arg-vars compiland)))
          #+x86    (- -1 numreq)
          #+x86-64 numreq)
    #+x86    (allocate-locals compiland (- (+ numreq numopt 1)))
    #+x86-64 (allocate-locals compiland (+ numreq numopt))
    (inst :align-stack)
    #+x86    (inst :initialize-thread-var)
    #+x86-64 (inst :initialize-thread-register)
    (let ((*current-segment* :elsewhere))
      (label ERROR)
      #+x86    (progn
                 (inst :push (fixnumize maxargs))
                 (inst :push (fixnumize minargs))
                 (p2-constant (compiland-name compiland) :stack)
                 (box-fixnum reg1)
                 (inst :push reg1))
      #+x86-64 (progn
                 (inst :mov :rdi :rsi)
                 (box-fixnum :rsi)
                 (p2-constant (compiland-name compiland) :rdi)
                 (inst :mov (fixnumize minargs) :rdx)
                 (inst :mov (fixnumize maxargs) :rcx))
      (emit-call-4 'error-wrong-number-of-arguments nil)
      (inst :exit))))
