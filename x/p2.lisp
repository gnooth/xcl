;;; p2.lisp
;;;
;;; Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

(defvar *code* nil) ; a sequence of instructions

(defvar *main* nil)

(defvar *elsewhere* nil)

(defvar *current-segment* :main)

(defun install-p2-handler (symbol handler)
  (put symbol 'p2-handler handler))

(defknown fixnumize (integer) integer)
(defun fixnumize (n)
  (ash n +fixnum-shift+))

(defknown single-valued-p (t) t)
(defun single-valued-p (form)
  (cond ((atom form)
         t)
        (t
         (let ((op (%car form)))
           (cond ((special-operator-p op)
                  (cond ((memq op '(FUNCTION GO QUOTE SETQ))
                         t)
                        ((eq op 'IF)
                         (and (single-valued-p (third form))
                              (single-valued-p (fourth form))))
                        ((eq op 'RETURN-FROM)
                         (single-valued-p (third form)))
                        ((eq op 'TAGBODY)
                         (not (unsafe-p (block-form (second form)))))
                        ((memq op '(LET LET*))
                         (let* ((body (block-body (second form)))
                                (last-body-form (car (last body))))
                           (single-valued-p last-body-form)))
                        ((eq op 'PROGN)
                         (single-valued-p (car (last (%cdr form)))))
                        ((eq op 'BLOCK)
                         (let ((block (second form)))
                           (dolist (return-form (block-return-p block))
                             (unless (single-valued-p return-form)
                               (return-from single-valued-p nil)))
                           (single-valued-p (car (last (block-body block))))))
                        ((memq op '(TRULY-THE THE))
                         ; FIXME (the (values fixnum &optional) 42) is single-valued
                         (let ((value-type (second form)))
                           (or (atom value-type)
                               (neq (%car value-type) 'VALUES))))
                        (t
                         nil)))
                 ((operator-single-valued-p op)
                  t)
                 (t
                  ;; Note that looking at FTYPE proclamations is tricky for the
                  ;; purposes of SINGLE-VALUED-P: "It is permissible for form
                  ;; to yield a different number of values than are specified
                  ;; by value-type, provided that the values for which types
                  ;; are declared are indeed of those types." (CLHS THE)
                  (let ((ftype (proclaimed-ftype op)))
                    (when ftype
                      (let ((result-type (ftype-result-type ftype)))
                        (when (and (consp result-type)
                                   (length-eql result-type 3)
                                   (eq (%car result-type) 'VALUES)
                                   (eq (%caddr result-type) '&OPTIONAL))
                          t))))))))))

(defknown maybe-emit-clear-values (*) t)
(defun maybe-emit-clear-values (&rest forms)
  (dolist (form forms)
    (unless (single-valued-p form)
      (emit-clear-values)
      (return))))

(defknown p2-check-var-type (var boolean) t)
(defun p2-check-var-type (var initform-supplied-p)
  (declare (type var var))
  (let ((type-check-form (var-type-check-form var)))
    (when type-check-form
      (let ((declared-type (canonicalize-type (var-declared-type var)))
            (derived-type (var-derived-type var)))
        (aver (neq declared-type :none))
        (cond (initform-supplied-p
               (let* ((initform (var-initform var))
                      (initform-derived-type (derive-type initform)))
                 (unless (and (neq initform-derived-type :unknown)
                              (subtypep initform-derived-type declared-type))
                   (p2 type-check-form nil))))
              (t
               ;; no initform
               (p2 type-check-form nil)))
        (cond ((neq derived-type :unknown)
               ;; We have both a declared type and a derived type. Use whichever
               ;; is more specific.
               (setf (var-derived-type var)
                     (if (subtypep derived-type declared-type)
                         derived-type
                         declared-type)))
              (t
               (setf (var-derived-type var) declared-type)))))))

(defknown emit (t) t)
(defun emit (thing)
  (ecase *current-segment*
    (:main
     (let ((main *main*))
       (when main
         ;; fast path
         (return-from emit (vector-push-extend thing main))))
     (setq *main* (make-array 64 :fill-pointer 0))
     (vector-push-extend thing *main*))
    (:elsewhere
     (let ((elsewhere *elsewhere*))
       (when elsewhere
         ;; fast path
         (return-from emit (vector-push-extend thing elsewhere))))
     (setq *elsewhere* (make-array 64 :fill-pointer 0))
     (vector-push-extend thing *elsewhere*))))

(defknown inst (*) t)
(defun inst (&rest args)
  (emit args))

(defknown emit-byte (t) t)
(defun emit-byte (byte)
;;   (declare (type (unsigned-byte 8) byte))
;;   (emit (make-instruction :byte 1 byte))
  (inst :byte byte)
  )

(defknown emit-bytes (*) t)
(defun emit-bytes (&rest bytes)
;;   (emit (make-instruction :bytes (length bytes) bytes))
  (emit (list* :bytes bytes))
  )

(defknown label (t) t)
(defun label (label)
;;   (emit (make-instruction :label 0 label))
  (emit (list :label label))
  )

(defknown make-label () t)
(defun make-label ()
  (gensym))

(defknown emit-call (t) t)
(defun emit-call (address)
  (aver (or (stringp address) (symbolp address)))
;;   #+x86-64
;;   (let ((LABEL1 (make-label))
;;         (LABEL2 (make-label)))
;;     (inst :test 15 :esp)
;;     (emit-jmp-short :z LABEL1)
;; ;;     (emit-byte #xcc) ; int3
;;     (inst :sub 8 :rsp)
;;     (inst :call address)
;;     (inst :add 8 :rsp)
;;     (emit-jmp-short t LABEL2)
;;     (label LABEL1)
;;     (inst :call address)
;;     (label LABEL2))
;;   #+x86
  (inst :call address)
  (clear-register-contents))

(defknown emit-jmp (t t) t)
(defun emit-jmp (test label)
;;   (emit (make-instruction :jmp
;;                           (if (or (eq test t) (eq test :jump-table))
;;                               5 6)
;;                           (list test label)))
  (inst :jmp test label)
  )

(defknown emit-jmp-short (t t) t)
(defun emit-jmp-short (test label)
;;   (emit (make-instruction :jmp-short
;;                           2
;;                           (list test label)))
  (inst :jmp-short test label)
  )

(defknown emit-recurse () t)
(defun emit-recurse ()
;;   (emit (make-instruction :recurse 5 nil))
  (inst :recurse)
  (clear-register-contents))

(defknown emit-function (t) t)
(defun emit-function (form)
  (declare (type symbol form))
;;   (emit (make-instruction :function 4 form))
  (inst :function form)
  )

(defknown p2-function-call (t t) t)
(defun p2-function-call (form target)
  (declare (type cons form))
  (let* ((op (car form))
         (args (cdr form))
         (numargs (length args)))
    (declare (type symbol op))
    (cond ((find-local-function op)
           (p2-local-function-call form target))
          (t
           (case numargs
             (0 (p2-function-call-0 op target))
             (1 (p2-function-call-1 op args target))
             (2 (p2-function-call-2 op args target))
             (3 (p2-function-call-3 op args target))
             (4 (p2-function-call-4 op args target))
             (5 (p2-function-call-5 op args target))
             (6 (p2-function-call-6 op args target))
             (t (p2-function-call-n numargs op args target)))))))

(defun p2-apply (form target)
  (let ((args (cdr form)))
    (when (and (length-eql args 2)
               (use-fast-call-p))
      (let* ((arg1 (%car args))
             (type1 (derive-type arg1)))
        (process-2-args args :default t)
        (emit-call-2 (cond ((and (neq type1 :unknown)
                                 (subtypep type1 'FUNCTION))
                            "RT_fast_apply_function_2")
                           (t
                            "RT_fast_apply_2"))
                     target))
      t)))

(defknown p2-tagbody-1 (t) t)
(defun p2-tagbody-1 (body)
  (let ((must-clear-values nil))
    (dolist (subform body)
      (cond ((or (symbolp subform) (integerp subform))
             (let ((tag (find-visible-tag subform)))
               (unless tag
                 (error "p2-tagbody-1 tag ~S not found~%" subform))
;;                (mumble "p2-tagbody-1 tag ~S~%" subform)
               (label (tag-label tag)))
             ;; register contents can't be trusted after a label
             (clear-register-contents)
             ;; constraints can't be trusted after a label
             (setq *constraints* nil))
            (t
             (p2 subform nil)
             (unless must-clear-values
               (unless (single-valued-p subform)
                 (note "P2-TAGBODY: not single-valued: ~S~%" subform)
                 (setq must-clear-values t))))))
    (when must-clear-values
      (emit-clear-values))))

(defknown flet-debug-name (compiland) list)
(defun flet-debug-name (compiland)
  (declare (type compiland compiland))
  (list 'FLET
        (compiland-name (compiland-parent compiland))
        (compiland-name compiland)))

(defknown p2-flet-process-compiland (t) t)
(defun p2-flet-process-compiland (local-function)
  (declare (type local-function local-function))
  (let* ((compiland (local-function-compiland local-function))
         (minargs (compiland-minargs compiland))
         (maxargs (compiland-maxargs compiland))
         (*current-compiland* compiland)
         (*speed* *speed*)
         (*space* *space*)
         (*safety* *safety*)
         (*debug* *debug*)
         (*code* nil))
    (declare (type compiland compiland))
    (p2-compiland compiland)
    (cond ((compile-file-p)
           ;; COMPILE-FILE
           (aver (streamp *compile-file-output-stream*))
           (let (form)
             (cond (*closure-vars*
                    (let ((ctf-name
                           (gensym (concatenate 'string
                                                (write-to-string (flet-debug-name compiland))
                                                "-CTF-"))))
                      (setf (local-function-ctf-name local-function) ctf-name)
                      (setq form
                            `(multiple-value-bind (final-code final-constants)
                                 (generate-code-vector ',*code*
                                                       ',(compiland-constants compiland))
                               (set-fdefinition ',ctf-name
                                                (make-closure-template-function ',(flet-debug-name compiland)
                                                                                final-code
                                                                                ,minargs
                                                                                ,maxargs
                                                                                final-constants))))))
                   (t
                    (let ((callable-name
                           (gensym (concatenate 'string
                                                (write-to-string (flet-debug-name compiland))
                                                "-"))))
                      (setf (local-function-callable-name local-function) callable-name)
                      (setq form
                            `(multiple-value-bind (final-code final-constants)
                                 (generate-code-vector ',*code*
                                                       ',(compiland-constants compiland))
                               (set-fdefinition ',callable-name
                                                (make-compiled-function ',(flet-debug-name compiland)
                                                                        final-code
                                                                        ,minargs
                                                                        ,maxargs
                                                                        final-constants)))))))
             (dump-top-level-form form *compile-file-output-stream*)))
          (t
           ;; COMPILE
           (multiple-value-bind (code constants)
               (generate-code-vector *code* (compiland-constants compiland))
             (cond (*closure-vars*
                    (let ((ctf (make-closure-template-function (flet-debug-name compiland)
                                                               code
                                                               minargs
                                                               maxargs
                                                               constants)))
                      (aver (compiland-child-p compiland))
                      (push ctf (compiland-constants (compiland-parent compiland)))
                      (setf (local-function-ctf local-function) ctf)))
                   (t
                    (setf (local-function-function local-function)
                          (make-compiled-function (flet-debug-name compiland)
                                                  code
                                                  minargs
                                                  maxargs
                                                  constants))
                    ;; REVIEW
                    ;; We need to put the local function in the constants vector of its parent.
                    ;; Is this the right way to do it?
                    (push (local-function-function local-function)
                          (compiland-constants (compiland-parent compiland)))
                    ;;(save-local-variable-information compiled-function)
                    )))))))

(defknown labels-debug-name (compiland) list)
(defun labels-debug-name (compiland)
  (declare (type compiland compiland))
  (list 'LABELS
        (compiland-name (compiland-parent compiland))
        (compiland-name compiland)))

(defknown p2-labels-process-compiland (t) t)
(defun p2-labels-process-compiland (local-function)
  (declare (type local-function local-function))
  (let* ((compiland (local-function-compiland local-function))
         (minargs (compiland-minargs compiland))
         (maxargs (compiland-maxargs compiland))
         (*current-compiland* compiland)
         (*speed* *speed*)
         (*space* *space*)
         (*safety* *safety*)
         (*debug* *debug*)
         (*code* nil))
    (declare (type compiland compiland))
    (p2-compiland compiland)
    (cond ((compile-file-p)
           ;; COMPILE-FILE
           (aver (streamp *compile-file-output-stream*))
           (let (form)
             (cond (*closure-vars*
                    (let ((ctf-name
                           (gensym (concatenate 'string
                                                (write-to-string (labels-debug-name compiland))
                                                "-CTF-"))))
                      (setf (local-function-ctf-name local-function) ctf-name)
                      (setq form
                            `(multiple-value-bind (final-code final-constants)
                                 (generate-code-vector ',*code*
                                                       ',(compiland-constants compiland))
                               (set-fdefinition ',ctf-name
                                                (make-closure-template-function ',(labels-debug-name compiland)
                                                                                final-code
                                                                                ,minargs
                                                                                ,maxargs
                                                                                final-constants))))))
                   (t
;;                     (let ((callable-name (local-function-callable-name local-function)))
                    (let ((callable-name
                           (gensym (concatenate 'string
                                                (write-to-string (labels-debug-name compiland))
                                                "-"))))
                      (setf (local-function-callable-name local-function) callable-name)
;;                       (aver (not (null callable-name)))
                      (push callable-name (compiland-constants (compiland-parent compiland)))
;;                       (setf (local-function-callable-name local-function) callable-name)
                      (setq form
                            `(multiple-value-bind (final-code final-constants)
                                 (generate-code-vector ',*code*
                                                       ',(compiland-constants compiland))
                               (set-fdefinition ',callable-name
                                                (make-compiled-function ',(labels-debug-name compiland)
                                                                        final-code
                                                                        ,minargs
                                                                        ,maxargs
                                                                        final-constants)))))))
             (dump-top-level-form form *compile-file-output-stream*)))
          (t
           ;; COMPILE
           (multiple-value-bind (code constants)
               (generate-code-vector *code* (compiland-constants compiland))
             (cond (*closure-vars*
                    (let ((ctf (make-closure-template-function (labels-debug-name compiland)
                                                               code
                                                               minargs
                                                               maxargs
                                                               constants)))
                      (aver (compiland-child-p compiland))
                      (push ctf (compiland-constants (compiland-parent compiland)))
                      (setf (local-function-ctf local-function) ctf)))
                   (t
                    ;; no closure vars
                    (setf (local-function-function local-function)
                          (make-compiled-function (labels-debug-name compiland)
                                                  code
                                                  minargs
                                                  maxargs
                                                  constants))
                    (aver (not (null (local-function-callable-name local-function))))
                    (set-fdefinition (local-function-callable-name local-function)
                                     (local-function-function local-function))
                    ;; REVIEW
                    (push (local-function-function local-function)
                          (compiland-constants (compiland-parent compiland)))
                    (push (local-function-callable-name local-function)
                          (compiland-constants (compiland-parent compiland)))
                    ;;(save-local-variable-information compiled-function)
                    )))))))

(defun p2-char (form target)
  (when (length-eql form 3)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (type1 (derive-type arg1))
           (arg2 (%cadr args)))
      (when (and (neq type1 :unknown)
                 (subtypep type1 'SIMPLE-STRING))
        (mumble "p2-char derived type is subtypep simple-string~%")
        (p2-function-call (list '%SCHAR arg1 arg2) target)
        t))))

(defun p2-char= (form target)
  (when (length-eql form 3)
    (cond ((and (zerop *safety*)
                (> *speed* *safety*))
           (p2-eq form target)
           t)
          ((and (eq (derive-type (%cadr form))  'CHARACTER)
                (eq (derive-type (%caddr form)) 'CHARACTER))
           (p2-eq form target)
           t))))

(defun p2-char/= (form target)
  (when (length-eql form 3)
    (cond ((and (zerop *safety*)
                (> *speed* *safety*))
           (p2-neq form target)
           t)
          ((and (eq (derive-type (%cadr form))  'CHARACTER)
                (eq (derive-type (%caddr form)) 'CHARACTER))
           (p2-neq form target)
           t))))

(defun p2-coerce (form target)
  (when (length-eql form 3)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args)))
      (when (quoted-form-p arg2)
        (let ((output-type (%cadr arg2))
              (type1 (derive-type arg1)))
          (mumble "p2-coerce type1 = ~S output-type = ~S~%" type1 output-type)
          (cond ((and (eq output-type 'LIST)
                      (subtypep type1 'VECTOR))
                 (mumble "p2-coerce coerce-vector-to-list case~%")
                 (p2 (list 'COERCE-VECTOR-TO-LIST arg1) target)
                 t)
                ((and (memq output-type '(VECTOR SIMPLE-VECTOR))
                      (subtypep type1 'LIST))
                 (mumble "p2-coerce coerce-list-to-vector case~%")
                 (p2 (list 'COERCE-LIST-TO-VECTOR arg1) target)
                 t)
                ((eq output-type 'FUNCTION)
                 (mumble "p2-coerce coerce-to-function case~%")
                 (p2 (list 'COERCE-TO-FUNCTION arg1) target)
                 t)))))))

;; REVIEW
(defun p2-declare (form target)
  (declare (ignore form target))
  (mumble "~&P2-DECLARE~%")
  t)

(defknown p2-delete (t t) t)
(defun p2-delete (form target)
  (let* ((args (cdr form))
         (numargs (length args)))
    (when (>= numargs 2)
      (let* ((arg1 (%car args))
             (arg2 (%cadr args))
             (type2 (derive-type arg2)))
        (when (and (neq type2 :unknown)
                   (subtypep type2 'LIST))
          (cond ((eql numargs 2)
                 (let ((type1 (derive-type arg1)))
                   (cond ((and (neq type1 :unknown)
                               (or (fixnum-type-p type1)
                                   (eq type1 'CHARACTER)
                                   (eq type1 'SYMBOL)
                                   (subtypep type1 'structure-object)))
                          (mumble "p2-delete list-delete-eq~%")
                          (p2-function-call (list 'LIST-DELETE-EQ arg1 arg2) target)
                          t)
                         (t
                          (mumble "p2-delete list-delete-eql~%")
                          (p2-function-call (list 'LIST-DELETE-EQL arg1 arg2) target)
                          t))))
                ((and (eql numargs 4)
                      (eq (third args) :test))
                 (case (fourth args)
                   (EQ
                    (mumble "p2-delete list-delete-eq~%")
                    (p2-function-call (list 'LIST-DELETE-EQ arg1 arg2) target)
                    t)
                   (EQL
                    (mumble "p2-delete list-delete-eql~%")
                    (p2-function-call (list 'LIST-DELETE-EQL arg1 arg2) target)
                    t)))))))))

(defun p2-%dpb (form target)
  (when (check-arg-count form 4)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (arg3 (%caddr args))
           (arg4 (fourth args))
           (type2 (derive-type arg2))
           (type3 (derive-type arg3)))
      (when (and (integer-constant-value type2)
                 (integer-constant-value type3)
                 (flushable arg2)
                 (flushable arg3))
        (mumble "p2-%dpb optimized case~%")
        (let* ((size (integer-constant-value type2))
               (position (integer-constant-value type3))
               (mask (1- (ash 1 size)))
               ;;                (*print-structure* nil)
               )
          ;;           (mumble "old form = ~S~%" form)
          (setq form
                `(logior (logand ,arg4 ,(lognot (ash mask position)))
                         (ash (logand ,arg1 ,mask) ,position)))
          ;;           (mumble "new form = ~S~%" form)
          )
        (p2 form target)
        t))))

(defun p2-eql (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (type1 (derive-type arg1))
           type2)
      (cond ((or (fixnum-type-p type1)
                 (eq type1 'CHARACTER)
                 (eq type1 'SYMBOL)
                 (fixnum-type-p (setq type2 (derive-type arg2)))
                 (eq type2 'CHARACTER)
                 (eq type2 'SYMBOL))
             ;; EQL fixnums, characters and symbols are EQ
             (p2-eq form target)
             t)
            (t
             (process-2-args args :default t)
             (emit-call-2 'eql target)
             t)))))

(defknown p2-fill (t t) t)
(defun p2-fill (form target)
  ;; FIXME
  (let* ((args (cdr form))
         (numargs (length args))
         (arg1 (car args))
         (type1 (derive-type arg1)))
    (mumble "p2-fill numargs = ~S derived type is ~S~%" numargs type1)
    (cond ((and (eql numargs 2)
                (neq type1 :unknown)
                (subtypep type1 'SIMPLE-BIT-VECTOR))
           (mumble "p2-fill simple-bit-vector-fill case~%")
           (p2-function-call (list* 'SIMPLE-BIT-VECTOR-FILL args) target))
          (t
           (p2-function-call form target))))
  t)

(defknown p2-find (t t) t)
(defun p2-find (form target)
  (let* ((args (cdr form))
         (numargs (length args)))
    (when (>= numargs 2)
      (let* ((arg1 (%car args))
             (arg2 (%cadr args))
             (type1 (derive-type arg1))
             (type2 (derive-type arg2))
             test)
        (when (and (eql numargs 4)
                   (eq (%caddr args) :test))
          (setq test (fourth args)))
        (cond ((or (eql numargs 2)
                   (and (eql numargs 4)
                        (or (eq test 'eql)
                            (equal test '(function eql)))))
               (cond ((and (neq type2 :unknown)
                           (subtypep type2 'STRING))
                      ;; this call is safe
                      (process-2-args args :default t)
                      (emit-call-2 'string-find target)
                      t)
                     ((and (neq type2 :unknown)
                           (subtypep type2 'LIST))
                      (p2-function-call `(list-find-eql ,arg1 ,arg2) target)
                      t)
                     (t
                      (p2-function-call `(find-eql ,arg1 ,arg2) target)
                      t)))
              ((and (eql numargs 4)
                    (or (eq test 'char=)
                        (equal test '(function char=)))
                    (neq type2 :unknown)
                    (subtypep type2 'STRING))
               (cond ((zerop *safety*)
                      (p2-function-call `(string-find ,arg1 ,arg2) target)
                      t)
                     ((and (neq type1 :unknown)
                           (subtypep type1 'CHARACTER))
                      (p2-function-call `(string-find ,arg1 ,arg2) target)
                      t)
                     (t
                      (mumble "p2-find no optimization~%")
                      nil)))
              (t
               (mumble "p2-find no optimization~%")
               nil))))))

(defknown p2-position-eql (t t) t)
(defun p2-position-eql (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (type2 (derive-type arg2)))
      (when (neq type2 :unknown)
        (cond ((subtypep type2 'LIST)
               (p2-function-call (list 'LIST-POSITION-EQL arg1 arg2) target)
               t)
              ((subtypep type2 'VECTOR)
               ;; this call is safe
               (process-2-args args :default t)
               (emit-call-2 'vector-position-eql target)
               t)
              (t
               nil))))))

(defun p2-make-array (form target)
  (when (length-eql form 2)
    (let* ((arg (%cadr form))
           (type (derive-type arg)))
      (cond ((fixnum-type-p type)
             (p2-function-call (list 'MAKE-SIMPLE-VECTOR arg) target)
             t)
            (t
             nil)))))

(defknown p2-test-form (t t) t)
(defun p2-test-form (test-form label) ; jump to label if test fails
  (cond ((eq test-form t)
         (return-from p2-test-form :consequent))
        ((null test-form)
         (return-from p2-test-form :alternate))
        ((and (consp test-form) (< *debug* 3))
         (let* ((op (%car test-form))
                (handler (get op 'p2-test-handler))
                (result (and handler (funcall handler test-form label))))
           (when result
             (return-from p2-test-form result)))))
  ;; otherwise...
  (p2-test-form-default test-form label))

(defknown maybe-add-constraint (t) t)
(defun maybe-add-constraint (test-form)
  (when (consp test-form)
    (let* ((operator (car test-form))
           (args (cdr test-form))
           (numargs (length args))
           (arg1 (car args)) ; might be null
           (arg2 (cadr args)) ; might be null
           (arg3 (caddr args)) ; might be null
           var
           constraint)
      (when (and (var-ref-p arg1)
                 (setq var (var-ref-var arg1)))
             (let ((type (case operator
                           (%typep              (if (and (eql numargs 2) (quoted-form-p arg2))
                                                    (canonicalize-type (cadr arg2))
                                                    :unknown))
                           (bit-vector-p        'BIT-VECTOR)
                           (characterp          'CHARACTER)
                           (consp               'CONS)
                           (endp                'NULL)
                           (fixnum-typep        (if (and (fixnump arg2) (fixnump arg3))
                                                    `(INTEGER ,arg2 ,arg3)
                                                    (canonicalize-type 'FIXNUM)))
                           (fixnump             'FIXNUM)
                           (functionp           'FUNCTION)
                           (hash-table-p        'HASH-TABLE)
                           (integerp            'INTEGER)
                           (listp               'LIST)
                           (numberp             'NUMBER)
                           (sequencep           'SEQUENCE)
                           (simple-array-p      'SIMPLE-ARRAY)
                           (simple-bit-vector-p 'SIMPLE-BIT-VECTOR)
                           (simple-string-p     'SIMPLE-STRING)
                           (simple-vector-p     'SIMPLE-VECTOR)
                           (stringp             'STRING)
                           (symbolp             'SYMBOL)
                           (vectorp             'VECTOR)
                           (t                   :unknown))))
               (cond ((eq type :unknown)
                      ; nothing to do
                      )
                     ((setq constraint (find-constraint var))
                      (setq type (canonicalize-type type))
;;                       (mumble "already have constraint for ~S: ~S~%"
;;                               (var-name var) (constraint-type constraint))
;;                       (mumble "new constraint: ~S~%" type)
                      (when (subtypep type (constraint-type constraint))
;;                         (mumble "adding new constraint~%")
                        (push (make-constraint :var var :type type) *constraints*)))
                     (t
                      (setq type (canonicalize-type type))
;;                       (mumble "adding constraint var = ~S type = ~S~%" (var-name var) type)
                      (push (make-constraint :var var :type type) *constraints*))))))))

(defknown maybe-add-negative-constraint (t) t)
(defun maybe-add-negative-constraint (test-form)
  (when (consp test-form)
    (let* ((operator (car test-form))
           (args (cdr test-form))
           (numargs (length args))
           (arg1 (car args)) ; might be null
;;            (arg2 (cadr args)) ; might be null
           var
           constraint)
      (when (and (eql numargs 1)
                 (var-ref-p arg1)
                 (zerop (var-writes (setq var (var-ref-var arg1)))))
        (let ((type (case operator
                      (atom                'CONS)
                      (endp                'CONS)
                      (t                   :unknown))))
          (cond ((eq type :unknown)
                 ; nothing to do
                 )
                ((setq constraint (find-constraint var))
                 (setq type (canonicalize-type type))
                 (mumble "already have constraint for ~S: ~S~%"
                         (var-name var) (constraint-type constraint))
                 (mumble "new constraint: ~S~%" type)
                 (when (subtypep type (constraint-type constraint))
                   (mumble "adding new constraint~%")
                   (push (make-constraint :var var :type type) *constraints*)))
                (t
                 (setq type (canonicalize-type type))
                 (mumble "adding negative constraint var = ~S type = ~S~%" (var-name var) type)
                 (push (make-constraint :var var :type type) *constraints*))))))))

(defun p2-if (form target)
  (let* ((test-form (second form))
         (op (and (consp test-form) (%car test-form))))
    (case op
      ((NOT NULL)
       (p2-if-not form target))
      (OR
       (p2-if-or form target))
      (AND
       (p2-if-and form target))
      (t
       (let* ((consequent (third form))
              (alternate (fourth form))
              (LABEL1 (make-label))
              (result (p2-test-form test-form LABEL1)))
         (case result
           (:consequent
            (p2 consequent target))
           (:alternate
            (p2 alternate target))
           (t
            (let ((LABEL2 (make-label)))
              (let ((*register-contents* (copy-register-contents)) ; REVIEW
                    (*constraints* *constraints*))
                (maybe-add-constraint test-form)
                (p2 consequent target))
              (unless (eq target :return)
                (emit-jmp-short t LABEL2))
              (label LABEL1)
              (maybe-add-negative-constraint test-form)
              (p2 alternate target)
              (unless (eq target :return)
                (label LABEL2))
              (clear-register-contents)
              (clear-constraints)))))))))

(defun p2-if-not (form target)
  (let ((test-form (second form)))
    (aver (and (consp test-form) (memq (%car test-form) '(NOT NULL))))
    (setq test-form (cadr test-form))
    (let* ((consequent (third form))
           (alternate (fourth form))
           (LABEL1 (make-label))
           (LABEL2 (make-label))
           (result (p2-test-form test-form label1)))
      (case result
        (:consequent
         (p2 alternate target))
        (:alternate
         (p2 consequent target))
        (t
         (let ((*register-contents* (copy-register-contents))) ; REVIEW
           (p2 alternate target))
         (unless (eq target :return)
           (emit-jmp-short t LABEL2))
         (label LABEL1)
         (p2 consequent target)
         (unless (eq target :return)
           (label LABEL2))
         (setq *constraints* nil) ; REVIEW
         )))))

(defun install-p2-test-handler (symbol handler)
  (put symbol 'p2-test-handler handler))

(defconstant *runtime-predicates* (make-hash-table :test 'eq))

(defun define-runtime-predicate (op runtime-name)
  (setf (gethash op *runtime-predicates*) runtime-name)
  (install-p2-test-handler op 'p2-test-runtime-predicate))

(define-runtime-predicate 'classp    "RT_classp")
;; (define-runtime-predicate 'endp      "RT_endp")
(define-runtime-predicate 'functionp "RT_functionp")
;; (define-runtime-predicate 'integerp  "RT_integerp")
(define-runtime-predicate 'listp     "RT_listp")
(define-runtime-predicate 'minusp    "RT_minusp")
(define-runtime-predicate 'numberp   "RT_numberp")
(define-runtime-predicate 'plusp     "RT_plusp")
(define-runtime-predicate 'stringp   "RT_stringp")
(define-runtime-predicate 'vectorp   "RT_vectorp")

(install-p2-test-handler 'atom          'p2-test-atom)
(install-p2-test-handler 'char=         'p2-test-char=)
(install-p2-test-handler 'characterp    'p2-test-characterp)
(install-p2-test-handler 'consp         'p2-test-consp)
(install-p2-test-handler 'endp          'p2-test-endp)
(install-p2-test-handler 'eq            'p2-test-eq)
(install-p2-test-handler 'eql           'p2-test-eql)
(install-p2-test-handler 'fixnump       'p2-test-fixnump)
(install-p2-test-handler 'neq           'p2-test-neq)
(install-p2-test-handler 'symbolp       'p2-test-symbolp)
(install-p2-test-handler 'two-arg-/=    'p2-test-numeric-comparison)
(install-p2-test-handler 'two-arg-<     'p2-test-numeric-comparison)
(install-p2-test-handler 'two-arg-<=    'p2-test-numeric-comparison)
(install-p2-test-handler 'two-arg-=     'p2-test-two-arg-=)
(install-p2-test-handler 'two-arg->     'p2-test-numeric-comparison)
(install-p2-test-handler 'two-arg->=    'p2-test-numeric-comparison)
(install-p2-test-handler 'two-arg-char= 'p2-test-char=)
(install-p2-test-handler 'zerop         'p2-test-zerop)

#+x86-64
(install-p2-test-handler 'equal         'p2-test-equal)

(defun p2-locally (form target)
  (let ((*speed*  *speed*)
        (*space*  *space*)
        (*safety* *safety*)
        (*debug*  *debug*)
;;         (*explain* *explain*)
        (*inline-declarations* *inline-declarations*))
    (multiple-value-bind (body declarations)
        (parse-body (cdr form))
      (process-optimization-declarations declarations)
      (p2-progn-body body target))))

(defun p2-oddp/evenp (form target)
  (when (length-eql form 2)
    (let* ((op (%car form))
           (arg (%cadr form))
           (type (derive-type arg)))
      (when (subtypep type 'INTEGER)
        (process-1-arg arg :default t)
        (emit-call-1 op target)
        t))))

(defun p2-plusp/minusp (form target)
  (when (length-eql form 2)
    (let* ((operator (%car form))
           (arg (%cadr form))
           (type (derive-type arg)))
      (cond ((and (eq operator 'PLUSP)
                  (equal type '(INTEGER 0 1)))
             (let ((reg #+x86-64 :rax #+x86 :eax)
                   (EXIT (make-label)))
               (process-1-arg arg reg t)
               (inst :test :al :al)
               (p2-symbol t reg)
               (inst :jmp-short :nz EXIT)
               (p2-symbol nil reg)
               (label EXIT)
               (move-result-to-target target))
             t)
            ((subtypep type 'REAL)
             (process-1-arg arg :default t)
             (emit-call-1 operator target)
             t)))))

(defknown p2-progn-body (t t) t)
(defun p2-progn-body (body target)
  (declare (type list body))
  (let ((must-clear-values nil)
        (inhibit-notes *inhibit-notes*))
    (loop
      (let ((subform (car body))
            (tail (cdr body)))
        (cond ((null tail)
               ;; last form
               (when must-clear-values
                 (emit-clear-values))
               (p2 subform target)
               (return))
              (t
               ;; not the last form
               (p2 subform nil)
               (unless must-clear-values
                 (unless (single-valued-p subform)
                   (unless inhibit-notes
                     (let ((*print-structure* nil))
                       (note "P2-PROGN-BODY: not single-valued: ~S~%" subform)))
                   (setq must-clear-values t)))
               (setq body tail)))))))

(defun p2-progn (form target)
  (p2-progn-body (cdr form) target))

(defun p2-quote (form target)
  (p2-constant (cadr form) target))

(defun p2-mapc2 (form target)
  (let ((args (cdr form)))
    (when (length-eql args 2)
      (process-2-args args :default nil)
      (emit-call-2 'fast-mapc2 target)
      t)))

(defun p2-mapcar2 (form target)
  (let ((args (cdr form)))
    (when (length-eql args 2)
      (process-2-args args :default nil)
      (emit-call-2 'fast-mapcar2 target)
      t)))

(defun p2-memql (form target)
  (when (check-arg-count form 2)
    (let* ((op (%car form))
           (args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (type1 (derive-type arg1)))
      (cond ((eq type1 'SYMBOL)
             (setq op 'memq))
            ((and (consp arg2)
                  (eq (%car arg2) 'QUOTE)
                  (length-eql arg2 2)
                  (every #'symbolp (%cadr arg2)))
             (setq op 'memq)))
      (p2-function-call-2 op args target))
    t))

(defun %p2-require-type (form target required-type)
  (declare (type symbol required-type))
  (when (check-arg-count form 1)
    (let ((op (%car form))
          (arg (%cadr form))
          type)
      (cond ((zerop *safety*)
             (process-1-arg arg target t))
            ((and (neq (setq type (derive-type arg)) :unknown)
                  (subtypep type required-type))
             (process-1-arg arg target t))
            (t
             (mumble "%p2-require-type full call to ~S~%" op)
             (process-1-arg arg :default t)
             (emit-call-1 op target)
             (when (var-ref-p arg)
               (set-register-contents +call-return-register+ (var-ref-var arg))))))
    t))

(defun p2-require-character (form target)
  (%p2-require-type form target 'character))

;; (defun p2-require-cons (form target)
;;   (%p2-require-type form target 'cons))

(defun p2-require-hash-table (form target)
  (%p2-require-type form target 'hash-table))

(defun p2-require-integer (form target)
  (%p2-require-type form target 'integer))

(defun p2-require-keyword (form target)
  (%p2-require-type form target 'keyword))

(defun p2-require-number (form target)
  (%p2-require-type form target 'number))

(defun p2-require-stream (form target)
  (%p2-require-type form target 'stream))

(defun p2-require-simple-string (form target)
  (%p2-require-type form target 'simple-string))

(defun p2-require-string (form target)
  (%p2-require-type form target 'string))

;; (defun p2-require-simple-vector (form target)
;;   (%p2-require-type form target 'simple-vector))

(defun p2-require-vector (form target)
  (%p2-require-type form target 'vector))

(defun p2-require-structure-type (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           type1)
      (cond ((zerop *safety*)
             (p2 arg1 target))
            ((and (quoted-form-p arg2)
                  (neq (setq type1 (derive-type arg1)) :unknown)
                  (subtypep type1 (%cadr arg2)))
             (p2 arg1 target))
            (t
             (process-2-args args :default t)
             (emit-call-2 'require-structure-type target)
             (when (var-ref-p arg1)
               (set-register-contents +call-return-register+ (var-ref-var arg1))))))
    t))

(defun p2-reverse/nreverse (form target)
  (when (check-arg-count form 1)
    (let* ((op (%car form))
           (arg (%cadr form))
           (type (derive-type arg)))
      (cond ((and (neq type :unknown)
                  (subtypep type 'sequence))
             (process-1-arg arg :default t)
             (emit-call-1 op target)
             t)
            (t
             nil)))))

(defknown p2-sxhash (t t) t)
(defun p2-sxhash (form target)
  (when (check-arg-count form 1)
    (process-1-arg (%cadr form) :default t)
    (emit-call-1 'sxhash target)
    t))

(defknown p2-two-arg-= (t t) t)
(defun p2-two-arg-= (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args)))
      (when (and (fixnum-type-p (derive-type arg1))
                 (fixnum-type-p (derive-type arg2)))
        (p2-eq form target)
        t))))

(defknown p2-two-arg-/= (t t) t)
(defun p2-two-arg-/= (form target)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args)))
      (when (and (fixnum-type-p (derive-type arg1))
                 (fixnum-type-p (derive-type arg2)))
        (p2-neq form target)
        t))))

(defun p2-the (form target)
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args)))
    (when (var-ref-p arg2)
      (add-type-constraint (var-ref-var arg2) arg1))
    (p2 arg2 target)))

(defun p2-truly-the (form target)
  (let* ((args (cdr form))
         (arg1 (first args))
         (arg2 (second args)))
    (when (var-ref-p arg2)
      (add-type-constraint (var-ref-var arg2) arg1))
    (p2 arg2 target)))

(defknown p2-type-of (t t) t)
(defun p2-type-of (form target)
  (when (check-arg-count form 1)
    (process-1-arg (%cadr form) :default t)
    (emit-call-1 'type-of target)
    t))

(defknown p2-typep (t t) t)
(defun p2-typep (form target)
  (when (length-eql form 3)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args)))
      (cond ((eq arg2 t)
             (p2 arg1 nil) ; for effect
             (p2-constant t target)
             t)
            ((quoted-form-p arg2)
             (let ((type (canonicalize-type (cadr arg2)))
                   new-form)
;;                (let ((*print-structure* nil)
;;                      (*print-array* nil))
;;                  (mumble "p2-typep arg2 = ~S type = ~S~%" (cadr arg2) type))
               (cond ((eq type 'structure-object)
                      (setq new-form `(structure-object-p ,arg1))
                      (p2 new-form target)
                      t)
                     ((and (symbolp type)
                           (subtypep type 'structure-object))
                      (process-2-args args :default t)
                      (emit-call-2 'structure-typep target)
                      t)
                     ((fixnum-type-p type)
                        (setq new-form `(fixnum-typep ,arg1 ,(cadr type) ,(caddr type)))
                      (p2-function-call new-form target)
                      t)
                     ((integer-type-p type)
                      (setq new-form `(integer-typep ,arg1 ',type))
                      (p2-function-call new-form target)
                      t)
                     ((and (consp type)
                           (memq (%car type) '(ARRAY SIMPLE-ARRAY BIT-VECTOR)))
                      (setq new-form `(builtin-typep ,arg1 ',type))
;;                       (let ((*print-structure* nil)
;;                             (*print-array* nil))
;;                         (mumble "p2-typep new-form = ~S~%" new-form))
                      (p2-function-call new-form target)
                      t)
                     ((and (consp type)
                           (eq (%car type) 'MEMBER))
                      (cond ((length-eql (cdr type) 1)
                             (setq new-form `(eql ,arg1 ',(%cadr type))))
                            (t
                             (setq new-form `(if (MEMQL ,arg1 '(,@(cdr type))) t nil))))
;;                       (let ((*print-structure* nil)
;;                             (*print-array* nil))
;;                         (mumble "p2-typep form = ~S new-form = ~S~%" form new-form))
                      (p2 new-form target)
                      t)
                     (t
;;                       (let ((*print-structure* nil)
;;                             (*print-array* nil))
;;                         (mumble "p2-typep full call arg2 = ~S type = ~S~%" arg2 type))
                      nil))))
            (t
;;              (let ((*print-structure* nil)
;;                    (*print-array* nil))
;;                (mumble "p2-typep full call arg2 = ~S~%" arg2))
             nil)))))

(defknown p2-vector2 (t t) t)
(defun p2-vector2 (form target)
  (when (check-arg-count form 2)
    (process-2-args (%cdr form) :default t)
    (emit-call-2 'VECTOR2 target)
    t))

(defknown p2-vector3 (t t) t)
(defun p2-vector3 (form target)
  (when (check-arg-count form 3)
    (process-3-args (%cdr form) :default t)
    (emit-call-3 'VECTOR3 target)
    t))

(defknown p2 (t t) t)
(defun p2 (form target)
  (cond ((consp form)
         (let* ((op (%car form))
                (handler (and (symbolp op) (get op 'p2-handler)))
                handled)
           (cond ((special-operator-p op)
                  (unless handler
                    (compiler-unsupported "P2: unsupported special operator ~S" op))
                  (funcall handler form target))
                 ((notinline-p op)
                  (p2-function-call form target))
                 ((and handler (< *debug* 3))
                  (setq handled (funcall handler form target))
;;                   (unless (or (eq handled t) (eq handled nil))
;;                     (mumble "broken handler ~S~%" handler)
;;                     (break))
                  (unless handled
                    (p2-function-call form target))
                  )
                 (t
                  (p2-function-call form target)))
;;            (unless (eq op 'setq)
;;              (clear-register-contents)) ; FIXME
           ))
        ((var-ref-p form)
         (p2-var-ref form target))
        ((constantp form)
         (p2-constant form target)
;;          (clear-register-contents) ; FIXME
         ))
  t)

(defknown p2-test-char= (t t) t)
(defun p2-test-char= (test-form label)
  (when (and (eql (length test-form) 3)
             (zerop *safety*)
             (> *speed* *safety*))
    (p2-test-eq test-form label)))

(defvar *available-registers* nil)

(defknown initialize-available-registers () t)
(defun initialize-available-registers ()
  #+x86-64
  (setq *available-registers* (list :rbx :r13))
  #+x86
  (setq *available-registers* (list :ebx)))

(defun get-available-register ()
  (pop *available-registers*))

(defun assign-registers-for-locals (compiland)
  (when *available-registers*
    (unless (or (compiland-unwind-protect-p compiland)
                (compiland-setjmp-p compiland)
                (compiland-longjmp-p compiland))
;;       (let ((locals (reverse *local-variables*)))
      (let ((locals (copy-list *local-variables*)))
;;         (dolist (var locals)
;;           (setf (var-register-worthiness var)
;;                 (+ (var-register-worthiness var)
;;                    (var-reads var)
;;                    (var-writes var))))
;;         (setq locals (sort locals #'> :key #'var-register-worthiness))
;;         (dolist (var locals)
;;           (mumble "~S ~S~%" (var-name var) (var-register-worthiness var)))
        (dolist (var locals)
          (declare (type var var))
          (unless (or (var-special-p var)
                      (var-used-non-locally-p var))
            (aver (eq (var-compiland-id var) (compiland-id compiland)))
            (aver (null (var-index var)))
            (aver (null (var-register var)))
            (when (>= (var-register-worthiness var) 100)
              (let ((reg (get-available-register)))
                (cond (reg
                       (setf (var-register var) reg)
;;                        (mumble "assign-registers-for-locals var = ~S reg = ~S~%" (var-name var) reg)
                       (push reg (compiland-registers-to-be-saved compiland)))
                      (t
                       ;; we've run out of available registers
                       (return)))))))))))
