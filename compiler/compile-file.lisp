;;; compile-file.lisp
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

(in-package "SYSTEM")

(resolve 'compile)

(in-package "COMPILER")

(require "DUMP-FORM")

(defmacro report-error (&rest forms)
  `(handler-case (progn ,@forms)
     (compiler-unsupported-feature-error (condition)
       (format t "~&~%; UNSUPPORTED-FEATURE: ~A~%" condition)
       nil)))

;;; Adapted from SBCL.
;;; Parse an EVAL-WHEN situations list, returning three flags,
;;; (VALUES COMPILE-TOPLEVEL LOAD-TOPLEVEL EXECUTE), indicating
;;; the types of situations present in the list.
(defun parse-eval-when-situations (situations)
  (when (or (not (listp situations))
	    (set-difference situations
			    '(:compile-toplevel
			      compile
			      :load-toplevel
			      load
			      :execute
			      eval)))
    (error "Bad EVAL-WHEN situation list: ~S." situations))
  (values (intersection '(:compile-toplevel compile) situations)
	  (intersection '(:load-toplevel load) situations)
	  (intersection '(:execute eval) situations)))

(defun process-top-level-macrolet (form stream compile-time-too)
  (let ((*compile-file-environment* (make-environment *compile-file-environment*)))
    (dolist (definition (cadr form))
      (environment-add-macro-definition
       *compile-file-environment*
       (car definition)
       (make-macro (car definition)
                   (coerce-to-function (make-expander-for-macrolet definition)))))
    (dolist (body-form (cddr form))
      (process-top-level-form body-form stream compile-time-too))))

(defun process-top-level-progn (forms stream compile-time-too)
  (dolist (form forms)
    (process-top-level-form form stream compile-time-too)))

(defknown note-top-level-form (t) t)
(defun note-top-level-form (form)
  (when *compile-print*
    (when *last-error-context*
      (terpri)
      (setq *last-error-context* nil))
    (fresh-line)
    (princ "; ")
    (let ((*print-length* 2)
          (*print-level* 2)
          (*print-pretty* nil)
          (*print-structure* nil))
      (prin1 form))
    (terpri)))

(defknown process-defconstant (t stream) t)
(defun process-defconstant (form stream)
  (cond (;;(structure-object-p (third form))
         (typep (third form) 'structure-object)
         (multiple-value-bind (creation-form initialization-form)
             (make-load-form (third form))
           (declare (ignore initialization-form))
           (dump-top-level-form `(DEFCONSTANT ,(second form) ,creation-form) stream)))
        (t
         (dump-top-level-form form stream)))
  (%stream-terpri stream)
  ;; "If a DEFCONSTANT form appears as a top level form, the compiler
  ;; must recognize that [the] name names a constant variable. An
  ;; implementation may choose to evaluate the value-form at compile
  ;; time, load time, or both. Therefore, users must ensure that the
  ;; initial-value can be evaluated at compile time (regardless of
  ;; whether or not references to name appear in the file) and that
  ;; it always evaluates to the same value."
  (eval form))

(defknown process-defun (t t t) t)
(defun process-defun (form stream compile-time-too)
  (note-top-level-form form)
  (when compile-time-too
    (eval form))
  (let* ((name (cadr form))
         (block-name (fdefinition-block-name name))
         (lambda-list (caddr form))
         (*speed* *speed*)
         (*space* *space*)
         (*safety* *safety*)
         (*debug* *debug*))
    (multiple-value-bind (body declarations doc)
        (parse-body (cdddr form))
      (declare (ignore doc)) ; FIXME
      (let* ((lambda-expression
              `(lambda ,lambda-list ,@declarations (block ,block-name ,@body))))
        (multiple-value-bind (code minargs maxargs constants l-v-info)
            (report-error (c::compile-defun-for-compile-file name lambda-expression))
          (cond (code
                 (setq form
                       `(multiple-value-bind (final-code final-constants)
                            (c::generate-code-vector ',code ',constants)
                          (set-fdefinition ',name
                                           (make-compiled-function ',name
                                                                   final-code
                                                                   ,minargs
                                                                   ,maxargs
                                                                   final-constants))
                          (setq *source-position* ,*source-position*)
                          (record-source-information ',name)
                          (set-local-variable-information #',name ',l-v-info))))
                (t
                 ;; FIXME This should be a warning or error of some sort...
                 (format t "~&~%; Unable to compile function ~A~%" name)
                 (let ((precompiled-function (precompile-form lambda-expression)))
                   (setq form
                         `(progn
                            (set-fdefinition ',name ,precompiled-function)
                            (setq *source-position* ,*source-position*)
                            (record-source-information ',name))))))))
      (when (inline-p name)
        ;; FIXME Need to support SETF functions too!
        (set-inline-expansion name
                              (generate-inline-expansion block-name
                                                         lambda-list
                                                         declarations
                                                         body))
        (dump-form `(set-inline-expansion ',name ',(inline-expansion name)) stream)
        (%stream-terpri stream)))
    (push name *functions-defined-in-current-file*)
    (note-name-defined name))
  (dump-top-level-form form stream)
  t)

(defknown process-defmacro (t t t) t)
(defun process-defmacro (form stream)
  (note-top-level-form form)
  (eval form)
  (let* ((name (second form))
         (lambda-expression (function-lambda-expression (macro-function name))))
    (multiple-value-bind (code minargs maxargs constants l-v-info)
        (report-error (c::compile-defun-for-compile-file name lambda-expression))
      (cond (code
             (setq form
                   `(multiple-value-bind (final-code final-constants)
                        (c::generate-code-vector ',code ',constants)
                      (set-macro-function ',name
                                          (make-compiled-function ',name
                                                                  final-code
                                                                  ,minargs
                                                                  ,maxargs
                                                                  final-constants))
                      (setq *source-position* ,*source-position*)
                      (record-source-information ',name)
                      (set-local-variable-information (macro-function ',name) ',l-v-info))))
            (t
             ;; FIXME this should be a warning or error of some sort
             (format t "~&~%; Unable to compile macro ~A~%" name)
             (setq form
                   `(progn
                      (set-macro-function ',name ,lambda-expression)
                      (setq *source-position* ,*source-position*)
                      (record-source-information ',name)))))))
  (dump-top-level-form form stream)
  t)

(defun convert-toplevel-form (form)
;;   (let* ((expr `(lambda () ,form))
;;          (classfile-name (next-classfile-name))
;;          (classfile (report-error (jvm:compile-defun nil expr nil classfile-name)))
;;          (compiled-function (verify-load classfile)))
;;     (setf form
;;           (if compiled-function
;;               `(funcall (load-compiled-function ,(file-namestring classfile)))
;;               (precompile-form form nil))))
;;   (mumble "converting top-level form~%")
;;   (pprint form)
;;   (terpri)

  (let ((lambda-expression `(lambda () ,form))
        (name (gensym)))
    (multiple-value-bind (code minargs maxargs constants)
        (report-error (c::compile-defun-for-compile-file name lambda-expression))
      (cond (code
             (setq form
                   `(multiple-value-bind (final-code final-constants)
                        (c::generate-code-vector ',code ',constants)
                      (set-fdefinition ',name
                                       (make-compiled-function ',name
                                                               final-code
                                                               ,minargs
                                                               ,maxargs
                                                               final-constants))
;;                       (setq *source-position* ,*source-position*)
;;                       (record-source-information ',name)
                      (funcall ',name)
;;                       (fmakunbound ',name)
                      )))
            (t
             ;; FIXME This should be a warning or error of some sort...
             (format t "~&~%; Unable to convert top-level form~%" name)
;;              (let ((precompiled-function (precompile-form lambda-expression)))
;;                (setq form
;; ;;                      `(progn
;; ;;                         (set-fdefinition ',name ,precompiled-function)
;; ;;                         (setq *source-position* ,*source-position*)
;; ;;                         (record-source-information ',name))
;;                      `(funcall ,precompiled-function)
;;                      ))
             (setq form (precompile-form form))
             ))))
  form
  )

(defun process-top-level-form (form stream compile-time-too)
  (when (atom form)
    ;; REVIEW support symbol macros
    (when compile-time-too
      (eval form))
    (return-from process-top-level-form))
  (let ((original-form form)
        (operator (%car form)))
    (case operator
      (DEFUN
       (process-defun form stream compile-time-too)
       (return-from process-top-level-form))
      (DEFMACRO
       (process-defmacro form stream)
       (return-from process-top-level-form))
      (MACROLET
       (process-top-level-macrolet form stream compile-time-too)
       (return-from process-top-level-form))
      ((IN-PACKAGE %IN-PACKAGE)
       (note-top-level-form form)
       (aver (length-eql form 2))
       (setq form `(%in-package ,(string (%cadr form))))
       (dump-top-level-form form stream)
       (eval form)
       (return-from process-top-level-form))
      (DEFPACKAGE
       (note-top-level-form form)
       (setq form (precompile-form form))
       (eval form)
       (dump-top-level-form form stream)
       (return-from process-top-level-form))
      ((DEFVAR DEFPARAMETER)
       (note-top-level-form form)
       (let ((name (second form)))
         (setq form (precompile-form form))
         (dump-top-level-form form stream)
         (if compile-time-too
             (eval form)
             ;; "If a DEFVAR or DEFPARAMETER form appears as a top level form,
             ;; the compiler must recognize that the name has been proclaimed
             ;; special. However, it must neither evaluate the initial-value
             ;; form nor assign the dynamic variable named NAME at compile
             ;; time."
             (%defvar name)))
       (return-from process-top-level-form))
      (DEFCONSTANT
       (note-top-level-form form)
       (process-defconstant form stream)
       (return-from process-top-level-form))
      ((DEFGENERIC DEFMETHOD)
       (note-top-level-form form)
       (note-name-defined (cadr form))
       (let ((*compile-print* nil))
         (process-top-level-form (macroexpand-1 form *compile-file-environment*)
                                 stream compile-time-too))
       (return-from process-top-level-form))
      (DEFTYPE
       (note-top-level-form form)
       (dump-top-level-form form stream)
       (eval form)
       (return-from process-top-level-form))
      (EVAL-WHEN
       (multiple-value-bind (ct lt e)
           (parse-eval-when-situations (cadr form))
         (let ((new-compile-time-too (or ct
                                         (and compile-time-too e)))
               (body (cddr form)))
           (cond (lt
                  (process-top-level-progn body stream new-compile-time-too))
                 (new-compile-time-too
                  (eval `(progn ,@body)))))
         (return-from process-top-level-form)))
      (LOCALLY
       ;; FIXME Need to handle special declarations too!
       (let ((*speed* *speed*)
             (*safety* *safety*)
             (*debug* *debug*)
             (*space* *space*)
             (*inline-declarations* *inline-declarations*))
         (multiple-value-bind (forms decls)
             (parse-body (cdr form) nil)
           (process-optimization-declarations decls)
           (process-top-level-progn forms stream compile-time-too)
           (return-from process-top-level-form))))
      (PROGN
       (process-top-level-progn (cdr form) stream compile-time-too)
       (return-from process-top-level-form))
      (DECLARE
       (compiler-style-warn "Misplaced declaration: ~S" form))
      (t
;;               (format t "falling through, form = ~S~%" form)
       (when (and (symbolp operator)
                  (macro-function operator *compile-file-environment*))
         (note-top-level-form form)
         ;; Note that we want MACROEXPAND-1 and not MACROEXPAND here, in
         ;; case the form being expanded expands into something that needs
         ;; special handling by PROCESS-TOP-LEVEL-FORM (e.g. DEFMACRO).
         (let ((*compile-print* nil))
           (process-top-level-form (macroexpand-1 form *compile-file-environment*)
                                   stream compile-time-too))
         (return-from process-top-level-form))

       (cond ((eq operator 'QUOTE)
              (return-from process-top-level-form))
;;              ((eq operator 'COMPILER-DEFSTRUCT)
;;               (setq form (precompile-form form)))
;;              ((and (memq operator '(EXPORT REQUIRE PROVIDE SHADOW))
;;                    (or (keywordp (second form))
;;                        (and (listp (second form))
;;                             (eq (first (second form)) 'QUOTE))))
;;               (setq form (precompile-form form)))
;;              ((and (eq operator '%SET-FDEFINITION)
;;                    (eq (car (second form)) 'QUOTE)
;;                    (consp (third form))
;;                    (eq (%car (third form)) 'FUNCTION)
;;                    (symbolp (cadr (third form))))
;;               (setq form (precompile-form form)))
             ((memq operator '(LET LET*))
              (let ((body (cddr form)))
                (cond ((dolist (subform body nil)
                         (when (and (consp subform) (eq (%car subform) 'DEFUN))
                           (return t)))
                       (setq form (convert-toplevel-form form)))
                      (t
                       (setq form (precompile-form form))))))
;;                     ((eq operator 'mop::ensure-method)
;;                      (setf form (convert-ensure-method form)))
;;              ((and (symbolp operator)
;;                    (not (special-operator-p operator))
;;                    (null (cdr form)))
;;               (setq form (precompile-form form)))
             (t
              (note-top-level-form form)
              ;; REVIEW convert-toplevel-form
              (setq form (precompile-form form))))
       (when (consp form)
         (dump-top-level-form form stream))
       (when compile-time-too
         (eval original-form))))))

(defun %compile-file (input-file output-file external-format)
  (declare (ignore external-format)) ; FIXME
  (unless (or (and (probe-file input-file) (not (file-directory-p input-file)))
              (pathname-type input-file))
    (let ((pathname (merge-pathnames (make-pathname :type "lisp") input-file)))
      (when (probe-file pathname)
        (setf input-file pathname))))
  (setf output-file (if output-file
                        (merge-pathnames output-file *default-pathname-defaults*)
                        (compile-file-pathname input-file)))
  (let* ((type (pathname-type output-file))
         (temp-file (merge-pathnames (make-pathname :type (concatenate 'string type "-tmp"))
                                     output-file))
         (warnings-p t)
         (failure-p t))
    (with-open-file (in input-file :direction :input)
      (let* ((c:*output-mode* :compile-file)
             (*compile-file-pathname* (pathname in))
             (*compile-file-truename* (truename in))
             (*source-file* *compile-file-truename*)
             (namestring (namestring *compile-file-truename*))
             (start (get-internal-real-time))
             elapsed)
        (when *compile-verbose*
          (format t "; Compiling ~A ...~%" namestring))
        (with-compilation-unit ()
          (with-open-file (out temp-file :direction :output :if-exists :supersede)
            (let ((*readtable* *readtable*)
                  (*package* *package*)
                  (*speed* *speed*)
                  (*space* *space*)
                  (*safety* *safety*)
                  (*debug* *debug*)
                  (*compile-file-output-stream* out)
;;                   (*explain* *explain*)
                  (*functions-defined-in-current-file* nil))
              (write "; -*- Mode: Lisp -*-" :escape nil :stream out)
              (%stream-terpri out)
              (let ((*standard-output* out))
                (describe-compiler-policy))
              (%stream-terpri out)
              (let ((*package* +keyword-package+)) ; make sure package prefix is printed
                (dump-top-level-form '(init-fasl nil) out)
                (dump-top-level-form `(setq *source-file* ,*compile-file-truename*) out))
              (loop
                (let* ((*source-position* (file-position in))
                       (form (read in nil in))
                       (*compiler-error-context* form))
                  (when (eq form in)
                    (return))
                  (process-top-level-form form out nil)))))
          (cond ((zerop (+ *errors* *warnings* *style-warnings*))
                 (setq warnings-p nil
                       failure-p  nil))
                ((zerop (+ *errors* *warnings*))
                 (setq failure-p nil))))

        (rename-file temp-file output-file)

        (setq elapsed (/ (- (get-internal-real-time) start) (float internal-time-units-per-second)))
        (when *compile-verbose*
          (format t "~&; Wrote ~A (~A seconds)~%" (namestring output-file) elapsed))))
    (values (truename output-file) warnings-p failure-p)))

(defun compile-file (input-file
                     &key
                     output-file
                     ((:verbose *compile-verbose*) *compile-verbose*)
                     ((:print *compile-print*) *compile-print*)
                     external-format)
  (loop
    (restart-case
        (return (%compile-file input-file output-file external-format))
      (retry ()
        :report (lambda (stream) (format stream "Retry compiling ~S" input-file))
        nil)
      (skip ()
        :report (lambda (stream) (format stream "Skip compiling ~S" input-file))
        (return)))))

;; From ABCL.
(defun compile-file-if-needed (input-file &rest allargs &key force-compile)
  (setf input-file (truename input-file))
  (cond (force-compile
         (remf allargs :force-compile)
         (apply 'compile-file input-file allargs))
        (t
         (let* ((source-write-time (file-write-date input-file))
                (output-file       (or (getf allargs :output-file)
                                       (compile-file-pathname input-file)))
                (target-write-time (and (probe-file output-file)
                                        (file-write-date output-file))))
           (if (or (null target-write-time)
                   (<= target-write-time source-write-time))
               (apply 'compile-file input-file allargs)
               output-file)))))
