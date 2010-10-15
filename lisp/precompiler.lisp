;;; precompiler.lisp
;;;
;;; Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

(in-package "EXTENSIONS")

;; PRECOMPILE is exported in symdefs.hpp.
(export '(precompile-form precompile-system))

(in-package "SYSTEM")

(export '(parse-defmacro %subtypep))

(export '(*inline-declarations*
          process-optimization-declarations
          inline-p notinline-p expand-inline))

(defvar *inline-declarations* nil)

(export '*compile-file-environment*)

(defvar *compile-file-environment* nil)

(export '*compile-file-output-stream*)

(defvar *compile-file-output-stream* nil)

(export '(*force-type-checks* *force-full-calls*))

(defvar *force-type-checks* nil)

(defvar *force-full-calls* nil)

(defun process-optimization-declarations (forms)
  (dolist (form forms)
    (unless (and (consp form) (eq (%car form) 'DECLARE))
      (return))
    (dolist (decl (%cdr form))
      (case (car decl)
        (OPTIMIZE
         (dolist (spec (%cdr decl))
           (let ((val 3)
                 (quality spec))
             (when (consp spec)
               (setq quality (%car spec)
                     val (cadr spec)))
             (when (and (fixnump val)
                        (<= 0 val 3))
               (case quality
                 (speed
                  (setq *speed* val))
                 (safety
                  (setq *safety* val))
                 (debug
                  (setq *debug* val))
                 (space
                  (setq *space* val))
                 (compilation-speed) ;; Ignored.
                 (t
                  (compiler-warn "Ignoring unknown optimization quality ~S in ~S." quality decl)))))))
        ((INLINE NOTINLINE)
         (dolist (symbol (%cdr decl))
           (push (cons symbol (%car decl)) *inline-declarations*)))
;;         (:explain
;;          (dolist (spec (%cdr decl))
;;            (let ((val t)
;;                  (quality spec))
;;              (when (consp spec)
;;                (setf quality (%car spec))
;;                (when (= (length spec) 2)
;;                  (setf val (%cadr spec))))
;;              (if val
;;                  (pushnew quality *explain*)
;;                  (setf *explain* (remove quality *explain*))))))
        )))
  t)

(defun inline-p (name)
  (declare (optimize speed))
  (let ((entry (assoc name *inline-declarations*)))
    (if entry
        (eq (cdr entry) 'INLINE)
        (and (symbolp name) (eq (get name '%inline) 'INLINE)))))

(defun notinline-p (name)
  (declare (optimize speed))
  (let ((entry (assoc name *inline-declarations*)))
    (if entry
        (eq (cdr entry) 'NOTINLINE)
        (and (symbolp name) (eq (get name '%inline) 'NOTINLINE)))))

(defun expand-inline (form expansion)
;;   (format t "expand-inline form = ~S~%" form)
;;   (format t "expand-inline expansion = ~S~%" expansion)
  (let* (;(op (car form))
;;          (proclaimed-ftype (proclaimed-ftype op))
         (args (cdr form))
         (vars (cadr expansion))
         (varlist nil))
    ;;     (format t "op = ~S proclaimed-ftype = ~S~%" op (proclaimed-ftype op))
    (when (eql (length vars) (length args)) ; otherwise wrong number of arguments
      (do ((vars vars (cdr vars))
           (args args (cdr args)))
          ((null vars))
        (push (list (car vars) (car args)) varlist))
      (setq form (list* 'LET (nreverse varlist)
                        (copy-tree (cddr expansion)))))
;;     (when proclaimed-ftype
;;       (let ((result-type (ftype-result-type proclaimed-ftype)))
;;         (when (and result-type
;;                    (neq result-type t)
;;                    (neq result-type '*))
;;           (setf new-form (list 'TRULY-THE result-type new-form)))))
;;     (format t "expand-inline new form = ~S~%" form)
    form))

(defvar *compiling* nil)

(export '*compiling*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "PRECOMPILER")
    (make-package "PRECOMPILER"
                  :nicknames '("PRE")
                  :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM"))))

(in-package "PRECOMPILER")

(defvar *local-variables* nil)

;; We don't have DEFSTRUCT yet, so we use a 2-element list to represent an
;; ordinary variable.
(defun make-var (name)
  (list name :variable))

(defun find-var (name)
  (dolist (var *local-variables*)
    (when (eq name (%car var))
      (return var))))

(defvar *blocks* nil)

(defun precompile-default (form)
  (cons (%car form) (mapcar #'precompile1 (%cdr form))))

(defun precompile-defparameter (form)
  (list* 'DEFPARAMETER (cadr form) (mapcar #'precompile1 (cddr form))))

(defun precompile-defvar (form)
  (list* 'DEFVAR (cadr form) (mapcar #'precompile1 (cddr form))))

(defun precompile-block (form)
  (if *compiling*
      (let ((args (cdr form)))
        (if (null (cdr args))
            nil
            (list* 'BLOCK (car args) (mapcar #'precompile1 (cdr args)))))
      (let* ((block-name (cadr form))
             (entry (cons block-name nil))
             (*blocks* (cons entry *blocks*))
             (body (mapcar #'precompile1 (cddr form))))
        (cond ((%cdr entry)
               (list* 'BLOCK block-name body))
              ((null (cdr body))
               (car body))
              (t
               (list* 'PROGN body))))))

(defun precompile-return (form)
  (precompile-return-from (list* 'RETURN-FROM nil (%cdr form))))

(defun precompile-return-from (form)
  (if *compiling*
      (list 'RETURN-FROM (cadr form) (precompile1 (caddr form)))
      (let* ((block-name (cadr form))
             (entry (assq block-name *blocks*)))
        (unless entry
          (error "No block named ~S is currently visible." block-name))
        (rplacd entry t)
        (list 'RETURN-FROM block-name (precompile1 (caddr form))))))

(defun precompile-car (form)
  (if (length-eql form 2)
      (let ((arg (precompile1 (%cadr form))))
        (if (and (consp arg)
                 (eq (%car arg) 'CDR)
                 (length-eql arg 2))
            (list 'CADR (%cadr arg))
            (list 'CAR arg)))
      ;; wrong number of arguments
      form))

(defun precompile-case/ecase (form)
  (let* ((keyform (cadr form))
         (clauses (cddr form))
         (result (list (precompile1 keyform))))
    (dolist (clause clauses)
      (push (precompile-case-clause clause) result))
    (cons (car form) (nreverse result))))

(defun precompile-case-clause (clause)
  (let ((keys (car clause))
        (forms (cdr clause)))
    (cons keys (mapcar #'precompile1 forms))))

(defun precompile-cond (form)
  (if *compiling*
      (precompile1 (macroexpand form))
      (let ((clauses (cdr form))
            (result nil))
        (dolist (clause clauses)
          (push (cons (precompile1 (car clause)) (mapcar #'precompile1 (%cdr clause))) result))
        (cons 'COND (nreverse result)))))

(defun precompile-do/do*-vars (varlist)
  (let ((result nil))
    (dolist (varspec varlist)
      (if (atom varspec)
          (push varspec result)
          (case (length varspec)
            (1
             (push (%car varspec) result))
            (2
             (let* ((var (%car varspec))
                    (init-form (%cadr varspec)))
               (unless (symbolp var)
                 (error 'type-error)) ;; FIXME
               (push (list var (precompile1 init-form))
                     result)))
            (3
             (let* ((var (%car varspec))
                    (init-form (%cadr varspec))
                    (step-form (%caddr varspec)))
               (unless (symbolp var)
                 (error 'type-error)) ;; FIXME
               (push (list var (precompile1 init-form) (precompile1 step-form))
                     result))))))
    (nreverse result)))

(defun precompile-do/do*-end-form (end-form)
  (let ((end-test-form (car end-form))
        (result-forms (%cdr end-form)))
    (list* (precompile1 end-test-form) (mapcar #'precompile1 result-forms))))

(defun precompile-do/do* (form)
  (if *compiling*
      (precompile1 (macroexpand form *compile-file-environment*))
      (let ((*blocks* (acons nil nil *blocks*)))
        (list* (car form)
               (precompile-do/do*-vars (cadr form))
               (precompile-do/do*-end-form (caddr form))
               (mapcar #'precompile1 (cdddr form))))))

(defun precompile-dolist (form)
  (cons 'DOLIST
        (cons (mapcar #'precompile1 (cadr form))
              (let ((*blocks* (acons nil nil *blocks*)))
                (mapcar #'precompile1 (%cddr form))))))

(defun precompile-dotimes (form)
  (if *compiling*
      (precompile1 (macroexpand form *compile-file-environment*))
      (cons 'DOTIMES
            (cons (mapcar #'precompile1 (cadr form))
                  (let ((*blocks* (acons nil nil *blocks*)))
                    (mapcar #'precompile1 (%cddr form)))))))

(defun precompile-eval-when (form)
  (list* 'EVAL-WHEN (cadr form) (mapcar #'precompile1 (%cddr form))))

(defun precompile-funcall (form)
  (let* ((args (cdr form))
         (callee (car args)))
    (when (and (consp callee)
               (eq (%car callee) 'function)
               (symbolp (cadr callee))
               (not (special-operator-p (%cadr callee)))
               (not (macro-function (%cadr callee) *compile-file-environment*))
               (memq (symbol-package (%cadr callee))
                     (list +common-lisp-package+ (find-package "SYSTEM"))))
      (setq form `(,(%cadr callee) ,@(%cdr args)))))
  (precompile-default form))

(defun precompile-function (form)
  (if (and (consp (cadr form)) (eq (caadr form) 'LAMBDA))
      (list 'FUNCTION (precompile1 (%cadr form)))
      form))

(defun precompile-if (form)
  (let* ((args (cdr form))
         (numargs (length args)))
    (case numargs
      (2
       (list 'IF (precompile1 (%car args)) (precompile1 (%cadr args)) nil))
      (3
       (list 'IF (precompile1 (%car args)) (precompile1 (%cadr args)) (precompile1 (%caddr args))))
      (t
       (error 'program-error
              :format-control
              "Wrong number of arguments for special operator ~S (expected 2-3, but received ~D)."
              :format-arguments
              (list 'IF numargs))))))

(defun rewrite-aux-vars-process-decls (forms arg-vars aux-vars)
  (declare (ignore aux-vars))
  (let ((lambda-decls nil)
        (let-decls nil))
    (dolist (form forms)
      (unless (and (consp form) (eq (car form) 'DECLARE)) ; shouldn't happen
        (return))
      (dolist (decl (cdr form))
        (case (car decl)
          ((OPTIMIZE DECLARATION DYNAMIC-EXTENT FTYPE INLINE NOTINLINE)
           (push (list 'DECLARE decl) lambda-decls))
          (SPECIAL
           (dolist (name (cdr decl))
             (if (memq name arg-vars)
                 (push (list 'DECLARE (list 'SPECIAL name)) lambda-decls)
                 (push (list 'DECLARE (list 'SPECIAL name)) let-decls)
                 )))
          (TYPE
           (dolist (name (cddr decl))
             (if (memq name arg-vars)
                 (push (list 'DECLARE (list 'TYPE (cadr decl) name)) lambda-decls)
                 (push (list 'DECLARE (list 'TYPE (cadr decl) name)) let-decls)
                 )))
          (t
           (dolist (name (cdr decl))
             (if (memq name arg-vars)
                 (push (list 'DECLARE (list (car decl) name)) lambda-decls)
                 (push (list 'DECLARE (list (car decl) name)) let-decls)
                 ))))))
    (setq lambda-decls (nreverse lambda-decls))
    (setq let-decls (nreverse let-decls))
    (values lambda-decls let-decls)))

(defun rewrite-aux-vars (form)
  (multiple-value-bind (body decls doc)
      (parse-body (cddr form))
    (declare (ignore doc)) ; FIXME
    (let* ((lambda-list (cadr form))
           (lets (cdr (memq '&AUX lambda-list)))
           aux-vars)
      (dolist (form lets)
        (cond ((consp form)
               (push (%car form) aux-vars))
              (t
               (push form aux-vars))))
      (setq aux-vars (nreverse aux-vars))
      (setq lambda-list (subseq lambda-list 0 (position '&AUX lambda-list)))
      (multiple-value-bind (lambda-decls let-decls)
          (rewrite-aux-vars-process-decls decls (lambda-list-names lambda-list) aux-vars)
        `(lambda ,lambda-list ,@lambda-decls (let* ,lets ,@let-decls ,@body))
        ))))

;; Returns list of declared specials.
(defun process-special-declarations (forms)
  (let ((specials nil))
    (dolist (form forms)
      (unless (and (consp form) (eq (%car form) 'DECLARE))
        (return))
      (let ((decls (%cdr form)))
        (dolist (decl decls)
          (when (eq (car decl) 'SPECIAL)
            (setq specials (append (%cdr decl) specials))))))
    specials))

(defun maybe-rewrite-lambda (form)
  (let* ((lambda-list (cadr form)))
    (when (memq '&AUX lambda-list)
      (setq form (rewrite-aux-vars form))
      (setq lambda-list (cadr form)))
    (multiple-value-bind (body decls doc)
        (parse-body (cddr form))
      (let* ((declared-specials (process-special-declarations decls))
             (specials nil))
        ;; scan for specials
        (let ((keyp nil))
          (dolist (var lambda-list)
            (cond ((eq var '&KEY)
                   (setq keyp t))
                  ((atom var)
                   (when (or (special-variable-p var) (memq var declared-specials))
                     (push var specials)))
                  ((not keyp) ;; e.g. "&optional (x 42)"
                   (setq var (%car var))
                   (when (or (special-variable-p var) (memq var declared-specials))
                     (push var specials)))
                  ;; keyword parameters
                  ((atom (%car var)) ;; e.g. "&key (x 42)"
                   (setq var (%car var))
                   (when (or (special-variable-p var) (memq var declared-specials))
                     (push var specials)))
                  (t
                   ;; e.g. "&key ((:x x) 42)"
                   (setq var (second (%car var))) ;; x
                   (when (or (special-variable-p var) (memq var declared-specials))
                     (push var specials))))))
        (when specials
          (setq lambda-list (copy-tree lambda-list))
          ;; for each special...
          (dolist (special specials)
            (let ((sym (gensym)))
              (let ((res nil)
                    (keyp nil))
                ;; walk through the lambda list and replace each occurrence
                (dolist (var lambda-list)
                  (cond ((eq var '&KEY)
                         (setq keyp t))
                        ((atom var)
                         (when (eq var special)
                           (setq var sym)))
                        ((not keyp) ;; e.g. "&optional (x 42)"
                         (when (eq (%car var) special)
                           (setf (car var) sym)))
                        ((atom (%car var)) ;; e.g. "&key (x 42)"
                         (when (eq (%car var) special)
                           (setf (car var) (list (make-keyword special) sym))))
                        (t
                         ;; e.g. "&key ((:x x) 42)"
                         (when (eq (second (%car var)) special)
                           (setf (second (%car var)) sym))))
                  (push var res))
                (setq lambda-list (nreverse res)))
              (setq body `((let* ((,special ,sym)) (declare (special ,special)) ,@body))))))
        `(lambda ,lambda-list ,@decls ,@(when doc `(,doc)) ,@body)))))

(defun precompile-lambda (form)
  (setq form (maybe-rewrite-lambda form))
  (let ((body (cddr form))
        (*speed* *speed*)
        (*safety* *safety*)
        (*debug* *debug*)
        (*space* *space*)
        (*inline-declarations* *inline-declarations*))
    (process-optimization-declarations body)
    (list* 'LAMBDA (cadr form) (mapcar #'precompile1 body))))

(defun precompile-let/let*-vars (vars)
  (let ((result nil))
    (declare (type list result))
    (dolist (var vars)
      (cond ((consp var)
             (when (> (length var) 2)
               (error 'program-error
                      :format-control "The LET/LET* binding specification ~S is invalid."
                      :format-arguments (list var)))
             (let ((v (%car var))
                   (expr (cadr var)))
               (unless (symbolp v)
                 (error 'simple-type-error
                        :format-control "The variable ~S is not a symbol."
                        :format-arguments (list v)))
               (push (list v (precompile1 expr)) result)
               (push (make-var v) *local-variables*)))
            (t
             (push var result)
             (push (make-var var) *local-variables*))))
    (nreverse result)))

(defun precompile-let (form)
  (let ((*local-variables* *local-variables*))
    (list* (car form)
           (precompile-let/let*-vars (cadr form))
           (mapcar #'precompile1 (cddr form)))))

;; (LET* ((X 1)) (LET* ((Y 2)) (LET* ((Z 3)) (+ X Y Z)))) =>
;; (LET* ((X 1) (Y 2) (Z 3)) (+ X Y Z))
(defun maybe-fold-let* (form)
  (if (and (length-eql form 3)
           (consp (%caddr form))
           (eq (%car (%caddr form)) 'LET*))
      (let ((third (maybe-fold-let* (%caddr form))))
        (list* 'LET* (append (%cadr form) (cadr third)) (cddr third)))
      form))

(defun precompile-let* (form)
  (setq form (maybe-fold-let* form))
  (let ((*local-variables* *local-variables*))
    (list* 'LET*
           (precompile-let/let*-vars (cadr form))
           (mapcar #'precompile1 (cddr form)))))

(defun precompile-local-function-def (def)
  (let ((name (car def))
        (lambda-list (cadr def))
        (body (%cddr def)))
    (dolist (thing lambda-list)
      (when (consp thing)
        (precompile1 (cadr thing)))) ; FIXME don't throw away this result!
    (let* ((block-name name)
           (entry (cons block-name nil))
           (*blocks* (cons entry *blocks*)))
      (setq body (mapcar #'precompile1 body)))
    (environment-add-function-definition *compile-file-environment*
                                         name
                                         (coerce-to-function (list* 'LAMBDA lambda-list body)))
    (list* name lambda-list body)))

(defun precompile-local-functions (defs)
  (let ((result nil))
    (dolist (def defs (nreverse result))
      (push (precompile-local-function-def def) result))))

(defun precompile-flet/labels (form)
  (let ((operator (car form))
        (local-functions (cadr form))
        (body (%cddr form))
        (*compile-file-environment* (make-environment *compile-file-environment*)))
    (list* operator
           (precompile-local-functions local-functions)
           (mapcar #'precompile1 body))))

(defun define-local-macro (name lambda-list body)
  (let* ((form (gensym))
         (env (gensym))
         (body (parse-defmacro lambda-list form body name 'macrolet
                               :environment env))
         (expander `(lambda (,form ,env) (block ,name ,body))))
    (make-macro name (coerce-to-function expander))))

(defun precompile-macrolet (form)
  (let ((*compile-file-environment* (make-environment *compile-file-environment*))
        (macros (cadr form)))
    (dolist (macro macros)
      (let ((name (car macro))
            (lambda-list (cadr macro))
            (forms (%cddr macro)))
        (environment-add-macro-definition *compile-file-environment* name
                                          (define-local-macro name lambda-list forms))))
    (multiple-value-bind (body decls)
        (parse-body (%cddr form) nil)
      (setq body (mapcar #'precompile1 body))
      (if decls
          (precompile-locally (list* 'LOCALLY (append decls body)))
          (precompile-progn (list* 'PROGN body))))))

(defun precompile-symbol-macrolet (form)
  (let* ((*local-variables* *local-variables*)
         (defs (cadr form)))
    (dolist (def defs)
      (let ((name (car def))
            (expansion (cadr def)))
        (when (special-variable-p name)
          (error 'program-error
                 :format-control "Attempt to bind the special variable ~S with SYMBOL-MACROLET."
                 :format-arguments (list name)))
        (push (list name :symbol-macro expansion) *local-variables*)))
    (multiple-value-bind (body decls)
        (parse-body (%cddr form) nil)
      (when decls
        (let ((specials nil))
          (dolist (decl decls)
            (when (eq (car decl) 'DECLARE)
              (dolist (declspec (%cdr decl))
                (when (eq (car declspec) 'SPECIAL)
                  (setq specials (append specials (%cdr declspec)))))))
          (when specials
            (let ((names (mapcar #'car defs)))
              (dolist (special specials)
                (when (memq special names)
                  (error 'program-error
                         :format-control "~S is a symbol-macro and may not be declared special."
                         :format-arguments (list special))))))))
      `(locally ,@decls ,@(mapcar #'precompile1 body)))))

(defun precompile-load-time-value (form)
  form)

;; (defun precompile-locally (form)
;;   (let ((body (mapcar #'precompile1 (cdr form))))
;;     (when (null body)
;;       (return-from precompile-locally nil))
;;     (if (and (consp (car body)) (eq (caar body) 'DECLARE))
;;         (cons 'LOCALLY body)
;;         (precompile-progn (cons 'PROGN body)))))
(defun precompile-locally (form)
  (let ((*inline-declarations* *inline-declarations*))
    (process-optimization-declarations (cdr form))
    (cons 'LOCALLY (mapcar #'precompile1 (cdr form)))))

(defun precompile-multiple-value-bind (form)
  (let ((vars (cadr form))
        (values-form (caddr form))
        (body (cdddr form)))
    (list* 'MULTIPLE-VALUE-BIND
           vars
           (precompile1 values-form)
           (mapcar #'precompile1 body))))

(defun precompile-multiple-value-list (form)
  (list 'MULTIPLE-VALUE-LIST (precompile1 (cadr form))))

(defun precompile-not/null (form)
  ;; (NOT (NOT X)) => T, which is not necessarily the value of X, so we can't
  ;; shorten that case.
  (unless (length-eql form 2)
    (error 'program-error
           :format-control "Wrong number of arguments for ~S (expected 1, but received ~D)."
           :format-arguments (list (car form) (length (cdr form)))))
  (let ((subform (precompile1 (%cadr form))))
    (when (consp subform)
      (let ((op (%car subform)))
        (when (and (memq op '(two-arg-< two-arg-> two-arg-<= two-arg->=))
                   (length-eql (%cdr subform) 2))
;;           (format t "~%rewriting ~S~%" form)
          (return-from precompile-not/null (list* (case op
                                                    (two-arg-<    'two-arg->=)
                                                    (two-arg->    'two-arg-<=)
                                                    (two-arg-<=   'two-arg->)
                                                    (two-arg->=   'two-arg-<))
                                                  (cdr subform))))))
    ;; otherwise...
    (list (%car form) subform)))
;;   (precompile-default form))

(defun precompile-progn (form)
  (let ((body (mapcar #'precompile1 (cdr form))))
    (if (length-eql body 1)
        (%car body)
        (cons 'PROGN body))))

(defun precompile-psetf (form)
  (setq form
        (list* 'PSETF
               (mapcar #'precompile1 (cdr form))))
  (precompile1 (expand-macro form)))

(defun precompile-psetq (form)
  ;; Make sure all the vars are symbols.
  (do* ((rest (cdr form) (cddr rest))
        (var (car rest)))
       ((null rest))
    (unless (symbolp var)
      (error 'error
             :format-control "~S is not a symbol."
             :format-arguments (list var))))
  ;; Delegate to PRECOMPILE-PSETF so symbol macros are handled correctly.
  (precompile-psetf form))

(defun precompile-quote (form)
  (unless (length-eql form 2)
    (error 'program-error
           :format-control "Wrong number of arguments for ~S (expected 1, but received ~D)."
           :format-arguments (list 'QUOTE (length (cdr form)))))
  (let ((arg (%cadr form)))
    (when (or (null arg) (numberp arg) (characterp arg) (eq arg t))
      (setq form arg)))
  form)

(defun precompile-setf (form)
  (if (length-eql form 3)
      (let ((place (%cadr form)))
        (cond ((symbolp place)
               (let ((var (find-var place)))
                 (if (and var (eq (second var) :symbol-macro))
                     (precompile1 (list* 'SETF (copy-tree (third var)) (%cddr form)))
                     (list 'SETQ place (precompile1 (%caddr form))))))
              ((and (consp place)
                    (eq (%car place) 'VALUES))
               (setq form
                     (list* 'SETF
                            (list* 'VALUES
                                   (mapcar #'precompile1 (%cdr place)))
                            (%cddr form)))
               (precompile1 (expand-macro form)))
              (t
               (precompile1 (expand-macro form)))))
      (precompile1 (expand-macro form))))

(defun precompile-setq (form)
  (let* ((args (cdr form))
         (len (length args)))
    (when (oddp len)
      (error 'program-error :format-control "Odd number of arguments to SETQ."))
    (if (eql len 2)
        (let* ((name (%car args))
               (var (find-var name))
               (value-form (%cadr args))
               (op (and (consp value-form) (%car value-form))))
          (unless (symbolp name)
            (error 'type-error :datum name :expected-type 'symbol))
          (cond ((and var (eq (second var) :symbol-macro))
                 (precompile1 (list 'SETF (copy-tree (third var)) value-form)))
                ((and (eq op '+)
                      (length-eql value-form 3)
                      (eq name (%cadr value-form))
                      (numberp (%caddr value-form)))
                 (list 'INCQ name (%caddr value-form)))
                ((and (eq op '+)
                      (length-eql value-form 3)
                      (eq name (%caddr value-form))
                      (numberp (%cadr value-form)))
                 (list 'INCQ name (%cadr value-form)))
                ((and (eq op '1+)
                      (length-eql value-form 2)
                      (eq name (%cadr value-form)))
                 (list 'INCQ name 1))
                (t
                 (list 'SETQ name (precompile1 value-form)))))
        (let ((result nil))
          (loop
            (when (null args)
              (return))
            (push (precompile-setq (list 'SETQ (car args) (cadr args))) result)
            (setq args (%cddr args)))
          (setq result (nreverse result))
          (push 'PROGN result)
          result))))

(defun precompile-subtypep (form)
  (if (length-eql form 3)
      (list* '%SUBTYPEP (mapcar #'precompile1 (%cdr form)))
      (precompile-default form)))

(defun precompile-tagbody (form)
  (do ((body (cdr form) (cdr body))
       (result nil))
      ((null body) (cons 'TAGBODY (nreverse result)))
    (cond ((atom (car body))
           (push (%car body) result))
          (t
           (let ((expansion (precompile1 (%car body))))
             ;; If the expansion turns out to be a bare symbol or integer, wrap it
             ;; with PROGN so it won't be mistaken for a tag in an enclosing TAGBODY.
             (when (or (symbolp expansion) (integerp expansion))
               (setq expansion (list 'PROGN expansion)))
             (push expansion result))))))

(defun precompile-go (form)
  (unless (length-eql form 2)
    (error 'program-error
           :format-control "Wrong number of arguments for ~S (expected 1, but received ~D)."
           :format-arguments (list 'GO (length (cdr form)))))
  form)

(defun precompile-the (form)
  (list (car form) (cadr form) (precompile1 (caddr form))))

(defun precompile-unless (form)
  (let ((test-form (cadr form)))
    (cond ((and (consp test-form)
                (length-eql test-form 2)
                (memq (%car test-form) '(NOT NULL)))
           (precompile-when (list* 'WHEN (%cadr test-form) (mapcar #'precompile1 (%cddr form)))))
          (t
           (list* 'UNLESS (mapcar #'precompile1 (%cdr form)))))))

(defun precompile-when (form)
  (let ((test-form (cadr form)))
    (cond ((and (consp test-form)
                (length-eql test-form 2)
                (memq (%car test-form) '(NOT NULL)))
           (precompile-unless (list* 'UNLESS (%cadr test-form) (mapcar #'precompile1 (%cddr form)))))
          (t
           (list* 'WHEN (mapcar #'precompile1 (%cdr form)))))))

(defun precompile-eql (form)
  (let* ((args (cdr form))
         (arg1 (car args))
         (arg2 (cadr args)))
    (if (or (keywordp arg1)
            (keywordp arg2)
            (and (quoted-form-p arg1) (symbolp (%cadr arg1)))
            (and (quoted-form-p arg2) (symbolp (%cadr arg2))))
        (list* 'EQ (mapcar #'precompile1 args))
        (list* 'EQL (mapcar #'precompile1 args)))))

(defun precompile-concatenate (form)
  (let* ((args (cdr form))
         (arg1 (car args)))
    (when (quoted-form-p arg1)
      (let ((type (%cadr arg1)))
        (when (memq type '(STRING SIMPLE-STRING SIMPLE-BASE-STRING))
          (setq form `(concatenate-to-string (list ,@(cdr args)))))))
    (precompile-default form)))

(dolist (pair '((*              two-arg-*)
                (+              two-arg-+)
                (-              two-arg--)
                (/              two-arg-/)
                (/=             two-arg-/=)
                (<              two-arg-<)
                (<=             two-arg-<=)
                (=              two-arg-=)
                (>              two-arg->)
                (>=             two-arg->=)
                (append         two-arg-append)
                (aref           vector-ref)
                (char-equal     two-arg-char-equal)
                (char/=         two-arg-char/=)
                (char<          two-arg-char<)
                (char<=         two-arg-char<=)
                (char=          two-arg-char=)
                (char>          two-arg-char>)
                (char>=         two-arg-char>=)
                (every          every2)
                (find           find-eql)
                (intersection   intersection-eql)
                (logand         two-arg-logand)
                (logior         two-arg-logior)
                (logxor         two-arg-logxor)
                (mapc           mapc2)
                (mapcar         mapcar2)
                (member         memql)
                (position       position-eql)
;;                 (sbit           sbit1)
                (subsetp        subsetp-eql)
                (typep          %typep)))
  (puthash3 (%car pair) +two-arg-operators+ (%cadr pair)))

(defun precompile-aset (form)
  (let ((args (cdr form)))
    (list* (if (length-eql args 3) 'vector-set 'aset) (mapcar #'precompile1 args))))

(defun precompile-set-sbit (form)
  (let ((args (cdr form)))
    (list* (if (length-eql args 3) 'set-sbit1 'aset) (mapcar #'precompile1 args))))

(defun precompile-get (form)
  (let ((args (cdr form)))
    (list* (if (length-eql args 2) 'GET2 'GET3) (mapcar #'precompile1 args))))

(defun precompile-gethash (form)
  (let ((args (cdr form)))
    (list* (if (length-eql args 2) 'GETHASH2 'GETHASH3) (mapcar #'precompile1 args))))

(defun precompile-puthash (form)
  (let* ((args (cdr form))
         (numargs (length args)))
    (ecase numargs
      (3
       (list* 'puthash3 (mapcar #'precompile1 args)))
      (4
       (list* 'puthash4 (mapcar #'precompile1 args))))))

(defun precompile-vector-push-extend (form)
  (let* ((args (cdr form)))
    (list* (case (length args)
             (2 'vector-push-extend-2)
             (3 'vector-push-extend-3)
             (t 'vector-push-extend))
           (mapcar #'precompile1 args))))

(defun precompile-make-array (form)
  (let* ((args (cdr form))
         arg1)
    (cond ((length-eql args 1)
           (cond ((fixnump (setq arg1 (%car args)))
                  (list 'MAKE-SIMPLE-VECTOR arg1))
                 ((and (quoted-form-p arg1)
                       (consp (setq arg1 (%cadr arg1)))
                       (length-eql arg1 1)
                       (fixnump (setq arg1 (%car arg1))))
                  (list 'MAKE-SIMPLE-VECTOR arg1))
                 (t
                  (list* 'MAKE-ARRAY (mapcar #'precompile1 args)))))
          (t
           (list* 'MAKE-ARRAY (mapcar #'precompile1 args))))))

(defun precompile-sort (form)
  (let* ((args (cdr form))
         (numargs (length args)))
    (when (>= numargs 2)
      (let ((predicate (%cadr args))
            operator)
        (cond ((and (listp predicate)
                    (length-eql predicate 2)
                    (eq (%car predicate) 'FUNCTION))
               (setq operator (%cadr predicate)))
              ((quoted-form-p predicate)
               (setq operator (%cadr predicate))))
;;         (format t "operator = ~S~%" operator)
        (when operator
          (let ((two-arg-operator (gethash2-1 operator +two-arg-operators+)))
            (when two-arg-operator
;;               (format t "old form = ~S~%" form)
              (setq form `(sort ,(%car args) ',two-arg-operator ,@(cddr args)))
;;               (format t "new form = ~S~%" form)
              )))))
    (precompile-default form)))

(defun precompile-subseq (form)
  (let* ((args (cdr form))
         (numargs (length args)))
    (list* (case numargs
             (2 'SUBSEQ2)
             (3 'SUBSEQ3)
             (t 'SUBSEQ))
           (mapcar #'precompile1 args))))

(defun precompile-list (form)
  (let* ((args (cdr form))
         (numargs (length args)))
    (if (< numargs call-arguments-limit)
        (case numargs
          (0
           nil)
          (1
           (list 'LIST1 (precompile1 (%car args))))
          (2
           (list* 'LIST2 (mapcar #'precompile1 args)))
          (3
           (list* 'LIST3 (mapcar #'precompile1 args)))
          (4
           (list* 'LIST4 (mapcar #'precompile1 args)))
          (5
           (list* 'LIST5 (mapcar #'precompile1 args)))
          (t
           (list* 'LIST (mapcar #'precompile1 args))))
        (precompile-form `(nconc (list ,@(subseq args 0 (1- call-arguments-limit)))
                                 (list ,@(nthcdr (1- call-arguments-limit) args)))))))

(defun precompile-vector (form)
  (let* ((args (cdr form)))
    (list* (case (length args)
             (2 'VECTOR2)
             (3 'VECTOR3)
             (t 'VECTOR))
           (mapcar #'precompile1 args))))

(defun expand-macro (form)
  (loop
    (when (and (consp form)
               (symbolp (%car form))
               (special-operator-p (%car form)))
      (return-from expand-macro form))
    (multiple-value-bind (result expanded)
        (macroexpand-1 form *compile-file-environment*)
      (unless expanded
        (return result))
      (setq form result))))

(defun precompile-function-call (form)
  (let ((operator (%car form)))
    (when (and (consp operator) (eq (%car operator) 'LAMBDA))
      (let ((new-form
             (list* 'DESTRUCTURING-BIND
                    (cadr operator)
                    (cons 'LIST (%cdr form))
                    (cddr operator))))
;;         (format t "new-form = ~S~%" new-form)
        (return-from precompile-function-call
                     (precompile1 new-form))))
    (unless *force-full-calls*
      (when (and *compiling*
                 (< *debug* 3)
                 (>= *speed* *space*))
        (let ((expansion (inline-expansion operator)))
          (when expansion
            (unless (notinline-p operator)
              (let ((new-form (expand-inline form expansion)))
                (unless (eq new-form form)
                  (return-from precompile-function-call (precompile1 new-form)))))))))
    ;; otherwise...
    (when (length-eql form 3)
      (let ((two-arg-operator (gethash2-1 operator +two-arg-operators+)))
        (when two-arg-operator
;;           (format t "two-arg-operator = ~S~%" two-arg-operator)
          (setq operator two-arg-operator))))
    (cons operator (mapcar #'precompile1 (%cdr form)))))

(defun precompile1 (form)
  (declare (optimize speed))
  (cond ((symbolp form)
         (let ((var (find-var form)))
           (cond ((and var (eq (second var) :symbol-macro))
                  (precompile1 (copy-tree (third var))))
;;                  ((null var)
;;                   (let ((expansion (expand-macro form)))
;;                     (if (eq expansion form)
;;                         form
;;                         (precompile1 expansion))))
                 (t
                  form))))
        ((atom form)
         form)
        (t
         (let ((op (%car form))
               handler)
           (when (symbolp op)
             (cond ((setq handler (get op 'p0-handler))
                    (return-from precompile1 (funcall handler form)))
                   ((special-operator-p op)
                    (error "PRECOMPILE-FORM: unsupported special operator ~S." op))
                   ((macro-function op *compile-file-environment*)
                    (return-from precompile1 (precompile1 (expand-macro form))))))
           (precompile-function-call form)))))

(defun precompile-form (form)
  (let ((*blocks* nil))
    (precompile1 form)))

(defun precompile (name &optional definition)
  (let* ((definition (or definition (fdefinition name)))
         (lambda-expression
          (cond ((functionp definition)
                 (function-lambda-expression definition))
                ((and (consp definition) (eq (%car definition) 'LAMBDA))
                 definition)
                (t
                 nil)))
         (result (and lambda-expression (coerce-to-function (precompile1 lambda-expression)))))
    (when name
      (when result
        ;; Preserve existing source information (if any).
        (let ((source (and (symbolp name) (get name '%source))))
          (set-fdefinition name result)
          (when source
            (put name '%source source)))))
    (values (or name result) nil nil)))

(defun install-p0-handler (symbol handler)
  (put symbol 'p0-handler handler))

(defun install-p0-handlers ()
  (dolist (pair '((AND                  precompile-default)
                  (ASET                 precompile-aset)
                  (AVER                 precompile-default)
                  (BLOCK                precompile-block)
                  (CAR                  precompile-car)
                  (CASE                 precompile-case/ecase)
                  (CATCH                precompile-default)
                  (CONCATENATE          precompile-concatenate)
                  (COND                 precompile-cond)
                  (DECLARE              identity)
                  (DEFPARAMETER         precompile-defparameter)
                  (DEFVAR               precompile-defvar)
                  (DO                   precompile-do/do*)
                  (DO*                  precompile-do/do*)
                  (DOLIST               precompile-dolist)
                  (DOTIMES              precompile-dotimes)
                  (ECASE                precompile-case/ecase)
                  (EQL                  precompile-eql)
                  (EVAL-WHEN            precompile-eval-when)
                  (FLET                 precompile-flet/labels)
                  (FUNCALL              precompile-funcall)
                  (FUNCTION             precompile-function)
                  (GET                  precompile-get)
                  (GETHASH              precompile-gethash)
                  (GO                   precompile-go)
                  (IF                   precompile-if)
                  (INCQ                 identity)
                  (LABELS               precompile-flet/labels)
                  (LAMBDA               precompile-lambda)
                  (LET                  precompile-let)
                  (LET*                 precompile-let*)
                  (LIST                 precompile-list)
                  (LOAD-TIME-VALUE      precompile-load-time-value)
                  (LOCALLY              precompile-locally)
                  (MACROLET             precompile-macrolet)
                  (MAKE-ARRAY           precompile-make-array)
                  (MULTIPLE-VALUE-BIND  precompile-multiple-value-bind)
                  (MULTIPLE-VALUE-CALL  precompile-default)
                  (MULTIPLE-VALUE-LIST  precompile-multiple-value-list)
                  (MULTIPLE-VALUE-PROG1 precompile-default)
                  (NOT                  precompile-not/null)
                  (NULL                 precompile-not/null)
                  (OR                   precompile-default)
                  (PROGN                precompile-progn)
                  (PROGV                precompile-default)
                  (PSETF                precompile-psetf)
                  (PSETQ                precompile-psetq)
                  (PUTHASH              precompile-puthash)
                  (QUOTE                precompile-quote)
                  (RETURN               precompile-return)
                  (RETURN-FROM          precompile-return-from)
                  (SET-SBIT             precompile-set-sbit)
                  (SETF                 precompile-setf)
                  (SETQ                 precompile-setq)
                  (SORT                 precompile-sort)
                  (SUBSEQ               precompile-subseq)
                  (SUBTYPEP             precompile-subtypep)
                  (SYMBOL-MACROLET      precompile-symbol-macrolet)
                  (TAGBODY              precompile-tagbody)
                  (THE                  precompile-the)
                  (THROW                precompile-default)
                  (TRULY-THE            precompile-the)
                  (UNLESS               precompile-unless)
                  (UNWIND-PROTECT       precompile-default)
                  (VECTOR               precompile-vector)
                  (VECTOR-PUSH-EXTEND   precompile-vector-push-extend)
                  (WHEN                 precompile-when)))
    (install-p0-handler (first pair) (second pair))))

(install-p0-handlers)

(defun generate-inline-expansion (block-name lambda-list declarations body)
  (dolist (symbol lambda-list)
    (when (memq symbol lambda-list-keywords)
      (return-from generate-inline-expansion nil)))
  (let ((body (precompile-form (list* 'BLOCK block-name (copy-tree body)))))
    `(lambda ,lambda-list ,@declarations ,body)))

(defmacro defun (name lambda-list &body body)
  (note-name-defined name)
  (multiple-value-bind (body declarations doc)
      (parse-body body)
    (declare (ignore doc)) ; FIXME
    (let* ((block-name (fdefinition-block-name name))
           (lambda-expression
            `(lambda ,lambda-list ,@declarations (block ,block-name ,@body))))
      (setq lambda-expression (precompile-form lambda-expression))
      (when (and (symbolp name) (eq (get name 'sys::%inline) 'INLINE))
        (let ((expansion (generate-inline-expansion block-name lambda-list declarations body)))
          (when expansion
            (set-inline-expansion name expansion))))
      `(%defun ',name ,lambda-expression))))

(when (special-operator-p 'defun)
  (set-symbol-function 'defun (macro-function 'defun))
  (set-symbol-plist 'defun nil)
  (set-macro-function 'defun (symbol-function 'defun)))

(defun precompile-package (package)
  (dolist (symbol (append (package-internal-symbols package) (package-external-symbols package)))
    (cond ((special-operator-p symbol)
           )
          ((autoloadp symbol)
           )
          ((macro-function symbol)
           )
          ((fboundp symbol)
           (unless (compiled-function-p (symbol-function symbol))
             (precompile symbol))))))

;; REVIEW
(defvar *precompiled* nil)

(defun precompile-system ()
  (dolist (package (mapcar #'find-package '("CL" "SYS" "EXT" "PRECOMPILER")))
    (precompile-package package))
  (setq *precompiled* t))

(unless *precompiled*
  (precompile-system))
