;;; compiler.lisp
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

;; REVIEW
(export '*output-mode*)

(defvar *output-mode* :compile)

(defun compile-file-p ()
  (eq *output-mode* :compile-file))


(export '*inhibit-notes*)

(defvar *inhibit-notes* t)

(defknown note (t) t)
(defun note (format-control &rest format-arguments)
  (unless *inhibit-notes*
    (let ((*print-structure* nil))
      (apply 'format t (concatenate 'string ";   " format-control) format-arguments))))

(export '*catch-errors*)

(defvar *catch-errors* t)

;; (defvar *compile-file-environment* nil)

(defvar *functions-defined-in-current-file* nil)

(defstruct compiland
  name
  lambda-expression
  minargs
  maxargs
  arity
  arg-vars
  local-vars
  p1-body
  parent
  omit-frame-pointer
  (id (gensym "COMPILAND-"))
  constants
  closure-data-index
  called-names
  (needs-thread-var-p t)
  leaf-p
  unwind-protect-p
  setjmp-p
  longjmp-p

  #+x86
  thread-var

  #+x86-64
  thread-register

  prolog
  epilog
  registers-to-be-saved
  (common-labels (make-hash-table :test 'eq) :type hash-table :read-only t)
  )

(defun compiland-child-p (compiland)
  (not (null (compiland-parent compiland))))

(declaim (type compiland *current-compiland*))
(defvar *current-compiland*)

(defstruct var
  name
  kind ; :required :optional :rest :key :local
  index
  register
  #+x86-64
  arg-register
  arg-index ; index in argument vector
  closure-index
  special-p
  (declared-type :none)
  (derived-type :unknown)
  ignore-p
  ignorable-p
  initform
  type-check-form
  (reads 0 :type fixnum)
  (writes 0 :type fixnum)
  refs
  used-non-locally-p
  readers ; a list of compilands
  writers ; a list of compilands
  (compiland-id (compiland-id *current-compiland*))
  constant-p
  constant-value
  (register-worthiness 0 :type fixnum)
  )

(defknown make-var (*) t)

;; (defmethod print-object ((obj var) stream)
;;    (print-unreadable-object (obj stream :type t :identity t)
;;      (princ (name obj) stream)))

(defun print-var (var stream)
  (print-unreadable-object (var stream :type t :identity t)
    (princ (var-name var) stream)))

(defstruct (var-ref (:constructor make-var-ref (var)))
  (var nil :type var)
  )

(defknown make-var-ref (*) t)

;; (defknown local-var-ref-p (t) t)
;; (defun local-var-ref-p (thing)
;;   (and (var-ref-p thing)
;;        (var-index (var-ref-var thing))))

(defknown constant-or-local-var-ref-p (t) t)
(defun constant-or-local-var-ref-p (thing)
  (cond #+nil
        ((local-var-ref-p thing)
         t)
        ((var-ref-p thing)
         (let ((var (var-ref-var thing)))
;;            (and (var-special-p var)
;;                 (constantp (var-name var))
;; ;;                 (null *compile-file-pathname*) ; REVIEW
;;                 (not (compile-file-p)) ; REVIEW
;;                 )))
           (cond ((var-special-p var)
                  (and (constantp (var-name var))
                       ;; REVIEW
                       (not (compile-file-p))))
                 ((var-used-non-locally-p var)
                  nil)
                 (t
                  t))))
        ((and (consp thing)
              (eq (%car thing) 'PROGN))
         (and (length-eql thing 2)
              (integerp (%cadr thing)))) ; e.g. (PROGN 0)
        (t
         (constantp thing))))

(defknown flushable (t) t)
(defun flushable (form)
  (cond ((var-ref-p form)
         t)
        ((constantp form)
         t)
        ((consp form) ; a function call
         (let* ((op (%car form))
                (args (%cdr form))
                (attributes (function-attributes op)))
           (cond ((memq :flushable attributes)
                  (every #'flushable args))
                 ((eq op 'PROGN)
                  (every #'flushable args))
                 (t
                  nil))))
        (t
         nil)))

(defvar *register-contents* nil)

(defknown clear-register-contents (*) t)
(defun clear-register-contents (&rest registers)
;;   (mumble "clear-register-contents ~S~%" registers)
  (cond ((null *register-contents*)
         (setq *register-contents* (make-hash-table :test 'eq)))
        (registers
         (dolist (register registers)
           (remhash register *register-contents*)))
        (t
         (clrhash *register-contents*))))

(defknown clear-var-registers (var) t)
(defun clear-var-registers (var)
;;   (mumble "clear-var-registers var = ~S~%" (var-name var))
  (maphash (lambda (k v)
             (cond ((eq var v)
                    (remhash k *register-contents*))
                   ((and (consp v)
                         (memq var v))
                    (setf (gethash k *register-contents*) (remove var v)))))
           *register-contents*))

(defknown set-register-contents (t t) t)
(defun set-register-contents (register var-or-vars)
;;   (mumble "set-register-contents register = ~S var = ~S~%" register (var-name var))
;;   (unless (eq register (var-register var))
  (cond ((consp var-or-vars)
         (dolist (var var-or-vars)
           (unless (eq register (var-register var))
             (setf (gethash register *register-contents*) var))))
        (t
         (let ((var var-or-vars))
           (unless (eq register (var-register var))
             (setf (gethash register *register-contents*) var))))))

(defknown find-register-containing-var (var) t)
;; caller may trash the returned register!
(defun find-register-containing-var (var)
;;   (mumble "find-register-containing-var var = ~S~%" (var-name var))
  (when (var-p var)
    (maphash (lambda (k v)
               (when (or (eq var v)
                         (and (consp v)
                              (memq var v)))
                 (return-from find-register-containing-var k)))
             *register-contents*)))

(defknown copy-register-contents () hash-table)
(defun copy-register-contents ()
  (let ((ht (make-hash-table :test 'eq)))
    (maphash (lambda (k v)
               (setf (gethash k ht) v))
             *register-contents*)
    ht))

(defun dump-register-contents ()
  (mumble "register contents:~%")
  (when (hash-table-p *register-contents*)
    (maphash (lambda (k v)
               (mumble "key = ~S value = ~S~%" k v))
             *register-contents*))
  (mumble "end register contents~%"))

(defstruct node
  name
  form
  )

(defstruct (m-v-c-node (:include node))
  function-var
  values-address-var
  values-length-var
  )

(defstruct (cblock (:conc-name block-)
                   (:include node)
                   (:constructor make-block)
                   (:predicate block-p))
;;   name
  (compiland *current-compiland*)
;;   form
  vars
  body
  return-p              ; is there a return from this block?
  non-local-return-p
  non-local-go-p
  exit                  ; a label (or nil)
  target
  free-specials
  last-special-binding-var
  tagbody-var
  block-var
  tags
  cleanup-label         ; REVIEW
  uwp-var
  uwp-values-var
  values-var            ; for MULTIPLE-VALUE-PROG1
  )

(defvar *in-cleanup* nil) ; unwind-protect node or nil

(defvar *visible-blocks* nil)

(defknown find-visible-block (t) t)
(defun find-visible-block (name)
  (dolist (block *visible-blocks*)
    (when (eq name (block-name block))
      (return block))))

(defstruct tag
  name
  label
  block
  (compiland *current-compiland*)
  non-local-go-p        ; is there a non-local GO targetting this tag?
  index
  )

(defvar *all-variables* nil)

;; undefined variables that we've already warned about
(defvar *undefined-variables* nil)

(defvar *local-variables* nil)

(defvar *closure-vars* nil)

;; variables visible at the current point of compilation
(defvar *visible-variables* nil)

(defknown find-visible-var (t) t)
(defun find-visible-var (name)
  (dolist (var *visible-variables*)
    (when (eq (var-name var) name)
      (return var))))

(defvar *closure-data-offset* nil) ; REVIEW name

(defvar *visible-tags* nil)

(defknown find-visible-tag (t) t)
(defun find-visible-tag (name)
  (dolist (tag *visible-tags*)
    (when (eql name (tag-name tag))
      (return tag))))

(defstruct local-function
  name
  callable-name ; REVIEW
  (compiland (required-argument) :type compiland)
  inline-expansion
  function
  ctf ; REVIEW
  ctf-name ; REVIEW
  var
  (call-count 0 :type fixnum)
  needs-function-object-p)

(defvar *local-functions* nil)

(defknown find-local-function (t) t)
(defun find-local-function (name)
  (dolist (local-function *local-functions* nil)
    (when (equal name (local-function-name local-function))
      (return local-function))))

(defun unsupported ()
  (compiler-unsupported "unsupported"))

;; "In addition to situations for which the standard specifies that conditions
;; of type WARNING must or might be signaled, warnings might be signaled in
;; situations where the compiler can determine that the consequences are
;; undefined or that a run-time error will be signaled. Examples of this
;; situation are as follows: violating type declarations, altering or assigning
;; the value of a constant defined with DEFCONSTANT, calling built-in Lisp
;; functions with a wrong number of arguments or malformed keyword argument
;; lists, and using unrecognized declaration specifiers." (3.2.5)
(defknown check-arg-count (t t) t)
(defun check-arg-count (form n)
  (declare (type list form))
  (let* ((op (car form))
         (args (cdr form))
         (ok (length-eql args n)))
    (unless ok
      (funcall (if (eq (symbol-package op) +common-lisp-package+)
                   'compiler-warn ; see above!
                   'compiler-style-warn)
               "Wrong number of arguments for ~A (expected ~D, but received ~D)."
               op n (length args)))
    ok))

(defknown find-var (t t) t)
(defun find-var (name list)
  (dolist (var list)
    (when (eq name (var-name var))
      (return var))))

(defun process-ignore/ignorable (declaration names vars)
  (when (memq declaration '(IGNORE IGNORABLE))
    (let ((what (if (eq declaration 'IGNORE) "ignored" "ignorable")))
      (dolist (name names)
        (let ((var (find-var name vars)))
          (cond ((null var)
                 (compiler-style-warn "Declaring unknown variable ~S to be ~A."
                                      name what))
                ((var-special-p var)
                 (compiler-style-warn "Declaring special variable ~S to be ~A."
                                      name what))
                ((eq declaration 'IGNORE)
                 (setf (var-ignore-p var) t))
                (t
                 (setf (var-ignorable-p var) t))))))))

(defun process-declarations-for-vars (body vars)
  (let ((free-specials nil))
    (dolist (form body)
      (unless (and (consp form) (eq (%car form) 'DECLARE))
        (return))
      (let ((decls (%cdr form)))
        (dolist (decl decls)
          (let ((declaration-identifier (car decl)))
            (case declaration-identifier
              ((DYNAMIC-EXTENT FTYPE INLINE NOTINLINE OPTIMIZE)
               ;; nothing to do here
               )
              ((IGNORE IGNORABLE)
               (process-ignore/ignorable (%car decl) (%cdr decl) vars)
               )
              (SPECIAL
               (dolist (name (%cdr decl))
                 (let ((var (find-var name vars)))
                   (cond ((and var
                               ;; see comment below (and DO-ALL-SYMBOLS.11)
                               (eq (var-compiland-id var) (compiland-id *current-compiland*)))
                          (setf (var-special-p var) t))
                         (t
                          (push (make-var :name name :special-p t) free-specials))))))
              (TYPE
               (dolist (name (cddr decl))
                 (let ((var (find-var name vars)))
                   (when (and var
                              ;; Don't apply a declaration in a local function to
                              ;; a variable defined in its parent. For an example,
                              ;; see CREATE-GREEDY-NO-ZERO-MATCHER in cl-ppcre.
                              ;; FIXME suboptimal, since we ignore the declaration
                              (eq (var-compiland-id var) (compiland-id *current-compiland*)))
                     (setf (var-declared-type var) (cadr decl))))))
              (t
               (unless (proclaimed-declaration-p declaration-identifier)
                 (dolist (name (cdr decl))
                   (let ((var (find-var name vars)))
                     (when var
                       (setf (var-declared-type var) declaration-identifier)))))))))))
    free-specials))

(defknown p1-check-var-type (var) t)
(defun p1-check-var-type (var)
  (declare (type var var))
  (unless (var-ignore-p var) ; REVIEW
    (when (or *force-type-checks*
              (> *safety* 0))
      (let ((declared-type (var-declared-type var)))
        (unless (eq declared-type :none)
          (setf (var-type-check-form var)
                (p1 `(require-type ,(var-name var) ',declared-type))))))))

(defknown unsafe-p (t) t)
(defun unsafe-p (args)
  (declare (optimize speed))
  (cond ((atom args)
         nil)
        (t
         (case (%car args)
           (QUOTE
            nil)
           (LAMBDA
            nil)
           ((RETURN RETURN-FROM GO THROW)
            t)
           (t
            (dolist (arg args)
              (when (unsafe-p arg)
                (return t))))))))

(defknown maybe-rewrite-function-call (t) cons)
(defun maybe-rewrite-function-call (form)
  (declare (type cons form))
  (let ((args (cdr form)))
    (cond ((unsafe-p args)
           (let (syms lets)
             ;; preserve order of evaluation of the arguments
             (dolist (arg args)
               (let ((sym (gensym)))
                 (push sym syms)
                 (push (list sym arg) lets)))
             (list 'LET* (nreverse lets) (list* (car form) (nreverse syms)))))
          (t
           form))))

(defknown p1-function-call (t) t)
(defun p1-function-call (form)
  (declare (type cons form))
  (let ((new-form (maybe-rewrite-function-call form)))
    (cond ((eq new-form form)
           (let* ((name (car form))
                  (local-function (find-local-function name))
                  (compiland *current-compiland*))
             (declare (type compiland compiland))
             (when local-function
               (when (inline-p name)
                 (let ((expansion (local-function-inline-expansion local-function)))
                   (when expansion
                     (return-from p1-function-call (p1 (expand-inline form expansion))))))
               (incf (local-function-call-count local-function))
               (let ((var (local-function-var local-function)))
                 (when var
                   (unless (eq (compiland-id compiland)
                               (var-compiland-id var))
                     (setf (var-used-non-locally-p var) t)))))
             (when (symbolp name)
               (pushnew name (compiland-called-names compiland))
               ;; REVIEW we may optimize out the call
               (unless (use-fast-call-p)
                 (setf (compiland-needs-thread-var-p compiland) t)))
             (when (and (boundp '*defined-functions*) (boundp '*undefined-functions*))
               (unless (or local-function
                           (fboundp name)
                           (eq name (compiland-name compiland))
                           (memq name *defined-functions*)
                           ;;                   (proclaimed-ftype name)
                           )
                 (note-undefined-function name (compiland-name compiland)))))
           (p1-default form))
          (t
           (p1 new-form)))))

(defun p1-if (form)
  (let ((test (cadr form)))
    (cond ((unsafe-p test)
           (cond ((and (consp test)
                       (memq (%car test) '(GO RETURN-FROM THROW)))
                  (p1 test))
                 (t
                  (let* ((var (gensym))
                         (new-form
                          `(let ((,var ,test))
                             (if ,var ,(third form) ,(fourth form)))))
                    (p1 new-form)))))
          (t
           (p1-default form)))))

(defknown p1 (t) t)
(defun p1 (form)
  (cond ((symbolp form)
         (let (value)
           (cond ((null form)
                  form)
                 ((eq form t)
                  form)
                 ((keywordp form)
                  form)
                 ((and (constantp form)
                       (progn
                         (setq value (symbol-value form))
                         (or (numberp value)
                             (stringp value)
                             (pathnamep value))))
                  (setq form value))
                 (t
                  (let ((var (find-visible-var form)))
                    (when (null var)
                      (unless (or (special-variable-p form)
                                  (memq form *undefined-variables*))
                        (compiler-warn "Undefined variable: ~S" form)
                        (push form *undefined-variables*))
                      (setq var (make-var :name form :kind :local :special-p t))
                      (push var *local-variables*)
                      (push var *visible-variables*))
                    (let ((ref (make-var-ref var)))
                      (unless (var-special-p var)
                        (when (var-ignore-p var)
                          (compiler-style-warn
                           "Variable ~S is read even though it was declared to be ignored."
                           (var-name var)))
                        (push ref (var-refs var))
                        (incf (var-reads var))
                        (pushnew *current-compiland* (var-readers var))
                        (unless (eq (var-compiland-id var) (compiland-id *current-compiland*))
                          (setf (var-used-non-locally-p var) t)
                          (pushnew var *closure-vars*)))
                      ref))))))
        ((atom form)
         form)
        (t
         (let ((op (%car form))
               handler)
           (cond ((symbolp op)
                  (when (compiler-macro-function op)
                    (unless (notinline-p op)
                      (multiple-value-bind (expansion expanded-p)
                          (compiler-macroexpand form)
                        (when expanded-p
                          (return-from p1 (p1 expansion))))))
                  (when (source-transform op)
                    (unless (notinline-p op)
                      (multiple-value-bind (expansion expanded-p)
                          (expand-source-transform form)
                        (when expanded-p
                          (return-from p1 (p1 expansion))))))
                  (cond ((setq handler (get op 'p1-handler))
                         (funcall handler form))
                        ((macro-function op *compile-file-environment*)
                         (p1 (macroexpand form *compile-file-environment*)))
                        ((special-operator-p op)
                         (compiler-unsupported "P1: unsupported special operator ~S" op))
                        (t
                         (p1-function-call form))))
                 ((and (consp op) (eq (%car op) 'LAMBDA))
                  (p1 (list* 'FUNCALL form)))
                 (t
                  form))))))

(defknown p1-body (t) t)
(defun p1-body (body)
  (mapcar #'p1 body))

(defknown p1-default (t) t)
(defun p1-default (form)
  (declare (type cons form))
  (cons (car form) (mapcar 'p1 (cdr form))))

(defun p1-eval-when (form)
  (let ((situations (cadr form)))
    (cond ((or (memq :execute situations)
               (memq 'eval situations))
           (list* 'PROGN (mapcar #'p1 (cddr form))))
          (t
           '(PROGN)))))

(defun p1-ldb (form)
  (let* ((args (cdr form))
         bytespec)
    (when (and (length-eql args 2)
               (consp (setq bytespec (%car args)))
               (eq (%car bytespec) 'BYTE)
               (length-eql bytespec 3))
      (let ((size (%cadr bytespec))
            (position (%caddr bytespec))
            (integer (%cadr args)))
;;         (setq form
;;               (cond ((and (integerp size) (integerp position))
;;                      `(logand (ash ,integer (- ,position))
;;                               (1- (ash 1 ,size))))
;;                     (t
;;                      `(%ldb ,size ,position ,integer))))
        (setq form
              `(logand (ash ,integer (- 0 ,position))
                       (1- (ash 1 ,size))))
        ))
    (p1-function-call form)))

(defun p1-dpb (form)
  (let* ((args (cdr form))
         bytespec)
    (cond ((and (length-eql args 3)
                (consp (setq bytespec (%cadr args)))
                (eq (%car bytespec) 'BYTE)
                (length-eql bytespec 3))
           (let ((newbyte (%car args))
                 (size (%cadr bytespec))
                 (position (%caddr bytespec))
                 (integer (%caddr args)))
             (setq form
                   (cond ((and (integerp size) (integerp position))
                          ;;                      (mumble "p1-dpb case 1~%")
                          (let ((mask (1- (ash 1 size)))
                                (sym (gensym)))
                            `(let ((,sym ,newbyte))
                               (logior (logand ,integer (lognot (ash ,mask ,position)))
                                       (ash (logand ,sym ,mask) ,position)))))
                         (t
                          ;;                      (mumble "p1-dpb case 2~%")
                          `(%dpb ,newbyte ,size ,position ,integer)))))
           ;;     (mumble "new form = ~S~%" form)
           (p1 form))
          (t
           (p1-function-call form)))))

(defun p1-list* (form)
  (let* ((args (cdr form)))
    (when (length-eql args 2)
      (setq form (list 'cons (%cadr form) (%caddr form))))
    (p1-function-call form)))

(defun p1-quote (form)
  (unless (length-eql form 2)
    (error "Wrong number of arguments for special operator ~A (expected 1, but received ~D)."
           'QUOTE
           (1- (length form))))
  (let ((arg (cadr form)))
    (if (or (numberp arg) (characterp arg))
        arg
        form)))

(defknown p1-funcall (t) t)
(defun p1-funcall (form)
  (when (length-eql (cdr form) 0)
    (compiler-warn "Wrong number of arguments for ~A." (car form))
    (return-from p1-funcall form))
  (let ((function-form (%cadr form)))
    (when (and (consp function-form)
               (eq (%car function-form) 'FUNCTION))
      (let ((name (%cadr function-form)))
;;         (mumble "name = ~S~%" name)
        (let ((source-transform (source-transform name)))
          (when source-transform
;;             (mumble "found source transform for ~S~%" name)
;;             (mumble "old form = ~S~%" form)
            ;;             (let ((new-form (expand-source-transform form)))
            ;;               (when (neq new-form form)
            ;;                 (mumble "new form = ~S~%" new-form)
            ;;                 (return-from p1-funcall (p1 new-form))))
            (let ((new-form (expand-source-transform (list* name (cddr form)))))
;;               (mumble "new form = ~S~%" new-form)
              (return-from p1-funcall (p1 new-form)))
            ))
        )))
  ;; otherwise...
  (p1-function-call form))

(defun p1-function (form)
  (unless (length-eql form 2)
    (error 'simple-error
           :format-control "Wrong number of arguments for special operator ~A (expected 1, but received ~D)."
           :format-arguments (list 'FUNCTION (1- (length form)))))
  (let ((arg (cadr form)))
    (cond ((symbolp arg)
           (let ((local-function (find-local-function arg)))
             (when local-function
               (setf (local-function-needs-function-object-p local-function) t)
               (let ((var (local-function-var local-function)))
                 (when var
                   (unless (eq (compiland-id *current-compiland*)
                               (var-compiland-id var))
                     (mumble "p1-function-call var ~S is used non-locally~%" (var-name var))
                     (setf (var-used-non-locally-p var) t))))))
           form)
          ((setf-function-name-p arg)
;;            ;; FIXME
;;            (when (find-local-function arg)
;;              (compiler-unsupported "P1-FUNCTION: local setf functions are not supported yet"))
           (let ((local-function (find-local-function arg)))
             (when local-function
               (setf (local-function-needs-function-object-p local-function) t)
               (let ((var (local-function-var local-function)))
                 (when var
                   (unless (eq (compiland-id *current-compiland*)
                               (var-compiland-id var))
                     (mumble "p1-function-call var ~S is used non-locally~%" (var-name var))
                     (setf (var-used-non-locally-p var) t))))))
           form) ; REVIEW
          ((and (consp arg) (eq (%car arg) 'LAMBDA))
           (let* ((*current-compiland* *current-compiland*)
                  (lambda-form arg)
                  (lambda-list (cadr lambda-form))
                  (body (cddr lambda-form))
                  (compiland (make-compiland :name (gensym "ANONYMOUS-LAMBDA-")
                                             :lambda-expression lambda-form
                                             :parent *current-compiland*)))
             (multiple-value-bind (body decls)
                 (parse-body body)
               (setf (compiland-lambda-expression compiland)
                     `(lambda ,lambda-list ,@decls ,@body))
               (let ((*visible-variables* *visible-variables*)
                     (*local-variables* nil)
                     (*current-compiland* compiland))
                 (p1-compiland compiland)))
             (list 'FUNCTION compiland)))
          (t
           (compiler-unsupported "P1-FUNCTION unsupported situation")))))

(defun p1-block (form)
  (let* ((block (make-block :name (cadr form)))
         (*visible-blocks* (cons block *visible-blocks*)))
    (setf (block-body block) (p1-body (cddr form)))
    (when (and (block-return-p block)
               ;; REVIEW we really want to do this only if some code in (or under) the
               ;; current block binds a special, so this is safe but overkill...
               (some 'var-special-p *all-variables*))
      (let ((var (make-var :name (gensym "LAST-SPECIAL-BINDING-") :kind :local)))
        (push var *local-variables*)
        (setf (block-last-special-binding-var block) var)
        (setf (compiland-needs-thread-var-p *current-compiland*) t)))
    (when (block-non-local-return-p block)
      (let ((var (make-var :name (gensym "BLOCK-") :kind :local)))
        (push var *local-variables*)
        (setf (block-block-var block) var)))
    (list 'BLOCK block)))

(defun maybe-rewrite-return-from (form)
  (let ((result-form (third form)))
    (when result-form
      (when (consp result-form)
        (when (eq (%car result-form) 'RETURN-FROM)
          ;; (return-from block1 (return-from block2 ...))
          (return-from maybe-rewrite-return-from result-form)))))
  form)

(defun p1-return-from (form)
  (let* ((name (second form))
         (block (find-visible-block name)))
    (when (null block)
      (compiler-error "RETURN-FROM ~S: no block named ~S is currently visible."
                      name name))
    (let ((new-form (maybe-rewrite-return-from form)))
      (when (neq new-form form)
        (return-from p1-return-from (p1 new-form))))
    (setq form (list* 'RETURN-FROM (cadr form) (mapcar #'p1 (cddr form))))
    (push form (block-return-p block))
;;     (cond ((eq (block-compiland block) *current-compiland*)
;;            ;; Local case. If the RETURN is nested inside an UNWIND-PROTECT
;;            ;; which is inside the block we're returning from, we'll do a non-
;;            ;; local return anyway so that UNWIND-PROTECT can catch it and run
;;            ;; its cleanup forms.
;;            (let ((protected
;;                   (dolist (enclosing-block *visible-blocks*)
;;                     (when (eq enclosing-block block)
;;                       (return nil))
;;                     (when (equal (block-name enclosing-block) '(UNWIND-PROTECT))
;;                       (return t)))))
;;              (when protected
;;                (setf (block-non-local-return-p block) t))))
;;           (t
;;            (setf (block-non-local-return-p block) t)))
    (unless (eq (block-compiland block) *current-compiland*)
;;       (compiler-unsupported "P1-RETURN-FROM non-local return")
      (mumble "p1-return-from block ~S has a non-local return~%" (block-name block))
      (setf (block-non-local-return-p block) t)
      (setf (compiland-needs-thread-var-p (block-compiland block)) t)
      (setf (compiland-needs-thread-var-p *current-compiland*) t)

      ;; REVIEW
      (setf (compiland-setjmp-p (block-compiland block)) t) ; setjmp
      (setf (compiland-longjmp-p *current-compiland*) t) ; longjmp
      ))
  form)

(defknown p1-catch (t) t)
(defun p1-catch (form)
  (aver (>= (length form) 2))
  (let* ((block (make-block :name '(CATCH)))
         (var (make-var :name (gensym "CATCH-") :kind :local)))
    (push var *local-variables*)
    (setf (block-block-var block) var)
    (setf (block-form block) (p1-default form))
    (setf (block-body block) (cddr (block-form block)))
    (setf (compiland-needs-thread-var-p *current-compiland*) t)
    (setf (compiland-setjmp-p *current-compiland*) t) ; setjmp
    (list 'CATCH block)))

(defknown rewrite-throw (t) t)
(defun rewrite-throw (form)
  (let ((args (cdr form)))
    (if (unsafe-p args)
        (let ((syms nil)
              (lets nil))
          ;; tag
          (let ((arg (car args)))
            (if (constantp arg)
                (push arg syms)
                (let ((sym (gensym)))
                  (push sym syms)
                  (push (list sym arg) lets))))
          ;; result
          ;; "If the result-form produces multiple values, then all the values
          ;; are saved."
          (let ((arg (cadr args)))
            (if (constantp arg)
                (push arg syms)
                (let ((sym (gensym)))
                  ;; single-valued-p expects p2 nodification, so we can't use it here
;;                   (cond ((single-valued-p arg)
;;                          (push sym syms)
;;                          (push (list sym arg) lets))
;;                         (t
                         (push (list 'VALUES-LIST sym) syms)
                         (push (list sym (list 'MULTIPLE-VALUE-LIST arg)) lets)
;;                          ))
                  )))
          (list 'LET* (nreverse lets) (list* 'THROW (nreverse syms))))
        form)))

(defknown p1-throw (t) t)
(defun p1-throw (form)
  (aver (length-eql form 3)) ; FIXME compiler error
  (let ((new-form (rewrite-throw form)))
    (when (neq new-form form)
      (return-from p1-throw (p1 new-form))))
  (setf (compiland-needs-thread-var-p *current-compiland*) t)
  (setf (compiland-longjmp-p *current-compiland*) t) ; longjmp
  (list* 'THROW (mapcar #'p1 (cdr form))))

(defun validate-name-and-lambda-list (name lambda-list context)
  (unless (or (symbolp name) (setf-function-name-p name))
    (compiler-error "~S is not a valid function name." name))
  (when (or (memq '&optional lambda-list)
            (memq '&key lambda-list))
    (let ((state nil))
      (dolist (arg lambda-list)
        (cond ((memq arg lambda-list-keywords)
               (setq state arg))
              ((memq state '(&optional &key))
               (when (and (consp arg) (not (constantp (second arg))))
                 (compiler-unsupported
                  "~A: can't handle ~A argument with non-constant initform."
                  context
                  (if (eq state '&optional) "optional" "keyword")))))))))

;; Adapted from ABCL.
(defun rewrite-lambda (form)
  (let* ((lambda-list (cadr form)))
    (if (not (or (memq '&optional lambda-list)
                 (memq '&key lambda-list)))
        ;; no need to rewrite: no arguments with possible initforms anyway
        form
        (multiple-value-bind (body decls doc)
            (parse-body (cddr form))
          (let (state let-bindings new-lambda-list
                      (non-constants 0))
            (do* ((vars lambda-list (cdr vars))
                  (var (car vars) (car vars)))
                 ((endp vars))
              (push (car vars) new-lambda-list)
              (let ((replacement (gensym)))
                (flet ((parse-compound-argument (arg)
                                                "Returns the values NAME, KEYWORD, INITFORM, INITFORM-P,
SUPPLIED-P and SUPPLIED-P-P assuming ARG is a compound argument."
                                                (destructuring-bind
                                                    (name &optional (initform nil initform-supplied-p)
                                                          (supplied-p nil supplied-p-supplied-p))
                                                    (if (listp arg) arg (list arg))
                                                  (if (listp name)
                                                      (values (cadr name) (car name)
                                                              initform initform-supplied-p
                                                              supplied-p supplied-p-supplied-p)
                                                      (values name (make-keyword name)
                                                              initform initform-supplied-p
                                                              supplied-p supplied-p-supplied-p)))))
                  (case var
                    (&optional (setf state :optional))
                    (&key (setf state :key))
                    ((&whole &environment &rest &body &allow-other-keys)
                     ;; do nothing special
                     )
                    (t
                     (cond
                      ((atom var)
                       (setf (car new-lambda-list)
                             (if (eq state :key)
                                 (list (list (make-keyword var) replacement))
                                 replacement))
                       (push (list var replacement) let-bindings))
                      ((constantp (second var))
                       ;; so, we must have a consp-type var we're looking at
                       ;; and it has a constantp initform
                       (multiple-value-bind
                           (name keyword initform initform-supplied-p
                                 supplied-p supplied-p-supplied-p)
                           (parse-compound-argument var)
                         (let ((var-form (if (eq state :key)
                                             (list keyword replacement)
                                             replacement))
                               (supplied-p-replacement (gensym)))
                           (setf (car new-lambda-list)
                                 (cond
                                  ((not initform-supplied-p)
                                   (list var-form))
                                  ((not supplied-p-supplied-p)
                                   (list var-form initform))
                                  (t
                                   (list var-form initform
                                         supplied-p-replacement))))
                           (push (list name replacement) let-bindings)
                           ;; if there was a 'supplied-p' variable, it might
                           ;; be used in the declarations. Since those will be
                           ;; moved below the LET* block, we need to move the
                           ;; supplied-p parameter too.
                           (when supplied-p-supplied-p
                             (push (list supplied-p supplied-p-replacement)
                                   let-bindings)))))
                      (t
                       (incf non-constants)
                       ;; this is either a keyword or an optional argument
                       ;; with a non-constantp initform
                       (multiple-value-bind
                           (name keyword initform initform-supplied-p
                                 supplied-p supplied-p-supplied-p)
                           (parse-compound-argument var)
                         (declare (ignore initform-supplied-p))
                         (let ((var-form (if (eq state :key)
                                             (list keyword replacement)
                                             replacement))
                               (supplied-p-replacement (gensym)))
                           (setf (car new-lambda-list)
                                 (list var-form nil supplied-p-replacement))
                           (push (list name `(if ,supplied-p-replacement
                                                 ,replacement ,initform))
                                 let-bindings)
                           (when supplied-p-supplied-p
                             (push (list supplied-p supplied-p-replacement)
                                   let-bindings)))))))))))
            (if (zerop non-constants)
                ;; there was no reason to rewrite...
                form
                (let ((rv
                       `(lambda ,(nreverse new-lambda-list)
                          ,@(when doc (list doc))
                          (let* ,(nreverse let-bindings)
                            ,@decls ,@body))))
                  rv)))))))

(defun p1-flet (form)
  (setf (compiland-needs-thread-var-p *current-compiland*) t)
  (let ((*visible-variables* *visible-variables*)
        (*local-functions* *local-functions*)
        (*current-compiland* *current-compiland*)
        (local-functions nil))
    (dolist (definition (second form))
      (let* ((name (first definition))
             (block-name (fdefinition-block-name name))
             (lambda-list (second definition))
             lambda-expression)
        (multiple-value-bind (body decls)
            (parse-body (cddr definition))
          (setq lambda-expression
                `(lambda ,lambda-list ,@decls (block ,block-name ,@body))))
        (let ((rewritten-lambda-expression (rewrite-lambda lambda-expression)))
          (when (neq rewritten-lambda-expression lambda-expression)
            (setq lambda-expression (rewrite-lambda lambda-expression))
            (setq lambda-list (second lambda-expression))))
        (validate-name-and-lambda-list name lambda-list 'FLET)
        (let* ((compiland (make-compiland :name name
                                          :parent *current-compiland*))
               (var (make-var :name (gensym)
                              :kind :local
                              ;; REVIEW
                              :used-non-locally-p nil))
               (local-function (make-local-function :name name
                                                    :compiland compiland
                                                    :var var)))
          (multiple-value-bind (body decls)
              (parse-body (cddr lambda-expression))
            (let* ((*visible-variables* *visible-variables*)
                   (*local-functions* *local-functions*)
                   (*current-compiland* compiland)
                   ; REVIEW added (not in abcl) - probably wrong if there are closure vars
                   (*local-variables* nil))
              (setf (compiland-lambda-expression compiland) (precompile-form lambda-expression))
              (setf (local-function-inline-expansion local-function)
                    (generate-inline-expansion block-name lambda-list decls body))
              (p1-compiland compiland)))
          (push local-function local-functions)
          (push var *all-variables*))))
    (setq local-functions (nreverse local-functions))
    ;; make the local functions visible
    (dolist (local-function local-functions)
      (push local-function *local-functions*)
      (let ((var (local-function-var local-function)))
        (when var
          (push var *visible-variables*))))
    (let ((*speed* *speed*)
          (*space* *space*)
          (*safety* *safety*)
          (*debug* *debug*)
          (*inline-declarations* *inline-declarations*))
      (multiple-value-bind (body decls)
          (parse-body (cddr form))
        (process-optimization-declarations decls)
        (let ((p1-body (p1-body body)))
          (dolist (local-function local-functions)
            (mumble "local function ~S: ~D call(s), needs-function-object-p = ~S~%"
                    (local-function-name local-function)
                    (local-function-call-count local-function)
                    (local-function-needs-function-object-p local-function))
            (let ((var (local-function-var local-function)))
              (mumble "var = ~S var-used-non-locally-p = ~S~%"
                      (var-name var) (var-used-non-locally-p var))
              (cond ((var-used-non-locally-p var)
                     (push var *closure-vars*))
                    (t
                     (push var *local-variables*)))))
          (setq local-functions
                (remove-if #'(lambda (local-function)
                               (and (zerop (local-function-call-count local-function))
                                    (not (local-function-needs-function-object-p local-function))))
                           local-functions))
          (if local-functions
              (list* 'FLET local-functions p1-body)
              (list* 'PROGN p1-body)))))))

(defun p1-labels (form)
  (setf (compiland-needs-thread-var-p *current-compiland*) t)
  (let ((*visible-variables* *visible-variables*)
        (*local-functions* *local-functions*)
        (*current-compiland* *current-compiland*)
        (local-functions nil))
    (dolist (definition (second form))
      (let* ((name (first definition))
             (block-name (fdefinition-block-name name))
             (lambda-list (second definition))
             lambda-expression)
        (multiple-value-bind (body decls)
            (parse-body (cddr definition))
          (setq lambda-expression
                `(lambda ,lambda-list ,@decls (block ,block-name ,@body))))
        (let ((rewritten-lambda-expression (rewrite-lambda lambda-expression)))
          (when (neq rewritten-lambda-expression lambda-expression)
            (setq lambda-expression (rewrite-lambda lambda-expression))
            (setq lambda-list (second lambda-expression))))
        (validate-name-and-lambda-list name lambda-list 'LABELS)
        (let* ((compiland (make-compiland :name name
                                          :parent *current-compiland*))
               (var (make-var :name (gensym)
                              :kind :local
                              ;; REVIEW
                              :used-non-locally-p t))
               (local-function (make-local-function :name name
                                                    :compiland compiland
                                                    :var var)))
;;           (multiple-value-bind (body decls)
;;               (parse-body (cddr lambda-expression))
          (setf (compiland-lambda-expression compiland) (precompile-form lambda-expression))
          (push local-function local-functions)
          (push var *closure-vars*))))
    (setq local-functions (nreverse local-functions))
    ;; make the local functions visible
    (dolist (local-function local-functions)
      (declare (type local-function local-function))
      (push local-function *local-functions*)
      (let ((var (local-function-var local-function)))
        (when var
          (push var *visible-variables*))))
    (dolist (local-function local-functions)
      (declare (type local-function local-function))
      (let ((*visible-variables* *visible-variables*)
            (*local-variables* nil) ; REVIEW
            (*current-compiland* (local-function-compiland local-function)))
        (p1-compiland (local-function-compiland local-function))))
    (let ((*speed* *speed*)
          (*space* *space*)
          (*safety* *safety*)
          (*debug* *debug*)
          (*inline-declarations* *inline-declarations*))
      (multiple-value-bind (body declarations)
          (parse-body (cddr form))
        (process-optimization-declarations declarations)
        (list* (car form) local-functions (p1-body body))))))

(defun p1-let-vars (varlist)
  (let ((vars nil))
    (dolist (varspec varlist)
      (cond ((consp varspec)
             (when (> (length varspec) 2)
               (error "The LET binding specification ~S is invalid."
                      varspec))
             (let ((name (%car varspec))
                   (initform (p1 (cadr varspec))))
               (push (make-var :name name :kind :local :initform initform) vars)))
            (t
             (push (make-var :name varspec :kind :local) vars))))
    (setq vars (nreverse vars))
    (dolist (var vars)
      (push var *all-variables*)
      (push var *local-variables*)
      (push var *visible-variables*))
    vars))

(defun p1-let*-vars (varlist)
  (let ((vars nil))
    (dolist (varspec varlist)
      (cond ((consp varspec)
             (when (> (length varspec) 2)
               (error "The LET* binding specification ~S is invalid."
                      varspec))
             (let* ((name (%car varspec))
                    (initform (p1 (%cadr varspec)))
                    (var (make-var :name name :kind :local :initform initform)))
               (push var vars)
               (push var *all-variables*)
               (push var *local-variables*)
               (push var *visible-variables*)))
            (t
             (let ((var (make-var :name varspec :kind :local)))
               (push var vars)
               (push var *all-variables*)
               (push var *local-variables*)
               (push var *visible-variables*)))))
    (nreverse vars)))

(defknown name-looks-special-p (symbol) boolean)
(defun name-looks-special-p (x)
  (declare (type symbol x))
  (let* ((name (symbol-name x))
         (len (length name)))
    (and (> len 2)
         (eql (char name 0) #\*)
         (eql (char name (1- len)) #\*))))

(defknown unused-var (var) t)
(defun unused-var (var)
  (declare (type var var))
  (unless (or (var-ignore-p var)
              (var-ignorable-p var))
    (compiler-style-warn "The variable ~S is defined but never used."
                         (var-name var))))

(defknown check-for-unused-vars (list) t)
(defun check-for-unused-vars (vars)
  (dolist (var vars)
    (declare (type var var))
    (when (and (not (var-special-p var))
               (zerop (var-reads var))
               (zerop (var-writes var)))
      (unused-var var))))

(defknown propagate-vars (cblock) t)
(defun propagate-vars (block)
  (declare (type cblock block))
;;   (mumble "propagate-vars called~%")
;;   (return-from propagate-vars nil)
;;   (dolist (var1 (block-vars block))
;;     (let ((initform (var-initform var1)))
;;       (when (var-ref-p initform)
;;         (mumble "var1 = ~S initform = ~S (var-ref-var initform) = ~S~%"
;;                 (var-name var1)
;;                 initform
;;                 (var-name (var-ref-var initform))))))

  (let ((removed nil))
    (dolist (var (block-vars block))
      (declare (type var var))
      (unless (or (var-special-p var)
                  (var-used-non-locally-p var)
                  (neq (var-declared-type var) :none)) ;; REVIEW
        (when (zerop (var-writes var))
          ;; no writes to the variable
          (let ((initform (var-initform var)))
            (cond ((var-ref-p initform)
                   (let ((source-var (var-ref-var initform)))
                     (unless (or (var-special-p source-var)
                                 (var-used-non-locally-p source-var))
                       (when (zerop (var-writes source-var))
                         ;; we can eliminate the variable
;;                          (mumble "propagate-vars eliminating ~S source-var = ~S~%"
;;                                  (var-name var) (var-name source-var))
                         (aver (eql (var-reads var) (length (var-refs var))))
                         (dolist (ref (var-refs var))
                           (aver (eq (var-ref-var ref) var))
;;                            (mumble "fixing ~S to use ~S~%" ref (var-name source-var))
                           (setf (var-ref-var ref) source-var)
                           (aver (not (memq ref (var-refs source-var))))
                           (push ref (var-refs source-var))
                           (incf (var-reads source-var))
                           )

;;                          (dolist (var1 (block-vars block))
;;                            (let ((initform (var-initform var1)))
;;                              (when (var-ref-p initform)
;;                                (mumble "var1 = ~S initform = ~S (var-ref-var initform) = ~S~%"
;;                                        (var-name var1)
;;                                        initform
;;                                        (var-ref-var initform)))))

                         (push var removed)))))
                  ((fixnump initform)
                   (setf (var-constant-p var) t)
                   (setf (var-constant-value var) initform))
                  )))))
    (when removed
      (dolist (var removed)
        (setf (block-vars block) (remove var (block-vars block)))
        (setq *all-variables* (remove var *all-variables*))
        (setq *local-variables* (remove var *local-variables*))
        (setq *visible-variables* (remove var *visible-variables*))
        ))))

(defun p1-let/let* (form)
  (let* ((*visible-variables* *visible-variables*)
         (op (car form))
         (varlist (cadr form)))
    (aver (memq op '(LET LET*)))
;;     (mumble "varlist = ~S~%" varlist)
    (when (eq op 'LET)
      ;; Convert to LET* if possible.
      (if (null (cdr varlist))
          (setq op 'LET*)
          (dolist (varspec varlist (setq op 'LET*))
            (or (atom varspec)
                (constantp (cadr varspec))
                (eq (car varspec) (cadr varspec))
                (return)))))
    (let ((vars (if (eq op 'LET)
                    (p1-let-vars varlist)
                    (p1-let*-vars varlist))))
      ;; Check for globally declared specials.
      (dolist (var vars)
        (declare (type var var))
        (when (special-variable-p (var-name var))
          (setf (var-special-p var) t)))

      (multiple-value-bind (body declarations)
          (parse-body (cddr form) nil)
        ;; For processing declarations, we want to walk the variable list from
        ;; last to first, since declarations apply to the last-defined variable
        ;; with the specified name.
        (let ((free-specials (process-declarations-for-vars declarations (reverse vars))))

          ;; Make free specials visible.
          (dolist (var free-specials)
            (push var *visible-variables*))

          (let ((*speed*  *speed*)
                (*space*  *space*)
                (*safety* *safety*)
                (*debug*  *debug*)
                (*inline-declarations* *inline-declarations*)
                ;;(*explain* *explain*)
                )
            (process-optimization-declarations declarations)
            (dolist (var (reverse vars))
              (declare (type var var))
              (when (name-looks-special-p (var-name var))
                (unless (var-special-p var)
                  (compiler-style-warn
                   "Using a lexical binding of the symbol ~S, not a dynamic binding,
;     even though the symbol name follows the usual naming convention
;     for special variables."
                   (var-name var))))
              (p1-check-var-type var))

            (setq body (p1-body body)))

          (check-for-unused-vars vars)

          (let ((block (make-block :name (list op)
                                   :form form
                                   :vars vars
                                   :body body
                                   :free-specials free-specials)))

            (propagate-vars block)

            (when (some #'var-special-p vars)
              (let ((var (make-var :name (gensym "LAST-SPECIAL-BINDING-") :kind :local)))
                (push var *local-variables*)
                (setf (block-last-special-binding-var block) var)))
            (list op block)))))))

(defknown process-special-declarations (t) t)
(defun process-special-declarations (forms)
  (let ((specials nil))
    (dolist (form forms)
      (unless (and (consp form) (eq (%car form) 'DECLARE))
        (return))
      (let ((decls (%cdr form)))
        (dolist (decl decls)
          (when (eq (car decl) 'special)
            (setq specials (append (cdr decl) specials))))))
    specials))

(defun p1-load-time-value (form)
  ;; load-time-value form &optional read-only-p => object
  (let ((numargs (length (cdr form))))
    (unless (memq numargs '(1 2))
      (compiler-error "Wrong number of arguments for special operator ~S (expected 1-2 but received ~D)."
                      'LOAD-TIME-VALUE numargs)))
  form)

(defun p1-locally (form)
  (let ((*speed*  *speed*)
        (*space*  *space*)
        (*safety* *safety*)
        (*debug*  *debug*)
;;         (*explain* *explain*)
        (*inline-declarations* *inline-declarations*))
    (multiple-value-bind (body declarations)
        (parse-body (cdr form))
      (process-optimization-declarations declarations)
      (let ((*visible-variables* *visible-variables*)
            (specials (process-special-declarations declarations)))
        (dolist (name specials)
          (push (make-var :name name :special-p t) *visible-variables*))
        `(locally ,@declarations ,@(mapcar #'p1 body))))))

(defun p1-m-v-b (form)
  (when (length-eql (cadr form) 1)
    (let ((new-form `(let* ((,(caadr form) ,(caddr form))) ,@(cdddr form))))
      (return-from p1-m-v-b (p1-let/let* new-form))))
  (let* ((*visible-variables* *visible-variables*)
         (varlist (cadr form))
         (values-form (caddr form))
         (vars nil))
    ;; Process the values-form first. ("The scopes of the name binding and
    ;; declarations do not include the values-form.")
    (setq values-form (p1 values-form))

    ;; FIXME workaround to avoid destructive modifications
    (setq form (copy-tree form))
    ;; FIXME avoid destructive modifications!
    (setf (caddr form) values-form)

    (dolist (name varlist)
      (let ((var (make-var :name name :kind :local)))
        (push var vars)
        (push var *all-variables*)
        (push var *local-variables*)
        (push var *visible-variables*)))
    ;; Check for globally declared specials.
    (dolist (var vars)
      (declare (type var var))
      (when (special-variable-p (var-name var))
        (setf (var-special-p var) t)))
    (multiple-value-bind (body declarations)
        (parse-body (cdddr form) nil)
      (let ((free-specials (process-declarations-for-vars declarations vars)))
        ;; Make free specials visible.
        (dolist (var free-specials)
          (push var *visible-variables*))
        (setq vars (nreverse vars))
        (dolist (var vars)
          (declare (type var var))
          (when (name-looks-special-p (var-name var))
            (unless (var-special-p var)
              (compiler-style-warn
               "Using a lexical binding of the symbol ~S, not a dynamic binding, ~@
even though the symbol name follows the usual naming convention ~@
for special variables."
               (var-name var))))
          (p1-check-var-type var))
        (setq body (p1-body body))
        (setf (compiland-needs-thread-var-p *current-compiland*) t)
        (check-for-unused-vars vars)
        (let ((block (make-block :name '(MULTIPLE-VALUE-BIND)
                                 :form form
                                 :vars vars
                                 :body body
                                 :free-specials free-specials)))
          (when (some #'var-special-p vars)
            (let ((var (make-var :name (gensym "LAST-SPECIAL-BINDING-") :kind :local)))
              (push var *local-variables*)
              (setf (block-last-special-binding-var block) var)))
          (list 'MULTIPLE-VALUE-BIND block))))))

(defun p1-m-v-c (form)
  (let ((node (make-m-v-c-node :form (p1-default form))))
    (let ((var (make-var :name (gensym "FUNCTION-") :kind :local)))
      (push var *local-variables*)
      (setf (m-v-c-node-function-var node) var))
    (let ((var (make-var :name (gensym "VALUES-ADDRESS-") :kind :local)))
      (push var *local-variables*)
      (setf (m-v-c-node-values-address-var node) var))
    (let ((var (make-var :name (gensym "VALUES-LENGTH-") :kind :local)))
      (push var *local-variables*)
      (setf (m-v-c-node-values-length-var node) var))
    (setf (compiland-needs-thread-var-p *current-compiland*) t)
    (list 'MULTIPLE-VALUE-CALL node)))

(defknown maybe-rewrite-progv (t) cons)
(defun maybe-rewrite-progv (form)
  (declare (type cons form))
  (let ((symbols-form (cadr form))
        (values-form (caddr form))
        (body (cdddr form)))
    (cond ((or (unsafe-p symbols-form)
               (unsafe-p values-form))
           (let ((syms (gensym))
                 (vals (gensym)))
             `(let ((,syms ,symbols-form)
                    (,vals ,values-form))
                (progv ,syms ,vals ,@body))))
          (t
           form))))

(defun p1-progv (form)
  (let ((new-form (maybe-rewrite-progv form)))
    (cond ((eq new-form form)
           (setf (compiland-needs-thread-var-p *current-compiland*) t)
           (let* ((var (make-var :name (gensym "LAST-SPECIAL-BINDING-") :kind :local))
                  (block (make-block :name '(PROGV)
                                     :form (p1-default form)
                                     :last-special-binding-var var)))
             (push var *local-variables*)
             (list 'PROGV block)))
          (t
           (mumble "p1-progv form = ~S~%" form)
           (mumble "p1-progv new-form = ~S~%" new-form)
           (p1 new-form)))))

(defun p1-tagbody (form)
  (let* ((*visible-tags* *visible-tags*)
         (block (make-block :name (list 'TAGBODY (gensym))))
         (*visible-blocks* (cons block *visible-blocks*))
         (body (cdr form))
         (compiland *current-compiland*))
    ;; make all the tags visible before processing the body forms
    (let ((tags nil))
      (dolist (subform body)
        (when (or (symbolp subform) (integerp subform))
          (let* ((tag (make-tag :name subform :label (gensym) :block block)))
            (push tag tags)
            (push tag *visible-tags*))))
      (setf (block-tags block) (nreverse tags)))
    (let ((new-body nil)
          (live t))
      (dolist (subform body)
        (cond ((or (symbolp subform) (integerp subform))
               (push subform new-body)
               (setq live t))
              ((not live)
               ;; nothing to do
               )
              (t
               (when (and (consp subform)
                          (memq (%car subform) '(GO RETURN-FROM THROW)))
                 ;; subsequent subforms are unreachable until we see another tag
                 (setq live nil))
               (push (p1 subform) new-body))))
      (setf (block-body block) (nreverse new-body)))
;;     (when (and (block-non-local-go-p block)
;;                ;; REVIEW we really want to do this only if some code in (or under) the
;;                ;; current block binds a special, so this is safe but overkill...
;;                (some 'var-special-p *all-variables*))
;;       (let ((var (make-var :name (gensym "LAST-SPECIAL-BINDING-") :kind :local)))
;;         (push var *local-variables*)
;;         (setf (block-last-special-binding-var block) var)
;;         (setf (compiland-needs-thread-var-p compiland) t)))
    (when (block-non-local-go-p block)
      (let ((var (make-var :name (gensym "TAGBODY-") :kind :local)))
        (push var *local-variables*)
        (setf (block-tagbody-var block) var))
      (setf (compiland-needs-thread-var-p compiland) t))
    (list 'TAGBODY block)))

(defun p1-go (form)
  (let* ((name (cadr form))
         (tag (find-visible-tag name)))
    (unless tag
      (error "tag not found: ~S" name))
;;     (let ((tag-block (tag-block tag)))
;;       (cond ((eq (tag-compiland tag) *current-compiland*)
;;              ;; Does the GO leave an enclosing UNWIND-PROTECT?
;;              (let ((protected
;;                     (dolist (enclosing-block *visible-blocks*)
;;                       (when (eq enclosing-block tag-block)
;;                         (return nil))
;;                       (when (equal (block-name enclosing-block) '(UNWIND-PROTECT))
;;                         (return t)))))
;;                (when protected
;;                  (setf (block-non-local-go-p tag-block) t))))
;;             (t
;;              (setf (block-non-local-go-p tag-block) t))))
    ;; FIXME
    (unless (eq (tag-compiland tag) *current-compiland*)
      (setf (tag-non-local-go-p tag) t)
      (setf (block-non-local-go-p (tag-block tag)) t)
      (setf (compiland-needs-thread-var-p (tag-compiland tag)) t)
      (setf (compiland-needs-thread-var-p *current-compiland*) t)

      ;; REVIEW
      (setf (compiland-setjmp-p (tag-compiland tag)) t) ; setjmp
      (setf (compiland-longjmp-p *current-compiland*) t) ; longjmp
      )
    )
  form)

(defun p1-unwind-protect (form)
  (cond ((length-eql form 2)
         ;; no cleanup forms: (unwind-protect (...)) => (progn (...))
         (p1 `(progn ,(%cadr form))))
        (t
         (let* ((compiland *current-compiland*)
                (block (make-block :name '(UNWIND-PROTECT)))
                (*visible-blocks* (cons block *visible-blocks*)))
           (setf (block-form block) (p1-default form))
           (let ((var (make-var :name (gensym "UWP-") :kind :local)))
             (push var *local-variables*)
             (setf (block-uwp-var block) var))
           (let ((var (make-var :name (gensym "UWP-VALUES-") :kind :local)))
             (push var *local-variables*)
             (setf (block-uwp-values-var block) var))
           (setf (compiland-needs-thread-var-p compiland) t)
           (setf (compiland-unwind-protect-p compiland) t)
           (list 'UNWIND-PROTECT block)))))

(defun p1-multiple-value-prog1 (form)
  (let* ((block (make-block :name '(MULTIPLE-VALUE-PROG1)))
         (*visible-blocks* (cons block *visible-blocks*)))
    (setf (block-form block) (p1-default form))
    (let ((var (make-var :name (gensym "VALUES-") :kind :local)))
      (push var *local-variables*)
      (setf (block-values-var block) var))
    (setf (compiland-needs-thread-var-p *current-compiland*) t)
    (list 'MULTIPLE-VALUE-PROG1 block)))

(defun p1-setq (form)
  (unless (length-eql form 3)
    (setq form (pre::precompile-setq form))
    (return-from p1-setq (p1 form)))
  (let* ((arg1 (%cadr form))
         (arg2 (%caddr form))
         (value-form arg2))
    (let ((var (find-visible-var arg1)))
      (cond ((null var)
             (unless (or (special-variable-p arg1)
                         (memq arg1 *undefined-variables*))
               (compiler-warn "Undefined variable: ~S" arg1)
               (push form *undefined-variables*))
             (setq var (make-var :name form :kind :local :special-p t))
             (push var *local-variables*)
             (push var *visible-variables*))
            (t
             (when (var-ignore-p var)
               (compiler-style-warn
                "Variable ~S is assigned even though it was declared to be ignored."
                (var-name var)))
             (incf (var-writes var))
             (pushnew *current-compiland* (var-writers var))
             (unless (eq (var-compiland-id var) (compiland-id *current-compiland*))
               (setf (var-used-non-locally-p var) t)
               (pushnew var *closure-vars*))))
      ;; REVIEW
      (when (consp arg2)
        (let ((op (%car arg2)))
          (cond ((and (eq op 'TWO-ARG-+)
                      (eq (cadr arg2) arg1))
                 (setf (var-register-worthiness var) 100))
                ((and (memq op '(CDR %CDR))
                      (eq (cadr arg2) arg1))
                 (setf (var-register-worthiness var) 100)))))
      (unless (zerop *safety*)
        (let ((type (var-declared-type var)))
          (unless (eq type :none)
            (setq value-form `(the ,type ,arg2)))))
      (list 'SETQ arg1 (p1 value-form)))))

(defun p1-incq (form)
  (aver (length-eql form 3))
  (aver (symbolp (second form)))
  (aver (numberp (third form)))
  (let ((symbol (second form))
        (increment (third form)))
;;     (setq form (list 'SETQ symbol (p1 (list 'two-arg-+ symbol increment))))
;;     (p1 form)
    (p1 `(setq ,symbol (two-arg-+ ,symbol ,increment)))))

(defun p1-the (form)
  (unless (length-eql form 3)
    (compiler-error "Wrong number of arguments for special operator ~A (expected 2, but received ~D)."
                    'THE
                    (1- (length form))))
  (let ((type (%cadr form))
        (expr (%caddr form)))
    (cond ((and (listp type) (eq (car type) 'VALUES))
           ;; FIXME
           (p1 expr))
          ((eq type t)
           (p1 expr))
          ((zerop *safety*)
;;            (mumble "p1-the *safety* is 0~%")
           (list 'TRULY-THE type (p1 expr)))
          (t
           (let ((new-form `(truly-the ,type (require-type ,expr ',type))))
;;              (mumble "p1-the form = ~S~%" form)
;;              (mumble "p1-the new-form = ~S~%" new-form)
             (p1 new-form))))))

(defun p1-truly-the (form)
  (unless (length-eql form 3)
    (compiler-error "Wrong number of arguments for special operator ~A (expected 2, but received ~D)."
                    'TRULY-THE
                    (1- (length form))))
  (list 'TRULY-THE (%cadr form) (p1 (%caddr form))))

(defun p1-require-type (form)
  (when (check-arg-count form 2)
    (let* ((args (%cdr form))
           (arg1 (%car args))
           (arg2 (%cadr args))
           (new-form nil))
      (when (quoted-form-p arg2)
        (let ((type (%cadr arg2)))
          (cond ((eq type 'SYMBOL)
                 (setq new-form `(require-symbol ,arg1)))
                ((eq type 'CHARACTER)
                 (setq new-form `(require-character ,arg1)))
                ((eq type 'CONS)
                 (setq new-form `(require-cons ,arg1)))
                ((eq type 'LIST)
                 (setq new-form `(require-list ,arg1)))
                ((eq type 'STRING)
                 (setq new-form `(require-string ,arg1)))
                ((eq type 'SIMPLE-STRING)
                 (setq new-form `(require-simple-string ,arg1)))
                ((eq type 'HASH-TABLE)
                 (setq new-form `(require-hash-table ,arg1)))
                ((eq type 'FIXNUM)
                 (setq new-form `(require-fixnum ,arg1)))
                ((eq type 'FUNCTION)
                 (setq new-form `(require-function ,arg1)))
                ((eq type 'INTEGER)
                 (setq new-form `(require-integer ,arg1)))
                ((eq type 'NUMBER)
                 (setq new-form `(require-number ,arg1)))
                ((eq type 'KEYWORD)
                 (setq new-form `(require-keyword ,arg1)))
                ((eq type 'STREAM)
                 (setq new-form `(require-stream ,arg1)))
                ((eq type 'UNSIGNED-BYTE)
                 (setq new-form `(require-unsigned-byte ,arg1)))
                ((or (eq type 'BOOLEAN)
                     (equal type '(MEMBER T NIL))
                     (equal type '(MEMBER NIL T)))
                 (setq new-form `(require-boolean ,arg1))))
          (when new-form
            (return-from p1-require-type (p1 new-form))))
        (let ((type (canonicalize-type (%cadr arg2))))
          (cond ((subtypep type 'SIMPLE-STRING)
                 ;; REVIEW
                 (setq new-form `(require-simple-string ,arg1)))
                ((subtypep type 'STRING)
                 ;; REVIEW
                 (setq new-form `(require-string ,arg1)))
                ((subtypep type 'CONS)
                 (setq new-form `(require-cons ,arg1)))
                ((and (consp type)
                      (memq (%car type) '(AND OR MEMBER SATISFIES)))
                 ;; FIXME
                 (unless (eql *safety* 3)
                   (mumble "p1-require-type 1 not doing type check for ~S~%" type)
                   (setq new-form `(progn ,arg1))))
                ;; REVIEW
                ((subtypep type 'FIXNUM)
                   (cond ((equal type '(INTEGER #.most-negative-fixnum #.most-positive-fixnum))
                          (setq new-form `(require-fixnum ,arg1)))
                         ((fixnum-type-p type)
                          (setq new-form
                                `(truly-the ,type
                                            (check-fixnum-bounds ,arg1
                                                                 ,(integer-type-low  type)
                                                                 ,(integer-type-high type)))))))
                ((subtypep type 'FUNCTION)
                 (setq new-form `(require-function ,arg1)))
                #+x86
                ((equal type '(INTEGER 0 4294967295))
                 (setq new-form `(require-ub32 ,arg1)))
                ((equal type '(INTEGER 0 *))
                 (setq new-form `(require-unsigned-byte ,arg1)))
;;                 ((eq type 'NUMBER)
;;                  (setq new-form `(require-number ,arg1)))
;;                 ((eq type 'KEYWORD)
;;                  (setq new-form `(require-keyword ,arg1)))
;;                 ((eq type 'STREAM)
;;                  (setq new-form `(require-stream ,arg1)))
                ((subtypep type 'SIMPLE-VECTOR)
                 (setq new-form `(require-simple-vector ,arg1)))
                ((subtypep type 'VECTOR)
                 ;; FIXME
                 (setq new-form `(require-vector ,arg1)))
                ((subtypep type 'STRUCTURE-OBJECT)
                 (cond ((and (symbolp type)
                             (null (deftype-expander type)))
                        (setq new-form `(require-structure-type ,arg1 ',type)))
                       (t
                        (let ((sym (gensym)))
                          (setq new-form
                                `(truly-the ,type
                                            (let ((,sym ,arg1))
                                              (unless (structure-object-p ,sym)
                                                (%type-error ,sym ',type))
                                              ,sym)))))))
                ((eq type 'WARNING)
                 ;; pass through (warn.18)
                 )
                ((or (eql *safety* 3)
                     *force-type-checks*)
                 ;; pass through
                 )
                (t
                 (unless (eq type t)
                   (let ((*print-structure* nil))
                     (mumble "p1-require-type 2 not doing type check for ~S~%" type)))
                 (setq new-form `(progn ,arg1))))))
      (if new-form
          (p1 new-form)
          (p1-function-call form)))))

(defun p1-ash (form)
  (cond ((and (length-eql form 3)
              (integerp (%cadr form))
              (integerp (%caddr form)))
         (ash (%cadr form) (%caddr form)))
        (t
         (p1-function-call form))))

(defun p1-length (form)
  (cond ((length-eql form 2)
         (let ((arg (%cadr form)))
           (cond ((vectorp arg)
                  ;; wrap result with PROGN so it won't be mistaken for a tag
                  `(progn ,(length arg)))
                 ((and (quoted-form-p arg)
                       (sequencep (%cadr arg)))
                  ;; wrap result with PROGN so it won't be mistaken for a tag
                  `(progn ,(length (%cadr arg))))
                 (t
                  (p1-function-call form)))))
        (t
         (p1-function-call form))))

(defun p1-minus (form)
  (let ((new-form (maybe-rewrite-function-call form)))
    (cond ((neq new-form form)
           (p1 new-form))
;;           ((length-eql form 2)
;;            ;; unary minus
;;            (p1 `(two-arg-- 0 ,(%cadr form))))
          (t
           (setq form (p1-default form))
           (let ((args (cdr form)))
             (cond ((every #'integerp args)
                    ;; wrap result with PROGN so it won't be mistaken for a tag
                    `(progn ,(apply #'- args)))
                   (t
                    form)))))))

(defun p1-1+ (form)
  (cond ((length-eql form 2)
         (p1 `(two-arg-+ ,(%cadr form) 1)))
        (t
         (p1-function-call form))))

(defun p1-1- (form)
  (cond ((length-eql form 2)
         (p1 `(two-arg-- ,(%cadr form) 1)))
        (t
         (p1-function-call form))))

(defun install-p1-handler (symbol handler)
  (setf (get symbol 'p1-handler) handler))

(install-p1-handler 'and                        'p1-default)
(install-p1-handler 'block                      'p1-block)
(install-p1-handler 'catch                      'p1-catch)
(install-p1-handler 'eval-when                  'p1-eval-when)
(install-p1-handler 'flet                       'p1-flet)
(install-p1-handler 'funcall                    'p1-funcall)
(install-p1-handler 'function                   'p1-function)
(install-p1-handler 'go                         'p1-go)
(install-p1-handler 'if                         'p1-if)
(install-p1-handler 'incq                       'p1-incq)
(install-p1-handler 'labels                     'p1-labels)
(install-p1-handler 'let                        'p1-let/let*)
(install-p1-handler 'let*                       'p1-let/let*)
(install-p1-handler 'load-time-value            'p1-load-time-value)
(install-p1-handler 'locally                    'p1-locally)
(install-p1-handler 'multiple-value-bind        'p1-m-v-b)
(install-p1-handler 'multiple-value-call        'p1-m-v-c)
(install-p1-handler 'multiple-value-list        'p1-default)
(install-p1-handler 'multiple-value-prog1       'p1-multiple-value-prog1)
(install-p1-handler 'or                         'p1-default)
(install-p1-handler 'progn                      'p1-default)
(install-p1-handler 'progv                      'p1-progv)
(install-p1-handler 'return-from                'p1-return-from)
(install-p1-handler 'quote                      'p1-quote)
(install-p1-handler 'setq                       'p1-setq)
(install-p1-handler 'tagbody                    'p1-tagbody)
(install-p1-handler 'the                        'p1-the)
(install-p1-handler 'throw                      'p1-throw)
(install-p1-handler 'truly-the                  'p1-truly-the)
(install-p1-handler 'unwind-protect             'p1-unwind-protect)

(install-p1-handler '-                          'p1-minus)
(install-p1-handler '1+                         'p1-1+)
(install-p1-handler '1-                         'p1-1-)
(install-p1-handler 'ash                        'p1-ash)
(install-p1-handler 'dpb                        'p1-dpb)
(install-p1-handler 'ldb                        'p1-ldb)
(install-p1-handler 'length                     'p1-length)
(install-p1-handler 'list*                      'p1-list*)
(install-p1-handler 'require-type               'p1-require-type)
(install-p1-handler 'two-arg--                  'p1-minus)

(defvar *ir2-only* nil)

(defvar *dump-code* nil)

(defun dump-code ()
  (when *dump-code*
    (let ((i 0))
      (dolist (instruction (coerce *code* 'list))
        (fresh-line *debug-io*)
        (mumble "~4,D: " i)
        (incf i)
        (cond ((vectorp instruction)
               (mumble "~S~%" instruction))
              ((consp instruction)
               (let ((list instruction))
                 (mumble "(~S" (car list))
                 (setq list (cdr list))
                 (loop
                   (when (null list)
                     (return))
                   (let ((thing (car list)))
                     (write-char #\space *debug-io*)
                     (if (var-p thing)
                         (print-var thing *debug-io*)
                         (mumble "~S" thing)))
                   (setq list (cdr list)))
                 (write-char #\) *debug-io*)))
              ((ir2-instruction-p instruction)
               (let ((operand1 (operand1 instruction))
                     (operand2 (operand2 instruction)))
                 (mumble "~S" (operator instruction))
                 (when (or operand1 operand2)
                   (write-char #\space *debug-io*)
                   (if (var-p operand1)
                       (print-var operand1 *debug-io*)
                       (mumble "~S" operand1)))
                 (when operand2
                   (write-char #\space *debug-io*)
                   (if (var-p operand2)
                       (print-var operand2 *debug-io*)
                       (mumble "~S" operand2))))))
        (terpri *debug-io*)))))

(defknown labelp (t) t)
(defun labelp (thing)
  ;; an uninterned symbol
  (and (symbolp thing)
       (null (symbol-package thing))))

(defknown optimize-ir2-1 () t)
(defun optimize-ir2-1 ()
  (let ((code *code*)
        (changed nil)
        (dead nil))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let* ((instruction (svref code i)))
;;         (aver (ir2-instruction-p instruction))
        (declare (type ir2-instruction instruction))
        (case (operator instruction)
          (:exit
           (cond (dead
                  (setf (svref code i) nil)
                  (setq changed t))
                 (t
                  (setq dead t))))
          ((:jmp-short :jmp)
           (cond (dead
                  (setf (svref code i) nil)
                  (setq changed t))
                 (t
                  (let ((test (operand1 instruction)))
                    (when (eq test t)
                      ;; unconditional jump
                      (setq dead t))))))
          (:label
           (setq dead nil))
          (t
           (when dead
             (setf (svref code i) nil)
             (setq changed t))))))
    (when changed
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-ir2-1b () t)
(defun optimize-ir2-1b ()
  (let ((code *code*)
        (changed nil)
        (ht nil))
    (declare (type simple-vector code))
    (dotimes (i (1- (length code)))
      (let ((instruction-1 (svref code i)))
        (when (ir2-instruction-p instruction-1)
          ;;           (unless (ir2-instruction-p instruction-1)
          ;;             (mumble "instruction-1 = ~S~%" instruction-1)
          ;;             (aver (ir2-instruction-p instruction-1)))
          (when (eq (operator instruction-1) :label)
            (let ((instruction-2 (svref code (1+ i))))
              ;;               (aver (ir2-instruction-p instruction-2))
              (declare (type ir2-instruction instruction-2))
              (when (eq (operator instruction-2) :label)
                ;; two labels in a row
                ;; kill the second one
                (unless ht
                  (setq ht (make-hash-table :test 'eq)))
                (let ((symbol-1 (operand1 instruction-1))
                      (symbol-2 (operand1 instruction-2)))
                  ;;                   (aver (not (null symbol-1)))
                  ;;                   (aver (symbolp symbol-1))
                  ;;                   (aver (not (null symbol-2)))
                  ;;                   (aver (symbolp symbol-2))
                  (setf (gethash symbol-2 ht) symbol-1)
                  (setf (svref code (1+ i)) nil)
                  (setq changed t))))))))
    (when changed
      (dotimes (i (length code))
        (let ((instruction (svref code i)))
          (when (ir2-instruction-p instruction)
            (case (operator instruction)
              ((:jmp-short :jmp)
               (let ((target (operand2 instruction)))
                 ;;                    (aver (not (null target)))
                 ;;                    (aver (symbolp target))
                 (let ((referral (gethash2-1 target ht)))
                   (when referral
                     (setf (operand2 instruction) referral)))))
              (:call
               ;;                  (aver (null (operand2 instruction)))
               (let ((target (operand1 instruction)))
                 (when (labelp target)
                   ;;                      (aver (not (null target)))
                   ;;                      (aver (symbolp target))
                   (let ((referral (gethash2-1 target ht)))
                     (when referral
                       (setf (operand1 instruction) referral))))))))))
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-ir2-1c () t)
(defun optimize-ir2-1c ()
  ;; change this:
  ;;
  ;;       jne     L1
  ;;       jmp     L0
  ;;   L1:
  ;;
  ;; to this:
  ;;
  ;;       je      L0
  ;;   L1:
  (let ((code *code*)
        (changed nil))
    (declare (type simple-vector code))
    (dotimes (i (- (length code) 2))
      (let ((instruction-1 (svref code i))
            (instruction-2 (svref code (+ i 1)))
            (instruction-3 (svref code (+ i 2))))
        (declare (type ir2-instruction instruction-1 instruction-2 instruction-3))
        (when (and ;(ir2-instruction-p instruction-1)
                   ;(ir2-instruction-p instruction-2)
                   ;(ir2-instruction-p instruction-3)
                   (memq (operator instruction-1) '(:jmp-short :jmp))
                   (memq (operator instruction-2) '(:jmp-short :jmp))
                   (eq (operator instruction-3) :label)
                   (neq (operand1 instruction-1) t)
                   (eq (operand1 instruction-3) (operand2 instruction-1))
                   (eq (operand1 instruction-2) t))
          (let* ((test (operand1 instruction-1))
                 (negated-test (case test
                                 (:e   :ne)
                                 (:z   :nz)
                                 (:ne  :e)
                                 (:nz  :z)
                                 (:l   :nl)
                                 (:nl  :l)
                                 (:ge  :l)
                                 (:g   :ng)
                                 (:ng  :g)
                                 (:a   :na)
                                 (:ae  :nae)
                                 (:na  :a)
                                 (:nae :ae)
                                 (:o   :no)
                                 (:no  :o)
                                 (t
                                  (aver nil)
                                  nil))))
            (cond (negated-test
                   (setf (svref code i) nil)
                   (setf (operand1 instruction-2) negated-test)
                   (setq changed t))
                  (t
                   (mumble "optimize-ir2-1c unsupported test ~S~%" test)
                   (aver nil)
                   nil))))))
    (when changed
      (setq *code* (delete nil code)))))

;; get rid of unused labels
(defknown optimize-2 () t)
(defun optimize-ir2-2 ()
  (let ((code *code*)
        (changed nil)
        (used-labels (make-hash-table :test 'EQ)))
    (declare (type simple-vector code))
    ;; first pass: populate used labels hash table
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
;;              (operator (and (ir2-instruction-p instruction)
;;                             (operator instruction))))
        (declare (type ir2-instruction instruction))
        (case (operator instruction)
          ((:jmp-short :jmp)
           (let ((target (operand2 instruction)))
;;              (aver (not (null target)))
;;              (aver (symbolp target))
             (setf (gethash target used-labels) t)))
          (:call
           (let ((target (operand1 instruction)))
             (when (labelp target)
               (setf (gethash target used-labels) t)))))))
    ;; second pass: replace unused labels with nil
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
        (declare (type ir2-instruction instruction))
        (when (and (eq (operator instruction) :label)
                   (neq (operand2 instruction) :external))
          (let ((target (operand1 instruction)))
;;             (aver (not (null target)))
;;             (aver (symbolp target))
            (unless (gethash2-1 target used-labels)
              (setf (svref code i) nil)
              (setq changed t))))))
    (when changed
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-ir2-3 () t)
(defun optimize-ir2-3 ()
  (let ((code *code*)
        (changed nil)
        (label nil)
        (ht (make-hash-table :test 'eq)))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let* ((instruction (svref code i))
             (operator (and (ir2-instruction-p instruction)
                            (operator instruction))))
        (cond (label
               ;; the previous instruction was a label
               ;; record this one if it's an unconditional jump
               (when (and (memq operator '(:jmp :jmp-short))
                          (eq (operand1 instruction) t))
                 (let ((target (operand2 instruction)))
                   (setf (gethash label ht) target)))
               (setq label nil))
              ((eq operator :label)
               ;; this instruction is a label
               (setq label (operand1 instruction))))))
    (unless (zerop (hash-table-count ht))
      (dotimes (i (length code))
        (let ((instruction (svref code i)))
;;           (aver (ir2-instruction-p instruction))
          (declare (type ir2-instruction instruction))
          (when (memq (operator instruction) '(:jmp-short :jmp))
            ;; this instruction is a conditional or unconditional jump
            (let ((target (operand2 instruction)))
;;               (aver (not (null target)))
;;               (aver (symbolp target))
              (let ((referral (gethash2-1 target ht)))
                (when (and referral (neq referral target))
                  (setf (operand2 instruction) referral)
                  (setq changed t))))))))
    changed))

;; get rid of unconditional jumps to the next instruction
(defknown optimize-ir2-4 () t)
(defun optimize-ir2-4 ()
  (let ((code *code*)
        (changed nil))
    (declare (type simple-vector code))
    (dotimes (i (1- (length code)))
      (let ((instruction-1 (svref code i))
            (instruction-2 (svref code (1+ i)))
            target)
;;         (aver (ir2-instruction-p instruction-1))
;;         (aver (ir2-instruction-p instruction-2))
        (declare (type ir2-instruction instruction-1 instruction-2))
        (when (and (memq (operator instruction-1) '(:jmp-short :jmp))
                   (eq (operand1 instruction-1) t))
          ;; instruction-1 is an unconditional jump
          (setq target (operand2 instruction-1))
          (when (eq (operator instruction-2) :label)
            ;; instruction-2 is a label
            (when (eq target (operand1 instruction-2))
              ;; instruction-1 is an unconditional jump to instruction-2
              (setf (svref code i) nil)
              (setq changed t))))))
    (when changed
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-ir2-5 () t)
(defun optimize-ir2-5 ()
  (let ((code *code*)
        (changed nil))
    (declare (type simple-vector code))
    (dotimes (i (1- (length code)))
      (let ((instruction-1 (svref code i))
            (instruction-2 (svref code (1+ i))))
;;         (aver (ir2-instruction-p instruction-1))
;;         (aver (ir2-instruction-p instruction-2))
        (declare (type ir2-instruction instruction-1 instruction-2))
        (when (and (eq (operator instruction-1) :sar)
                   (eq (operator instruction-2) :shl))
          (let ((reg1 (operand2 instruction-1))
                (reg2 (operand2 instruction-2)))
            (when (eq reg1 reg2)
              (let ((shift1 (operand1 instruction-1))
                    (shift2 (operand1 instruction-2)))
                (when (and (fixnump shift1)
                           (fixnump shift2)
                           (> shift2 shift1))
                  (setf (svref code i) nil)
                  (setf (svref code (1+ i)) (make-ir2-instruction :shl (- shift2 shift1) reg1))
                  (setq changed t))))))))
    (when changed
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-ir2-8 () t)
(defun optimize-ir2-8 ()
  ;; Look for a sequence of 3 instructions like this:
  ;;     (:compare-immediate nil reg64)
  ;;     (:jmp :ne label)
  ;;     (:move-immediate nil reg32)
  ;; where reg32 is the 32-bit register corresponding to reg64. In this case we
  ;; can get rid of the third instruction, since nil is already in reg64.
  (let ((code *code*)
        (changed nil))
    (declare (type simple-vector code))
    (dotimes (i (- (length code) 2)) ; we're looking for a sequence of 3 instructions
      (let ((instruction-1 (svref code i)))
        (when instruction-1
          (aver (ir2-instruction-p instruction-1)))
        (when (and instruction-1
                   (eq (operator instruction-1) :compare-immediate)
                   (eq (operand1 instruction-1) nil)
                   (memq (operand2 instruction-1)
                         #+x86-64 '(:rax :rcx :rdx :rbx)
                         #+x86    '(:eax :ecx :edx :ebx)))
          (let ((reg (operand2 instruction-1))
                (instruction-2 (svref code (+ i 1))))
            (when (and instruction-2
                       (eq (operator instruction-2) :jmp-short)
                       (eq (operand1 instruction-2) :ne))
              (let ((instruction-3 (svref code (+ i 2))))
                (when (and instruction-3
                           (eq (operator instruction-3) :move-immediate)
                           (equal (operand1 instruction-3)
                                  #+x86-64 '(:constant-32 nil)
                                  #+x86    '(:constant nil))
                           (eq (operand2 instruction-3)
                               #+x86-64 (reg32 reg)
                               #+x86    reg))
                  (mumble "optimize-ir2-8 optimizing...~%")
                  (setf (svref code (+ i 2)) nil)
                  (setq changed t))))))))
    (when changed
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-tail-calls () t)
(defun optimize-tail-calls()
  (let ((code *code*)
        (changed nil))
    (declare (type simple-vector code))
    (dotimes (i (1- (length code)))
      (let ((instruction-1 (svref code i))
            (instruction-2 (svref code (1+ i))))
        (when (and (ir2-instruction-p instruction-1)
                   (ir2-instruction-p instruction-2)
                   (eq (operator instruction-1) :call)
                   (eq (operator instruction-2) :exit))
          (when (and (symbolp (operand1 instruction-1))
                     (kernel-function-p (operand1 instruction-1))
                     (<= 0 (function-arity (operand1 instruction-1)) 6))
            (setf (svref code i) (make-ir2-instruction :tail-call (operand1 instruction-1) nil))
            (setf (svref code (1+ i)) nil)
            (setf changed t)))))
    (when changed
      (setq *code* (delete nil code)))
    changed))

(defknown optimize-ir2 () t)
(defun optimize-ir2 ()
  (when (> *speed* 0)
    (loop
      (let ((changed nil))
        (setq changed (or (optimize-ir2-1)  changed))
        (setq changed (or (optimize-ir2-1b) changed))
        (setq changed (or (optimize-ir2-1c) changed))
        (setq changed (or (optimize-ir2-2)  changed))
        (setq changed (or (optimize-ir2-3)  changed))
        (setq changed (or (optimize-ir2-4)  changed))
        (setq changed (or (optimize-ir2-5)  changed))
        (setq changed (or (optimize-ir2-8)  changed))
        (unless changed
          (return))))
    #+x86-64
    (when (> *speed* *debug*)
    (optimize-tail-calls))))

(defun analyze-ir2 ()
  (let* ((code *code*)
         (thread-var-used-p nil)
         (need-stack-frame-p nil)
         (leaf-p t)
         (compiland *current-compiland*)
         #+x86-64
         (thread-var :r12)
         #+x86
         (thread-var (compiland-thread-var compiland))
         )
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (svref code i)))
        (unless (ir2-instruction-p instruction)
          (mumble "analyze-ir2 not ir2-instruction-p: ~S~%" instruction)
          (aver nil)
          (return-from analyze-ir2))
        (let ((operator (operator instruction))
              (operand1 (operand1 instruction))
              (operand2 (operand2 instruction)))
          (cond ((eq operator :call)
                 (setq leaf-p nil))
                ((eq operator :allocate-local)
                 ;; not a use of the var in question
                 )
                ((and thread-var
                      (or (eq operand1 thread-var)
                          (eq operand2 thread-var)))
                 (setq thread-var-used-p t))
                ((and thread-var
                      (consp operand1)
                      (memq thread-var operand1))
                 (setq thread-var-used-p t))
                ((and thread-var
                      (consp operand2)
                      (memq thread-var operand2))
                 (setq thread-var-used-p t))
                ((or (var-p operand1)
                     (var-p operand2))
;;                  (mumble "~S ~S ~S~%" operator operand1 operand2)
                 (unless (eq operator :initialize-arg-var)
                   (setq need-stack-frame-p t)))))))
;;     (mumble "analyze-ir2 thread-var-used-p = ~S need-stack-frame-p = ~S leaf-p = ~S~%"
;;             thread-var-used-p need-stack-frame-p leaf-p)
    (unless thread-var-used-p
      (when (compiland-needs-thread-var-p compiland)
;;         (mumble "analyze-ir2 thread var not used~%")
        (setf (compiland-needs-thread-var-p compiland) nil)
        #+x86
        (setf (compiland-thread-var compiland) nil)))
    #+x86-64
    (when leaf-p
;;       (mumble "analyze-ir2 leaf-p~%")
      (setf (compiland-leaf-p compiland) leaf-p)
      (unless need-stack-frame-p
        (when (trivial-p compiland)
;;           (mumble "analyze-ir2 omit frame pointer~%")
          (setf (compiland-omit-frame-pointer compiland) t))))
    ))

(defconstant +assemble-instruction-output+
  (make-array 16 :element-type '(unsigned-byte 8) :fill-pointer 0))

(defun assemble-ir2-instruction (instruction)
;;   (declare (type cons instruction))
  (aver (ir2-instruction-p instruction))
  (case (operator instruction)
    (:label
     (make-instruction :label 0 (operand1 instruction)))
    (:jmp-short
     (let ((test  (operand1 instruction))
           (label (operand2 instruction)))
       (make-instruction :jmp-short 2 (list test label))))
    (:jmp
     (let ((test  (operand1 instruction))
           (label (operand2 instruction)))
       (make-instruction :jmp
                          (if (memq test '(t :jump-table))
                              5
                              6)
                          (list test label))))
    (:function
     (make-instruction :function 4 (operand1 instruction)))
    (t
     (let ((asm:*output* +assemble-instruction-output+))
       (setf (fill-pointer +assemble-instruction-output+) 0)
;;        (asm::assemble-instruction instruction)
       (if (consp instruction)
           (asm::assemble-instruction instruction)
           (asm::assemble-instruction (list (operator instruction)
                                            (operand1 instruction)
                                            (operand2 instruction))))
       (make-instruction :bytes (length asm:*output*) (coerce-vector-to-list asm:*output*))))))

(defun p1-lambda-list (lambda-list compiland)
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux)
      (parse-lambda-list lambda-list)
    (declare (ignore keys allowp aux))
    (let ((vars nil))
      (cond (auxp
             (compiler-unsupported "&AUX variable in lambda list"))
            ((or optional keyp)
             (let ((names (lambda-list-names lambda-list))
                   (arg-index 0))
               (dolist (name names)
                 (let ((var (make-var :name name :kind :required :arg-index arg-index)))
                   (push var vars))
                 (incf arg-index))))
            (restp
             (let ((arg-index 0))
               (dolist (name required)
                 (let ((var (make-var :name name :kind :required :arg-index arg-index)))
                   (push var vars))
                 (incf arg-index))
               (let ((var (make-var :name rest :kind :rest :arg-index arg-index)))
                 (push var vars))))
            ((> (length required) 6)
             ;; required parameters only, more than 6
             (let ((arg-index 0))
               (dolist (name required)
                 ;; index will be adjusted in p2-function-prolog
                 (let ((var (make-var :name name :kind :required :arg-index arg-index)))
                   (push var vars))
                 (incf arg-index))))
            (t
             ;; required parameters only, 6 or fewer
             #+x86
             (let ((index 2)) ; first arg is at 8(%ebp)
               (dolist (name required)
                 (let ((var (make-var :name name :kind :required :index index)))
                   (push var vars))
                 (incf index)))
             #+x86-64
             (let ((index 0))
               (dolist (name lambda-list)
                 (let* ((register (case index
                                    (0 :rdi)
                                    (1 :rsi)
                                    (2 :rdx)
                                    (3 :rcx)
                                    (4 :r8)
                                    (5 :r9)
                                    (t
                                     (compiler-unsupported "P1-LAMBDA-LIST shouldn't happen"))))
                        (var (make-var :name name :kind :required :arg-register register)))
                   (push var vars))
                 (incf index)))))
      (let* ((minargs (length required))
             (maxargs (if (or optional restp keyp)
                          (1- call-arguments-limit)
                          minargs)))
        (setf (compiland-minargs compiland) minargs)
        (setf (compiland-maxargs compiland) maxargs)
        (when (eql minargs maxargs)
          (setf (compiland-arity compiland) minargs)))
      (setf (compiland-arg-vars compiland) (nreverse vars)))))

(defun generate-local-variable-information (compiland)
  (let ((alist nil))
    (dolist (var (compiland-arg-vars compiland))
      (declare (type var var))
      (setq alist (acons (var-index var) (var-name var) alist)))
    (dolist (var (compiland-local-vars compiland))
      (declare (type var var))
      (setq alist (acons (var-index var) (var-name var) alist)))
    alist))

(defun save-local-variable-information (compiled-function)
;;   (let ((alist nil))
;;     (dolist (var (compiland-arg-vars *current-compiland*))
;;       (declare (type var var))
;;       (setq alist (acons (var-index var) (var-name var) alist)))
;;     (dolist (var (compiland-local-vars *current-compiland*))
;;       (declare (type var var))
;;       (setq alist (acons (var-index var) (var-name var) alist)))

;;     (let ((plist (function-plist compiled-function)))
;;       (setf (getf plist 'sys::locals) (generate-local-variable-information *current-compiland*))
;;       (set-function-plist compiled-function plist))
  (let ((info (generate-local-variable-information *current-compiland*)))
    (set-local-variable-information compiled-function info)))

(defun p1-compiland (compiland)
  (declare (type compiland compiland))
  (aver (null *local-variables*))
  (let* ((*current-compiland* compiland)
         (lambda-expression (compiland-lambda-expression compiland))
         (lambda-list (cadr lambda-expression)))

    (p1-lambda-list lambda-list compiland)

    (multiple-value-bind (body decls)
        (parse-body (cddr lambda-expression))
      (process-optimization-declarations decls)
      (let ((*visible-variables* *visible-variables*))

        (dolist (var (compiland-arg-vars compiland))
          (push var *all-variables*)
          (push var *visible-variables*))

        (let ((free-specials (process-declarations-for-vars decls *visible-variables*)))
          ;; make free specials visible
          (dolist (var free-specials)
            (push var *visible-variables*)))

        (dolist (var (reverse (compiland-arg-vars compiland)))
          (p1-check-var-type var))

        (unless (length-eql body 1)
          (setq body (list (list* 'progn body))))

        ;; do pass 1
        (setf (compiland-p1-body compiland) (p1 (car body)))

        (check-for-unused-vars (compiland-arg-vars compiland))

        (let ((needs-thread-var-p (compiland-needs-thread-var-p compiland)))
          (unless needs-thread-var-p
            (let ((called-names (compiland-called-names compiland)))
              (when called-names
                (dolist (name called-names)
                  (unless (operator-single-valued-p name)
                    (setq needs-thread-var-p t))))
              (unless needs-thread-var-p
                (dolist (var *local-variables*)
                  (when (var-special-p var)
                    (unless (constantp (var-name var))
                      (setq needs-thread-var-p t))))))
            (setf (compiland-needs-thread-var-p compiland) needs-thread-var-p)))

        (setf (compiland-local-vars compiland) *local-variables*)

        (unless (compiland-child-p compiland)
          (when *closure-vars*
            (let ((i 0))
              (dolist (var (reverse *closure-vars*))
                (setf (var-closure-index var) i)
                (incf i)))))))))

(defun allocate-thread-var (compiland)
  #+x86-64
  (setf (compiland-thread-register compiland) :r12)
  #+x86
  (let ((var (make-var :name (gensym "THREAD-") :kind :local)))
    (setf (compiland-thread-var compiland) var)))

(defknown p2-check-argument-types (compiland) t)
(defun p2-check-argument-types (compiland)
  (declare (type compiland compiland))
  (dolist (var (compiland-arg-vars compiland))
    (declare (type var var))
    (let ((type-check-form (var-type-check-form var)))
      (when type-check-form
        (p2 type-check-form nil)
        (aver (neq (var-declared-type var) :none))
        (setf (var-derived-type var) (canonicalize-type (var-declared-type var)))))))

(defun trivial-p (compiland)
  (let ((arity (compiland-arity compiland)))
    (and arity
         (<= arity 6)
         (null *closure-vars*))))

#+nil
(defun repeat-p2 (compiland)
  (setq *code* nil
        *main* nil
        *elsewhere* nil)
  (dolist (var *all-variables*)
    (setf (var-derived-type var) :unknown)
    #+x86-64
    (setf (var-index var) nil) ; REVIEW
    (setf (var-register-worthiness var) 0)
    (setf (var-register var) nil))
  (clrhash (compiland-common-labels compiland))
  (setf (compiland-constants compiland) nil)
  (clear-register-contents)
  (clear-constraints)
  (p2-trivial-function-prolog compiland)
  (p2-check-argument-types compiland)
  (p2 (compiland-p1-body compiland) :return)
  (aver (vectorp *main*))
  (if *elsewhere*
      (setq *code* (concatenate 'simple-vector *main* *elsewhere*))
      (setq *code* (coerce *main* 'simple-vector)))
  (optimize-ir2)
  (analyze-ir2))

(defun p2-compiland (compiland)
  (declare (type compiland compiland))
  (let ((*local-variables* (compiland-local-vars compiland))
        (*visible-variables* (reverse (append *closure-vars* (compiland-arg-vars compiland))))
        (*register-contents* *register-contents*)
        (*constraints* nil)
        (*main* nil)
        (*elsewhere* nil))

    ;; make sure we have a clean slate
    (clrhash (compiland-common-labels compiland))
    (setf (compiland-constants compiland) nil)

    (when (compiland-needs-thread-var-p compiland)
      (allocate-thread-var compiland))
    (clear-register-contents)
    (cond ((trivial-p compiland)
;;            (assign-registers-for-locals compiland)
;;            #+x86
;;            (let ((delta (length (compiland-registers-to-be-saved compiland))))
;;              (dolist (var (compiland-arg-vars compiland))
;;                (when (eq (var-kind var) :required)
;;                  (setf (var-index var) (+ (var-index var) delta)))))
           (p2-trivial-function-prolog compiland)
           )
          (t
           (p2-function-prolog compiland)))
    (p2-check-argument-types compiland)
    (p2 (compiland-p1-body compiland) :return)
    (aver (vectorp *main*))
    (if *elsewhere*
        (setq *code* (concatenate 'simple-vector *main* *elsewhere*))
        (setq *code* (coerce *main* 'simple-vector)))

    (when *dump-code*
      (mumble "code before generating prolog:~%")
      (dump-code) ; IR2
      )

    (optimize-ir2)
    (analyze-ir2)

;;     (when (trivial-p compiland)
;;       (let ((*code* nil)
;;             (*main* nil)
;;             (*elsewhere* nil))
;;         (p2-trivial-function-prolog compiland)
;;         (if *elsewhere*
;;             (setf (compiland-prolog compiland) (concatenate 'simple-vector *main* *elsewhere*))
;;             (setf (compiland-prolog compiland) (concatenate 'simple-vector *main*))))
;;       (setq *code* (concatenate 'simple-vector (compiland-prolog compiland) *code*)))

;;     (dump-code) ; IR2

    #+nil
    (when (and (trivial-p compiland)
               (not (compiland-child-p compiland))
               (not (compiland-needs-thread-var-p compiland)))
      (repeat-p2 compiland)
      ;;       (when (compiland-omit-frame-pointer compiland)
      ;;         (repeat-p2 compiland))
      )

    #+x86-64
    (when (trivial-p compiland)
      (assign-registers-for-locals compiland))

    (cond (*ir2-only*
           (let ((*dump-code* t))
             (mumble "code (including prolog) after IR2 optimization:~%")
             (dump-code)))
          (t
           (dump-code)
           (p3)
           ;;     (dump-code)
           ;;     (optimize-code)
           (setq *code* (coerce *code* 'list))))))

(defun compile-defun (name lambda-expression)
  (aver (eq (car lambda-expression) 'LAMBDA))
  (let* ((*compiling* t)
         (*output-mode* :compile)
         (compiland
          (make-compiland :name name
                          :lambda-expression (precompile-form lambda-expression)))
         (*current-compiland* compiland)
         (*all-variables* nil)
         (*undefined-variables* nil)
         (*local-variables* nil)
         (*closure-vars* nil)
         (*visible-variables* nil)
         (*code* nil))
    (initialize-available-registers)
    (p1-compiland compiland)
    (p2-compiland compiland)
    (unless *ir2-only*
      (multiple-value-bind (code constants)
          (generate-code-vector *code* (nreverse (compiland-constants *current-compiland*)))
        (let ((compiled-function (make-compiled-function name
                                                         code
                                                         (compiland-minargs compiland)
                                                         (compiland-maxargs compiland)
                                                         constants)))
          (save-local-variable-information compiled-function)
          compiled-function)))))

(defun dump-ir2 (name)
  (let ((*ir2-only* t)
        (*dump-code* t))
    (compile-defun name (function-lambda-expression (fdefinition name)))))

(defun compile-lambda-for-compile-file (lambda-expression)
  (aver (eq (car lambda-expression) 'LAMBDA))
  (aver (not (null *compile-file-pathname*)))
  (aver (eq *output-mode* :compile-file))
  (let* ((*compiling* t)
         (compiland
          (make-compiland :lambda-expression (precompile-form lambda-expression)))
         (*current-compiland* compiland)
         (*all-variables* nil)
         (*undefined-variables* nil)
         (*local-variables* nil)
         (*closure-vars* nil)
         (*visible-variables* nil)
         (*code* nil))
    (initialize-available-registers)
    (p1-compiland compiland)
    (p2-compiland compiland)
    (values *code*
            (compiland-minargs compiland)
            (compiland-maxargs compiland)
            (nreverse (compiland-constants compiland))
            (generate-local-variable-information compiland)
            )))

(defun compile-defun-for-compile-file (name lambda-expression)
  (aver (eq (car lambda-expression) 'LAMBDA))
  (aver (not (null *compile-file-pathname*)))
  (aver (eq *output-mode* :compile-file))
  (let* ((*compiling* t)
         (compiland
          (make-compiland :name name
                          :lambda-expression (precompile-form lambda-expression)))
         (*current-compiland* compiland)
         (*all-variables* nil)
         (*undefined-variables* nil)
         (*local-variables* nil)
         (*closure-vars* nil)
         (*visible-variables* nil)
         (*code* nil))
    (initialize-available-registers)
    (p1-compiland compiland)
    (p2-compiland compiland)
    (values *code*
            (compiland-minargs compiland)
            (compiland-maxargs compiland)
            (nreverse (compiland-constants compiland))
            (generate-local-variable-information compiland)
            )))

(defun %compile (name &optional definition)
  (if name
      (mumble "~&; Compiling ~S~%" name)
;;       (let ((*print-readably* nil))
;;         (mumble "~&; Compiling top-level form ~S~%" definition))
      (mumble "~&; Compiling top-level form~%"))
  (let ((macro-p (macro-function name)))
    (unless definition
      (when (autoloadp name)
        (resolve name))
      (setq definition (or macro-p (fdefinition name))))
    (when (compiled-function-p definition)
      (when name
        (if macro-p
            (setf (macro-function name) definition)
            (setf (fdefinition name) definition)))
      (return-from %compile (values (or name definition) nil nil)))

    ;; FIXME
    (when (typep definition 'generic-function)
      (mumble "not compiling generic function ~S~%" name)
      (return-from %compile (values name nil nil)))

    (let ((lambda-expression nil)
          (environment nil))
      (cond ((functionp definition)
             (multiple-value-setq (lambda-expression environment) (function-lambda-expression definition)))
            ((and (consp definition) (eq (%car definition) 'LAMBDA))
             (unless (and (>= (length definition) 2) (listp (second definition)))
               (error "Lambda expression has a missing or non-list lambda list: ~S"
                      definition))
             (setq lambda-expression definition)))

      (unless lambda-expression
        (error "Can't find a definition for ~S." name))

      (unless (or (null environment) (environment-empty-p environment))
        (mumble "; Unable to compile LAMBDA form defined in non-null lexical environment.")
        ;; REVIEW
        (values (or name definition) nil nil))

      (let* ((*speed* *speed*)
             (*space* *space*)
             (*safety* *safety*)
             (*debug* *debug*)
             (compiled-function
              (if *catch-errors*
                  (handler-case
                      (compile-defun name lambda-expression)
                    (compiler-unsupported-feature-error (condition)
                                                        (mumble "~&; ~A~%" condition)))
                  (compile-defun name lambda-expression))))
        (cond ((null compiled-function)
               (fresh-line)
               (if name
                   (mumble "; Unable to compile ~S~%" name)
                   (let ((*print-readably* nil))
                     (mumble "; Unable to compile ~S~%" definition)))
               (precompile name lambda-expression))
              (name
               ;; preserve existing source information (if any)
               (let ((source (and (symbolp name) (get name '%source))))
                 (if macro-p
                     (setf (macro-function name) compiled-function)
                     (setf (fdefinition name) compiled-function))
                 (when source
                   (put name '%source source)))
               (values name nil nil))
              (t
               ;; "If the name is nil, the resulting compiled function is returned
               ;; directly as the primary value."
               (values compiled-function nil nil)))))))

(defun compile (name &optional (definition nil definition-supplied-p))
  (let ((*compiler-busy-p* t))
    (when definition-supplied-p
      (unless (or (functionp definition)
                  (and (consp definition) (eq (%car definition) 'lambda)))
        (error 'type-error :datum definition :expected-type 'function)))
    (if *enable-compiler*
        (%compile name definition)
        (precompile name definition))))

;; replaces do-nothing stub defined in kernel
(defun autocompile (function)
  (when *enable-autocompile*
    (let* ((*compiler-busy-p* t)
           (verbose *autocompile-verbose*) ; false by default
           (*compile-verbose* verbose)
           (*mumble* verbose))
      (values (compile nil function)))))

(setq *enable-autocompile* t)
