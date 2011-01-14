;;; loop.lisp

;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: loop.lisp,v 1.38 2005/04/16 07:34:27 yuji Exp $
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

;; #-sacla
;; (progn
;;   (defpackage "SACLA-LOOP"
;;     (:documentation "An ANSI Common Lisp Loop facility.")
;;     (:use "COMMON-LISP")
;;     (:shadow "ERROR" "WARN" "LOOP" "LOOP-FINISH")
;;     (:export "LOOP" "LOOP-FINISH"))
;;   (in-package "SACLA-LOOP"))
(unless (find-package "SACLA-LOOP")
  (make-package "SACLA-LOOP" :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM")))
(in-package "SACLA-LOOP")

;; #-sacla
;; (progn
  (defvar *message-prefix* "")
  #-xcl
  (defun error (datum &rest arguments)
    (when (stringp datum)
      (setq datum (concatenate 'string *message-prefix* datum)))
    (apply #'cl:error datum arguments))
  #-xcl
  (defun warn (datum &rest arguments)
    (when (stringp datum)
      (setq datum (concatenate 'string *message-prefix* datum)))
    (apply #'cl:warn datum arguments))

  (define-modify-macro appendf (&rest args) append "Append onto list")
;;   (defun mapappend (function &rest lists)
;;     (apply #'append (apply #'mapcar function lists)))
  #-xcl
  (define-condition simple-program-error (simple-condition program-error) ())
;; )

(defun globally-special-p (symbol)
  (aver (symbolp symbol))
  (if (constantp symbol)
      (values nil t)
      #+abcl      (values (EXT:SPECIAL-VARIABLE-P symbol t))
      #+allegro   (values (excl::variable-special-p symbol nil) t)
      #+clisp     (values (ext:special-variable-p symbol nil) t)
      #+cmu       (values (walker:variable-globally-special-p symbol) t)
      #+ecl       (values (si:specialp symbol) t)
      #+gcl       (values (si:specialp symbol) t)
      #+lispworks (values (eq :special (hcl:variable-information symbol)) t)
      #+sbcl      (values (sb-walker:var-globally-special-p symbol) t)
      #+xcl       (values (ext:special-variable-p symbol) t)
      #-(or abcl allegro clisp cmu ecl gcl lispworks sbcl xcl)
      (progn
        (warn "Implementation-specific globally-special-p should be defined.")
        (values nil nil))))

(defvar *loop-clauses*
  (let ((table (make-hash-table)))
    (mapc #'(lambda (spec)
              (destructuring-bind (clause-name . keywords) spec
                (dolist (key keywords) (setf (gethash key table) clause-name))))
          '((for-as-clause :for :as)
            (with-clause :with)
            (do-clause :do :doing)
            (return-clause :return)
            (initially-clause :initially)
            (finally-clause :finally)
            (accumulation-clause :collect :collecting :append :appending
             :nconc :nconcing :count :counting :sum :summing :maximize :maximizing
             :minimize :minimizing)
            (conditional-clause :if :when :unless)
            (repeat-clause :repeat)
            (always-never-thereis-clause :always :never :thereis)
            (while-clause :while)
            (until-clause :until)))
    table)
  "A table mapping loop keywords to their processor function-designator.")

(defvar *for-as-subclauses*
  (let ((table (make-hash-table)))
    (mapc #'(lambda (spec)
              (destructuring-bind (subclause-name . keywords) spec
                (dolist (key keywords)
                  (setf (gethash key table) subclause-name))))
          '((for-as-arithmetic-subclause
             :from :downfrom :upfrom :to :downto :upto :below :above :by)
            (for-as-in-list-subclause :in)
            (for-as-on-list-subclause :on)
            (for-as-equals-then-subclause :=)
            (for-as-across-subclause :across)
            (for-as-being-subclause :being)))
    table)
  "A table mapping for-as prepositions to their processor function-designator.")

(defvar *for-as-prepositions*
  (let ((prepositions nil))
    (maphash #'(lambda (key value) (declare (ignore value)) (push key prepositions))
             *for-as-subclauses*)
    prepositions))

(defvar *environment*)
(defvar *loop-tokens*)
(defvar *current-keyword* nil)
(defvar *current-clause* nil)

(defun append-context (message)
  (concatenate 'string message
               (let ((clause (ldiff *current-clause* *loop-tokens*)))
                 (format nil "~%Current LOOP context:~{ ~S~}" clause))))


(defun loop-error (datum &rest arguments)
  (when (stringp datum) (setq datum (append-context datum)))
  (apply #'error datum arguments))

(defun loop-warn (datum &rest arguments)
  (when (stringp datum) (setq datum (append-context datum)))
  (apply #'warn datum arguments))


(defun keyword? (&optional keyword-list-designator)
  (and *loop-tokens*
       (symbolp (car *loop-tokens*))
       (let ((keyword-list (designator-list keyword-list-designator))
             (keyword (make-keyword (car *loop-tokens*))))
         (and (or (null keyword-list) (list-find-eq keyword keyword-list))
              (setq *current-clause* *loop-tokens*
                    *loop-tokens* (rest *loop-tokens*)
                    *current-keyword* keyword)))))

(defun keyword1 (keyword-list-designator &key prepositionp)
  (let ((keywords (designator-list keyword-list-designator)))
    (or (keyword? keywords)
        (let ((length (length keywords))
              (kind (if prepositionp "preposition" "keyword")))
          (case length
            (0 (loop-error "A loop ~A is missing." kind))
            (1 (loop-error "Loop ~A ~S is missing." kind (car keywords)))
            (t (loop-error "One of the loop ~As ~S must be supplied."
                           kind keywords)))))))

(defun preposition? (&optional keyword-list-designator)
  (let ((*current-keyword* *current-keyword*)
        (*current-clause* *current-clause*))
    (keyword? keyword-list-designator)))

(defun preposition1 (&optional keyword-list-designator)
  (let ((*current-keyword* *current-keyword*)
        (*current-clause* *current-clause*))
    (keyword1 keyword-list-designator :prepositionp t)))



(defvar *loop-name* nil)
(defvar *it-symbol* nil)
(defvar *it-visible-p* nil)
(defvar *anonymous-accumulator* nil)
(defvar *boolean-terminator* nil)
(defvar *accumulators* nil)
(defvar *loop-components* nil)



(defun clause1 ()
  (multiple-value-bind (clause-function-designator present-p)
      (gethash *current-keyword* *loop-clauses*)
    (unless present-p
      (loop-error "Unknown loop keyword ~S encountered." (car *current-clause*)))
    (let ((*message-prefix* (format nil "LOOP ~A clause: " *current-keyword*)))
      (funcall clause-function-designator))))

(defun clause* ()
  (loop
   (let ((key (keyword?)))
     (unless key (return))
     (clause1))))

(defun lp (&rest tokens)
  (let ((*loop-tokens* tokens)
        *current-keyword*
        *current-clause*)
    (clause*)
    (when *loop-tokens* (error "~S remained after lp." *loop-tokens*))))

(defun form1 ()
  (unless *loop-tokens* (loop-error "A normal lisp form is missing."))
  (pop *loop-tokens*))

(defun compound-forms* ()
  (when (and *loop-tokens* (consp (car *loop-tokens*)))
    (cons (pop *loop-tokens*) (compound-forms*))))

(defun compound-forms+ ()
  (or (compound-forms*) (loop-error "At least one compound form is needed.")))

(defun simple-var-p (var) (and (not (null var)) (symbolp var)))

(defun simple-var1 ()
  (unless (and *loop-tokens* (simple-var-p (car *loop-tokens*)))
    (loop-error "A simple variable name is missing."))
  (pop *loop-tokens*))

(defun empty-p (d-var-spec)
  (or (null d-var-spec)
      (and (consp d-var-spec)
           (empty-p (car d-var-spec))
           (empty-p (cdr d-var-spec)))))

(defun d-var-spec-p (spec)
  (or (simple-var-p spec)
      (null spec)
      (and (consp spec) (d-var-spec-p (car spec)) (d-var-spec-p (cdr spec)))))

(defun d-var-spec1 ()
  (unless (and *loop-tokens* (d-var-spec-p (car *loop-tokens*)))
    (loop-error "A destructured-variable-spec is missing."))
  (let ((d-var-spec (pop *loop-tokens*)))
    d-var-spec))

(defun stray-of-type-error ()
  (loop-error "OF-TYPE keyword should be followed by a type spec."))

(defun type-spec? ()
  (let ((type t)
        (supplied-p nil))
    (when (or (and (preposition? :of-type) (or *loop-tokens* (stray-of-type-error)))
              (and *loop-tokens* (memq (car *loop-tokens*) '(fixnum float t nil))))
      (setq type (pop *loop-tokens*) supplied-p t))
    (values type supplied-p)))


(defun car-type (d-type-spec)
  (if (consp d-type-spec) (car d-type-spec) d-type-spec))
(defun cdr-type (d-type-spec)
  (if (consp d-type-spec) (cdr d-type-spec) d-type-spec))
(defun default-value (type)
  (cond
    ((subtypep type 'bignum) (1+ most-positive-fixnum))
    ((subtypep type 'integer) 0)
    ((subtypep type 'ratio) 1/10)
    ((subtypep type 'float) 0.0)
    ((subtypep type 'number) 0)
    ((subtypep type 'character) #\Space)
    ((subtypep type 'string) "")
    ((subtypep type 'bit-vector) #*0)
    ((subtypep type 'vector) #())
    ((subtypep type 'package) *package*)
    (t nil)))
(defun default-type (type)
  (if (eq type t)
      t
      (let ((value (default-value type)))
        (if (typep value type)
            type
            (let ((default-type (type-of value)))
              (if (subtypep type default-type)
                  default-type
                  (if (null value)
                      `(or null ,type)
                      `(or ,default-type ,type))))))))

(defun default-binding (type var)
  `(,(default-type type) ,var ,(default-value type)))

(defvar *temporaries* nil
  "Temporary variables used in with-clauses and for-as-clauses.")

(defvar *ignorable* nil
  "Ignorable temporary variables in *temporaries*.")

(defun constant-bindings (d-type-spec d-var-spec value)
  (let ((bindings nil))
    (labels ((dig (type var value)
               (cond
                 ((null var) nil) ;; do nothing
                 ((simple-var-p var) (appendf bindings `((,type ,var ',value))))
                 (t (dig (car-type type) (car var) (car value))
                    (dig (cdr-type type) (cdr var) (cdr value))))))
      (dig d-type-spec d-var-spec value)
      bindings)))

(defun default-bindings (d-type-spec d-var-spec)
  (let ((bindings nil))
    (labels ((dig (type var)
               (cond
                 ((null var) nil) ;; do nothing
                 ((simple-var-p var)
                  (appendf bindings `(,(default-binding type var))))
                 (t (dig (car-type type) (car var))
                    (dig (cdr-type type) (cdr var))))))
      (dig d-type-spec d-var-spec)
      bindings)))

(defun ordinary-bindings (d-type-spec d-var-spec value-form)
  (let ((temporaries *temporaries*)
        (bindings nil))
    (labels
        ((dig (type var form temp)
           ;; a TEMP moves horizontally, or cdr-wise, on an FORM.
           ;; a TEMP can be reused by pushing it back onto TEMPORARIES.
           (cond
             ((empty-p var) nil)
             ((simple-var-p var)
              (when temp (push temp temporaries))
              (appendf bindings `((,type ,var ,form))))
             ((empty-p (car var))
              (dig (cdr-type type) (cdr var) `(cdr ,form) temp))
             ((empty-p (cdr var))
              (when temp (push temp temporaries))
              (dig (car-type type) (car var) `(car ,form) nil))
             (t (unless temp (setq temp (or (pop temporaries) (gensym))))
                (dig (car-type type) (car var) `(car (setq ,temp ,form)) nil)
                (dig (cdr-type type) (cdr var) `(cdr ,temp) temp)))))
      (dig d-type-spec d-var-spec value-form nil)
      (setq *temporaries* temporaries)
      bindings)))

;; (defun quoted-form-p (form)
;;   (let ((expansion (macroexpand form *environment*)))
;;     (and (consp expansion) (eq (%car expansion) 'quote))))

(defun quoted-object (form)
  (let ((expansion (macroexpand form *environment*)))
    (destructuring-bind (quote-special-operator object) expansion
      (aver (eq quote-special-operator 'quote))
      object)))

(defun bindings (d-type-spec d-var-spec
                 &optional (value-form "NEVER USED" value-form-p))
  (cond
    ((null value-form-p) (default-bindings d-type-spec d-var-spec))
    ((quoted-form-p value-form) (constant-bindings d-type-spec d-var-spec
                                                   (quoted-object value-form)))
    (t (ordinary-bindings d-type-spec d-var-spec value-form))))

(defun fill-in (&rest args)
  (when args
    (appendf (getf *loop-components* (first args)) (second args))
    (apply #'fill-in (cddr args))))

(defun declarations (bindings)
  (let ((declarations (mapcan #'(lambda (binding)
                                  (destructuring-bind (type var . rest) binding
                                    (declare (ignore rest))
                                    (unless (eq type 't) `((type ,type ,var)))))
                              bindings)))
    (when declarations `((declare ,@declarations)))))

(defun let-form (bindings) `(let ,(mapcar #'cdr bindings) ,@(declarations bindings)))

(defun with (var &optional (type t) &key (= (default-value type)))
  (fill-in :binding-forms `(,(let-form `((,type ,var ,=))))))

(defun multiple-value-list-form-p (form)
  (let (expanded-p)
    (loop
     (when (and (consp form) (eq (first form) 'multiple-value-list))
       (return t))
     (multiple-value-setq (form expanded-p) (macroexpand-1 form *environment*))
     (unless expanded-p (return nil)))))

(defun multiple-value-list-argument-form (form)
  (let ((expansion form)
        (expanded-p nil))
    (loop
     (when (and (consp expansion) (eq (first expansion) 'multiple-value-list))
       (return (second expansion)))
     (multiple-value-setq (expansion expanded-p)
       (macroexpand-1 expansion *environment*))
     (unless expanded-p
       (error "~S is not expanded into a multiple-value-list form." form)))))

(defun destructuring-multiple-value-bind (d-type-spec d-var-spec value-form)
  (let ((mv-bindings nil)
        (d-bindings nil)
        (padding-temps nil)
        temp)
    (do ((vars d-var-spec (cdr vars))
         (types d-type-spec (cdr-type types)))
        ((endp vars))
      (if (listp (car vars))
          (progn (setq temp (gensym))
                 (appendf mv-bindings `((t ,temp)))
                 (appendf d-bindings `((,(car-type types) ,(car vars) ,temp)))
                 (when (empty-p (car vars)) (push temp padding-temps)))
          (appendf mv-bindings `((,(car-type types) ,(car vars))))))
    (fill-in :binding-forms `((multiple-value-bind ,(mapcar #'second mv-bindings)
                                  ,(multiple-value-list-argument-form value-form)
                                ,@(declarations mv-bindings)
                                ,@(when padding-temps
                                        `((declare (ignore ,@padding-temps)))))))
    (let ((bindings (mappend #'(lambda (d-binding) (apply #'bindings d-binding))
                             d-bindings)))
      (when bindings (fill-in :binding-forms `(,(let-form bindings)))))))

(defun d-var-type-spec ()
  (let ((var (d-var-spec1))
        (type (type-spec?)))
    (when (empty-p var)
      (unless (memq type '(nil t)) (loop-warn "Type spec ~S is ignored." type))
      (setq var (gensym)
            type t))
    (values var type)))

(defun with-clause ()
  (let ((d-bindings nil))
    (loop (multiple-value-bind (var type) (d-var-type-spec)
            (let ((rest (when (preposition? :=) `(,(form1)))))
              (appendf d-bindings `((,type ,var ,@rest)))))
          (unless (preposition? :and) (return)))
    (destructuring-bind (d-binding0 . rest) d-bindings
      (if (and (null rest)
               (cddr d-binding0)
               (destructuring-bind (type var form) d-binding0
                 (declare (ignore type))
                 (and (consp var) (multiple-value-list-form-p form))))
          (apply #'destructuring-multiple-value-bind d-binding0)
          (let ((bindings (mappend #'(lambda (d-binding) (apply #'bindings d-binding))
                                   d-bindings)))
            (fill-in :binding-forms `(,(let-form bindings))))))))


(defun dispatch-for-as-subclause (var type)
  (unless *loop-tokens* (loop-error "A preposition is missing."))
  (let ((preposition (preposition1 *for-as-prepositions*)))
    (multiple-value-bind (subclause-function-designator present-p)
        (gethash preposition *for-as-subclauses*)
      (unless present-p
        (loop-error "Unknown preposition ~S is supplied." preposition))
      (push preposition *loop-tokens*)
      (funcall subclause-function-designator var type))))

(defun for (var type &rest rest)
  (let ((*loop-tokens* rest))
    (dispatch-for-as-subclause var type)))

(defvar *for-as-components*)
(defun for-as-fill-in (&rest key-list-pairs)
  (when key-list-pairs
    (destructuring-bind (key list . rest) key-list-pairs
      (appendf (getf *for-as-components* key) list)
      (apply #'for-as-fill-in rest))))

(defvar *hash-group* '(:hash-key :hash-keys :hash-value :hash-values))
(defvar *symbol-group* '(:symbol :symbols :present-symbol :present-symbols
                         :external-symbol :external-symbols))

(defun loop-finish-test-forms (tests)
  (case (length tests)
    (0 nil)
    (1 `((when ,@tests (loop-finish))))
    (t `((when (or ,@tests) (loop-finish))))))

(defun psetq-forms (args)
  (aver (evenp (length args)))
  (case (length args)
    (0 nil)
    (2 `((setq ,@args)))
    (t `((psetq ,@args)))))

(defun for-as-clause ()
  (let ((*for-as-components* nil))
    (loop (multiple-value-bind (var type) (d-var-type-spec)
            (dispatch-for-as-subclause var type))
          (unless (preposition? :and) (return)))
    (destructuring-bind (&key bindings bindings2
                              before-head head-psetq head-tests after-head
                              before-tail tail-psetq tail-tests after-tail)
        *for-as-components*
      (fill-in :binding-forms `(,@(when bindings  `(,(let-form bindings)))
                                ,@(when bindings2 `(,(let-form bindings2))))
               :head `(,@before-head
                       ,@(psetq-forms head-psetq)
                       ,@(loop-finish-test-forms head-tests)
                       ,@after-head)
               :tail `(,@before-tail
                       ,@(psetq-forms tail-psetq)
                       ,@(loop-finish-test-forms tail-tests)
                       ,@after-tail)))))

(defun for-as-parallel-p ()
  (or *for-as-components*
      (and *loop-tokens*
           (symbolp (car *loop-tokens*))
           (string= (symbol-name (car *loop-tokens*)) "AND"))))

(defun gensym-ignorable ()
  (let ((var (gensym)))
    (push var *ignorable*)
    var))

(defun destructuring-multiple-value-setq (d-var-spec value-form &key iterator-p)
  (let (d-bindings mv-vars temp)
    (do ((vars d-var-spec (cdr vars)))
        ((endp vars))
      (if (listp (car vars))
          (progn (setq temp (or (pop *temporaries*) (gensym-ignorable)))
                 (appendf mv-vars `(,temp))
                 (appendf d-bindings `((t ,(car vars) ,temp))))
          (appendf mv-vars `(,(car vars)))))
    (let ((mv-setq-form `(multiple-value-setq ,mv-vars ,value-form))
          (bindings nil))
      (do ((d-bindings d-bindings (cdr d-bindings)))
          ((endp d-bindings))
        (destructuring-bind (type var temp) (car d-bindings)
          (declare (ignore type var))
          (push temp *temporaries*)
          (appendf bindings (apply #'bindings (car d-bindings)))))
      (when iterator-p (setq mv-setq-form `(unless ,mv-setq-form (loop-finish))))
      (if bindings
          `(progn ,mv-setq-form (setq ,@(mappend #'cdr bindings)))
          mv-setq-form))))

(defun along-with (var type &key equals (then equals))
  (for-as-fill-in :bindings (apply #'bindings type var (when (quoted-form-p equals)
                                                         `(,equals))))
  (unless (quoted-form-p equals)
    (for-as-fill-in :after-head
                    `((setq ,@(mappend #'cdr (bindings type var equals))))))
  (for-as-fill-in :after-tail
                  `((setq ,@(mappend #'cdr (bindings type var then))))))

(defun for-as-equals-then-subclause (var type)
  ;; 6.1.1.4 Expanding Loop Forms
  ;; http://www.lispworks.com/reference/HyperSpec/Body/06_aad.htm
  ;; the form1 and form2 in a for-as-equals-then form includes the lexical
  ;; environment of all the loop variables.
  (preposition1 :=)
  (let* ((first (form1))
         (then  (if (preposition? :then) (form1) first))
         (parallel-p (for-as-parallel-p)))
    (for-as-fill-in :bindings (apply #'bindings type var (when (quoted-form-p first)
                                                           `(,first))))
    (if (and (not parallel-p) (consp var) (multiple-value-list-form-p first))
        (for-as-fill-in :before-head
                        `(,(destructuring-multiple-value-setq var
                             (multiple-value-list-argument-form first))))
        (unless (quoted-form-p first)
          (for-as-fill-in :head-psetq (mappend #'cdr (bindings type var first)))))
    (if (and (not parallel-p) (consp var) (multiple-value-list-form-p then))
        (for-as-fill-in :before-tail
                        `(,(destructuring-multiple-value-setq var
                             (multiple-value-list-argument-form then))))
        (for-as-fill-in :tail-psetq (mappend #'cdr (bindings type var then))))))


(defun for-as-arithmetic-step-and-test-functions (used-prepositions)
  (let ((up-p (subsetp used-prepositions '(:below :upto :upfrom :from :to :by))))
    (values (if up-p '+ '-)
            (cond ((memq :to     used-prepositions) (if up-p '> '<))
                  ((memq :upto   used-prepositions) '>)
                  ((memq :below  used-prepositions) '>=)
                  ((memq :downto used-prepositions) '<)
                  ((memq :above  used-prepositions) '<=)
                  (t nil)))))

(defun zero (type)
  (cond
    ((subtypep type 'short-float)  0.0s0)
    ((subtypep type 'single-float) 0.0f0)
    ((subtypep type 'double-float) 0.0d0)
    ((subtypep type 'long-float)   0.0l0)
    ((subtypep type 'float)        0.0)
    (t 0)))

(defun one (type)
  (cond
    ((subtypep type 'short-float)  1.0s0)
    ((subtypep type 'single-float) 1.0f0)
    ((subtypep type 'double-float) 1.0d0)
    ((subtypep type 'long-float)   1.0l0)
    ((subtypep type 'float)        1.0)
    (t 1)))

(defun for-as-arithmetic-possible-prepositions (used-prepositions)
  (append
   (cond
     ((intersection '(:from :downfrom :upfrom) used-prepositions) nil)
     ((intersection '(:downto :above) used-prepositions) '(:from :downfrom))
     ((intersection '(:upto :below) used-prepositions) '(:from :upfrom))
     (t '(:from :downfrom :upfrom)))
   (cond
     ((intersection '(:to :downto :upto :below :above) used-prepositions) nil)
     ((list-find-eq :upfrom used-prepositions) '(:to :upto :below))
     ((list-find-eq :downfrom used-prepositions) '(:to :downto :above))
     (t '(:to :downto :upto :below :above)))
   (unless (list-find-eq :by used-prepositions) '(:by))))

(defun for-as-arithmetic-subclause (var type)
  (unless (simple-var-p var) (loop-error "Destructuring on a number is invalid."))
  (multiple-value-bind (subtype-p valid-p)
      (subtypep type 'real)
    (when (and (not subtype-p) valid-p)
      (setq type 'number)))
  (let (from to by preposition used candidates bindings)
    (loop (setq candidates (or (for-as-arithmetic-possible-prepositions used)
                               (return)))
          (push (or (setq preposition (preposition? candidates)) (return))
                used)
          (let ((value-form (form1)))
            (if (memq preposition '(:from :downfrom :upfrom))
                (progn (setq from value-form)
                       (appendf bindings `((,type ,var ,from))))
                (progn (when (not (constantp value-form *environment*))
                         (let ((temp (gensym)))
                           (appendf bindings `((number ,temp ,value-form)))
                           (setq value-form temp)))
                       (ecase preposition
                         ((:to :downto :upto :below :above) (setq to value-form))
                         (:by (setq by value-form)))))))
    (unless (intersection used '(:from :downfrom :upfrom))
      (appendf bindings `((,type ,var ,(zero type)))))
    (multiple-value-bind (step test) (for-as-arithmetic-step-and-test-functions used)
      (let ((tests (when test `((,test ,var ,to)))))
        (for-as-fill-in :bindings bindings
                        :head-tests tests
                        :tail-psetq `(,var (,step ,var ,(or by (one type))))
                        :tail-tests tests)))))


(defun cl-external-p (symbol)
  (multiple-value-bind (cl-symbol status)
      (find-symbol (symbol-name symbol) "CL")
    (and (eq symbol cl-symbol) (eq status :external))))

(defun constant-function-p (form)
  (let ((expansion (macroexpand form *environment*)))
    (and (consp expansion)
         (eq (first expansion) 'function)
         (symbolp (second expansion))
         (let ((symbol (second expansion)))
           (and (cl-external-p symbol) (fboundp symbol))))))

(defvar *list-end-test* 'atom)
(defun by-step-fun () (if (preposition? :by) (form1) '#'cdr))

(defun for-as-on-list-subclause (var type)
  (preposition1 :on)
  ;; Check with atom. See 6.1.2.1.3 The for-as-on-list subclause.
  ;; http://www.lispworks.com/reference/HyperSpec/Body/06_abac.htm
  (let* ((form (form1))
         (by-step-fun (by-step-fun))
         (test *list-end-test*)
         (list-var  (if (simple-var-p var) var (gensym "LIST-")))
         (list-type (if (simple-var-p var) type t))
         (at-least-one-iteration-p (and (quoted-form-p form)
                                        (not (funcall test (quoted-object form))))))
    (for-as-fill-in :bindings `((,list-type ,list-var ,form)
                                ,@(unless (constant-function-p by-step-fun)
                                    (let ((temp (gensym "STEPPER-")))
                                      (prog1 `((t ,temp ,by-step-fun))
                                        (setq by-step-fun temp)))))
                    :head-tests (unless at-least-one-iteration-p
                                  `((,test ,list-var)))
                    :tail-psetq `(,list-var (funcall ,by-step-fun ,list-var))
                    :tail-tests `((,test ,list-var)))
    (unless (simple-var-p var)
      (along-with var type :equals (if at-least-one-iteration-p form list-var)
                           :then list-var))))

(defun for-as-in-list-subclause (var type)
  (preposition1 :in)
  ;; Check with endp. See 6.1.2.1.2 The for-as-in-list subclause.
  ;; http://www.lispworks.com/reference/HyperSpec/Body/06_abab.htm
  (let ((*list-end-test* 'endp))
    (for `(,var) `(,type) :on (form1) :by (by-step-fun))))

(defun constant-vector-p (form) (or (quoted-form-p form) (vectorp form)))
(defun constant-vector (form)
  (cond
    ((quoted-form-p form) (quoted-object form))
    ((vectorp form) form)
    (t (error "~S is not a vector form." form))))

(defun for-as-across-subclause (var type)
  (preposition1 :across)
  (let* ((form (form1))
         (vector (if (constant-vector-p form) form (gensym "VECTOR-")))
         (length (if (constant-vector-p form)
                     (length (constant-vector form))
                     (gensym "LENGTH-")))
         (i (gensym "INDEX-"))
         (at-least-one-iteration-p (and (constant-vector-p form) (plusp length))))
    (unless (constant-vector-p form)
      (for-as-fill-in :bindings  `((t ,vector ,form))
                      :bindings2 `((fixnum ,length (length ,vector)))))
    (for-as-fill-in :bindings `((fixnum ,i 0))
                    :head-tests (unless at-least-one-iteration-p `((= ,i ,length)))
                    :tail-psetq `(,i (1+ ,i))
                    :tail-tests `((= ,i ,length)))
    (along-with var type :equals (if at-least-one-iteration-p
                                     `',(aref (constant-vector form) 0)
                                     `(aref ,vector ,i))
                         :then `(aref ,vector ,i))))

(defun using-other-var (kind)
  (let ((using-phrase (when (preposition? :using) (pop *loop-tokens*)))
        (other-key-name (if (list-find-eq kind '(:hash-key :hash-keys))
                            "HASH-VALUE"
                            "HASH-KEY")))
    (when using-phrase
      (destructuring-bind (other-key other-var) using-phrase
        (unless (string= other-key other-key-name)
          (loop-error "Keyword ~A is missing." other-key-name))
        other-var))))

(defun hash-d-var-spec (returned-p var other-var kind)
  (if (list-find-eq kind '(:hash-key :hash-keys))
      `(,returned-p ,var ,other-var)
      `(,returned-p ,other-var ,var)))

(defun for-as-hash-subclause (var type kind)
  (let* ((hash-table (progn (preposition1 '(:in :of)) (form1)))
         (other-var (using-other-var kind))
         (for-as-parallel-p (for-as-parallel-p))
         (returned-p (or (pop *temporaries*) (gensym-ignorable)))
         (iterator (gensym))
         narrow-typed-var narrow-type)
    (when (and (simple-var-p var) (not (typep 'nil type)))
      (setq narrow-typed-var var
            narrow-type type)
      (setq var (gensym)
            type `(or null ,type))
      (for-as-fill-in :bindings `(,(default-binding narrow-type narrow-typed-var))))
    (flet ((iterator-form () `(with-hash-table-iterator (,iterator ,hash-table))))
      (if for-as-parallel-p
          (progn (unless (constantp hash-table *environment*)
                   (let ((temp (gensym "HASH-TABLE-")))
                     (for-as-fill-in :bindings `((t ,temp ,hash-table)))
                     (setq hash-table temp)))
                 (fill-in :iterator-forms `(,(iterator-form))))
          (fill-in :binding-forms `(,(iterator-form)))))
    (let* ((d-var-spec (hash-d-var-spec returned-p var other-var kind))
           (d-mv-setq (destructuring-multiple-value-setq d-var-spec `(,iterator)
                        :iterator-p t))
           (setters `(,d-mv-setq
                      ,@(when narrow-typed-var `((setq ,narrow-typed-var ,var))))))
      (push returned-p *temporaries*)
      (for-as-fill-in :bindings `(,@(bindings type var)
                                  ,@(when other-var (bindings t other-var)))
                      :after-head setters
                      :after-tail setters))))



(defun for-as-package-subclause (var type kind)
  (let* ((package (if (preposition? '(:in :of)) (form1) '*package*))
         (for-as-parallel-p (for-as-parallel-p))
         (returned-p (or (pop *temporaries*) (gensym-ignorable)))
         (iterator (gensym))
         (kinds (ecase kind
                  ((:symbol :symbols) '(:internal :external :inherited))
                  ((:present-symbol :present-symbols) '(:internal :external))
                  ((:external-symbol :external-symbols) '(:external)))))
    (unless (typep 'nil type) (setq type `(or null ,type)))
    (flet ((iterator-form () `(with-package-iterator (,iterator ,package ,@kinds))))
      (if for-as-parallel-p
          (progn (unless (constantp package *environment*)
                   (let ((temp (gensym "PACKAGE-")))
                     (for-as-fill-in :bindings `((t ,temp ,package)))
                     (setq package temp)))
                 (fill-in :iterator-forms `(,(iterator-form))))
          (fill-in :binding-forms `(,(iterator-form)))))
    (let* ((d-var-spec `(,returned-p ,var))
           (d-mv-setq (destructuring-multiple-value-setq d-var-spec `(,iterator)
                         :iterator-p t)))
      (push returned-p *temporaries*)
      (for-as-fill-in :bindings (bindings type var)
                      :after-head `(,d-mv-setq)
                      :after-tail `(,d-mv-setq)))))

(defun for-as-being-subclause (var type)
  (preposition1 :being)
  (preposition1 '(:each :the))
  (let* ((kind (preposition1 (append *hash-group* *symbol-group*))))
    (cond
      ((list-find-eq kind *hash-group*) (for-as-hash-subclause var type kind))
      ((list-find-eq kind *symbol-group*) (for-as-package-subclause var type kind))
      (t (loop-error "Internal logic error")))))

(defun form-or-it ()
  (if (and *it-visible-p* (preposition? :it))
      (or *it-symbol* (setq *it-symbol* (gensym)))
      (form1)))

(defun enumerate (items)
  (case (length items)
    (1 (format nil "~S" (first items)))
    (2 (format nil "~S and ~S" (first items) (second items)))
    (t (format nil "~{~S, ~}and ~S" (butlast items) (first (last items))))))

(defun invalid-accumulator-combination-error (keys)
  (loop-error "Accumulator ~S cannot be mixed with ~S."
              *current-keyword* (enumerate keys)))

(defun accumulator-kind (key)
  (ecase key
    ((:collect :collecting :append :appending :nconc :nconcing) :list)
    ((:sum :summing :count :counting) :total)
    ((:maximize :maximizing :minimize :minimizing) :limit)))

(defun accumulator-spec (name)
  (let* ((kind (accumulator-kind *current-keyword*))
         (spec (assoc name *accumulators*))
         (plist (cdr spec)))
    (if spec
        (if (not (eq kind (getf plist :kind)))
            (invalid-accumulator-combination-error (reverse (getf plist :keys)))
            (progn
              (pushnew *current-keyword* (getf plist :keys))
              (when (memq kind '(:total :limit))
                (multiple-value-bind (type supplied-p) (type-spec?)
                  (when supplied-p (push type (getf plist :types)))))))
        (let ((var (or name (gensym "ACCUMULATOR-"))))
          (setq plist `(:var ,var :kind ,kind :keys (,*current-keyword*)))
          (ecase kind
            (:list (setf (getf plist :splice) (gensym "SPLICE-"))
                   (unless name (fill-in :results `((cdr ,var)))))
            ((:total :limit)
             (multiple-value-bind (type supplied-p) (type-spec?)
               (when supplied-p
                 (when (eq type 'COMPLEX)
                   ;; avoid type error when accumulator is initialized to zero
                   (setq type '(OR BIT COMPLEX)))
                 (push type (getf plist :types))))
             (when (eq kind :limit)
               (let ((first-p (gensym "FIRST-P-")))
                 (setf (getf plist :first-p) first-p)
                 (with first-p t := t)))
             (unless name (fill-in :results `(,var)))))
          (push (setq spec `(,name ,@plist)) *accumulators*)))
    spec))

(defun ambiguous-loop-result-error ()
  (error 'program-error
         :format-control
         (append-context "~S cannot be used without `into' preposition with ~S")
         :format-arguments `(,*anonymous-accumulator* ,*boolean-terminator*)))

(defun accumulate-in-list (form accumulator-spec)
  (destructuring-bind (name &key var splice &allow-other-keys) accumulator-spec
    (declare (ignore name))
    (let* ((copy-f (ecase *current-keyword*
                    ((:collect :collecting) 'list)
                    ((:append :appending) 'copy-list)
                    ((:nconc :nconcing) 'identity)))
           (collecting-p (memq *current-keyword* '(:collect :collecting)))
           (last-f (if collecting-p 'cdr 'last))
           (splicing-form (if collecting-p
                              `(rplacd ,splice (setq ,splice (list ,form)))
                              `(setf (cdr ,splice) (,copy-f ,form)
                                     ,splice       (,last-f ,splice)))))
      (if (globally-special-p var)
          (lp :do `(if ,splice
                    ,splicing-form
                    (setq ,splice (,last-f (setq ,var (,copy-f ,form))))))
          (lp :do splicing-form)))))

(defun accumulation-clause ()
  (let* ((form (form-or-it))
         (name (if (preposition? :into)
                   (simple-var1)
                   (progn
                     (setq *anonymous-accumulator* *current-keyword*)
                     (when *boolean-terminator* (ambiguous-loop-result-error))
                     nil)))
         (accumulator-spec (accumulator-spec name)))
    (destructuring-bind (name &rest plist &key var &allow-other-keys)
        accumulator-spec
      (declare (ignore name))
      (ecase *current-keyword*
        ((:collect :collecting :append :appending :nconc :nconcing)
         (accumulate-in-list form accumulator-spec))
        ((:count :counting) (lp :if form :do `(incf ,var)))
        ((:sum :summing) (lp :do `(incf ,var ,form)))
        ((:maximize :maximizing :minimize :minimizing)
         (let ((first-p (getf plist :first-p))
               (fun (if (memq *current-keyword* '(:maximize :maximizing)) '< '>)))
           (lp :do `(let ((value ,form))
                     (cond
                       (,first-p (setq ,first-p nil ,var value))
                       ((,fun ,var value) (setq ,var value)))))))))))

(defun return-clause () (lp :do `(return-from ,*loop-name* ,(form-or-it))))


(defun do-clause () (fill-in :body (compound-forms+)))

(defun selectable-clause ()
  (let ((*current-keyword* *current-keyword*)
        (*current-clause* *current-clause*))
    (unless (keyword? '(:if :when :unless :do :doing :return :collect :collecting
                        :append :appending :nconc :nconcing :count :counting
                        :sum :summing :maximize :maximizing :minimize :minimizing))
      (loop-error "A selectable-clause is missing."))
    (ecase *current-keyword*
      ((:if :when :unless) (conditional-clause))
      ((:do :doing) (do-clause))
      ((:return) (return-clause))
      ((:collect :collecting :append :appending :nconc :nconcing :count :counting
                 :sum :summing :maximize :maximizing :minimize :minimizing)
       (accumulation-clause)))))

(defun conditional-clause ()
  (let* ((*it-symbol* nil)
         (middle (gensym "MIDDLE-"))
         (bottom (gensym "BOTTOM-"))
         (test-form (if (eq *current-keyword* :unless) `(not ,(form1)) (form1)))
         (condition-form `(unless ,test-form (go ,middle))))
    ;; condition-form is destructively modified in the following code for IT.
    (lp :do condition-form)
    (let ((*it-visible-p* t)) (selectable-clause))
    (loop (unless (preposition? :and) (return)) (selectable-clause))
    (cond
      ((preposition? :else)
       (lp :do `(go ,bottom))
       (fill-in :body `(,middle))
       (let ((*it-visible-p* t)) (selectable-clause))
       (loop (unless (preposition? :and) (return)) (selectable-clause))
       (fill-in :body `(,bottom)))
      (t (fill-in :body `(,middle))))
    (preposition? :end)
    (when *it-symbol*
      (with *it-symbol*)
      (setf (second condition-form)
            `(setq ,*it-symbol* ,(second condition-form))))))

(defun initially-clause () (fill-in :initially (compound-forms+)))
(defun finally-clause () (fill-in :finally (compound-forms+)))
(defun while-clause () (lp :unless (form1) :do '(loop-finish) :end))
(defun until-clause () (lp :while `(not ,(form1))))
(defun repeat-clause ()
  (let* ((form (form1))
         (type (typecase (if (quoted-form-p form) (quoted-object form) form)
                 (fixnum  'fixnum)
                 (t       'real))))
    (lp :for (gensym) :of-type type :downfrom form :to 1)))
(defun always-never-thereis-clause ()
  (setq *boolean-terminator* *current-keyword*)
  (when *anonymous-accumulator* (ambiguous-loop-result-error))
  (ecase *current-keyword*
    (:always (lp :unless (form1) :return nil :end) (fill-in :results '(t)))
    (:never (lp :always `(not ,(form1))))
    (:thereis (lp :if (form1) :return :it :end) (fill-in :results '(nil)))))

(defun variable-clause* ()
  (loop (let ((key (keyword? '(:with :initially :finally :for :as :repeat))))
          (if key (clause1) (return)))))

(defun main-clause* ()
  (loop
   (if (keyword? '(:do :doing :return :if :when :unless :initially :finally
                   :for :as :while :until :repeat :always :never :thereis
                   :collect :collecting :append :appending :nconc :nconcing
                   :count :counting :sum :summing :maximize :maximizing
                   :minimize :minimizing))
       (clause1)
       (return))))

(defun name-clause? ()
  (when (keyword? :named)
    (unless *loop-tokens* (loop-error "A loop name is missing."))
    (let ((name (pop *loop-tokens*)))
      (unless (symbolp name)
        (loop-error "~S cannot be a loop name which must be a symbol." name))
      (setq *loop-name* name))))

(defun bound-variables (binding-form)
  (let ((operator (first binding-form))
        (second (second binding-form)))
    (ecase operator
      ((let let* symbol-macrolet) (mapcar #'first second))
      ((multiple-value-bind) second)
      ((with-package-iterator with-hash-table-iterator) `(,(first second))))))

(defun check-multiple-bindings (variables)
  (mapl #'(lambda (vars)
            (when (member (first vars) (rest vars))
              (loop-error 'program-error
                          :format-control "Variable ~S is bound more than once."
                          :format-arguments (list (first vars)))))
        variables))


(defmacro with-loop-context (tokens &body body)
  `(let ((*loop-tokens* ,tokens)
         (*loop-name* nil)
         (*current-keyword* nil)
         (*current-clause* nil)
         (*loop-components* nil)
         (*temporaries* nil)
         (*ignorable* nil)
         (*accumulators* nil)
         (*anonymous-accumulator* nil)
         (*boolean-terminator* nil)
         (*message-prefix* "LOOP: "))
    ,@body))

(defun with-iterator-forms (iterator-forms form)
  (if (null iterator-forms)
      form
      (destructuring-bind ((iterator-macro spec) . rest) iterator-forms
        `(,iterator-macro ,spec
          ,(with-iterator-forms rest form)))))

(defun with-binding-forms (binding-forms form)
  (if (null binding-forms)
      form
      (destructuring-bind (binding-form0 . rest) binding-forms
        (append binding-form0 (list (with-binding-forms rest form))))))

(defun with-temporaries (temporary-specs form)
  (destructuring-bind (temporaries &key ignorable) temporary-specs
    (if temporaries
        `(let ,temporaries
          ,@(when ignorable `((declare (ignorable ,@ignorable))))
          ,form)
        form)))

(defun with-list-accumulator (accumulator-spec form)
  (destructuring-bind (name &key var splice &allow-other-keys) accumulator-spec
    (let* ((anonymous-p (null name))
           (list-var (if (or anonymous-p (globally-special-p var))
                         var
                         (gensym "LIST-")))
           (value-form (if (and (not anonymous-p) (globally-special-p var))
                           nil
                           '(list nil)))
           (form (if (and (not anonymous-p) (not (globally-special-p var)))
                     `(symbol-macrolet ((,var (cdr ,list-var)))
                       ,form)
                     form)))
      `(let ((,list-var ,value-form))
        ;;(declare (dynamic-extent ,list-var))
        (declare (type list ,list-var))
        (let ((,splice ,list-var))
          (declare (type cons ,splice))
          ,form)))))

(defun with-numeric-accumulator (accumulator-spec form)
  (destructuring-bind (name &key var types &allow-other-keys) accumulator-spec
    (labels ((type-eq (a b) (and (subtypep a b) (subtypep b a))))
      (when (null types) (setq types '(number)))
      (destructuring-bind (type0 . rest) types
        (when (and rest (notevery #'(lambda (type) (type-eq type0 type)) types))
          (warn "Different types ~A are declared for ~A accumulator."
                (enumerate types) (or name "the anonymous")))
        (let ((type (if rest `(or ,type0 ,@rest) type0)))
          `(let ((,var ,(zero type)))
            (declare (type ,type ,var))
            ,form))))))

(defun with-accumulators (accumulator-specs form)
  (if (null accumulator-specs)
      form
      (destructuring-bind (spec . rest) accumulator-specs
        (ecase (getf (cdr spec) :kind)
          (:list
           (with-list-accumulator    spec (with-accumulators rest form)))
          ((:total :limit)
           (with-numeric-accumulator spec (with-accumulators rest form)))))))

(defun reduce-redundant-code ()
  (when (null (getf *loop-components* :initially))
    (let ((rhead (reverse (getf *loop-components* :head)))
          (rtail (reverse (getf *loop-components* :tail)))
          (neck nil))
      (loop
       (when (or (null rhead) (null rtail) (not (equal (car rhead) (car rtail))))
         (return))
       (push (pop rhead) neck)
       (pop rtail))
      (setf (getf *loop-components* :head) (nreverse rhead)
            (getf *loop-components* :neck) neck
            (getf *loop-components* :tail) (nreverse rtail)))))

(defmacro extended-loop (&rest tokens &environment environment)
  (let ((*environment* environment))
    (with-loop-context tokens
      (let ((body-tag (gensym "LOOP-BODY-"))
            (epilogue-tag (gensym "LOOP-EPILOGUE-")))
        (name-clause?)
        (variable-clause*)
        (main-clause*)
        (when *loop-tokens*
          (error "Loop form tail ~S remained unprocessed." *loop-tokens*))
        (reduce-redundant-code)
        (destructuring-bind (&key binding-forms iterator-forms initially
                                  head neck body tail finally results)
            *loop-components*
          (check-multiple-bindings
           (append *temporaries* (mappend #'bound-variables binding-forms)
                   (mapcar #'(lambda (spec) (getf (cdr spec) :var)) *accumulators*)))
          `(block ,*loop-name*
            ,(with-temporaries `(,*temporaries* :ignorable ,*ignorable*)
               (with-accumulators *accumulators*
                 (with-binding-forms binding-forms
                   (with-iterator-forms iterator-forms
                     `(macrolet ((loop-finish () '(go ,epilogue-tag)))
                       (tagbody
                          ,@head
                          ,@initially
                          ,body-tag
                          ,@neck
                          ,@body
                          ,@tail
                          (go ,body-tag)
                          ,epilogue-tag
                          ,@finally
                          ,@(when results
                                  `((return-from ,*loop-name* ,(car results))))))))))))))))

(defmacro simple-loop (&rest compound-forms)
  (let ((top (gensym)))
    `(block nil
      (tagbody
         ,top
         ,@compound-forms
         (go ,top)))))

(defmacro loop (&rest forms)
  (if (every #'consp forms)
      `(simple-loop ,@forms)
      `(extended-loop ,@forms)))

(provide "LOOP")
