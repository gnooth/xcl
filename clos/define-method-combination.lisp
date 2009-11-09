;;; define-method-combination.lisp
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

;; FIXME where does this belong?
(defun declarationp (expr)
  (and (consp expr) (eq (car expr) 'declare)))

(defclass method-combination (metaobject) ())

(defstruct method-combination-type
  name
  lambda-list
  group-specifiers
  args-lambda-list
  generic-function-symbol
  documentation
  function
  short-form-options)

(defclass standard-method-combination (method-combination) ; clos
  ((type :reader method-combination-type :initarg :type)
   (arguments :reader method-combination-arguments :initarg :arguments)))

;; MOP p. 191
;; "The METHOD-COMBINATION-OPTIONS argument is a list of arguments to the
;; method combination type."
(defgeneric find-method-combination
  (generic-function method-combination-type-name method-combination-options))

(defmethod find-method-combination
  ((gf standard-generic-function) method-combination-type method-combination-options)
  (multiple-value-bind (type presentp)
      (gethash method-combination-type *method-combination-types*)
    (if presentp
        (make-instance 'standard-method-combination
                       :type type
                       :arguments method-combination-options)
        (error "Method combination ~S does not exist." method-combination-type))))

;; (defstruct (%method-combination (:conc-name method-combination-)
;;                                 (:constructor make-method-combination))
;;   name
;;   operator
;;   identity-with-one-argument
;;   documentation)

;; (defun expand-short-defcombin (whole)
;;   (let* ((name (cadr whole))
;;          (documentation
;;           (getf (cddr whole) :documentation ""))
;;          (identity-with-one-arg
;;           (getf (cddr whole) :identity-with-one-argument nil))
;;          (operator
;;           (getf (cddr whole) :operator name)))
;;     `(progn
;;        (setf (get ',name 'method-combination-object)
;;              (make-method-combination :name ',name
;;                                       :operator ',operator
;;                                       :identity-with-one-argument ',identity-with-one-arg
;;                                       :documentation ',documentation))
;;        ',name)))

;; (defun expand-long-defcombin (whole)
;;   (declare (ignore whole))
;;   (error "The long form of DEFINE-METHOD-COMBINATION is not implemented."))


;; #+sacla
;; (progn

;; FIXME also in loop.lisp
(defun %keyword (designator)
  (intern (string designator) "KEYWORD"))


;; (defclass method-combination (metaobject) ()) ; clos
;; (defstruct method-combination-type
;;   (name)
;;   (lambda-list)
;;   (group-specifiers)
;;   (args-lambda-list)
;;   (generic-function-symbol)
;;   (documentation)
;;   (function)
;;   (short-form-options))
;; (defclass standard-method-combination (method-combination) ; clos
;;   ((type :reader method-combination-type :initarg :type)
;;    (arguments :reader method-combination-arguments :initarg :arguments)))

(defparameter *method-combination-types* (make-hash-table))

(defun define-method-combination-type (name &rest initargs)
  (let ((combination-type (apply #'make-method-combination-type
                                 :allow-other-keys t :name name initargs)))
    (setf (gethash name *method-combination-types*) combination-type)))

(defun method-group-p (selecter qualifiers)
  ;; selecter::= qualifier-pattern | predicate
  (etypecase selecter
    (list (or (equal selecter qualifiers)
              (let ((last (last selecter)))
                (when (eq '* (cdr last))
                  (let* ((prefix `(,@(butlast selecter) ,(car last)))
                         (pos (mismatch prefix qualifiers)))
                    (or (null pos) (= pos (length prefix))))))))
    ((eql *) t)
    (symbol (funcall (symbol-function selecter) qualifiers))))

(defun check-variable-name (name)
  (flet ((valid-variable-name-p (name)
                                (and (symbolp name) (not (constantp name)))))
    (assert (valid-variable-name-p name))))

(defun canonicalize-method-group-spec (spec)
  ;; spec ::= (name {qualifier-pattern+ | predicate} [[long-form-option]])
  ;; long-form-option::= :description description | :order order |
  ;;                     :required required-p
  ;; a canonicalized-spec is a simple plist.
  (let* ((rest spec)
         (name (prog2 (check-variable-name (car rest))
                 (car rest)
                 (setq rest (cdr rest))))
         (option-names '(:description :order :required))
         (selecters (let ((end (or (position-if #'(lambda (it)
                                                   (member it option-names))
                                                rest)
                                   (length rest))))
                      (prog1 (subseq rest 0 end)
                        (setq rest (subseq rest end)))))
         (description (getf rest :description ""))
         (order (getf rest :order :most-specific-first))
         (required-p (getf rest :required)))
    `(:name ,name
            :predicate #'(lambda (qualifiers)
                          (loop for item in ',selecters
                            thereis (method-group-p item qualifiers)))
            :description ,description
            :order ,order
            :required ,required-p)))

(defconstant +gf-args-variable+ (gensym "GF-ARGS-VARIABLE-")
  "A Variable name whose value is a list of all arguments to a generic function.")

(defun extract-required-part (lambda-list)
  (flet ((skip (key lambda-list)
               (if (eq (first lambda-list) key)
                   (cddr lambda-list)
                   lambda-list)))
    (ldiff (skip '&environment (skip '&whole lambda-list))
           (member-if #'(lambda (it) (member it lambda-list-keywords))
                      lambda-list))))

(defun extract-specified-part (key lambda-list)
  (case key
    ((&eval &whole)
     (list (second (member key lambda-list))))
    (t
     (let ((here (cdr (member key lambda-list))))
       (ldiff here
              (member-if #'(lambda (it) (member it lambda-list-keywords))
                         here))))))

(defun extract-optional-part (lambda-list)
  (extract-specified-part '&optional lambda-list))

(defun parse-define-method-combination-arguments-lambda-list (lambda-list)
  ;; Define-method-combination Arguments Lambda Lists
  ;; http://www.lispworks.com/reference/HyperSpec/Body/03_dj.htm
  (let ((required (extract-required-part lambda-list))
        (whole    (extract-specified-part '&whole    lambda-list))
        (optional (extract-specified-part '&optional lambda-list))
        (rest     (extract-specified-part '&rest     lambda-list))
        (keys     (extract-specified-part '&key      lambda-list))
        (aux      (extract-specified-part '&aux      lambda-list)))
    (values (first whole)
            required
            (mapcar #'(lambda (spec)
                       (if (consp spec)
                           `(,(first spec) ,(second spec) ,@(cddr spec))
                           `(,spec nil)))
                    optional)
            (first rest)
            (mapcar #'(lambda (spec)
                       (let ((key (if (consp spec) (car spec) spec))
                             (rest (when (consp spec) (rest spec))))
                         `(,(if (consp key) key `(,(%keyword key) ,key))
                           ,(car rest)
                           ,@(cdr rest))))
                    keys)
            (mapcar #'(lambda (spec)
                       (if (consp spec)
                           `(,(first spec) ,(second spec))
                           `(,spec nil)))
                    aux))))

(defmacro getk (plist key init-form)
  "Similar to getf except eval and return INIT-FORM if KEY has no value in PLIST."
  (let ((not-exist (gensym))
        (value (gensym)))
    `(let ((,value (getf ,plist ,key ,not-exist)))
       (if (eq ,not-exist ,value) ,init-form ,value))))

(defmacro with-args-lambda-list (args-lambda-list generic-function-symbol
                                                  &body forms)
  (let ((gf-lambda-list (gensym))
        (nrequired (gensym))
        (noptional (gensym))
        (rest-args (gensym)))
    (multiple-value-bind (whole required optional rest keys aux)
        (parse-define-method-combination-arguments-lambda-list args-lambda-list)
      `(let* ((,gf-lambda-list (slot-value ,generic-function-symbol 'lambda-list))
              (,nrequired (length (extract-required-part ,gf-lambda-list)))
              (,noptional (length (extract-optional-part ,gf-lambda-list)))
              (,rest-args (subseq ,+gf-args-variable+ (+ ,nrequired ,noptional)))
              ,@(when whole `((,whole ,+gf-args-variable+)))
              ,@(loop for var in required and i upfrom 0
                  collect `(,var (when (< ,i ,nrequired)
                                   (nth ,i ,+gf-args-variable+))))
              ,@(loop for (var init-form) in optional and i upfrom 0
                  collect
                  `(,var (if (< ,i ,noptional)
                             (nth (+ ,nrequired ,i) ,+gf-args-variable+)
                             ,init-form)))
              ,@(when rest `((,rest ,rest-args)))
              ,@(loop for ((key var) init-form) in keys and i upfrom 0
                  collect `(,var (getk ,rest-args ',key ,init-form)))
              ,@(loop for (var init-form) in aux and i upfrom 0
                  collect `(,var ,init-form)))
         ,@forms))))

;; (defun invalid-method-error (method format-control &rest args)
;;   (declare (ignorable method))
;;   (apply #'error format-control args))

;; (defun method-combination-error (format-control &rest args)
;;   (apply #'error format-control args))

(defmacro with-method-groups (method-group-specs methods-form &body forms)
  (flet ((grouping-form (spec methods-var)
                        (let ((predicate (getf spec :predicate))
                              (group (gensym))
                              (leftovers (gensym))
                              (method (gensym)))
                          `(let ((,group '())
                                 (,leftovers '()))
                             (dolist (,method ,methods-var)
                               (if (funcall ,predicate (slot-value ,method 'qualifiers))
                                   (push ,method ,group)
                                   (push ,method ,leftovers)))
                             (ecase ,(getf spec :order)
                               (:most-specific-last )
                               (:most-specific-first (setq ,group (nreverse ,group))))
                             ,@(when (getf spec :required)
                                 `((when (null ,group)
                                     (error "Method group ~S must not be empty."
                                            ',(getf spec :name)))))
                             (setq ,methods-var (nreverse ,leftovers))
                             ,group))))
    (let ((rest (gensym))
          (method (gensym)))
      `(let* ((,rest ,methods-form)
              ,@(mapcar #'(lambda (spec)
                           `(,(getf spec :name) ,(grouping-form spec rest)))
                        method-group-specs))
         (dolist (,method ,rest)
           (invalid-method-error ,method
                                 "Method ~S with qualifiers ~S does not~ belong ~
                                  to any method group."
                                 ,method (slot-value ,method 'qualifiers)))
         ,@forms))))

(defun method-combination-type-lambda
  (&key name lambda-list args-lambda-list generic-function-symbol
        method-group-specs declarations forms &allow-other-keys)
  (let ((methods (gensym)))
    `(lambda (,generic-function-symbol ,methods ,@lambda-list)
       ,@declarations
       (let ((*message-prefix* ,(format nil "METHOD COMBINATION TYPE ~S: " name)))
         (with-method-groups ,method-group-specs
           ,methods
           ,@(if (null args-lambda-list)
                 forms
                 `((with-args-lambda-list ,args-lambda-list
                     ,generic-function-symbol
                     ,@forms))))))))

(defun long-form-method-combination-args (args)
  ;; define-method-combination name lambda-list (method-group-specifier*) args
  ;; args ::= [(:arguments . args-lambda-list)]
  ;;          [(:generic-function generic-function-symbol)]
  ;;          [[declaration* | documentation]] form*
  (let ((rest args))
    (labels ((nextp (key) (and (consp (car rest)) (eq key (caar rest))))
             (args-lambda-list ()
                               (when (nextp :arguments)
                                 (prog1 (cdr (car rest)) (setq rest (cdr rest)))))
             (generic-function-symbol ()
                                      (if (nextp :generic-function)
                                          (prog1 (second (car rest)) (setq rest (cdr rest)))
                                          (gensym)))
             (declaration* ()
                           (let ((end (position-if-not #'declarationp rest)))
                             (when end
                               (prog1 (subseq rest 0 end) (setq rest (nthcdr end rest))))))
             (documentation? ()
                             (when (stringp (car rest))
                               (prog1 (car rest) (setq rest (cdr rest)))))
             (form* () rest))
      (let ((declarations '()))
        `(:args-lambda-list ,(args-lambda-list)
                            :generic-function-symbol ,(generic-function-symbol)
                            :documentation ,(prog2 (setq declarations (declaration*))
                                              (documentation?))
                            :declarations (,@declarations ,@(declaration*))
                            :forms ,(form*))))))

(defun define-long-form-method-combination (name lambda-list method-group-specs
                                                 &rest args)
  (let* ((initargs `(:name ,name
                           :lambda-list ,lambda-list
                           :method-group-specs
                           ,(mapcar #'canonicalize-method-group-spec method-group-specs)
                           ,@(long-form-method-combination-args args)))
         (lambda-expression (apply #'method-combination-type-lambda initargs)))
    ;;(format t "~&~S~%" lambda-expression)
    (apply #'define-method-combination-type name
           `(,@initargs
;;              :function ,(compile nil lambda-expression)
             :function ,(coerce-to-function lambda-expression)
             :short-form-options nil))))

;; (defun define-short-form-method-combination
;;   (name &key identity-with-one-argument (documentation "") (operator name))
;;   (aver nil)
;;   (define-long-form-method-combination name
;;     '(&optional (order :most-specific-first))
;;     `((around (:around))
;;       (primary (,name) :order order :required t))
;;     documentation
;;     `(let ((form (if (and ,identity-with-one-argument (null (rest primary)))
;;                      `(call-method ,(first primary))
;;                      (cons ',operator (mapcar #'(lambda (method)
;;                                                  `(call-method ,method))
;;                                               primary)))))
;;        (if around
;;            `(call-method ,(first around) (,@(rest around) (make-method ,form)))
;;            form)))
;;   (let ((combination-type (gethash name *method-combination-types*)))
;;     (setf (method-combination-type-short-form-options combination-type)
;;           `(:documentation ,documentation
;;                            :operator ,operator
;;                            :identity-with-one-argument ,identity-with-one-argument)))
;;   name)

;; (defmacro define-method-combination (name &rest args) ; clos
;;   "Define new types of method combination."
;;   (format t "~&define-method-combination: ~S~%" name)
;;   `(let ((*message-prefix*
;;           ,(format nil "DEFINE-METHOD-COMBINATION ~S: " name)))
;;      (apply #',(if (listp (first args))
;;                    'define-long-form-method-combination
;;                    'define-short-form-method-combination) ',name ',args)))

;; ) ; end sacla

(defstruct (%method-combination (:conc-name method-combination-)
                                (:constructor make-method-combination))
  name
  operator
  identity-with-one-argument
  documentation)

(defun expand-short-defcombin (whole)
  (let* ((name (cadr whole))
         (documentation
          (getf (cddr whole) :documentation ""))
         (identity-with-one-arg
          (getf (cddr whole) :identity-with-one-argument nil))
         (operator
          (getf (cddr whole) :operator name)))
    `(progn
       (setf (get ',name 'method-combination-object)
             (make-method-combination :name ',name
                                      :operator ',operator
                                      :identity-with-one-argument ',identity-with-one-arg
                                      :documentation ',documentation))
       ',name)))

;; (defun expand-long-defcombin (whole)
;;   (declare (ignore whole))
;;   (error "The long form of DEFINE-METHOD-COMBINATION is not implemented."))

(defmacro define-method-combination (&whole form name &rest args)
;;   (declare (ignore name))
  (cond ((and args
              (listp (car args)))
;;          (expand-long-defcombin form)
         (mumble "define-method-combination long form name = ~S~%" name)
         (error "unsupported")
         `(let ((*message-prefix*
                 ,(format nil "DEFINE-METHOD-COMBINATION ~S: " name)))
            (apply #'define-long-form-method-combination ',name ',args)))
        (t
         (expand-short-defcombin form))))
