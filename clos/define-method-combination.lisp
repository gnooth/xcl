;;; define-method-combination.lisp
;;;
;;; Copyright (C) 2009 Peter Graves <peter@armedbear.org>
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

;;; Adapted from Sacla.

;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: clos.lisp,v 1.28 2004/09/24 07:31:33 yuji Exp $
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

(in-package "SYSTEM")

;; FIXME where does this belong?
(defun declarationp (expr)
  (and (consp expr) (eq (%car expr) 'DECLARE)))

;; short form
(defun expand-short-defcombin (whole)
  (let* ((name (cadr whole))
         (documentation
          (getf (cddr whole) :documentation ""))
         (identity-with-one-argument
          (getf (cddr whole) :identity-with-one-argument nil))
         (operator
          (getf (cddr whole) :operator name)))
    `(progn
       (setf (get ',name 'method-combination-object)
             (make-instance 'short-method-combination
                            :name ',name
                            :operator ',operator
                            :identity-with-one-argument ',identity-with-one-argument
                            :documentation ',documentation))
       ',name)))

;; long form
(defstruct method-combination-type
  name
  lambda-list
  group-specifiers
  args-lambda-list
  generic-function-symbol
  documentation
  function
  short-form-options)

(defclass long-method-combination (method-combination)
  ((type :reader long-combination-type :initarg :type)
   (arguments :accessor long-combination-arguments :initarg :arguments :initform nil)))

(defun copy-long-method-combination (mc)
  (declare (type long-method-combination mc))
  (make-instance 'long-method-combination
                 :type (long-combination-type mc)))

;; MOP p. 191
;; "The METHOD-COMBINATION-OPTIONS argument is a list of arguments to the
;; method combination type."
(defgeneric find-method-combination (generic-function
                                     method-combination-type-name ; a symbol
                                     method-combination-options))

(defmethod find-method-combination ((gf standard-generic-function)
                                    method-combination-type-name
                                    method-combination-options)
  (let ((mc (get method-combination-type-name 'method-combination-object)))
    (cond ((null mc)
           (error "Method combination ~S does not exist." method-combination-type-name))
          ((eq mc *standard-method-combination*)
           (when method-combination-options
             (error "The STANDARD method combination accepts no options."))
           mc)
          (t
           (method-combination-with-options mc method-combination-options)))))

(defun define-method-combination-type (name &rest initargs)
  (let ((combination-type (apply #'make-method-combination-type
                                 :allow-other-keys t :name name initargs)))
    (setf (get name 'method-combination-object)
          (make-instance 'long-method-combination :type combination-type))))

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
    `(list :name ',name
           :predicate (lambda (qualifiers)
                        (loop for item in ',selecters
                          thereis (method-group-p item qualifiers)))
           :description ',description
           :order ,order
           :required ',required-p)))

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
                         `(,(if (consp key) key `(,(make-keyword key) ,key))
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
              (,rest-args (subseq ,+gf-args-var+ (+ ,nrequired ,noptional)))
              ,@(when whole `((,whole ,+gf-args-var+)))
              ,@(loop for var in required and i upfrom 0
                  collect `(,var (when (< ,i ,nrequired)
                                   (nth ,i ,+gf-args-var+))))
              ,@(loop for (var init-form) in optional and i upfrom 0
                  collect
                  `(,var (if (< ,i ,noptional)
                             (nth (+ ,nrequired ,i) ,+gf-args-var+)
                             ,init-form)))
              ,@(when rest `((,rest ,rest-args)))
              ,@(loop for ((key var) init-form) in keys and i upfrom 0
                  collect `(,var (getk ,rest-args ',key ,init-form)))
              ,@(loop for (var init-form) in aux and i upfrom 0
                  collect `(,var ,init-form)))
         ,@forms))))

(defmacro with-method-groups (method-group-specs methods-form &body forms)
  (flet ((grouping-form (spec methods-var)
                        (let ((predicate (coerce-to-function (getf spec :predicate)))
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
                                 "Method ~S with qualifiers ~S does not belong to any method group."
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
                     :method-group-specs ,method-group-specs
                     ,@(long-form-method-combination-args args)))
         (lambda-expression (apply #'method-combination-type-lambda initargs)))
    (apply #'define-method-combination-type name
           `(,@initargs
;;              :function ,(compile nil lambda-expression)
             :function ,(coerce-to-function lambda-expression)
             :short-form-options nil))
    name))

(defmacro define-method-combination (&whole form name &rest args)
  (cond ((and args
              (listp (car args)))
         (destructuring-bind (lambda-list method-groups &rest body) args
           `(apply #'define-long-form-method-combination
                   ',name
                   ',lambda-list
                   (list ,@(mapcar #'canonicalize-method-group-spec method-groups))
                   ',body)))
        (t
         (expand-short-defcombin form))))

(defmethod compute-effective-method ((generic-function standard-generic-function)
                                     (method-combination long-method-combination)
                                     methods)
  (let* ((type (long-combination-type method-combination))
         (type-function (method-combination-type-function type))
         (arguments (long-combination-arguments method-combination))
         (effective-method (apply type-function generic-function methods arguments)))
    (values effective-method
            `(:arguments ,(method-combination-type-args-lambda-list type)
                         :generic-function
                         ,(method-combination-type-generic-function-symbol type)))))
