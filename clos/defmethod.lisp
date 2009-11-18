;;; defgeneric.lisp
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

(declaim (inline make-method-form-p))
(defun make-method-form-p (object)
  (and (consp object) (eq (%car object) 'make-method)))

(defvar *call-next-method-p*)
(defvar *next-method-p-p*)

(defknown walk-form (t) t)
(defun walk-form (form)
  (cond ((atom form)
         (cond ((eq form 'call-next-method)
                (setq *call-next-method-p* t))
               ((eq form 'next-method-p)
                (setq *next-method-p-p* t))))
        (t
         (walk-form (%car form))
         (walk-form (%cdr form)))))

(defun %make-method-lambda (generic-function method lambda-expression environment)
  (declare (ignore generic-function method environment)) ; REVIEW
  (let* ((lambda-list (allow-other-keys (cadr lambda-expression)))
         (numargs (length lambda-list)))
    (multiple-value-bind (body declarations) (parse-body (cddr lambda-expression))
      (let ((*call-next-method-p* nil)
            (*next-method-p-p* nil)
            (next-methods (gensym))
            method-lambda)
        (walk-form body)
        (cond ((or *call-next-method-p* *next-method-p-p*)
;;                (format t "%make-method-lambda hairy case numargs = ~D~%" numargs)
               (setq method-lambda
                     `(lambda (,+gf-args-var+ ,next-methods)
                        (flet ((call-next-method (&rest cnm-args)
                                 (unless cnm-args
                                   (setq cnm-args ,+gf-args-var+))
                                 (if (null ,next-methods)
                                     (error "No next method for generic function.")
                                     ;; (call-method (car next-methods) (cdr next-methods))
                                     (let ((next-method (car ,next-methods)))
                                       (cond ((typep next-method 'method)
;;                                               (mumble "case 1~%")
                                              (funcall (method-function next-method)
                                                       ,+gf-args-var+
                                                       (cdr ,next-methods)))
;;                                              ((and (consp next-method)
;;                                                    (eq (%car next-method) 'MAKE-METHOD))
                                             ((make-method-form-p next-method)
;;                                               (mumble "MAKE-METHOD case~%")
;;                                               (funcall next-method ,+gf-args-var+)
                                              (let* ((form (second next-method))
                                                     (lambda-form
                                                      (list 'LAMBDA (list +gf-args-var+) form)))
                                                (funcall (coerce-to-function lambda-form) ,+gf-args-var+))
                                              )
                                             (t
                                              (aver nil)
;;                                               (mumble "case 3 ~S~%" (type-of next-method))
                                              (funcall next-method ,+gf-args-var+)
                                              )))
                                     ))
                               (next-method-p ()
                                 (not (null ,next-methods))))
                          (apply #'(lambda ,lambda-list ,@declarations ,@body) ,+gf-args-var+)))))
              ((null (intersection lambda-list lambda-list-keywords))
               (setq method-lambda
                     (case numargs
                       (0
                        `(lambda (,+gf-args-var+ ,next-methods)
                           (declare (ignore ,next-methods))
                           ,@declarations
                           ,@body))
                       (1
                        `(lambda (,+gf-args-var+ ,next-methods)
                           (declare (ignore ,next-methods))
                           (let ((,(%car lambda-list) (%car ,+gf-args-var+)))
                             (declare (ignorable ,(%car lambda-list)))
                             ,@declarations
                             ,@body)))
                       (2
                        `(lambda (,+gf-args-var+ ,next-methods)
                           (declare (ignore ,next-methods))
                           (let ((,(%car lambda-list) (%car ,+gf-args-var+))
                                 (,(%cadr lambda-list) (%cadr ,+gf-args-var+)))
                             (declare (ignorable ,(%car lambda-list)
                                                 ,(%cadr lambda-list)))
                             ,@declarations
                             ,@body)))
                       (3
                        `(lambda (,+gf-args-var+ ,next-methods)
                           (declare (ignore ,next-methods))
                           (let ((,(%car lambda-list) (%car ,+gf-args-var+))
                                 (,(%cadr lambda-list) (%cadr ,+gf-args-var+))
                                 (,(%caddr lambda-list) (%caddr ,+gf-args-var+)))
                             (declare (ignorable ,(%car lambda-list)
                                                 ,(%cadr lambda-list)
                                                 ,(%caddr lambda-list)))
                             ,@declarations
                             ,@body)))
                       (t
;;                         (format t "%make-method-lambda numargs > 3 case~%")
                        `(lambda (,+gf-args-var+ ,next-methods)
                           (declare (ignore ,next-methods))
                           (apply #'(lambda ,lambda-list ,@declarations ,@body) ,+gf-args-var+))
                        ))))
              (t
;;                (format t "%make-method-lambda default case~%")
               (setq method-lambda
                     `(lambda (,+gf-args-var+ ,next-methods)
                        (declare (ignore ,next-methods))
                        (apply #'(lambda ,lambda-list ,@declarations ,@body) ,+gf-args-var+)))))
        (values method-lambda nil)))))

(defun compute-method-fast-function (lambda-expression specializers)
  ;;   (declare (ignore specializers)) ; REVIEW
  (let ((lambda-list (allow-other-keys (cadr lambda-expression))))
    (when (intersection lambda-list lambda-list-keywords)
      (return-from compute-method-fast-function nil))
    ;; only required args
    (let ((body (cddr lambda-expression))
          (*call-next-method-p* nil)
          (*next-method-p-p* nil))
      (multiple-value-bind (body declarations) (parse-body body)
        (walk-form body)
        (when (or *call-next-method-p* *next-method-p-p*)
          (return-from compute-method-fast-function nil))
        (unless (eql (length lambda-list) (length specializers))
          (format t "(length lambda-list) = ~S (length specializers) = ~S~%"
                  (length lambda-list) (length specializers))
          (return-from compute-method-fast-function nil))
        (case (length lambda-list)
          (1
           `(lambda ,lambda-list
              (declare (ignorable ,@lambda-list))
              ,@declarations
              ;;               (require-type ,(%car lambda-list) ',(%car specializers))
              ,@body)
           )
          (2
           `(lambda ,lambda-list
              (declare (ignorable ,@lambda-list))
              ,@declarations
              ;;               (require-type ,(%car lambda-list) ',(%car specializers))
              ;;               (require-type ,(%cadr lambda-list) ',(%cadr specializers))
              ,@body)
           )
          (3
           `(lambda ,lambda-list
              (declare (ignorable ,@lambda-list))
              ,@declarations
              ;;               (require-type ,(%car lambda-list) ',(%car specializers))
              ;;               (require-type ,(%cadr lambda-list) ',(%cadr specializers))
              ;;               (require-type ,(%caddr lambda-list) ',(%caddr specializers))
              ,@body)
           )
          (t
           nil))))))

(defun parse-defmethod (args)
  (let ((function-name (car args))
        (qualifiers nil)
        (specialized-lambda-list nil)
        (body nil)
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
        (:qualifiers
         (if (and (atom arg) (not (null arg)))
             (push arg qualifiers)
             (progn
               (setq specialized-lambda-list arg)
               (setq parse-state :body))))
        (:body
         (push arg body))))
    (setq qualifiers (nreverse qualifiers))
    (setq body       (nreverse body))
    (let ((lambda-list  (extract-lambda-list  specialized-lambda-list))
          (specializers (extract-specializers specialized-lambda-list))
          (block-name   (fdefinition-block-name function-name)))
      (multiple-value-bind (real-body declarations documentation)
          (parse-body body)
        (values function-name
                qualifiers
                lambda-list
                specializers
                `((block ,(fdefinition-block-name function-name) ,@real-body))
                `(lambda ,lambda-list ,@declarations (block ,block-name ,@real-body))
                declarations
                documentation)))))

(defun prototypes-for-make-method-lambda (name)
  (let ((gf (and (fboundp name)
                 (fdefinition name))))
    (if (or (null gf)
            (not (typep gf 'generic-function)))
        (values (class-prototype +the-class-standard-generic-function+)
                (class-prototype +the-class-standard-method+))
        (values gf
                (class-prototype (or (generic-function-method-class gf)
                                     +the-class-standard-method+))))))

(defmacro defmethod (&rest args)
  (multiple-value-bind
      (name qualifiers lambda-list specializers body lambda-expression declarations documentation)
      (parse-defmethod args)
    (declare (ignore body declarations documentation)) ; REVIEW
    (let (specializers-form)
      (dolist (specializer specializers)
        (cond ((and (consp specializer) (eq (car specializer) 'eql))
               (push `(list 'eql ,(cadr specializer)) specializers-form))
              (t
               (push `',specializer specializers-form))))
      (setq specializers-form `(list ,@(nreverse specializers-form)))
      (let* ((method-lambda
              (if *mop-working-p*
                  (multiple-value-bind (proto-gf proto-method)
                      (prototypes-for-make-method-lambda name)
                    (make-method-lambda
                     proto-gf
                     proto-method
                     lambda-expression
                     nil))
                  (%make-method-lambda
                   nil
                   nil
                   lambda-expression
                   nil)))
             (method-function (precompile-form method-lambda))
             (method-fast-function (compute-method-fast-function lambda-expression specializers)))
        `(ensure-method ',name
                        :lambda-list ',lambda-list
                        :qualifiers ',qualifiers
                        :specializers ,specializers-form
                        :function (function ,method-function)
                        ,@(if method-fast-function `(:fast-function (function ,method-fast-function))))))))
