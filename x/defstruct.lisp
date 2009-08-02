;;; defstruct.lisp
;;;
;;; Copyright (C) 2003-2009 Peter Graves <peter@armedbear.org>
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

(defconstant +structure-types+ (make-hash-table :test 'eq))

(defun make-structure-slot-definition (&key name
                                            index
                                            reader
                                            initform
                                            (type t)
                                            read-only)
  (declare (optimize speed))
  (%make-structure-slot-definition name index reader initform type read-only))

(export 'compiler-defstruct)

;;; DEFSTRUCT-DESCRIPTION

(defmacro dd-name (x)                `(aref ,x  0))
(defmacro dd-conc-name (x)           `(aref ,x  1))
(defmacro dd-default-constructor (x) `(aref ,x  2))
(defmacro dd-constructors (x)        `(aref ,x  3))
(defmacro dd-copier (x)              `(aref ,x  4))
(defmacro dd-include (x)             `(aref ,x  5))
(defmacro dd-type (x)                `(aref ,x  6))
(defmacro dd-named (x)               `(aref ,x  7))
(defmacro dd-initial-offset (x)      `(aref ,x  8))
(defmacro dd-predicate (x)           `(aref ,x  9))
(defmacro dd-print-function (x)      `(aref ,x 10))
(defmacro dd-print-object (x)        `(aref ,x 11))
(defmacro dd-direct-slots (x)        `(aref ,x 12))
(defmacro dd-slots (x)               `(aref ,x 13))

(defun make-defstruct-description (&key name
                                        conc-name
                                        default-constructor
                                        constructors
                                        copier
                                        include
                                        type
                                        named
                                        initial-offset
                                        predicate
                                        print-function
                                        print-object
                                        direct-slots
                                        slots)
  (let ((dd (make-array 14)))
    (setf (dd-name dd) name
          (dd-conc-name dd) conc-name
          (dd-default-constructor dd) default-constructor
          (dd-constructors dd) constructors
          (dd-copier dd) copier
          (dd-include dd) include
          (dd-type dd) type
          (dd-named dd) named
          (dd-initial-offset dd) initial-offset
          (dd-predicate dd) predicate
          (dd-print-function dd) print-function
          (dd-print-object dd) print-object
          (dd-direct-slots dd) direct-slots
          (dd-slots dd) slots)
    dd))

(defvar *dd-name*)
(defvar *dd-conc-name*)
(defvar *dd-default-constructor*)
(defvar *dd-constructors*)
(defvar *dd-copier*)
(defvar *dd-include*)
(defvar *dd-type*)
(defvar *dd-named*)
(defvar *dd-initial-offset*)
(defvar *dd-predicate*)
(defvar *dd-print-function*)
(defvar *dd-print-object*)
(defvar *dd-direct-slots*)
(defvar *dd-slots*)

(defvar *inherited-accessors-alist*)

(defun make-simple-vector-for-defstruct (size initialization-list)
  (declare (type index size) (type list initialization-list))
  (let ((v (make-simple-vector size))
        (i 0))
    (declare (type index i))
    (dolist (value initialization-list)
      (%svset v i value)
      (incf i))
    v))

(defun define-keyword-constructor (constructor)
  (let* ((constructor-name (car constructor))
         (keys nil)
         (values nil)
         (types nil))
    (dolist (slot *dd-slots*)
      (let ((name (slot-name slot))
            (initform (slot-initform slot))
            (type (slot-type slot)))
        (if (or name (slot-reader slot))
            (let ((dummy (gensym)))
              (push (list (list (make-keyword name) dummy) initform) keys)
              (push dummy values)
              (push type types))
            (push initform values))))
    (setq keys   (cons '&key (nreverse keys))
          values (nreverse values)
          types  (nreverse types))
    (cond ((eq *dd-type* 'LIST)
           `((defun ,constructor-name ,keys
               (list ,@values))))
          ((or (eq *dd-type* 'VECTOR)
               (and (consp *dd-type*) (eq (car *dd-type*) 'VECTOR)))
           (let* ((element-type (if (consp *dd-type*) (cadr *dd-type*) t))
                  (upgraded-type (upgraded-array-element-type element-type)))
             (cond ((eq upgraded-type t)
                    `((defun ,constructor-name ,keys
                        (make-simple-vector-for-defstruct ,(length values) (list ,@values)))))
                   (t
                    `((defun ,constructor-name ,keys
                        (make-array ,(length values)
                                    :element-type ',upgraded-type
                                    :initial-contents (list ,@values))))))))
          (t
           (let* ((dd-name *dd-name*)
                  (numslots (length values))
                  (type-names (gethash dd-name +structure-types+))
                  (i 0))
             `((defknown ,constructor-name (*) ,dd-name)
;;                (defun ,constructor-name ,keys
;;                  (declare ,@(mapcar (lambda (var type) `(type ,type ,var))
;;                                     values types))
;;                  (%make-structure ',type-names (list ,@values) ,numslots))
               (defun ,constructor-name ,keys
                 (declare ,@(mapcar (lambda (var type) `(type ,type ,var))
                                    values types))
                 (let ((instance (truly-the ,dd-name (%%make-structure ',type-names ,numslots))))
                   ,@(mapcar (lambda (var)
                               (prog1
                                 `(structure-set instance ,i ,var)
                                 (incf i)))
                             values)
                   instance))
               ))))))

(defun find-slot-definition (name)
  (dolist (slot-definition *dd-slots*)
    (when (string= name (slot-name slot-definition))
      (return slot-definition))))

(defun get-slot (name)
  (let ((res nil))
    (dolist (slot *dd-slots*)
      (when (string= name (slot-name slot))
        (setq res slot)
        (return)))
    (if res
        (values (slot-type res) (slot-initform res))
        (values t nil))))

(defun define-boa-constructor (constructor)
  (multiple-value-bind (req opt restp rest keyp keys allowp auxp aux)
      (parse-lambda-list (cadr constructor))
    (let ((arglist nil)
          (vars nil)
          (types nil)
          (skipped-vars nil))
      (dolist (arg req)
        (push arg arglist)
        (push arg vars)
        (push (get-slot arg) types))
      (when opt
        (push '&optional arglist)
        (dolist (arg opt)
          (cond ((consp arg)
                 (destructuring-bind
                     (name
                      &optional
                      (def (nth-value 1 (get-slot name)))
                      (supplied-test nil supplied-test-p))
                     arg
                   (push `(,name ,def ,@(if supplied-test-p `(,supplied-test) nil)) arglist)
                   (push name vars)
                   (push (get-slot name) types)))
                (t
                 (multiple-value-bind (type default) (get-slot arg)
                   (push `(,arg ,default) arglist)
                   (push arg vars)
                   (push type types))))))
      (when restp
        (push '&rest arglist)
        (push rest arglist)
        (push rest vars)
        (push 'list types))
      (when keyp
        (push '&key arglist)
        (dolist (key keys)
          (if (consp key)
              (destructuring-bind (thing
                                   &optional
                                   (def nil def-p)
                                   (supplied-test nil supplied-test-p))
                                  key
                                  (let ((name (if (consp thing)
                                                  (destructuring-bind (key var) thing
                                                    (declare (ignore key))
                                                    var)
                                                  thing)))
                                    (multiple-value-bind (type slot-def)
                                        (get-slot name)
                                      (push `(,thing ,(if def-p def slot-def)
                                              ,@(if supplied-test-p `(,supplied-test) nil))
                                            arglist)
                                      (push name vars)
                                      (push type types))))
              (multiple-value-bind (type default) (get-slot key)
                (push `(,key ,default) arglist)
                (push key vars)
                (push type types)))))
      (when allowp
        (push '&allow-other-keys arglist))
      (when auxp
        (push '&aux arglist)
        (dolist (arg aux)
          (push arg arglist)
          (if (and (consp arg) (length-eql arg 2))
              (let ((var (%car arg)))
                (push var vars)
                (push (get-slot var) types))
              (push (if (consp arg) (%car arg) arg) skipped-vars))))
      (setq arglist (nreverse arglist))
      (setq vars (nreverse vars))
      (setq types (nreverse types))
      (setq skipped-vars (nreverse skipped-vars))
      (let ((values nil))
        (dolist (slot *dd-slots*)
          (let ((name (slot-name slot))
                var)
            (cond ((find name skipped-vars :test #'string=)
                   (push nil values))
                  ((setq var (find name vars :test #'string=))
                   (push var values))
                  (t
                   (push (slot-initform slot) values)))))
        (setq values (nreverse values))
        (let* ((constructor-name (car constructor))
               (numslots (length values)))
          (cond ((eq *dd-type* 'LIST)
                 `((defun ,constructor-name ,arglist
                     (list ,@values))))
                ((or (eq *dd-type* 'VECTOR)
                     (and (consp *dd-type*) (eq (car *dd-type*) 'VECTOR)))
                 (let* ((element-type (if (consp *dd-type*) (cadr *dd-type*) t))
                        (upgraded-type (upgraded-array-element-type element-type)))
                   (cond ((eq upgraded-type t)
                          `((defun ,constructor-name ,arglist
                              (make-simple-vector-for-defstruct ,(length values) (list ,@values)))))
                          (t
                           `((defun ,constructor-name ,arglist
                               (make-array ,(length values)
                                           :element-type ',element-type
                                           :initial-contents (list ,@values))))))))
                (t
                 (let ((dd-name *dd-name*)
                       (type-names (gethash *dd-name* +structure-types+)))
                   `((defknown ,constructor-name (*) ,dd-name)
                     (defun ,constructor-name ,arglist
                       (%make-structure ',type-names (list ,@values) ,numslots)))))))))))

(defun default-constructor-name ()
  (intern (concatenate 'string "MAKE-" (symbol-name *dd-name*))))

(defun define-constructors ()
  (if *dd-constructors*
      (let ((results ()))
        (dolist (constructor *dd-constructors*)
          (when (car constructor)
            (setf results (nconc results
                                 (if (cadr constructor)
                                     (define-boa-constructor constructor)
                                     (define-keyword-constructor constructor))))))
        results)
      (define-keyword-constructor (cons (default-constructor-name) nil))))

(defun name-index ()
  (dolist (slot *dd-slots*)
    (let ((name (slot-name slot))
          (initform (slot-initform slot)))
      (when (and (null name)
                 (equal initform (list 'quote *dd-name*)))
        (return-from name-index (slot-index slot)))))
  ;; We shouldn't get here.
  nil)

(defun define-predicate ()
  (when (and *dd-predicate*
             (or *dd-named* (null *dd-type*)))
    (let ((pred (if (symbolp *dd-predicate*)
                    *dd-predicate*
                    (intern *dd-predicate*))))
      (cond ((eq *dd-type* 'list)
             (let ((index (name-index)))
               `((defun ,pred (object)
                   (and (consp object)
                        (> (length object) ,index)
                        (eq (nth ,index object) ',*dd-name*))))))
            ((or (eq *dd-type* 'vector)
                 (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
             (let ((index (name-index)))
               `((defun ,pred (object)
                   (and (vectorp object)
                        (> (length object) ,index)
                        (eq (aref object ,index) ',*dd-name*))))))
            (t
             `((declaim (inline ,pred))
               (defun ,pred (object) (typep object ',*dd-name*))))))))

(defun define-reader (slot accessor)
  (let ((slot-index (slot-index slot)))
    (set-slot-reader slot accessor)
    (cond ((eq *dd-type* 'list)
           `((defun ,accessor (instance) (elt instance ,slot-index))))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           `((defun ,accessor (instance)
               (vector-ref (the vector instance) ,slot-index))
             (define-source-transform ,accessor (instance)
               (list 'vector-ref
                     (list 'the 'vector instance)
                     ,slot-index))))
          (t
           (let ((dd-name *dd-name*)
                 (slot-type (slot-type slot)))
           `((defun ,accessor (instance)
               (structure-ref (the ,dd-name instance) ,slot-index))
             (define-source-transform ,accessor (instance)
               ,(if (eq slot-type t)
                    ``(structure-ref (the ,',dd-name ,instance) ,,slot-index)
                    ``(truly-the ,',slot-type (structure-ref (the ,',dd-name ,instance) ,,slot-index))))))))))

(defun define-writer (slot accessor)
  (let ((slot-index (slot-index slot))
        (slot-type (slot-type slot)))
    (cond ((eq *dd-type* 'list)
           `((defun (setf ,accessor) (value instance)
               (setf (elt instance ,slot-index) value))))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           `((defun (setf ,accessor) (value instance)
               (vector-set (the vector instance) ,slot-index value))))
          (t
           (let ((dd-name *dd-name*))
             `((defun (setf ,accessor) (value instance)
                 (structure-set (the ,dd-name instance) ,slot-index value))
               (define-source-transform (setf ,accessor) (value instance)
                 `(structure-set (the ,',dd-name ,instance) ,,slot-index (the ,',slot-type ,value)))))))))

(defun define-access-functions ()
  (let ((result nil))
    (dolist (slot *dd-slots*)
      (let ((accessor (if *dd-conc-name*
                          (intern (concatenate 'string
                                               (symbol-name *dd-conc-name*)
                                               (symbol-name (slot-name slot))))
                          (slot-name slot)))
            entry)
        (cond ((setq entry (assoc accessor *inherited-accessors-alist*))

               (format t "define-access-functions ~S not redefining ~S~%" *dd-name* accessor)
               (unless (eql (cdr entry) (slot-index slot))
                 (format t "define-access-functions ~S index mismatch new = ~S old = ~S~%"
                         *dd-name* (cdr entry) (slot-index slot)))
               )
              (t
               (setf result (nconc result (define-reader slot accessor)))
               (unless (slot-read-only-p slot)
                 (setf result (nconc result (define-writer slot accessor))))))))
    result))

(defun define-copier ()
  (when *dd-copier*
    (cond ((eq *dd-type* 'list)
           `((setf (fdefinition ',*dd-copier*) #'copy-list)))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           `((setf (fdefinition ',*dd-copier*) #'copy-seq)))
          (t
           `((setf (fdefinition ',*dd-copier*) #'copy-structure))))))

(defun define-print-function ()
  (cond (*dd-print-function*
         (if (cadr *dd-print-function*)
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (funcall (function ,(cadr *dd-print-function*))
                          instance stream *current-print-level*)))
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (write-string (%write-to-string instance) stream))))) ;; REVIEW
        (*dd-print-object*
         (if (cadr *dd-print-object*)
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (funcall (function ,(cadr *dd-print-object*))
                          instance stream)))
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (write-string (%write-to-string instance) stream))))) ;; REVIEW
        (t
         nil)))

(defun parse-1-option (option)
  (case (car option)
    (:conc-name
     (setf *dd-conc-name* (if (symbolp (cadr option))
                              (cadr option)
                              (make-symbol (string (cadr option))))))
    (:constructor
     (let* ((args (cdr option))
            (numargs (length args)))
       (case numargs
         (0 ; Use default name.
          (push (list (default-constructor-name) nil) *dd-constructors*))
         (1
          (push (list (car args) nil) *dd-constructors*))
         (2
          (push args *dd-constructors*)))))
    (:copier
     (when (length-eql option 2)
       (setq *dd-copier* (cadr option))))
    (:include
     ;; include-option::= (:include included-structure-name {slot-description}*)
     (setq *dd-include* (cdr option)))
    (:initial-offset
     (setq *dd-initial-offset* (cadr option)))
    (:predicate
     (when (length-eql option 2)
       (setq *dd-predicate* (cadr option))))
    (:print-function
     (setq *dd-print-function* option))
    (:print-object
     (setq *dd-print-object* option))
    (:type
     (setq *dd-type* (cadr option)))))

(defun parse-name-and-options (name-and-options)
  (setq *dd-name* (the symbol (car name-and-options)))
  (setq *dd-conc-name* (make-symbol (concatenate 'string (symbol-name *dd-name*) "-")))
  (setq *dd-copier* (intern (concatenate 'string "COPY-" (symbol-name *dd-name*))))
  (setq *dd-predicate* (concatenate 'string (symbol-name *dd-name*) "-P"))
  (let ((options (cdr name-and-options)))
    (dolist (option options)
      (cond ((consp option)
             (parse-1-option option))
            ((eq option :named)
             (setf *dd-named* t))
            ((memq option '(:constructor :copier :predicate :named :conc-name))
             (parse-1-option (list option)))
            (t
             (error "Unrecognized DEFSTRUCT option: ~S." option))))))

(defun compiler-defstruct (name &key
                                conc-name
                                default-constructor
                                constructors
                                copier
                                include
                                type
                                named
                                initial-offset
                                predicate
                                print-function
                                print-object
                                direct-slots
                                slots
                                type-names)
  (setf (get name 'structure-definition)
        (make-defstruct-description :name name
                                    :conc-name conc-name
                                    :default-constructor default-constructor
                                    :constructors constructors
                                    :copier copier
                                    :include include
                                    :type type
                                    :named named
                                    :initial-offset initial-offset
                                    :predicate predicate
                                    :print-function print-function
                                    :print-object print-object
                                    :direct-slots direct-slots
                                    :slots slots))
  (when (or (null type) named)
    (make-structure-class name slots (car include))
    (setf (gethash name +structure-types+) type-names))
  #-xcl
  (when default-constructor
    (proclaim `(ftype (function * t) ,default-constructor))))

(defmacro defstruct (name-and-options &rest slots)
  (let ((*dd-name* nil)
        (*dd-conc-name* nil)
        (*dd-default-constructor* nil)
        (*dd-constructors* nil)
        (*dd-copier* nil)
        (*dd-include* nil)
        (*dd-type* nil)
        (*dd-named* nil)
        (*dd-initial-offset* nil)
        (*dd-predicate* nil)
        (*dd-print-function* nil)
        (*dd-print-object* nil)
        (*dd-direct-slots* nil)
        (*dd-slots* nil)
        (*inherited-accessors-alist* nil)
        type-names)
    (parse-name-and-options (if (atom name-and-options)
                                (list name-and-options)
                                name-and-options))
    #-xcl
    (check-declaration-type *dd-name*)
    (if *dd-constructors*
        (dolist (constructor *dd-constructors*)
          (unless (cadr constructor)
            (setf *dd-default-constructor* (car constructor))
            (return)))
        (setf *dd-default-constructor* (default-constructor-name)))
    (when (stringp (car slots))
;;       (%set-documentation *dd-name* 'structure (pop slots))
      ;; FIXME
      (pop slots)
      )
    (when *dd-include*
      (let* ((include (car *dd-include*))
             (includes (list include)))
        (loop
          (let ((def (get include 'structure-definition)))
            (setq include (and def (car (dd-include def))))
            (unless include
              (return))
            (push include includes)))
        (dolist (x includes)
          (let ((def (get x 'structure-definition)))
              (dolist (slot (dd-slots def))
                (pushnew (cons (slot-reader slot) (slot-index slot))
                         *inherited-accessors-alist*
                         :test #'eq :key #'car))))))
    (dolist (slot slots)
      (let* ((name (if (atom slot) slot (car slot)))
             (reader (if *dd-conc-name*
                         (intern (concatenate 'string
                                              (symbol-name *dd-conc-name*)
                                              (symbol-name name)))
                         name))
             (initform (if (atom slot) nil (cadr slot)))
             (slot-definition (apply #'make-structure-slot-definition
                                     :name name
                                     :reader reader
                                     :initform initform
                                     (if (atom slot) nil (cddr slot)))))
        (push slot-definition *dd-direct-slots*)))
    (setf *dd-direct-slots* (nreverse *dd-direct-slots*))
    (let ((index 0))
      (when *dd-include*
        (let ((def (get (car *dd-include*) 'structure-definition)))
          (unless def
            (error 'simple-error
                   :format-control "Class ~S is undefined."
                   :format-arguments (list (car *dd-include*))))
          (dolist (slot-definition (dd-slots def))
            ;; MUST COPY SLOT DEFINITION!
            (setf slot-definition (copy-structure-slot-definition slot-definition))
            (set-slot-index slot-definition index)
            (push slot-definition *dd-slots*)
            (incf index)))
        (when (cdr *dd-include*)
          (dolist (slot (cdr *dd-include*))
            (let* ((name (if (atom slot) slot (%car slot)))
                   (initform (if (atom slot) nil (cadr slot)))
                   (slot-definition (find-slot-definition name)))
              (when slot-definition
                (set-slot-initform slot-definition initform))))))
      (when *dd-initial-offset*
        (dotimes (i *dd-initial-offset*)
          (push (make-structure-slot-definition :name nil
                                                :index index
                                                :reader nil
                                                :initform nil
                                                :type t
                                                :read-only t)
                *dd-slots*)
          (incf index)))
      (when *dd-named*
        (push (make-structure-slot-definition :name nil
                                              :index index
                                              :reader nil
                                              :initform (list 'quote *dd-name*)
                                              :type t
                                              :read-only t)
              *dd-slots*)
        (incf index))
      (dolist (slot-definition *dd-direct-slots*)
        (set-slot-index slot-definition index)
        (push slot-definition *dd-slots*)
        (incf index)))
    (setf *dd-slots* (nreverse *dd-slots*))
    (when (or (null *dd-type*) *dd-named*)
      (let ((supertypes (and *dd-include* (gethash (car *dd-include*) +structure-types+))))
        (setq type-names
              (if supertypes
                  (append (list *dd-name*) supertypes)
                  (list *dd-name*)))
        (setf (gethash *dd-name* +structure-types+) type-names)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (compiler-defstruct ',*dd-name*
                             :conc-name ',*dd-conc-name*
                             :default-constructor ',*dd-default-constructor*
                             ,@(if *dd-constructors* `(:constructors ',*dd-constructors*))
                             :copier ',*dd-copier*
                             ,@(if *dd-include* `(:include ',*dd-include*))
                             ,@(if *dd-type* `(:type ',*dd-type*))
                             ,@(if *dd-named* `(:named ,*dd-named*))
                             ,@(if *dd-initial-offset* `(:initial-offset ,*dd-initial-offset*))
                             :predicate ',*dd-predicate*
                             ,@(if *dd-print-function* `(:print-function ',*dd-print-function*))
                             ,@(if *dd-print-object* `(:print-object ',*dd-print-object*))
                             :direct-slots ',*dd-direct-slots*
                             :slots ',*dd-slots*
                             ,@(if type-names `(:type-names ',type-names))))
       ,@(define-constructors)
       ,@(define-predicate)
       ,@(define-access-functions)
       ,@(define-copier)
       ,@(define-print-function)
       ',*dd-name*)))

(defun defstruct-default-constructor (arg)
  (let ((type (cond ((symbolp arg)
                     arg)
                    ((classp arg)
                     (%class-name arg))
                    (t
                     (type-of arg)))))
    (when type
      (let ((def (get type 'structure-definition)))
        (and def (dd-default-constructor def))))))
