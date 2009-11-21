;;; clos.lisp
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

;;; Originally based on Closette.

;;; Closette Version 1.0 (February 10, 1991)
;;;
;;; Copyright (c) 1990, 1991 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;;
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;;
;;; Closette is an implementation of a subset of CLOS with a metaobject
;;; protocol as described in "The Art of The Metaobject Protocol",
;;; MIT Press, 1991.

(in-package "SYSTEM")

(defparameter *mop-working-p* nil)

(defvar *debug-clos* t)

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) ':instance))

(defun std-allocate-instance (class)
  (allocate-standard-instance (class.layout class)))

(defun slot-location (class slot-name)
  (layout-slot-location (class.layout class) slot-name))

(defconstant +the-class-standard-class+ (find-class 'standard-class))

(defconstant +the-class-funcallable-standard-class+ (find-class 'funcallable-standard-class))

(defun std-class-p (class)
  (let ((class-of-class (class-of class)))
    (or (eq class-of-class +the-class-standard-class+)
        (eq class-of-class +the-class-funcallable-standard-class+))))

(defun slot-value (object slot-name)
  (cond (;(memq (class-of (class-of object))
         ;      (list +the-class-standard-class+ +the-class-funcallable-standard-class+))
         (std-class-p (class-of object))
         (std-instance-slot-value object slot-name))
        (t
         (slot-value-using-class (class-of object) object slot-name))))

(defun (setf slot-value) (new-value object slot-name)
  (cond (;(memq (class-of (class-of object))
         ;      (list +the-class-standard-class+ +the-class-funcallable-standard-class+))
         (std-class-p (class-of object))
         (set-std-instance-slot-value object slot-name new-value))
        (t
         (setf-slot-value-using-class new-value (class-of object) object slot-name))))

(defun std-slot-boundp (instance slot-name)
  (let* ((layout (std-instance-layout instance))
         (location (layout-slot-location layout slot-name)))
    (if location
        (let ((val (if (consp location)
                       (cdr location)
                       (iref instance location))))
          (not (eq val +unbound-marker+)))
        ;; "If SLOT-MISSING is invoked and returns a value, a boolean
        ;; equivalent to its primary value is returned by SLOT-BOUNDP."
        (not (not (slot-missing (class-of instance) instance slot-name 'slot-boundp))))))

(defun slot-boundp (object slot-name)
  (if (std-class-p (class-of object))
      (std-slot-boundp object slot-name)
      (slot-boundp-using-class (class-of object) object slot-name)))

(defun std-slot-makunbound (instance slot-name)
  (let* ((layout (std-instance-layout instance))
         (location (layout-slot-location layout slot-name)))
    (cond ((consp location)
           (setf (cdr location) +unbound-marker+))
          ((fixnump location)
           (setf (iref instance location) +unbound-marker+))
          (t
           (slot-missing (class-of instance) instance slot-name 'slot-makunbound))))
  instance)

(defun slot-makunbound (object slot-name)
  (if (std-class-p (class-of object))
      (std-slot-makunbound object slot-name)
      (slot-makunbound-using-class (class-of object) object slot-name)))

(defun std-slot-exists-p (instance slot-name)
  (not (null (find slot-name (class.slots (class-of instance))
                   :key #'slot-definition-name))))

(defun slot-exists-p (object slot-name)
  (if (std-class-p (class-of object))
      (std-slot-exists-p object slot-name)
      (slot-exists-p-using-class (class-of object) object slot-name)))

(defun sub-specializer-p (spec1 spec2 class)
  (let ((cpl (class.precedence-list class)))
    (not (null (find spec2 (cdr (memq spec1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

;; (defun class-name (class) (std-slot-value class 'name))
;; (defun (setf class-name) (new-value class)
;;   (setf (slot-value class 'name) new-value))

;; (defun class-direct-superclasses (class)
;;   (slot-value class 'direct-superclasses))
;; (defun (setf class-direct-superclasses) (new-value class)
;;   (setf (slot-value class 'direct-superclasses) new-value))

(defun %class-direct-slots (class)
  (cond ((typep class 'standard-class)
         (slot-value class 'direct-slots)
;;          (class.direct-slots class)
         )
        ((typep class 'funcallable-standard-class)
         (slot-value class 'direct-slots)
;;          (class.direct-slots class)
         )
        (t
;;          (format t "%class-direct-slots wrong type ~S ~S~%" class (type-of class))
         nil)
      ))
(defun (setf %class-direct-slots) (new-value class)
  (setf (slot-value class 'direct-slots) new-value))

;; (defun class-precedence-list (class)
;;   (slot-value class 'precedence-list))
;; (defun (setf class-precedence-list) (new-value class)
;;   (setf (slot-value class 'precedence-list) new-value))

(defun %class-slots (class)
;;   (when (typep class 'standard-class)
    (slot-value class 'slots)
;;     )
  )
(defun (setf %class-slots) (new-value class)
  (setf (slot-value class 'slots) new-value))

;; (defun class-direct-subclasses (class)
;;   (slot-value class 'direct-subclasses))
;; (defun (setf class-direct-subclasses) (new-value class)
;;   (setf (slot-value class 'direct-subclasses) new-value))

;; MOP p.238 specializer-direct-methods (generic function)
(defun class-direct-methods (class)
  (slot-value class 'direct-methods))
(defun (setf class-direct-methods) (new-value class)
  (setf (slot-value class 'direct-methods) new-value))

(defun %class-direct-default-initargs (class)
  (when (typep class 'standard-class)
    (slot-value class 'direct-default-initargs)))
;; (defun (setf class-direct-default-initargs) (new-value class)
;;   (setf (slot-value class 'direct-default-initargs) new-value))

;; MOP p. 212 generic function
;; (defun class-default-initargs (class)
;;   (when (typep class 'standard-class)
;;     (slot-value class 'default-initargs)))
;; (defun (setf class-default-initargs) (new-value class)
;;   (setf (slot-value class 'default-initargs) new-value))

(defvar *preserve-layout* nil)

(defun ensure-class-preserving-layout (&rest args)
  (let ((*preserve-layout* t))
    (apply #'ensure-class args)))

(defun canonicalize-direct-superclasses (direct-superclasses)
  (let ((classes nil))
    (dolist (class-specifier direct-superclasses)
      (if (classp class-specifier)
          (push class-specifier classes)
          (let ((class (find-class class-specifier nil)))
            (unless class
              (setq class (make-instance 'forward-referenced-class))
              (setf (class.name class) class-specifier
                    (class.direct-subclasses class) nil
                    (class.direct-superclasses class) nil
                    (find-class class-specifier) class))
            (push class classes))))
    (nreverse classes)))

(defun %ensure-class-using-class (class
                                  name
                                  &rest args
                                  &key
                                  direct-superclasses
                                  (metaclass +the-class-standard-class+)
                                  &allow-other-keys)
  ;; check for duplicate slots
  (let ((slots (getf args :direct-slots)))
    (dolist (s1 slots)
      (let ((name1 (getf s1 :name)))
        (dolist (s2 (cdr (memq s1 slots)))
          (when (eq name1 (getf s2 :name))
            (error 'program-error "Duplicate slot ~S" name1))))))
  ;; check for duplicate argument names in :DEFAULT-INITARGS class option
  (let ((names nil))
    (dolist (default-initarg (getf args :direct-default-initargs))
      (push (car default-initarg) names))
    (do* ((names names (cdr names))
          (name (car names) (car names)))
         ((null names))
      (when (memq name (cdr names))
        (error 'program-error
               :format-control "Duplicate initialization argument name ~S in :DEFAULT-INITARGS."
               :format-arguments (list name)))))
  (setq direct-superclasses (canonicalize-direct-superclasses direct-superclasses))
;;   (dolist (class direct-superclasses)
;;     (when (typep class 'built-in-class)
;;       (error "Attempt to define a subclass of a built-in-class: ~S" class)))
  (cond (class
         (cond ((typep class 'built-in-class)
                (error "The symbol ~S names a built-in class." name))
               ((typep class 'forward-referenced-class)
                (let ((new-class (apply 'make-instance-standard-class
                                        +the-class-standard-class+
                                        :name name
                                        :direct-superclasses direct-superclasses
                                        args)))
                  (setf (find-class name) new-class)
                  (dolist (subclass (class.direct-subclasses class))
                    (setf (class.direct-superclasses subclass)
                          (substitute new-class class
                                      (class.direct-superclasses subclass))))
                  (setq class new-class)))
               (t
                ;; we're redefining the class
                (when (class.layout class)
                  (unless *preserve-layout*
                    (%make-instances-obsolete class)))
                (apply #'std-after-initialization-for-classes
                       class
                       :direct-superclasses direct-superclasses
                       args))))
        (t
         (let ((initargs args))
           ;; "The :METACLASS argument is not included in the initialization arguments." MOP p. 183
           (remf initargs :metaclass)
           (setq class (apply (cond ((eq metaclass +the-class-standard-class+)
                                     'make-instance-standard-class)
                                    ((eq metaclass +the-class-funcallable-standard-class+)
                                     'make-instance-funcallable-standard-class)
                                    (t
                                     'make-instance))
                              metaclass :name name :direct-superclasses direct-superclasses initargs)))
         (setf (find-class name) class)))
  class)

;; MOP p.183 generic function
(defun ensure-class-using-class (class name &rest args)
  (apply '%ensure-class-using-class class name args))

;; MOP p. 182
(defun ensure-class (name &rest args)
  (apply #'ensure-class-using-class
         (let ((class (find-class name nil)))
           (when (and class (eq name (class-name class)))
             ;; NAME is the proper name of CLASS.
             class))
         name
         args))

(defun make-instance-standard-class (metaclass
                                     &key
                                     name
                                     direct-superclasses
                                     direct-slots
                                     direct-default-initargs
                                     &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (allocate-instance-standard-class)))
    (setf (class.name class) name)
    (setf (class.direct-subclasses class) nil)
    (setf (class.direct-methods class) nil)
    (setf (class.direct-default-initargs class) direct-default-initargs)
    (std-after-initialization-for-classes class
                                          :direct-slots direct-slots
                                          :direct-superclasses direct-superclasses)
    class))

(defun make-instance-funcallable-standard-class (metaclass
                                                 &key
                                                 name
                                                 direct-superclasses
                                                 direct-slots
                                                 direct-default-initargs
                                                 &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (allocate-instance-funcallable-standard-class)))
    (setf (class.name class) name)
    (setf (class.direct-subclasses class) nil)
    (setf (class.direct-methods class) nil)
    (setf (class.direct-default-initargs class) direct-default-initargs)
    (std-after-initialization-for-classes class
                                          :direct-slots direct-slots
                                          :direct-superclasses direct-superclasses)
    class))

(defun std-after-initialization-for-classes (class
                                             &key
                                             direct-superclasses
                                             direct-slots
                                             &allow-other-keys)
  (let ((supers (or direct-superclasses
                    (list (find-class 'standard-object)))))
    (when *mop-working-p*
      (dolist (superclass supers)
        (unless (validate-superclass class superclass)
          (error "~S cannot be a superclass of ~S." superclass class))))
    (setf (class.direct-superclasses class) supers)
    (dolist (superclass supers)
      (push class (class.direct-subclasses superclass))))
  (let ((slots
          (mapcar #'(lambda (slot-properties)
                      (apply #'make-direct-slot-definition
                             :class class
                             slot-properties))
                    direct-slots)))
    (setf (class.direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (slot-definition-readers direct-slot))
        (add-reader-method class reader (slot-definition-name direct-slot)))
      (dolist (writer (slot-definition-writers direct-slot))
        (add-writer-method class writer (slot-definition-name direct-slot)))))
  (if (memq (class-of class) (list +the-class-standard-class+ +the-class-funcallable-standard-class+))
      (std-finalize-inheritance class)
      (finalize-inheritance class))
  t)

;;; Slot definition metaobjects

;;; N.B. Quietly retain all unknown slot options (rather than signaling an
;;; error), so that it's easy to add new ones.

(defun make-direct-slot-definition (;;&rest properties
                                    &key name
                                          (initargs nil)
                                          (initform nil)
                                          (initfunction nil)
                                          (readers nil)
                                          (writers nil)
                                          (class nil)
                                          (allocation :instance)
                                    &allow-other-keys)
  (let ((slot (allocate-standard-instance
               (class.layout (find-class-1 'standard-direct-slot-definition)))))
    (setf (slot-definition.name slot) name)
    (setf (slot-definition.initargs slot) initargs)
    (setf (slot-definition.initform slot) initform)
    (setf (slot-definition.initfunction slot) initfunction)
    (setf (slot-definition.allocation slot) allocation)
    (setf (slot-definition.type slot) t) ; FIXME
    (setf (slot-definition.class slot) class)
    (setf (slot-definition.documentation slot) nil) ; FIXME
    (setf (direct-slot-definition.readers slot) readers)
    (setf (direct-slot-definition.writers slot) writers)
    slot))

(defun make-effective-slot-definition (;;&rest properties
                                       &key name
                                            (initargs nil)
                                            (initform nil)
                                            (initfunction nil)
                                            (class nil)
                                            (allocation :instance)
                                            (allocation-class nil)
                                       &allow-other-keys)
  (let ((slot (allocate-standard-instance
               (class.layout (find-class-1 'standard-effective-slot-definition)))))
    (setf (slot-definition.name slot) name)
    (setf (slot-definition.initargs slot) initargs)
    (setf (slot-definition.initform slot) initform)
    (setf (slot-definition.initfunction slot) initfunction)
    (setf (slot-definition.allocation slot) allocation)
    (setf (slot-definition.type slot) t) ; FIXME
    (setf (slot-definition.class slot) class)
    (setf (slot-definition.documentation slot) nil) ; FIXME
    (setf (effective-slot-definition.location slot) nil)
    (setf (effective-slot-definition.allocation-class slot) allocation-class)
    slot))

;; MOP p. 222
(defun slot-definition-name (slot)
  (slot-definition.name slot))

;; MOP p. 221
(defun slot-definition-initargs (slot)
  (slot-definition.initargs slot))

;; MOP p. 221
(defun slot-definition-initform (slot)
  (slot-definition.initform slot))

;; MOP p. 222
(defun slot-definition-initfunction (slot)
  (slot-definition.initfunction slot))

;; not in MOP
(defun slot-definition-class (slot)
  (slot-definition.class slot))

(defun slot-definition-readers (slot)
  (direct-slot-definition.readers slot))

(defun slot-definition-writers (slot)
  (direct-slot-definition.writers slot))

;; MOP p. 221
(defun slot-definition-allocation (slot)
  (slot-definition.allocation slot))

(defun slot-definition-allocation-class (slot)
  (effective-slot-definition.allocation-class slot))

(defun slot-definition-location (slot)
  (aver (typep slot 'effective-slot-definition))
  (effective-slot-definition.location slot))

;;; finalize-inheritance

(defknown update-class (t t) t)
(defun update-class (class finalize-p)
  (when (or finalize-p (class.finalized-p class))
    (let ((cpl (if (memq (class-of class) (list +the-class-standard-class+ +the-class-funcallable-standard-class+))
                   (std-compute-class-precedence-list class)
                   (compute-class-precedence-list class))))
      (if *preserve-layout*
          (unless (equal cpl (class.precedence-list class))
            (format t "std-finalize-inheritance: cpl mismatch~%"))
          (setf (class.precedence-list class) cpl)))

    (dolist (class (class.precedence-list class))
      (when (typep class 'forward-referenced-class)
        (return-from update-class)))
    (let ((class-slots
           (if (memq (class-of class)
                     (list +the-class-standard-class+ +the-class-funcallable-standard-class+))
               (std-compute-slots class)
               (compute-slots class))))
      (when *debug-clos*
        (when *preserve-layout*
          (when (class.layout class)
            (let ((layout-slot-names (layout-slot-names (class.layout class)))
                  (class-slot-names (mapcar #'slot-definition-name class-slots)))
              (unless (equal layout-slot-names class-slot-names)
                (format t "class = ~S~%" class)
                (format t "  layout slot names = ~S~%" layout-slot-names)
                (format t "  class slot names  = ~S~%" class-slot-names)
                (let ((diff
                       (union (set-difference layout-slot-names class-slot-names)
                              (set-difference class-slot-names layout-slot-names))))
                  (format t "  diff = ~S~%" diff)))))))
      (setf (class.slots class) class-slots))

    (setf (class.default-initargs class)
          (if (eq (class-of class) +the-class-standard-class+)
              (std-compute-default-initargs class)
              (compute-default-initargs class)))

    (let ((old-layout (class.layout class))
          (length 0)
          (instance-slots nil)
          (shared-slots nil))
      (dolist (slot (class.slots class))
        (aver (typep slot 'effective-slot-definition))
        (case (slot-definition-allocation slot)
          (:instance
           (setf (effective-slot-definition.location slot) length)
           (incf length)
           (push (slot-definition.name slot) instance-slots))
          (:class
           (unless (effective-slot-definition.location slot)
             (let ((allocation-class (effective-slot-definition.allocation-class slot)))
               (setf (effective-slot-definition.location slot)
                     (if (eq allocation-class class)
                         (cons (slot-definition.name slot) +unbound-marker+)
                         (slot-location allocation-class (slot-definition.name slot))))))
           (push (effective-slot-definition.location slot) shared-slots))))
      (when old-layout
        ;; redefined class
        (%make-instances-obsolete class)
        (dolist (location shared-slots)
          (let* ((slot-name (car location))
                 (old-location (layout-slot-location old-layout slot-name)))
            (cond ((and old-location (consp old-location))
                   ;; "The value of a slot that is specified as shared both in
                   ;; the old class and in the new class is retained. If such a
                   ;; shared slot was unbound in the old class, it is unbound in
                   ;; the new class."
                   (aver (eq (car old-location) slot-name))
                   (setf (cdr location) (cdr old-location)))
                  (t
                   ;; "Slots that were local in the old class and that are shared
                   ;; in the new class are initialized. Newly added shared slots
                   ;; are initialized."
                   (let* ((slot-definition (find slot-name (%class-slots class) :key #'slot-definition-name))
                          (initfunction (slot-definition-initfunction slot-definition)))
                     (when initfunction
                       (setf (cdr location) (funcall initfunction))))))
            )))

      (unless (and (class.layout class) *preserve-layout*)
        (setf (class.layout class)
              (make-layout class (nreverse instance-slots) (nreverse shared-slots)))))
    (setf (class.finalized-p class) t))

  (dolist (sub (class.direct-subclasses class))
      (update-class sub nil))
  )

(defun std-finalize-inheritance (class)
  (update-class class t))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                        (mapappend #'local-precedence-ordering
                                   classes-to-order))
                      #'std-tie-breaker-rule)))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result nil))
    (loop
     (let ((minimal-elements
            (remove-if
             #'(lambda (class)
                 (member class remaining-constraints
                         :key #'cadr))
             remaining-elements)))
       (when (null minimal-elements)
             (if (null remaining-elements)
                 (return-from topological-sort result)
               (error "Inconsistent precedence graph.")))
       (let ((choice (if (null (cdr minimal-elements))
                         (car minimal-elements)
                       (funcall tie-breaker
                                minimal-elements
                                result))))
         (setq result (append result (list choice)))
         (setq remaining-elements
               (remove choice remaining-elements))
         (setq remaining-constraints
               (remove choice
                       remaining-constraints
                       :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (class.direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
              (let ((to-be-processed
                       (set-difference superclasses seen)))
                (if (null to-be-processed)
                    superclasses
                    (let ((class-to-process
                             (car to-be-processed)))
                      (all-superclasses-loop
                        (cons class-to-process seen)
                        (union (class.direct-superclasses
                                 class-to-process)
                               superclasses)))))))
    (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (class.direct-superclasses class)))
          (class.direct-superclasses class)))

;;; Slot inheritance

#+nil
(defun std-compute-slots (class)
;;   (format t "std-compute-slots ~S~%" class)
  (let* ((all-slots (mapappend #'%class-direct-slots
;;                                (if *preserve-layout*
                                   (reverse (class.precedence-list class))
;;                                    (class.precedence-list class))
                               ))
         (all-names (remove-duplicates (mapcar #'slot-definition-name all-slots))))
;;     (format t "all-names = ~S~%" all-names)
    (mapcar #'(lambda (name)
;;                (funcall (if (eq (class-of class) +the-class-standard-class+)
                (funcall (if (memq (class-of class)
                                   (list +the-class-standard-class+ +the-class-funcallable-standard-class+))
                             #'std-compute-effective-slot-definition
                             #'compute-effective-slot-definition)
                         class
                         name
;;                          (remove name all-slots
;;                                  :key #'slot-definition-name
;;                                  :test-not #'eq)
                         all-slots
                         ))
            all-names)))

;; Adapted from SBCL.
(defun std-compute-slots (class)
  ;; As specified, we must call COMPUTE-EFFECTIVE-SLOT-DEFINITION once
  ;; for each different slot name we find in our superclasses. Each
  ;; call receives the class and a list of the dslotds with that name.
  ;; The list is in most-specific-first order.
  (let ((name-dslotds-alist nil))
    (dolist (c (reverse (class.precedence-list class)))
      (dolist (slot (%class-direct-slots c))
        (let* ((name (slot-definition.name slot))
               (entry (assq name name-dslotds-alist)))
          (if entry
              (push slot (cdr entry))
              (push (list name slot) name-dslotds-alist)))))
    (mapcar (lambda (direct)
              (std-compute-effective-slot-definition class ; FIXME compute-effective-slot-definition
                                                     (car direct)
                                                     (cdr direct)))
            (nreverse name-dslotds-alist))))


(defun std-compute-effective-slot-definition (class name direct-slots)
  (declare (ignore class))
;;   (format t "std-compute-effective-slot-definition ~S~%" class)
  (setq direct-slots
        (remove name direct-slots
                :key #'slot-definition-name
                :test-not #'eq))
  (let ((initer (find-if-not #'null direct-slots
                             :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
     :name (slot-definition-name (car direct-slots))
     :initform (if initer
                   (slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (slot-definition-initfunction initer)
                       nil)
     :initargs (remove-duplicates
                (mapappend #'slot-definition-initargs
                           direct-slots))
     :allocation (slot-definition-allocation (car direct-slots))
     :allocation-class (slot-definition-class (car direct-slots))
     )))

(defun std-compute-default-initargs (class)
  (let ((default-initargs (mapappend #'%class-direct-default-initargs (class.precedence-list class))))
    (when default-initargs
      (let ((names nil)
            (result nil))
        (dolist (initarg default-initargs (nreverse result))
          (let ((name (car initarg)))
            (unless (member name names)
              (push initarg result)
              (push name names))))))))

;;; EQL specializers

(setf (class.layout (find-class 'eql-specializer))
      (make-layout (find-class 'eql-specializer)
                   '(direct-methods object)
                   nil))

(defmacro eql-specializer.direct-methods (arg) `(iref ,arg 0))
(defmacro eql-specializer.object         (arg) `(iref ,arg 1))

;; MOP p. 188
(defun eql-specializer-object (arg)
  (require-type arg (find-class 'eql-specializer))
  (eql-specializer.object arg))

(defun eql-specializer-p (arg)
  (typep arg (find-class 'eql-specializer)))

(defparameter *eql-specializer-table* (make-hash-table :test 'eql))
(declaim (type hash-table *eql-specializer-table*))

;; MOP p. 206
(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
            (let ((instance (std-allocate-instance (find-class 'eql-specializer))))
              (setf (eql-specializer.object instance) object)
              instance))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defconstant +the-class-standard-generic-function+ (find-class 'standard-generic-function))

(defconstant +standard-generic-function-layout+
  (make-layout +the-class-standard-generic-function+
               '(name
                 lambda-list
                 declarations
                 %documentation
                 methods
                 method-class
                 %method-combination
                 argument-precedence-order
                 initial-methods
                 classes-to-emf-table
                 minargs)
               nil))

(setf (class.layout +the-class-standard-generic-function+) +standard-generic-function-layout+)

(defmacro generic-function.name                      (arg) `(iref ,arg  0))
(defmacro generic-function.lambda-list               (arg) `(iref ,arg  1))
(defmacro generic-function.declarations              (arg) `(iref ,arg  2))
(defmacro generic-function.documentation             (arg) `(iref ,arg  3))
(defmacro generic-function.methods                   (arg) `(iref ,arg  4))
(defmacro generic-function.method-class              (arg) `(iref ,arg  5))
(defmacro generic-function.method-combination        (arg) `(iref ,arg  6))
(defmacro generic-function.argument-precedence-order (arg) `(iref ,arg  7))
(defmacro generic-function.initial-methods           (arg) `(iref ,arg  8))
(defmacro generic-function.classes-to-emf-table      (arg) `(iref ,arg  9))
(defmacro generic-function.minargs                   (arg) `(iref ,arg 10))

(defun generic-function-name (gf)
  (slot-value gf 'name))
(defun (setf generic-function-name) (new-value gf)
  (setf (slot-value gf 'name) new-value))

(defun generic-function-lambda-list (gf)
  (slot-value gf 'lambda-list))
(defun (setf generic-function-lambda-list) (new-value gf)
  (setf (slot-value gf 'lambda-list) new-value))

(defun generic-function-methods (gf)
  (slot-value gf 'methods))
(defun (setf generic-function-methods) (new-value gf)
  (setf (slot-value gf 'methods) new-value))

(defun generic-function-method-class (gf)
  (slot-value gf 'method-class))
(defun (setf generic-function-method-class) (new-value gf)
  (setf (slot-value gf 'method-class) new-value))

;; MOP p. 217 generic function
;; should return a method combination metaobject
(defun generic-function-method-combination (gf)
  (generic-function.method-combination gf))

(defun generic-function-initial-methods (gf)
  (slot-value gf 'initial-methods))
(defun (setf generic-function-initial-methods) (new-value gf)
  (setf (slot-value gf 'initial-methods) new-value))

;;; Internal accessor for effective method function table

(defknown classes-to-emf-table (t) hash-table)
(defun classes-to-emf-table (gf)
  (slot-value gf 'classes-to-emf-table))
;; (defun (setf classes-to-emf-table) (new-value gf)
;;   (setf (slot-value gf 'classes-to-emf-table) new-value))

;;;
;;; Method metaobjects and standard-method
;;;

(defconstant +the-class-standard-method+ (find-class 'standard-method))

(defconstant +standard-method-layout+
  (make-layout +the-class-standard-method+
               '(lambda-list
                 qualifiers
                 specializers
                 %generic-function
                 %function
                 fast-function)
               nil))

(setf (class.layout +the-class-standard-method+) +standard-method-layout+)

(defun method-lambda-list (method) (slot-value method 'lambda-list))
(defun (setf method-lambda-list) (new-value method)
  (setf (slot-value method 'lambda-list) new-value))

(defun method-qualifiers (method) (slot-value method 'qualifiers))
(defun (setf method-qualifiers) (new-value method)
  (setf (slot-value method 'qualifiers) new-value))

;; MOP p.219 generic function
(defun method-specializers (method) (slot-value method 'specializers))
(defun (setf method-specializers) (new-value method)
  (setf (slot-value method 'specializers) new-value))

(defun method-generic-function (method)
  (slot-value method '%generic-function))
(defun (setf method-generic-function) (new-value method)
  (setf (slot-value method '%generic-function) new-value))

(defun method-function (method)
  (slot-value method '%function))
(defun (setf method-function) (new-value method)
  (setf (slot-value method '%function) new-value))

(defun method-fast-function (method)
  (slot-value method 'fast-function))
(defun (setf method-fast-function) (new-value method)
  (setf (slot-value method 'fast-function) new-value))

(defun make-instance-standard-method-combination ()
  (let* ((class (find-class 'standard-method-combination))
         (instance (allocate-standard-instance (or (class.layout class)
                                                   (make-layout class nil nil)))))
    instance))

(defparameter *standard-method-combination*
  (setf (get 'standard 'method-combination-object) (make-instance-standard-method-combination)))

;; (defun canonicalize-method-combination (thing)
;;   (cond ((eq thing 'standard)
;;          *standard-method-combination*)
;;         ((consp thing)
;;          (let* ((type-name (%car thing))
;;                 (options (%cdr thing))
;;                 (mc (get type-name 'method-combination-object)))
;;            (cond (mc
;;                   (method-combination-with-options mc options))
;;                  (t
;;                   (error "Unsupported method combination type ~A." type-name)))))))

(defun canonicalize-method-combination (thing)
  (cond ((eq thing 'standard)
         '*standard-method-combination*)
        ((consp thing)
         (let* ((type-name (%car thing))
                (options (%cdr thing)))
           `(%find-method-combination ',type-name ',options)))))

(defun canonicalize-defgeneric-options (options)
  (mapappend #'canonicalize-defgeneric-option options))

(defun canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class
     (list ':generic-function-class
           `(find-class ',(cadr option))))
    (:method-class
     (list ':method-class
           `(find-class ',(cadr option))))
    (:method-combination
     (list :method-combination (canonicalize-method-combination (cdr option))))
    (:argument-precedence-order
     (list :argument-precedence-order `',(cdr option)))
    (t
     (list `',(car option) `',(cadr option)))))

;; "The :METHOD-COMBINATION option is followed by a symbol that names a type of
;; method combination. The arguments (if any) that follow that symbol depend on
;; the type of method combination. Note that the standard method combination
;; type does not support any arguments. However, all types of method
;; combination defined by the short form of define-method-combination accept an
;; optional argument named order, defaulting to :MOST-SPECIFIC-FIRST, where a
;; value of :MOST-SPECIFIC-LAST reverses the order of the primary methods
;; without affecting the order of the auxiliary methods."
(defmacro defgeneric (function-name lambda-list &rest options-and-method-descriptions)
  (let ((options nil)
        (methods nil)
        (documentation nil))
    (dolist (item options-and-method-descriptions)
      (case (car item)
        (declare) ; FIXME
        (:documentation
         (when documentation
           (error 'program-error
                  :format-control "Documentation option was specified twice for generic function ~S."
                  :format-arguments (list function-name)))
         (setf documentation t)
         (push item options))
        (:method
         (push
          `(push (defmethod ,function-name ,@(cdr item))
                 (generic-function-initial-methods (fdefinition ',function-name)))
          methods))
        (t
         (push item options))))
    (setq options (nreverse options)
          methods (nreverse methods))
    `(prog1
       (ensure-generic-function
        ',function-name
        :lambda-list ',lambda-list
        ,@(canonicalize-defgeneric-options options))
       ,@methods)))

;; Adapted from OpenMCL.
(defun canonicalize-argument-precedence-order (apo req)
  (declare (type list apo req))
  (cond ((equal apo req)
         nil)
        ((not (eql (length apo) (length req)))
         (error 'program-error
                :format-control "Specified argument precedence order ~S does not match lambda list."
                :format-arguments (list apo)))
        (t (let ((res nil))
             (dolist (arg apo (nreverse res))
               (let ((index (position arg req)))
                 (if (or (null index) (memq index res))
                     (error 'program-error
                            :format-control "Specified argument precedence order ~S does not match lambda list."
                            :format-arguments (list apo)))
                 (push index res)))))))

(defun find-generic-function (name &optional (errorp t))
  (let ((function (and (fboundp name) (fdefinition name))))
    (when function
      (when (typep function 'generic-function)
        (return-from find-generic-function function))
;;       (when (and *traced-names* (find name *traced-names* :test #'equal))
;;         (setf function (untraced-function name))
;;         (when (typep function 'generic-function)
;;           (return-from find-generic-function function)))
      ))
  (if errorp
      (error "There is no generic function named ~S." name)
      nil))

(defun lambda-lists-congruent-p (lambda-list1 lambda-list2)
  (let* ((plist1 (analyze-lambda-list lambda-list1))
         (args1 (getf plist1 :required-args))
         (plist2 (analyze-lambda-list lambda-list2))
         (args2 (getf plist2 :required-args)))
    (eql (length args1) (length args2))))

;; CL
;; "The keyword arguments correspond to the option arguments of defgeneric,
;; except that the :METHOD-CLASS and :GENERIC-FUNCTION-CLASS arguments can be
;; class objects as well as names."
(defun ensure-generic-function (function-name
                                &rest all-keys
                                &key
                                lambda-list
                                (generic-function-class +the-class-standard-generic-function+)
                                (method-class +the-class-standard-method+)
                                (method-combination *standard-method-combination*)
                                (argument-precedence-order nil apo-p)
                                &allow-other-keys)
  (let ((gf (find-generic-function function-name nil)))
    (cond (gf
           (unless (or (null (generic-function-methods gf))
                       (lambda-lists-congruent-p lambda-list (generic-function-lambda-list gf)))
               (error 'simple-error
                      :format-control
                      "The lambda list ~S is incompatible with the existing methods of ~S."
                      :format-arguments
                      (list lambda-list gf)))
           (let* ((plist (analyze-lambda-list lambda-list))
                  (required-args (getf plist ':required-args)))
             (when apo-p
               (setf (generic-function.argument-precedence-order gf)
                     (if argument-precedence-order
                         (canonicalize-argument-precedence-order argument-precedence-order
                                                                 required-args)
                         nil))))
           (finalize-generic-function gf)
           gf)
          (t
           (when (fboundp function-name)
             (when *mop-working-p*
               (error 'program-error
                      :format-control "~A already names an ordinary function, macro, or special operator."
                      :format-arguments (list function-name))))
           (setq gf (apply (if (eq generic-function-class +the-class-standard-generic-function+)
                               #'make-instance-standard-generic-function
                               #'make-instance)
                           generic-function-class
                           :name function-name
                           :method-class method-class
                           :method-combination method-combination
                           all-keys))
           gf))))

(defun make-instance-standard-generic-function (generic-function-class
                                                &key
                                                name
                                                lambda-list
                                                method-class
                                                method-combination
                                                argument-precedence-order
                                                documentation)
  (declare (ignore generic-function-class)) ; REVIEW
  (declare (type method-combination method-combination))
  (let ((gf (allocate-instance-standard-generic-function +standard-generic-function-layout+)))
    (setf (generic-function.name gf) name)
    (setf (generic-function.lambda-list gf) lambda-list)
    (setf (generic-function.methods gf) nil)
    (setf (generic-function.method-class gf) method-class)
    (setf (generic-function.method-combination gf) method-combination)
    (setf (generic-function.initial-methods gf) nil)
    (setf (generic-function.classes-to-emf-table gf) (make-hash-table :test #'equal))
    (let* ((plist (analyze-lambda-list lambda-list))
           (required-args (getf plist ':required-args)))
      (setf (generic-function.argument-precedence-order gf)
            (if argument-precedence-order
                (canonicalize-argument-precedence-order argument-precedence-order
                                                        required-args)
                nil))
      (setf (generic-function.minargs gf) (length required-args)))
    (setf (generic-function.documentation gf) documentation)
    (finalize-generic-function gf)
    gf))

(defun finalize-generic-function (gf)
  (set-funcallable-instance-function
   gf
   (if (eq (class-of gf) +the-class-standard-generic-function+)
       (std-compute-discriminating-function gf)
       (compute-discriminating-function gf)))
  (setf (fdefinition (generic-function-name gf)) gf)
  (clrhash (classes-to-emf-table gf)))

(defun check-method-lambda-list (method-lambda-list generic-function)
  (let* ((gf-lambda-list (generic-function-lambda-list generic-function))
         (gf-restp (not (null (memq '&rest gf-lambda-list))))
         (gf-plist (analyze-lambda-list gf-lambda-list))
         (gf-keysp (getf gf-plist :keysp))
         (gf-keywords (getf gf-plist :keywords))
         (method-plist (analyze-lambda-list method-lambda-list))
         (method-restp (not (null (memq '&rest method-lambda-list))))
         (method-keysp (getf method-plist :keysp))
         (method-keywords (getf method-plist :keywords))
         (method-allow-other-keys-p (getf method-plist :allow-other-keys)))
    (unless (eql (length (getf gf-plist :required-args))
                 (length (getf method-plist :required-args)))
      (error "The method has the wrong number of required arguments for the generic function ~S."
             (generic-function-name generic-function)))
    (unless (eql (length (getf gf-plist :optional-args))
                 (length (getf method-plist :optional-args)))
      (error "The method has the wrong number of optional arguments for the generic function ~S."
             (generic-function-name generic-function)))
    (unless (eq (or gf-restp gf-keysp) (or method-restp method-keysp))
      (error "The method and the generic function ~S differ in whether they accept &REST or &KEY arguments."
             (generic-function-name generic-function)))
    (when (consp gf-keywords)
      (unless (or (and method-restp (not method-keysp))
                  method-allow-other-keys-p
                  (every (lambda (k) (memq k method-keywords)) gf-keywords))
        (error "The method does not accept all of the keyword arguments defined for the generic function ~S."
               (generic-function-name generic-function))))))

(defun ensure-method (name &rest all-keys)
  (let ((gf (find-generic-function name nil))
        (method-lambda-list (getf all-keys :lambda-list)))
    (if gf
        (check-method-lambda-list method-lambda-list gf)
        (setq gf (ensure-generic-function name :lambda-list method-lambda-list)))
    (let* ((method-class (generic-function-method-class gf))
           (new-method (apply
                        (if (eq method-class +the-class-standard-method+)
                            #'make-instance-standard-method
                            #'make-instance)
                        method-class
                        all-keys)))
      (%add-method gf new-method)
      new-method)))

(defun canonicalize-specializers (specializers)
  (mapcar #'canonicalize-specializer specializers))

(defun canonicalize-specializer (specializer)
  (cond ((classp specializer)
         specializer)
        ((eql-specializer-p specializer)
         specializer)
        ((symbolp specializer)
         (find-class specializer))
        ((and (consp specializer)
              (eq (car specializer) 'eql))
         (let ((object (cadr specializer)))
           (when (and (consp object)
                      (eq (car object) 'quote))
             (setf object (cadr object)))
           (intern-eql-specializer object)))
        (t
         (error "Unknown specializer: ~S" specializer))))

(defun gf-required-arglist (gf)
  (let ((plist (analyze-lambda-list (generic-function-lambda-list gf))))
    (getf plist :required-args)))

(defun required-classes (gf args)
  (declare (optimize speed (safety 0)))
  (let ((numargs (length args))
        (minargs (generic-function.minargs gf)))
    (unless minargs
      (setq minargs (length (gf-required-arglist gf)))
      (setf (generic-function.minargs gf) minargs))
    (when (< numargs minargs)
      (error 'program-error
             :format-control "Wrong number of arguments for generic function ~S."
             :format-arguments (list (generic-function-name gf))))
    (let (result)
      (dotimes (i minargs (nreverse result))
        (push (class-of (%car args)) result)
        (setq args (%cdr args))))))

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (keysp (getf plist :keysp))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds
      ,@(if rv `(&rest ,rv) nil)
      ,@(if (or ks keysp aok) `(&key ,@ks) nil)
      ,@(if aok '(&allow-other-keys) nil)
      ,@(if opts `(&optional ,@opts) nil)
      ,@(if auxs `(&aux ,@auxs) nil))))

(defun extract-specializers (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

(defun get-keyword-from-arg (arg)
  (if (listp arg)
      (if (listp (car arg))
          (caar arg)
          (make-keyword (car arg)))
      (make-keyword arg)))

(defmacro push-on-end (item place)
  `(setq ,place (nconc ,place (list ,item))))

(defun analyze-lambda-list (lambda-list)
  (let ((keys nil)            ; just the keywords
        (key-args nil)        ; keyword argument specs
        (keysp nil)
        (required-names nil)  ; just the variable names
        (required-args nil)   ; variable names and specializers
        (specializers nil)    ; just the specializers
        (rest-var nil)
        (optionals nil)
        (auxs nil)
        (allow-other-keys nil)
        (state :parsing-required))
    (dolist (arg lambda-list)
      (if (member arg lambda-list-keywords)
          (ecase arg
            (&optional
             (setq state :parsing-optional))
            (&rest
             (setq state :parsing-rest))
            (&key
             (setq keysp t)
             (setq state :parsing-key))
            (&allow-other-keys
             (setq allow-other-keys 't))
            (&aux
             (setq state :parsing-aux)))
          (case state
            (:parsing-required
             (push-on-end arg required-args)
             (if (listp arg)
                 (progn (push-on-end (car arg) required-names)
                   (push-on-end (cadr arg) specializers))
                 (progn (push-on-end arg required-names)
                   (push-on-end 't specializers))))
            (:parsing-optional (push-on-end arg optionals))
            (:parsing-rest (setq rest-var arg))
            (:parsing-key
             (push-on-end (get-keyword-from-arg arg) keys)
             (push-on-end arg key-args))
            (:parsing-aux
             (push-on-end arg auxs)))))
    (list :required-names required-names
          :required-args required-args
          :specializers specializers
          :rest-var rest-var
          :keywords keys
          :key-args key-args
          :keysp keysp
          :auxiliary-args auxs
          :optional-args optionals
          :allow-other-keys allow-other-keys)))

(defun make-instance-standard-method (method-class
                                      &key
                                      lambda-list
                                      qualifiers
                                      specializers
                                      function
                                      fast-function
;;                                       body
;;                                       environment
                                      )
  (declare (ignore method-class)) ; REVIEW
  (aver (not (null function)))
  (let ((method (std-allocate-instance +the-class-standard-method+)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (method-specializers method) (canonicalize-specializers specializers))
;;     (setf (method-body method) body)
;;     (setf (method-environment method) environment)
    (setf (method-generic-function method) nil)

;;     (setf (method-function method) (std-compute-method-function method))
;;     (setf (method-function method) (or function
;;                                        (std-compute-method-function method)))

    ;; REVIEW
    (let ((compiled-function nil))
      (unless (autoloadp 'compile)
        (multiple-value-bind (lambda-expression environment)
            (function-lambda-expression function)
          (declare (ignore lambda-expression))
          (unless environment
            (setq compiled-function (autocompile function)))))
      (setf (method-function method) (or compiled-function function)))

    (let ((compiled-fast-function nil))
      (when fast-function
        (unless (autoloadp 'compile)
          (multiple-value-bind (lambda-expression environment)
              (function-lambda-expression fast-function)
            (declare (ignore lambda-expression))
            (unless environment
              (setq compiled-fast-function (autocompile fast-function))))))
      (setf (method-fast-function method) (or compiled-fast-function fast-function)))

    method))

(defun %add-method (gf method)
  (when (method-generic-function method)
    (error 'simple-error
           :format-control "ADD-METHOD: ~S is a method of ~S."
           :format-arguments (list method (method-generic-function method))))
  (let ((old-method
           (find-method gf (method-qualifiers method)
                        (method-specializers method) nil)))
    (when old-method (remove-method gf old-method)))
  (setf (method-generic-function method) gf)
  (push method (generic-function-methods gf))
  (dolist (specializer (method-specializers method))
    (when (classp specializer) ; FIXME eql specializers
      (pushnew method (class-direct-methods specializer))))
  (finalize-generic-function gf)
  gf)

;; CL generic function
(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf)))
  (setf (method-generic-function method) nil)
  (dolist (specializer (method-specializers method))
    (when (classp specializer)
      (setf (class-direct-methods specializer)
            (remove method (class-direct-methods specializer)))))
  (finalize-generic-function gf)
  gf)

;; CL generic function
(defun find-method (gf qualifiers specializers &optional (errorp t))
  ;; "If the specializers argument does not correspond in length to the number
  ;; of required arguments of the generic-function, an an error of type ERROR
  ;; is signaled."
  (unless (eql (length specializers) (length (gf-required-arglist gf)))
    (error "The specializers argument has length ~D, but ~S has ~D required parameter~:P."
           (length specializers)
           gf
           (length (gf-required-arglist gf))))
  (let* ((canonical-specializers (canonicalize-specializers specializers))
         (method
          (find-if #'(lambda (method)
                      (and (equal qualifiers
                                  (method-qualifiers method))
                           (equal canonical-specializers
                                  (method-specializers method))))
                   (generic-function-methods gf))))
    (if (and (null method) errorp)
        (error "No such method for ~S." (generic-function-name gf))
        method)))

(defconstant +gf-args-var+ (make-symbol "GF-ARGS-VAR"))

;;; Reader and writer methods

(defun add-reader-method (class function-name slot-name)
  (let* ((lambda-list '(object))
         (operator (if (std-class-p class) 'std-instance-slot-value 'slot-value))
         (method-function `(lambda (,+gf-args-var+ next-methods)
                             (declare (ignore next-methods))
                             (,operator (car ,+gf-args-var+) ',slot-name)))
         (method-fast-function (compute-method-fast-function
                                `(lambda ,lambda-list (,operator ,(car lambda-list) ',slot-name))
                                (list class))))
    (ensure-method function-name
                   :lambda-list lambda-list
                   :qualifiers nil
                   :specializers (list class)
                   :function (coerce-to-function method-function)
                   :fast-function (coerce-to-function method-fast-function))))

(defun add-writer-method (class function-name slot-name)
  (let* ((lambda-list '(new-value object))
         (method-function `(lambda (,+gf-args-var+ next-methods)
                             (declare (ignore next-methods))
                             (setf (slot-value (cadr ,+gf-args-var+) ',slot-name) (car ,+gf-args-var+)))))
    (ensure-method function-name
                   :lambda-list lambda-list
                   :qualifiers nil
                   :specializers (list (find-class 't) class)
                   :function (coerce-to-function method-function))))

;;;
;;; Generic function invocation
;;;

;;; compute-discriminating-function

(defun methods-contain-eql-specializer-p (methods)
  (dolist (method methods nil)
    (when (dolist (spec (method-specializers method) nil)
            (when (eql-specializer-p spec) (return t)))
      (return t))))

(defun std-compute-discriminating-function (gf)
  (when (and (eq (generic-function.method-combination gf) 'standard)
             (eql (length (generic-function.methods gf)) 1)
             (not (methods-contain-eql-specializer-p (generic-function.methods gf)))
             (eql (generic-function.minargs gf) 1))
    (let* ((method (%car (generic-function.methods gf)))
           (fast-function (method-fast-function method)))
      (when fast-function
        (let* (;(class (car (method-specializers method)))
;;                (lambda-expression
;;                 `(lambda (arg)
;;                    (declare (optimize speed))
;;                    (require-type arg ,class) ; FIXME no-applicable-method
;;                    (funcall ,fast-function arg))
;;                 )
               (dfun
;;                 (coerce-to-function lambda-expression)
                fast-function
                )
               )
          (unless (compiled-function-p dfun)
            (when (fboundp 'compile)
              (unless (autoloadp 'compile)
                (setq dfun (or (autocompile dfun) dfun)))))
          (return-from std-compute-discriminating-function dfun)))))

  (let* ((lambda-expression
          (cond ((methods-contain-eql-specializer-p (generic-function-methods gf))
                 `(lambda (&rest ,+gf-args-var+)
                    (slow-method-lookup ,gf ,+gf-args-var+ nil)))
                (t
                 `(lambda (&rest ,+gf-args-var+)
                    (let* ((classes (required-classes ,gf ,+gf-args-var+))
                           (emfun (gethash2-1 classes (classes-to-emf-table ,gf))))
                      (if emfun
                          (funcall emfun ,+gf-args-var+)
                          (slow-method-lookup ,gf ,+gf-args-var+ classes)))))))
         (dfun (coerce-to-function lambda-expression)))
    (when (fboundp 'compile)
      (unless (autoloadp 'compile)
        (setq dfun (or (autocompile dfun) dfun))))
    dfun))

(defconstant +the-class-standard-method-combination+ (find-class 'standard-method-combination))

(defun slow-method-lookup (gf args classes)
  (let ((applicable-methods (compute-applicable-methods gf args)))
    (unless applicable-methods
      (apply #'no-applicable-method gf args))
    (let* ((mc (generic-function-method-combination gf))
           (effective-method
            (if (and (eq (class-of gf) +the-class-standard-generic-function+)
                     (eq (class-of mc) +the-class-standard-method-combination+))
                (compute-standard-effective-method gf applicable-methods)
                (compute-effective-method gf mc applicable-methods)))
           (form (list 'LAMBDA (list +gf-args-var+) (precompile-form effective-method)))
           (emfun (coerce-to-function form)))
      ;; REVIEW
      (setq emfun (or (autocompile emfun) emfun))
      (when classes
        (setf (gethash classes (classes-to-emf-table gf)) emfun))
      (funcall emfun args))))

(defun method-applicable-p (method args)
  (do* ((specializers (method-specializers method) (cdr specializers))
        (args args (cdr args)))
       ((null specializers) t)
    (let ((specializer (car specializers))
          (arg (car args)))
      (if (eql-specializer-p specializer)
          (unless (eql arg (eql-specializer-object specializer))
            (return nil))
          (unless (subclassp (class-of arg) specializer)
            (return nil))))))

;; CL generic function
;; MOP p. 170
(defun compute-applicable-methods (gf args)
  (let ((required-classes (required-classes gf args))
        (methods nil))
    (dolist (method (generic-function-methods gf))
      (when (method-applicable-p method args)
        (push method methods)))
    (if (or (null methods) (null (%cdr methods)))
        methods
        (sort methods
              (if (eq (class-of gf) +the-class-standard-generic-function+)
                  #'(lambda (m1 m2)
                     (std-method-more-specific-p gf m1 m2 required-classes
                                                 (generic-function.argument-precedence-order gf)))
                  #'(lambda (m1 m2)
                     (method-more-specific-p gf m1 m2 required-classes)))))))

;; ;;; compute-applicable-methods-using-classes

;; ;; MOP p. 171 (generic function)
;; ;; Closette
;; (defun compute-applicable-methods-using-classes (gf required-classes)
;;   (sort
;;     (copy-list
;;       (remove-if-not #'(lambda (method)
;;                          (every #'subclassp
;;                                 required-classes
;;                                 (method-specializers method)))
;;                      (generic-function-methods gf)))
;;     #'(lambda (m1 m2)
;;         (funcall
;;           (if (eq (class-of gf) +the-class-standard-generic-function+)
;;               #'std-method-more-specific-p
;;               #'method-more-specific-p)
;;           gf m1 m2 required-classes))))

;;; method-more-specific-p

(defun std-method-more-specific-p (gf method1 method2 required-classes argument-precedence-order)
  (declare (ignore gf))
  (if argument-precedence-order
      (let ((specializers-1 (method-specializers method1))
            (specializers-2 (method-specializers method2)))
        (dolist (index argument-precedence-order)
          (let ((spec1 (nth index specializers-1))
                (spec2 (nth index specializers-2)))
            (unless (eq spec1 spec2)
              (cond ((eql-specializer-p spec1)
                     (return t))
                    ((eql-specializer-p spec2)
                     (return nil))
                    (t
                     (return (sub-specializer-p spec1 spec2 (nth index required-classes)))))))))
      (do ((specializers-1 (method-specializers method1) (cdr specializers-1))
           (specializers-2 (method-specializers method2) (cdr specializers-2))
           (classes required-classes (cdr classes)))
          ((null specializers-1) nil)
        (let ((spec1 (car specializers-1))
              (spec2 (car specializers-2)))
          (unless (eq spec1 spec2)
            (cond ((eql-specializer-p spec1)
                   (return t))
                  ((eql-specializer-p spec2)
                   (return nil))
                  (t
                   (return (sub-specializer-p spec1 spec2 (car classes))))))))))

#+closette
(defun primary-method-p (method)
  (null (method-qualifiers method)))
#+closette
(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))
#+closette
(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))
#+closette
(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

;; #+closette
;; (defun std-compute-effective-method-function (gf methods)
;;   (let ((primaries (remove-if-not #'primary-method-p methods))
;;         (around (find-if #'around-method-p methods)))
;;     (when (null primaries)
;;       (error "No primary method for the generic function ~S." gf))
;;     (if around
;;         (let ((next-emfun
;;                 (funcall
;;                    (if (eq (class-of gf) +the-class-standard-generic-function+)
;;                        #'std-compute-effective-method-function
;;                        #'compute-effective-method-function)
;;                    gf (remove around methods))))
;;           #'(lambda (args)
;;               (funcall (method-function around) args next-emfun)))
;;         (let ((next-emfun (compute-primary-emfun (cdr primaries)))
;;               (befores (remove-if-not #'before-method-p methods))
;;               (reverse-afters
;;                 (reverse (remove-if-not #'after-method-p methods))))
;;           #'(lambda (args)
;;               (dolist (before befores)
;;                 (funcall (method-function before) args nil))
;;               (multiple-value-prog1
;;                 (funcall (method-function (car primaries)) args next-emfun)
;;                 (dolist (after reverse-afters)
;;                   (funcall (method-function after) args nil))))))))

#+closette
(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))
        #'(lambda (args)
           (funcall (method-function (car methods)) args next-emfun)))))

(defmacro call-method-list (&rest calls)
  `(progn ,@calls))

(defun make-call-methods (methods)
  `(call-method-list
    ,@(mapcar #'(lambda (method) `(call-method ,method nil)) methods)))

;; FIXME args
(defmacro call-method (method &optional next-method-list)
  ;; "NEXT-METHOD-LIST can contain method objects or lists, the first element
  ;; of which must be the symbol MAKE-METHOD and the second element of which
  ;; must be a form."

;;   `(if (and (consp ,method) (eq (first ,method) 'make-method))
;;        (funcall (coerce-to-function (second ,method)) args) ; REVIEW
;;        (funcall (method-function ,method) args ,next-method-list)))


  (cond ((typep method 'method)
         `(funcall ,(method-function method) ,+gf-args-var+ ',next-method-list))
        (t
;;          (mumble "call-method method = ~S~%" method)
         `(funcall ,method ,+gf-args-var+))))

;; (defmacro call-method (method &optional next-method-list)
;;   (if (and (consp method) (eq (first method) 'make-method))
;;       `(funcall (coerce-to-function ,(second method)) args) ; REVIEW
;;       `(funcall (method-function ,method) args ,next-method-list)))

;; standard method combination
(defun compute-standard-effective-method (gf methods)
  (let* ((befores nil)
         (primaries nil)
         (afters nil)
         (arounds nil))
    (dolist (method methods)
      (let ((qualifiers (method-qualifiers method)))
        (cond ((null qualifiers)
               (push method primaries))
              ((cdr qualifiers)
               (error "Invalid method qualifiers."))
              ((equal '(:before) qualifiers)
               (push method befores))
              ((equal '(:after) qualifiers)
               (push method afters))
              ((equal '(:around) qualifiers)
               (push method arounds))
              (t
               (error "Invalid method qualifiers.")))))
    (when (null primaries)
      (error "No primary method for the generic function ~S." gf))
    (setq primaries (nreverse primaries)
          befores   (nreverse befores)
	  afters    (nreverse afters)
	  arounds   (nreverse arounds))
    (let ((main-effective-method
           (cond ((and (null befores) (null afters) (null arounds))
                  `(call-method ,(first primaries) ,(rest primaries)))
                 (t
                  `(multiple-value-prog1
                       (progn
                         ,(make-call-methods befores)
                         (call-method ,(first primaries) ,(rest primaries)))
                       ,(make-call-methods (nreverse afters)))))))
      (cond (arounds
             (values `(call-method ,(first arounds)
                                   (,@(rest arounds) (make-method ,main-effective-method)))
                     nil))
            (t
             (values main-effective-method nil))))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus
;;; generic functions.

;; (defun kludge-arglist (lambda-list)
;;   (if (and (member '&key lambda-list)
;;            (not (member '&allow-other-keys lambda-list)))
;;       (append lambda-list '(&allow-other-keys))
;;       (if (and (not (member '&rest lambda-list))
;;                (not (member '&key lambda-list)))
;;           (append lambda-list '(&key &allow-other-keys))
;;           lambda-list)))

;; "When a generic function or any of its methods mentions &key in a lambda
;; list, the specific set of keyword arguments accepted by the generic function
;; varies according to the applicable methods. The set of keyword arguments
;; accepted by the generic function for a particular call is the union of the
;; keyword arguments accepted by all applicable methods and the keyword
;; arguments mentioned after &key in the generic function definition, if any."
;; 7.6.5
;; Adapted from Sacla.
(defun allow-other-keys (lambda-list)
  (declare (type list lambda-list))
  (if (and (memq '&key lambda-list)
           (not (memq '&allow-other-keys lambda-list)))
      (let* ((key-end (or (position '&aux lambda-list) (length lambda-list)))
             (aux-part (subseq lambda-list key-end)))
        `(,@(subseq lambda-list 0 key-end) &allow-other-keys ,@aux-part))
      lambda-list))

;; #+closette
;; (defun std-compute-method-function (method)
;;   (let ((form (method-body method))
;;         (lambda-list (method-lambda-list method)))
;;     (compile-in-lexical-environment (method-environment method)
;;       `(lambda (args next-emfun)
;;          (flet ((call-next-method (&rest cnm-args)
;;                   (if (null next-emfun)
;;                       (error "No next method for the~@
;;                               generic function ~S."
;;                              (method-generic-function ',method))
;;                       (funcall next-emfun (or cnm-args args))))
;;                 (next-method-p ()
;;                   (not (null next-emfun))))
;;             (apply #'(lambda ,(kludge-arglist lambda-list)
;;                        ,form)
;;                    args))))))

;; (defun std-compute-method-function (method)
;;   (let ((body (method-body method))
;;         (lambda-list (method-lambda-list method)))
;; ;;     (compile-in-lexical-environment (method-environment method)
;;     (compile-in-lexical-environment nil
;;       `(lambda (gf-args next-methods)
;;          (flet ((call-next-method (&rest args)
;;                   (unless args (setq args gf-args))
;;                   (if (null next-methods)
;;                       (error "No next method for the generic function ~S."
;;                              (method-generic-function ',method))
;;                       (call-method (car next-methods) (cdr next-methods))))
;;                 (next-method-p ()
;;                   (not (null next-methods))))
;;            (apply #'(lambda ,(allow-other-keys lambda-list)
;;                      ,@body)
;;                   gf-args))))))
#+sacla
;; MOP p. 207
;; "This method returns a method lambda which accepts two arguments, the list
;; of arguments to the generic function, and the list of next methods. What
;; initialization arguments may be returned in the second value are
;; unspecified."
;; "Either the function COMPILE, the special form FUNCTION or the function
;; COERCE must be used to convert the lambda expression to a method function."
(defmethod make-method-lambda ((generic-function standard-generic-function)
                               (method standard-method)
                               lambda-expression
                               environment)
  (let* ((lambda-list (second lambda-expression))
         (gf-args (first lambda-list))
         (next-methods (second lambda-list))
         (name (generic-function-name generic-function))
         (args (gensym)))
    (multiple-value-bind (decls forms)
        (declarations-and-forms (cddr lambda-expression))
      `(lambda ,lambda-list
         ,@decls
         (block ,(if (symbolp name) name (second name))
           (labels ((next-method-p () ,next-methods)
                    (call-next-method (&rest ,args)
                      (unless ,args (setq ,args ,gf-args))
                      (if (next-method-p)
                          (funcall (method-function (car ,next-methods))
                                   ,args (cdr ,next-methods))
                          (apply #'no-next-method ,generic-function ,method
                                 ,(first lambda-list)))))
             ,@forms))))))

;; MOP p. 207
;; "This generic function returns two values. The first is a lambda expression,
;; the second is a list of initialization arguments and values."
(defgeneric make-method-lambda (generic-function method lambda-expression environment))

(defmethod make-method-lambda ((generic-function standard-generic-function)
                               (method standard-method)
                               lambda-expression
                               environment)
  (%make-method-lambda generic-function method lambda-expression environment))

;;; Slot access

;; MOP p. 235
(defgeneric slot-value-using-class (class instance slot-name))

(defmethod slot-value-using-class ((class standard-class) instance slot-name)
  (std-instance-slot-value instance slot-name))

(defmethod slot-value-using-class ((class funcallable-standard-class) instance slot-name)
  (std-instance-slot-value instance slot-name))

(defgeneric (setf slot-value-using-class) (new-value class instance slot-name))

(defmethod (setf slot-value-using-class) (new-value (class standard-class) instance slot-name)
  (declare (ignore class))
  (set-std-instance-slot-value instance slot-name new-value))

;;; N.B. To avoid making a forward reference to a (setf xxx) generic function:
(defun setf-slot-value-using-class (new-value class object slot-name)
  (setf (slot-value-using-class class object slot-name) new-value))

(defgeneric slot-exists-p-using-class (class instance slot-name))

(defmethod slot-exists-p-using-class (class instance slot-name)
  nil)

(defmethod slot-exists-p-using-class ((class standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))

(defmethod slot-exists-p-using-class ((class funcallable-standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))

(defmethod slot-exists-p-using-class ((class structure-class) instance slot-name)
  ;; FIXME structure-slot-definition should be a standard class
  (dolist (structure-slot-definition (class.slots class))
    (when (eq (slot-name structure-slot-definition) slot-name)
      (return-from slot-exists-p-using-class t)))
  nil)

;; MOP p. 233
(defgeneric slot-boundp-using-class (class instance slot-name))

(defmethod slot-boundp-using-class ((class standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))

(defmethod slot-boundp-using-class ((class funcallable-standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class
           ((class standard-class) instance slot-name)
  (std-slot-makunbound instance slot-name))

(defgeneric slot-missing (class instance slot-name operation &optional new-value))

(defmethod slot-missing ((class t) instance slot-name operation &optional new-value)
  (declare (ignore instance operation new-value))
  (error "The slot ~S is missing from the class ~S." slot-name class))

(defgeneric slot-unbound (class instance slot-name))

(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :instance instance :name slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class &rest initargs))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-standard-instance (or (class.layout class)
                                  (make-layout class nil nil))))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
;;   (format t "calling allocate-funcallable-standard-instance ~S~%" class)
  (allocate-funcallable-standard-instance (or (class.layout class)
                                              (make-layout class nil nil))))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let ((type-names (mapcar #'%class-name (class-precedence-list class)))
        (numslots (length (class.slots class))))
    (%make-structure type-names
                     (make-list numslots :initial-element +unbound-marker+)
                     numslots)))

(defmethod allocate-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot allocate an instance of ~S." class))

(defgeneric make-instance (class &key))

(defmethod make-instance ((class class) &rest initargs)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (check-initargs class initargs)
  (let ((default-initargs (class-default-initargs class)))
    (when default-initargs
      (let ((additional-initargs nil)
            (not-found (gensym)))
        (dolist (default-initarg (class.default-initargs class))
          (let ((initarg (getf initargs (car default-initarg) not-found)))
            (when (eq initarg not-found)
              (push (car default-initarg) additional-initargs)
              (push (funcall (caddr default-initarg)) additional-initargs))))
        (setq initargs (append initargs (reverse additional-initargs))))))
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key))

(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
;;   (check-initargs (class-of instance) initargs)
  (apply #'shared-initialize instance nil initargs))

(defgeneric shared-initialize (instance slot-names &key))

(defmethod shared-initialize ((instance standard-object)
                              slot-names
                              &rest initargs)
  (when (oddp (length initargs))
    (error 'program-error :format-control "Odd number of keyword arguments."))
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
          (get-properties initargs (slot-definition-initargs slot))
        (declare (ignore init-key))
        (if foundp
            (setf (slot-value instance slot-name) init-value)
            (when (and (not (slot-boundp instance slot-name))
                       (not (null (slot-definition-initfunction slot)))
                       (or (eq slot-names t)
                           (member slot-name slot-names)))
              (setf (slot-value instance slot-name)
                    (funcall (slot-definition-initfunction slot))))))))
  instance)

;; "The set of valid initialization arguments for a class is the set of valid
;; initialization arguments that either fill slots or supply arguments to
;; methods, along with the predefined initialization argument :ALLOW-OTHER-KEYS."
;; 7.1.2
#+nil
(defun check-initargs (class initargs)
  (when (oddp (length initargs))
    (error 'program-error
           :format-control "Odd number of keyword arguments."))
  (unless (getf initargs :allow-other-keys)
    (let ((slots (class.slots class)))
      (do* ((tail initargs (cddr tail))
            (initarg (car tail) (car tail)))
           ((null tail))
        (unless (or (valid-initarg-p initarg slots)
                    (eq initarg :allow-other-keys))
          (error 'program-error
                 :format-control "Invalid initarg ~S."
                 :format-arguments (list initarg)))))))

;; FIXME
(defun check-initargs (class initargs)
  (declare (ignore class initargs)))

(defun valid-initarg-p (initarg slots)
  (dolist (slot slots nil)
    (let ((valid-initargs (slot-definition-initargs slot)))
      (when (memq initarg valid-initargs)
        (return t)))))

;;; change-class

(defgeneric change-class (instance new-class &key))

(defmethod change-class ((old-instance standard-object) (new-class standard-class)
                         &rest initargs)
  (let ((old-slots (class-slots (class-of old-instance)))
        (new-slots (class-slots new-class))
        (new-instance (allocate-instance new-class)))
    ;; "The values of local slots specified by both the class CTO and the class
    ;; CFROM are retained. If such a local slot was unbound, it remains
    ;; unbound."
    (dolist (new-slot new-slots)
      (when (instance-slot-p new-slot)
        (let* ((slot-name (slot-definition-name new-slot))
               (old-slot (find slot-name old-slots :key #'slot-definition-name)))
          ;; "The values of slots specified as shared in the class CFROM and as
          ;; local in the class CTO are retained."
          (when (and old-slot (slot-boundp old-instance slot-name))
            (setf (slot-value new-instance slot-name)
                  (slot-value old-instance slot-name))))))
    (swap-slots old-instance new-instance)
    (rotatef (std-instance-layout new-instance) (std-instance-layout old-instance))
    (apply #'update-instance-for-different-class new-instance old-instance initargs)
    old-instance))

(defmethod change-class ((instance standard-object) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

;;; update-instance-for-different-class

(defgeneric update-instance-for-different-class (old new &key))

(defmethod update-instance-for-different-class ((old standard-object)
                                                (new standard-object)
                                                &rest initargs)
  (let ((added-slots
          (remove-if #'(lambda (slot-name)
                         (slot-exists-p old slot-name))
                     (mapcar #'slot-definition-name
                             (class-slots (class-of new))))))
    (check-initargs (class-of new) initargs)
    (apply #'shared-initialize new added-slots initargs)))

;;; make-instances-obsolete

(defgeneric make-instances-obsolete (class))

(defmethod make-instances-obsolete ((class standard-class))
  (%make-instances-obsolete class))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))

;;; update-instance-for-redefined-class

(defgeneric update-instance-for-redefined-class (instance
                                                 added-slots
                                                 discarded-slots
                                                 property-list
                                                 &rest initargs
                                                 &key
                                                 &allow-other-keys))

(defmethod update-instance-for-redefined-class ((instance standard-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (declare (ignore discarded-slots property-list))
;;   (check-initargs (class-of instance) initargs)
  (apply #'shared-initialize instance added-slots initargs))

;;;
;;;  Methods having to do with class metaobjects.
;;;

(defmethod initialize-instance :after ((class standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

;;; Finalize inheritance

;; MOP p. 190
(defgeneric finalize-inheritance (class))

(defmethod finalize-inheritance ((class standard-class))
  (std-finalize-inheritance class)
  t)

(defmethod finalize-inheritance ((class funcallable-standard-class))
  (std-finalize-inheritance class)
  t)

(defmethod finalize-inheritance ((class forward-referenced-class))
  (error "~S was called on a forward referenced class." 'finalize-inheritance))

(defgeneric class-name (class))

(defmethod class-name ((class class))
  (class.name class))

(defgeneric (setf class-name) (new-value class))

(defmethod (setf class-name) (new-value (class class))
  (setf (class.name class) new-value))

;; MOP p. 212
(defgeneric class-default-initargs (class))

(defmethod class-default-initargs ((class built-in-class))
  nil)

(defmethod class-default-initargs ((class forward-referenced-class))
  (error "~S was called on a forward referenced class." 'class-default-initargs))

;; MOP p. 213
(defgeneric class-finalized-p (class))

(defmethod class-finalized-p ((class built-in-class))
  t)

(defmethod class-finalized-p ((class forward-referenced-class))
  nil)

;; MOP p. 173
(defgeneric compute-class-precedence-list (class))

(defmethod compute-class-precedence-list ((class class))
  (std-compute-class-precedence-list class))

;; MOP p. 178
(defgeneric compute-slots (class))

(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))

(defmethod compute-slots ((class funcallable-standard-class))
  (std-compute-slots class))

;; MOP p. 177
(defgeneric compute-effective-slot-definition (class name direct-slots))

(defmethod compute-effective-slot-definition ((class standard-class) name direct-slots)
  (std-compute-effective-slot-definition class name direct-slots))

(defmethod compute-effective-slot-definition ((class funcallable-standard-class) name direct-slots)
  (std-compute-effective-slot-definition class name direct-slots))

;; MOP p. 174
(defgeneric compute-default-initargs (class))

(defmethod compute-default-initargs ((class standard-class))
  (std-compute-default-initargs class))

(defmethod compute-default-initargs ((class funcallable-standard-class))
  (std-compute-default-initargs class))

;; MOP p. 240
(defgeneric validate-superclass (class superclass))

(defmethod validate-superclass ((class class) (superclass class))
  (let ((class-of-class (class-of class))
        (class-of-superclass (class-of superclass)))
    (or (eq superclass (find-class t))
        (eq class-of-superclass class-of-class)
        (and (eq class-of-class +the-class-standard-class+)
             (eq class-of-superclass +the-class-funcallable-standard-class+))
        (and (eq class-of-class +the-class-funcallable-standard-class+)
             (eq class-of-superclass +the-class-standard-class+)))))

(defmethod validate-superclass ((class class) (superclass built-in-class))
  (eq superclass (find-class 'stream)))

(defmethod validate-superclass ((class class) (superclass forward-referenced-class))
  t)

;;;
;;; Methods having to do with generic function metaobjects.
;;;

(defmethod initialize-instance :after ((gf standard-generic-function) &key)
  (finalize-generic-function gf))

;;;
;;; Methods having to do with method metaobjects.
;;;

;; (defmethod initialize-instance :after ((method standard-method) &key)
;;   (setf (method-function method) (compute-method-function method)))

;;;
;;; Methods having to do with generic function invocation.
;;;

;; MOP p. 175
(defgeneric compute-discriminating-function (gf))

(defmethod compute-discriminating-function ((gf standard-generic-function))
  (std-compute-discriminating-function gf))

;; Closette
(defgeneric method-more-specific-p (gf method1 method2 required-classes))

(defmethod method-more-specific-p ((gf standard-generic-function)
                                   method1 method2 required-classes)
  (std-method-more-specific-p gf method1 method2 required-classes
                              (generic-function.argument-precedence-order gf)))

;; MOP p. 176
;; FIXME "The METHOD-COMBINATION argument is a method combination metaobject."
(defgeneric compute-effective-method (generic-function method-combination methods))

;; (defmethod compute-effective-method ((generic-function standard-generic-function)
;;                                      method-combination methods)
;;   (std-compute-effective-method generic-function method-combination methods))

;; CL
(defgeneric no-applicable-method (generic-function &rest args))

(defmethod no-applicable-method (generic-function &rest args)
  (error "There is no applicable method for the generic function ~S when called with arguments ~S."
         generic-function
         args))

;; CL
(defgeneric add-method (generic-function method))

;; CL
(defmethod add-method ((generic-function standard-generic-function) (method method))
  (let ((method-lambda-list (method-lambda-list method)))
    (check-method-lambda-list method-lambda-list generic-function))
  (%add-method generic-function method))

(defgeneric make-load-form (object &optional environment))

(defmethod make-load-form ((object t) &optional environment)
  (declare (ignore environment))
  (apply #'no-applicable-method #'make-load-form (list object)))

(defmethod make-load-form ((class class) &optional environment)
  (declare (ignore environment))
  (let ((name (class-name class)))
    (unless (and name (eq (find-class name nil) class))
      (error 'simple-type-error
             :format-control "Can't use anonymous or undefined class as a constant: ~S."
             :format-arguments (list class)))
    `(find-class ',name)))

;; FIXME move this to C++
(defun false () nil)

(ensure-class-preserving-layout
 'metaobject
 :direct-superclasses '(standard-object)
 )

(ensure-class-preserving-layout
 'specializer
 :direct-superclasses '(metaobject)
 :direct-slots
 `((:name direct-methods :initform nil :initfunction ,#'false
    :readers (specializer-direct-methods)))
 )

(ensure-class-preserving-layout
 'eql-specializer
 :direct-superclasses '(specializer)
 :direct-slots '((:name object :initargs (:object)))
 )

(ensure-class-preserving-layout
 'class
 :direct-superclasses '(specializer)
 :direct-slots
 `((:name prototype :initform nil :initfunction ,#'false)
   (:name name :initargs (:name) :initform nil :initfunction ,#'false :readers (class-name))
   (:name layout :initform nil :initfunction ,#'false)
   (:name precedence-list :initform nil  :initfunction ,#'false :readers (class-precedence-list))
   (:name direct-superclasses  :initform nil  :initfunction ,#'false :readers (class-direct-superclasses))
   (:name direct-subclasses  :initform nil  :initfunction ,#'false :readers (class-direct-subclasses)))
 )

(ensure-class-preserving-layout
 'standard-class
 :direct-superclasses '(class)
 :direct-slots
 `((:name direct-slots :initform nil :initfunction ,#'false
    :readers (class-direct-slots)
    :writers ((setf class-direct-slots)))
   (:name slots :initform nil :initfunction ,#'false
    :readers (class-slots))
   (:name direct-default-initargs :initform nil :initfunction ,#'false
    :readers (class-direct-default-initargs))
   (:name default-initargs :initform nil :initfunction ,#'false
    :readers (class-default-initargs))
   (:name finalized-p :initform nil :initfunction ,#'false
    :readers (class-finalized-p)))
 )

(ensure-class-preserving-layout
 'funcallable-standard-class
 :direct-superclasses '(class)
 :direct-slots
 `((:name direct-slots :initform nil :initfunction ,#'false
    :readers (class-direct-slots)
    :writers ((setf class-direct-slots)))
   (:name slots :initform nil :initfunction ,#'false
    :readers (class-slots))
   (:name direct-default-initargs :initform nil :initfunction ,#'false
    :readers (class-direct-default-initargs))
   (:name default-initargs :initform nil :initfunction ,#'false
    :readers (class-default-initargs))
   (:name finalized-p :initform nil :initfunction ,#'false
    :readers (class-finalized-p)))
 )

(ensure-class-preserving-layout
 'structure-class
 :direct-superclasses '(class)
 :direct-slots
 `((:name direct-slots :initform nil :initfunction ,#'false
          :readers (class-direct-slots)
          ;;           :writers ((setf class-direct-slots))
          )
   (:name slots :initform nil :initfunction ,#'false
          :readers (class-slots)
          ))
 )

;; REVIEW readers, writers
(ensure-class-preserving-layout
 'standard-generic-function
 :direct-superclasses '(generic-function)
 :direct-slots
 `((:name name
    :initargs (:name)
    :initform nil :initfunction ,#'false)
   (:name lambda-list
    :initargs (:lambda-list)
    :initform nil :initfunction ,#'false)
   (:name declarations
    :initform nil :initfunction ,#'false)
   (:name %documentation
    :initform nil :initfunction ,#'false)
   (:name methods
    :initform nil :initfunction ,#'false)
   (:name method-class
    :initargs (:method-class)
    :initform nil :initfunction ,#'false)
   (:name %method-combination
    :initargs (:method-combination)
    :initform nil :initfunction ,#'false)
   (:name argument-precedence-order
    :initargs (:argument-precedence-order)
    :initform nil :initfunction ,#'false
    :readers (generic-function-argument-precedence-order))
   (:name initial-methods
    :initform nil :initfunction ,#'false)
   (:name classes-to-emf-table
    :initform '(make-hash-table :test 'equal)
    :initfunction ,#'(lambda () (make-hash-table :test 'equal)))
   (:name minargs
    :initform nil :initfunction ,#'false)
   ))

(ensure-class-preserving-layout
 'standard-method
 :direct-superclasses '(method)
 :direct-slots
 `((:name lambda-list
    :initargs (:lambda-list)
    :initform nil :initfunction ,#'false)
   (:name qualifiers
    :initargs (:qualifiers)
    :initform nil :initfunction ,#'false)
   (:name specializers
    :initargs (:specializers)
    :initform nil :initfunction ,#'false)
   (:name %generic-function
    :initargs (:generic-function)
    :initform nil :initfunction ,#'false)
   (:name %function
    :initargs (:function)
    :initform nil :initfunction ,#'false)
   (:name fast-function
    :initargs (:fast-function)
    :initform nil :initfunction ,#'false)
   ))

(ensure-class-preserving-layout
 'slot-definition
 :direct-superclasses '(metaobject)
 :direct-slots
 `((:name name
    :initargs (:name)
    :initform nil :initfunction ,#'false
    :readers (slot-definition-name))
   (:name initargs
    :initargs (:initargs)
    :initform nil :initfunction ,#'false)
   (:name initform
    :initargs (:initform)
    :initform nil :initfunction ,#'false)
   (:name initfunction
    :initargs (:initfunction)
    :initform nil :initfunction ,#'false)
   (:name allocation
    :initargs (:allocation)
    :initform nil :initfunction ,#'false)
   (:name %type
    :initargs (:type)
    :initform nil :initfunction ,#'false)
   ;; REVIEW
   (:name %class
    :initargs (:class)
    :initform nil :initfunction ,#'false)
   (:name %documentation
    :initargs (:documentation)
    :initform nil :initfunction ,#'false)
    ))

(ensure-class-preserving-layout
 'direct-slot-definition
 :direct-superclasses '(slot-definition)
 :direct-slots
 `((:name readers
    :initargs (:readers)
    :initform nil :initfunction ,#'false
    :readers (slot-definition-readers))
   (:name writers
    :initargs (:writers)
    :initform nil :initfunction ,#'false
    :readers (slot-definition-writers)))
 )

(ensure-class-preserving-layout
 'effective-slot-definition
 :direct-superclasses '(slot-definition)
 :direct-slots
 `((:name location
          :initargs (:location)
          :initform nil :initfunction ,#'false
          :readers (slot-definition-location))
   (:name allocation-class
          :initargs (:allocation-class)
          :initform nil :initfunction ,#'false
          :readers (slot-definition-allocation-class)))
 )

(ensure-class-preserving-layout
 'standard-slot-definition
 :direct-superclasses '(slot-definition)
 )

(ensure-class-preserving-layout
 'standard-direct-slot-definition
 :direct-superclasses '(standard-slot-definition direct-slot-definition)
 )

(ensure-class-preserving-layout
 'standard-effective-slot-definition
 :direct-superclasses '(standard-slot-definition effective-slot-definition)
 )

(defgeneric class-finalized-p (class))

(defmethod class-finalized-p ((class structure-class))
  t)

(defgeneric class-prototype (class))

(defmethod class-prototype :before (class)
  (unless (class-finalized-p class)
    (error "~S is not finalized." class)))

(defmethod class-prototype ((class class))
  (or (class.prototype class)
      (setf (class.prototype class) (allocate-instance class))))

(defmethod class-direct-slots ((class class))
  nil)

(defmethod class-direct-default-initargs ((class class))
  nil)

;; MOP p. 215
(defmethod class-default-initargs ((class class))
  nil)

(defmethod class-default-initargs ((class forward-referenced-class))
  (error "~S was called on a forward referenced class." 'class-default-initargs))

(defmethod class-precedence-list ((class forward-referenced-class))
  (error "~S was called on a forward referenced class." 'class-precedence-list))

(defmethod class-slots ((class forward-referenced-class))
  (error "~S was called on a forward referenced class." 'class-slots))

(defmacro define-condition (name (&rest parent-types) (&rest slot-specs) &body options)
  (let ((parent-types (or parent-types '(condition)))
        (report nil))
    (dolist (option options)
      (when (eq (car option) :report)
        (setf report (cadr option))
        (return)))
    (typecase report
      (null
       `(progn
          (defclass ,name ,parent-types ,slot-specs ,@options)
          ',name))
      (string
       `(progn
          (defclass ,name ,parent-types ,slot-specs ,@options)
          (defmethod print-object ((condition ,name) stream)
            (if *print-escape*
                (call-next-method)
                (progn (write-string ,report stream) condition)))
          ',name))
      (t
       `(progn
          (defclass ,name ,parent-types ,slot-specs ,@options)
          (defmethod print-object ((condition ,name) stream)
            (if *print-escape*
                (call-next-method)
                (funcall #',report condition stream)))
          ',name)))))

(defun make-condition (type &rest initargs)
  (if (consp type)
      (if (eq (car type) 'OR)
          (apply #'make-condition (cadr type) initargs)
          nil)
      (or (%make-condition type initargs)
          (let ((class (if (symbolp type) (find-class type) type)))
            (apply #'make-instance class initargs)))))

(setq *preserve-layout* t)

(define-condition condition (standard-object)
  ((format-control :initarg :format-control)
   (format-arguments :initarg :format-arguments)))

(define-condition warning () ())
(define-condition style-warning (warning) ())
(define-condition serious-condition () ())
(define-condition storage-condition (serious-condition) ())
(define-condition error (serious-condition) ())
(define-condition type-error (error)
  ((datum :initarg :datum)
   (expected-type :initarg :expected-type)))
(define-condition package-error (error)
  ((package :initarg :package)))
(define-condition control-error (error) ())
(define-condition print-not-readable (error)
  ((object :initarg :object)))
(define-condition program-error (error) ())
(define-condition file-error (error)
  ((pathname :initarg :pathname)))
(define-condition stream-error (error)
  ((stream :initarg :stream)))
(define-condition end-of-file (stream-error) ())
(define-condition parse-error (error) ())
(define-condition reader-error (parse-error stream-error) ())
(define-condition cell-error (error)
  ((name :initarg :name)))
(define-condition unbound-variable (cell-error) ())
(define-condition undefined-function (cell-error) ())
(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance)))
(define-condition arithmetic-error (error)
  ((operation :initarg :operation)
   (operands  :initarg :operands)))
(define-condition division-by-zero (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition simple-condition (condition) ())
(define-condition simple-warning (simple-condition warning) ())
(define-condition simple-error (simple-condition error) ())
(define-condition simple-type-error (simple-condition type-error) ())

(setq *preserve-layout* nil)

;; MOP p. 183
(defgeneric ensure-class-using-class (class name &rest args &allow-other-keys))

;; MOP p. 184
(defmethod ensure-class-using-class ((class class) name &rest args &key)
  (apply '%ensure-class-using-class class name args))

;; MOP p. 185
(defmethod ensure-class-using-class ((class forward-referenced-class) name &rest args &key)
  (apply '%ensure-class-using-class class name args))

;; MOP p. 186
(defmethod ensure-class-using-class ((class null) name &rest args &key)
  (apply '%ensure-class-using-class class name args))

(defgeneric print-object (object stream))

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :identity t)
    (format stream "~S" (class-name (class-of object))))
  object)

(defmethod print-object ((object class) stream)
  (if (slot-boundp object 'name)
      (print-unreadable-object (object stream :identity t)
        (format stream "~S ~S" (class-name (class-of object)) (class-name object)))
      (call-next-method))
  object)

(defmethod print-object ((object slot-definition) stream)
  (if (and (slot-exists-p object 'name) (slot-boundp object 'name))
      (print-unreadable-object (object stream :identity t)
        (format stream "~S ~S" (class-name (class-of object)) (slot-definition-name object)))
      (call-next-method))
  object)

(defmethod print-object ((c condition) stream)
  (if *print-escape*
      (call-next-method)
      (if (and (slot-boundp c 'format-control) (simple-condition-format-control c))
          (apply #'format stream
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c))
          (format stream "Condition ~A was signalled." (class-name (class-of c))))))

(defmethod print-object ((object standard-generic-function) stream)
  (if (and (slot-exists-p object 'name) (slot-boundp object 'name))
      (print-unreadable-object (object stream :identity t)
        (format stream "~S ~S" (class-name (class-of object)) (slot-value object 'name)))
      (call-next-method))
  object)

(defmethod print-object ((method standard-method) stream)
  (if (slot-boundp method '%generic-function)
      (print-unreadable-object (method stream :identity t)
        (let ((generic-function (method-generic-function method)))
          (format stream "~S ~S~{ ~S~} ~S"
                  (class-name (class-of method))
                  (and generic-function
                       (generic-function-name generic-function))
                  (method-qualifiers method)
;;                   (mapcar #'%class-name (method-specializers method))
                  ;; REVIEW
                  (mapcar #'(lambda (x) (if (classp x) (%class-name x) x)) (method-specializers method))
                  )))
      (call-next-method))
  method)

(defgeneric documentation (x doc-type))

(defgeneric (setf documentation) (new-value x doc-type))

(defmethod documentation ((x t) doc-type)
  (%documentation x doc-type))

(defmethod (setf documentation) (new-value (x t) doc-type)
  (%set-documentation x doc-type new-value))

(defun invalid-method-error (method format-control &rest args)
  (let ((message (apply #'format nil format-control args)))
    (error "Invalid method error for ~S:~%    ~A" method message)))

(defun method-combination-error (format-control &rest args)
  (let ((message (apply #'format nil format-control args)))
    (error "Method combination error in CLOS dispatch:~%    ~A" message)))

;; FIXME
(defgeneric no-next-method (generic-function method &rest args))

;; FIXME
(defgeneric function-keywords (method))

(defclass short-method-combination (method-combination)
  ((name
    :reader short-combination-name
    :initarg :name)
   (operator
    :reader short-combination-operator
    :initarg :operator)
   (identity-with-one-argument
    :reader short-combination-identity-with-one-argument
    :initarg :identity-with-one-argument)
   (options
    :accessor short-combination-options
    :initform nil
    :initarg :options)
   ;; REVIEW
   (%documentation
    :initform nil
    :initarg :documentation)))

(defun copy-short-method-combination (mc)
  (make-instance 'short-method-combination
                 :name (short-combination-name mc)
                 :operator (short-combination-operator mc)
                 :identity-with-one-argument (short-combination-identity-with-one-argument mc)
                 :options (short-combination-options mc)
                 ;; REVIEW documentation
                 ))

(defun method-combination-with-options (mc options)
  (declare (type method-combination mc))
  (when options
    (etypecase mc
      (short-method-combination
       (setq mc (copy-short-method-combination mc))
       (setf (short-combination-options mc) (copy-list options)))
      (long-method-combination
       (setq mc (copy-long-method-combination mc))
       (setf (long-combination-arguments mc) (copy-list options)))))
  mc)

(defun %find-method-combination (method-combination-type-name method-combination-options)
  (let ((mc (get method-combination-type-name 'method-combination-object)))
    (cond ((null mc)
           (error "Method combination ~S does not exist." method-combination-type-name))
          ((eq mc *standard-method-combination*)
           (when method-combination-options
             (error "The STANDARD method combination accepts no options."))
           mc)
          (t
           (method-combination-with-options mc method-combination-options)))))

;; MOP p. 191
;; "The METHOD-COMBINATION-OPTIONS argument is a list of arguments to the
;; method combination type."
(defgeneric find-method-combination (generic-function
                                     method-combination-type-name ; a symbol
                                     method-combination-options))

(defmethod find-method-combination ((gf standard-generic-function)
                                    method-combination-type-name
                                    method-combination-options)
  (%find-method-combination method-combination-type-name method-combination-options))

(defmethod compute-effective-method ((generic-function standard-generic-function)
                                     (method-combination standard-method-combination)
                                     methods)
  (compute-standard-effective-method generic-function methods))

(defmethod compute-effective-method ((gf standard-generic-function)
                                     (mc short-method-combination)
                                     methods)
  (aver (typep mc 'short-method-combination))
  (let* ((mc-name (short-combination-name mc))
         (options (short-combination-options mc))
         (order (car options))
         (befores nil)
         (primaries nil)
         (afters nil)
         (arounds nil))
    (aver (neq mc-name 'standard))
    (dolist (method methods)
      (let ((qualifiers (method-qualifiers method)))
        (cond ((null qualifiers)
               (error "Method combination type mismatch."))
              ((cdr qualifiers)
               (error "Invalid method qualifiers."))
              ((equal '(:before) qualifiers) ; FIXME short form does not support :before
               (push method befores))
              ((equal '(:after) qualifiers) ; FIXME short form does not support :after
               (push method afters))
              ((equal '(:around) qualifiers)
               (push method arounds))
              ((eq (car qualifiers) mc-name)
               (push method primaries))
              (t
               (error "Invalid method qualifiers.")))))
    (unless (eq order :most-specific-last)
      (setq primaries (nreverse primaries)))
    (setq befores (nreverse befores)
	  afters  (nreverse afters)
	  arounds (nreverse arounds))
    (when (null primaries)
      (error "No primary method for the generic function ~S." gf))
    (let* ((operator (short-combination-operator mc))
           (ioa (short-combination-identity-with-one-argument mc))
           (main-effective-method
            (if (and (null (cdr primaries))
                     (not (null ioa)))
                `(call-method ,(first primaries) nil)
                `(,operator ,@(mapcar
                               (lambda (primary)
                                 `(call-method ,primary nil))
                               primaries)))))
      (cond (arounds
             (values `(call-method ,(first arounds)
                                   (,@(rest arounds) (make-method ,main-effective-method)))
                     nil))
            (t
             (values main-effective-method
                     nil))))))

;; built-in method combination types
(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)

(setq *mop-working-p* t)

(provide "CLOS")
