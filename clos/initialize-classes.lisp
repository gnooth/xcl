;;; initialize-classes.lisp
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

(defsetf std-instance-layout set-std-instance-layout)

;; class, built-in-class, forward-referenced-class
(defmacro class.direct-methods                       (arg) `(iref ,arg  0))
(defmacro class.prototype                            (arg) `(iref ,arg  1))
(defmacro class.name                                 (arg) `(iref ,arg  2))
(defmacro class.layout                               (arg) `(iref ,arg  3))
(defmacro class.precedence-list                      (arg) `(iref ,arg  4))
(defmacro class.direct-superclasses                  (arg) `(iref ,arg  5))
(defmacro class.direct-subclasses                    (arg) `(iref ,arg  6))
;; structure-class
(defmacro class.direct-slots                         (arg) `(iref ,arg  7))
(defmacro class.slots                                (arg) `(iref ,arg  8))
;; standard-class, funcallable-standard-class
(defmacro class.direct-default-initargs              (arg) `(iref ,arg  9))
(defmacro class.default-initargs                     (arg) `(iref ,arg 10))
(defmacro class.finalized-p                          (arg) `(iref ,arg 11))

(defconstant +built-in-class-layout+
  (make-layout (find-class-1 'built-in-class)
               ;; instance slots
               '(direct-methods prototype name layout precedence-list
                 direct-superclasses direct-subclasses)
               ;; shared slots
               nil))

(defun initialize-built-in-class (class-name direct-superclasses precedence-list)
  (let ((class (find-class-1 class-name)))
    (setf (class.direct-superclasses class) (mapcar #'find-class-1 direct-superclasses))
    (setf (class.precedence-list class) (mapcar #'find-class-1 precedence-list))
    (setf (std-instance-layout class) +built-in-class-layout+)))

(defconstant +structure-class-layout+
  (make-layout (find-class-1 'structure-class)
               '(direct-methods prototype name layout precedence-list
                 direct-superclasses direct-subclasses
                 direct-slots slots)
               nil))

(defun initialize-structure-class (class-name direct-superclasses precedence-list)
  (let ((class (find-class-1 class-name)))
    (setf (class.direct-superclasses class) (mapcar #'find-class-1 direct-superclasses))
    (setf (class.precedence-list class) (mapcar #'find-class-1 precedence-list))
    (setf (std-instance-layout class) +structure-class-layout+)))

(defconstant +standard-class-layout+
  (make-layout (find-class-1 'standard-class)
               '(direct-methods prototype name layout precedence-list
                 direct-superclasses direct-subclasses
                 direct-slots slots
                 direct-default-initargs default-initargs
                 finalized-p)
               nil))

(defconstant +funcallable-standard-class-layout+
  (make-layout (find-class-1 'funcallable-standard-class)
               '(direct-methods prototype name layout precedence-list
                 direct-superclasses direct-subclasses
                 direct-slots slots
                 direct-default-initargs default-initargs
                 finalized-p)
               nil))

(defun initialize-standard-class (class-name direct-superclasses precedence-list)
  (let ((class (find-class-1 class-name)))
    (setf (class.direct-superclasses class) (mapcar #'find-class-1 direct-superclasses))
    (setf (class.precedence-list class) (mapcar #'find-class-1 precedence-list))
    (setf (class.finalized-p class) t)
    (setf (std-instance-layout class) +standard-class-layout+)))

(defun initialize-funcallable-standard-class (class-name direct-superclasses precedence-list)
  (let ((class (find-class-1 class-name)))
    (setf (class.direct-superclasses class) (mapcar #'find-class-1 direct-superclasses))
    (setf (class.precedence-list class) (mapcar #'find-class-1 precedence-list))
    (setf (class.finalized-p class) t)
    (setf (std-instance-layout class) +funcallable-standard-class-layout+)))

(defun initialize-condition-class (class-name direct-superclasses precedence-list)
  (initialize-standard-class class-name direct-superclasses precedence-list))

(initialize-built-in-class 'array '(t) '(array t))
;; (initialize-built-in-class 'base-string '(string) '(base-string string vector array sequence t))
;; (initialize-built-in-class 'bignum '(integer) '(bignum integer rational real number t))
(initialize-built-in-class 'bit-vector '(vector) '(bit-vector vector array sequence t))
(initialize-built-in-class 'broadcast-stream '(stream) '(broadcast-stream stream t))

(initialize-standard-class 'metaobject
                           '(standard-object)
                           '(metaobject standard-object t))
(initialize-standard-class 'specializer
                           '(metaobject)
                           '(specializer metaobject standard-object t))
(initialize-standard-class 'eql-specializer
                           '(specializer)
                           '(eql-specializer specializer metaobject standard-object t))
(initialize-standard-class 'class
                           '(specializer)
                           '(class specializer metaobject standard-object t))
(initialize-standard-class 'built-in-class
                           '(class)
                           '(built-in-class class specializer metaobject standard-object t))
(initialize-standard-class 'forward-referenced-class
                           '(class)
                           '(forward-referenced-class class specializer metaobject standard-object t))
(initialize-standard-class 'standard-class
                           '(class)
                           '(standard-class class specializer metaobject standard-object t))
(initialize-standard-class 'funcallable-standard-class
                           '(class)
                           '(funcallable-standard-class class specializer metaobject standard-object t))

(initialize-built-in-class 'character '(t) '(character t))
(initialize-built-in-class 'complex '(number) '(complex number t))
(initialize-built-in-class 'concatenated-stream '(stream) '(concatenated-stream stream t))

(initialize-built-in-class 'cons '(list) '(cons list sequence t))
;; (initialize-built-in-class 'double-float '(float) '(double-float float real number t))
(initialize-built-in-class 'echo-stream '(stream) '(echo-stream stream t))
(initialize-built-in-class 'environment '(t) '(environment t))


;; (initialize-built-in-class 'fixnum '(integer) '(fixnum integer rational real number t))
(initialize-built-in-class 'file-stream '(stream) '(file-stream stream t))
(initialize-built-in-class 'float '(real) '(float real number t))
(initialize-built-in-class 'function '(t) '(function t))
(initialize-funcallable-standard-class 'generic-function
                                       '(metaobject funcallable-standard-object)
                                       '(generic-function metaobject funcallable-standard-object
                                         standard-object function t))
(initialize-built-in-class 'hash-table '(t) '(hash-table t))
(initialize-built-in-class 'integer '(rational) '(integer rational real number t))
(initialize-built-in-class 'layout '(t) '(layout t))
(initialize-built-in-class 'list '(sequence) '(list sequence t))
(initialize-built-in-class 'logical-pathname '(pathname) '(logical-pathname pathname t))
(initialize-standard-class 'method '(metaobject) '(method metaobject standard-object t))
;; (initialize-built-in-class 'method-combination '() '(method-combination t))
(initialize-built-in-class 'nil-vector '(string) '(nil-vector string vector array sequence t))
(initialize-built-in-class 'null '(list) '(null symbol list sequence t))
(initialize-built-in-class 'number '(t) '(number t))
(initialize-built-in-class 'package '(t) '(package t))
(initialize-built-in-class 'pathname '(t) '(pathname t))
(initialize-built-in-class 'random-state '(t) '(random-state t))
(initialize-built-in-class 'ratio '(rational) '(ratio rational real number t))
(initialize-built-in-class 'rational '(real) '(rational real number t))
(initialize-built-in-class 'readtable '(t) '(readtable t))
(initialize-built-in-class 'real '(number) '(real number t))

(initialize-built-in-class 'restart '(t) '(restart t))

(initialize-built-in-class 'sequence '(t) '(sequence t))
;; (initialize-built-in-class 'simple-array '(array) '(simple-array array t))
;; (initialize-built-in-class 'simple-base-string
;;                            '(base-string simple-string)
;;                            '(simple-base-string base-string simple-string string vector simple-array array sequence t))
;; (initialize-built-in-class 'simple-bit-vector
;;                            '(bit-vector simple-array)
;;                            '(simple-bit-vector bit-vector vector simple-array array sequence t))
;; (initialize-built-in-class 'simple-string
;;                            '(string simple-array)
;;                            '(simple-string string vector simple-array array sequence t))
;; (initialize-built-in-class 'simple-vector
;;                            '(vector simple-array)
;;                            '(simple-vector vector simple-array array sequence t))
;; (initialize-built-in-class 'single-float '(float) '(single-float float real number t))
(initialize-built-in-class 'server-socket '(t) '(server-socket t))
(initialize-built-in-class 'slime-input-stream '(stream) '(slime-input-stream stream t))
(initialize-built-in-class 'slime-output-stream '(stream) '(slime-output-stream stream t))
(initialize-built-in-class 'socket-stream '(stream) '(socket-stream stream t))

(initialize-funcallable-standard-class 'standard-generic-function
                                       '(generic-function)
                                       '(standard-generic-function generic-function metaobject
                                         funcallable-standard-object standard-object function t))

(initialize-standard-class 'standard-method
                           '(method)
                           '(standard-method method metaobject standard-object t))
(initialize-standard-class 'standard-object '(t) '(standard-object t))
(initialize-built-in-class 'stream '(t) '(stream t))
(initialize-built-in-class 'string '(vector) '(string vector array sequence t))
(initialize-built-in-class 'string-stream '(stream) '(string-stream stream t))

(initialize-standard-class 'structure-class
                           '(class)
                           '(structure-class class specializer metaobject standard-object t))

(initialize-structure-class 'structure-object '(t) '(structure-object t))

(initialize-built-in-class 'symbol '(t) '(symbol t))
(initialize-built-in-class 'synonym-stream '(stream) '(synonym-stream stream t))
(initialize-built-in-class 't nil '(t))
(initialize-built-in-class 'thread '(t) '(thread t))
(initialize-built-in-class 'two-way-stream '(stream) '(two-way-stream stream t))
(initialize-built-in-class 'vector '(array sequence) '(vector array sequence t))

;; condition classes
(initialize-condition-class 'condition '(standard-object) '(condition standard-object t))
(initialize-condition-class 'arithmetic-error
                            '(error)
                            '(arithmetic-error error serious-condition condition standard-object t))
(initialize-condition-class 'cell-error
                            '(error)
                            '(cell-error error serious-condition condition standard-object t))
(initialize-condition-class 'compiler-error '(condition) '(compiler-error condition standard-object t))
(initialize-condition-class 'compiler-unsupported-feature-error '(condition) '(compiler-unsupported-feature-error condition standard-object t))
(initialize-condition-class 'control-error
                            '(error)
                            '(control-error error serious-condition condition standard-object t))
(initialize-condition-class 'division-by-zero
                            '(arithmetic-error)
                            '(division-by-zero arithmetic-error error serious-condition condition standard-object t))
(initialize-condition-class 'end-of-file
                            '(stream-error)
                            '(end-of-file stream-error error serious-condition condition standard-object t))
(initialize-condition-class 'error '(serious-condition) '(error serious-condition condition standard-object t))
(initialize-condition-class 'file-error '(error) '(file-error error serious-condition condition standard-object t))
(initialize-condition-class 'floating-point-inexact
                            '(arithmetic-error)
                            '(floating-point-inexact arithmetic-error error serious-condition condition standard-object t))
(initialize-condition-class 'floating-point-invalid-operation
                            '(arithmetic-error)
                            '(floating-point-invalid-operation arithmetic-error error serious-condition condition standard-object t))
(initialize-condition-class 'floating-point-overflow
                            '(arithmetic-error)
                            '(floating-point-overflow arithmetic-error error serious-condition condition standard-object t))
(initialize-condition-class 'floating-point-underflow
                            '(arithmetic-error)
                            '(floating-point-underflow arithmetic-error error serious-condition condition standard-object t))
(initialize-condition-class 'package-error
                            '(error)
                            '(package-error error serious-condition condition standard-object t))
(initialize-condition-class 'parse-error
                            '(error)
                            '(parse-error error serious-condition condition standard-object t))
(initialize-condition-class 'print-not-readable '(error) '(print-not-readable error serious-condition condition standard-object t))
(initialize-condition-class 'program-error
                            '(error)
                            '(program-error error serious-condition condition standard-object t))
(initialize-condition-class 'reader-error '(parse-error stream-error) '(reader-error parse-error stream-error error serious-condition condition standard-object t))
(initialize-condition-class 'serious-condition '(condition) '(serious-condition condition standard-object t))
(initialize-condition-class 'simple-condition '(condition) '(simple-condition condition standard-object t))
(initialize-condition-class 'simple-error '(simple-condition error) '(simple-error simple-condition error serious-condition condition standard-object t))
(initialize-condition-class 'simple-type-error
                            '(simple-condition type-error)
                            '(simple-type-error simple-condition type-error error serious-condition condition standard-object t))
(initialize-condition-class 'simple-warning
                            '(simple-condition warning)
                            '(simple-warning simple-condition warning condition standard-object t))
(initialize-condition-class 'storage-condition
                            '(serious-condition)
                            '(storage-condition serious-condition condition standard-object t))
(initialize-condition-class 'stream-error '(error) '(stream-error error serious-condition condition standard-object t))
(initialize-condition-class 'style-warning '(warning) '(style-warning warning condition standard-object t))
(initialize-condition-class 'type-error '(error) '(type-error error serious-condition condition standard-object t))
(initialize-condition-class 'unbound-slot '(cell-error) '(unbound-slot cell-error error serious-condition condition standard-object t))
(initialize-condition-class 'unbound-variable '(cell-error) '(unbound-variable cell-error error serious-condition condition standard-object t))
(initialize-condition-class 'undefined-function '(cell-error) '(undefined-function cell-error error serious-condition condition standard-object t))
(initialize-condition-class 'warning '(condition) '(warning condition standard-object t))

(setf (class.layout        (find-class-1 'structure-class)) +structure-class-layout+)
(setf (std-instance-layout (find-class-1 'structure-class)) +standard-class-layout+)

(setf (class.layout        (find-class-1 'standard-class)) +standard-class-layout+)
(setf (std-instance-layout (find-class-1 'standard-class)) +standard-class-layout+)

(setf (class.layout        (find-class-1 'funcallable-standard-class)) +funcallable-standard-class-layout+)
(setf (std-instance-layout (find-class-1 'funcallable-standard-class)) +standard-class-layout+)

(dolist (class (mapcar #'find-class-1 '(class built-in-class forward-referenced-class)))
  (setf (class.layout class)
        (make-layout class
                     '(direct-methods prototype name layout precedence-list
                       direct-superclasses direct-subclasses)
                     nil))
  (setf (std-instance-layout class) +standard-class-layout+))

(setf (std-instance-layout (find-class-1 'standard-object)) +standard-class-layout+)

;; REVIEW
(initialize-funcallable-standard-class 'funcallable-standard-object
                                       '(standard-object function)
                                       '(funcallable-standard-object standard-object function t))

(initialize-standard-class 'slot-definition
                           '(metaobject)
                           '(slot-definition metaobject standard-object t))
(initialize-standard-class 'direct-slot-definition
                           '(slot-definition)
                           '(direct-slot-definition slot-definition metaobject standard-object t))
(initialize-standard-class 'effective-slot-definition
                           '(slot-definition)
                           '(effective-slot-definition slot-definition metaobject standard-object t))
(initialize-standard-class 'standard-slot-definition
                           '(slot-definition)
                           '(standard-slot-definition slot-definition metaobject standard-object t))
(initialize-standard-class 'standard-direct-slot-definition
                           '(standard-slot-definition direct-slot-definition)
                           '(standard-direct-slot-definition standard-slot-definition direct-slot-definition slot-definition metaobject standard-object t))
(initialize-standard-class 'standard-effective-slot-definition
                           '(standard-slot-definition effective-slot-definition)
                           '(standard-effective-slot-definition standard-slot-definition effective-slot-definition slot-definition metaobject standard-object t))

;; slot-definition
(defmacro slot-definition.name                          (arg) `(iref ,arg 0))
(defmacro slot-definition.initargs                      (arg) `(iref ,arg 1))
(defmacro slot-definition.initform                      (arg) `(iref ,arg 2))
(defmacro slot-definition.initfunction                  (arg) `(iref ,arg 3))
(defmacro slot-definition.allocation                    (arg) `(iref ,arg 4))
(defmacro slot-definition.type                          (arg) `(iref ,arg 5))
(defmacro slot-definition.class                         (arg) `(iref ,arg 6))
(defmacro slot-definition.documentation                 (arg) `(iref ,arg 7))
;; direct-slot-definition
(defmacro direct-slot-definition.readers                (arg) `(iref ,arg 8))
(defmacro direct-slot-definition.writers                (arg) `(iref ,arg 9))
;; effective-slot-definition
(defmacro effective-slot-definition.location            (arg) `(iref ,arg 8))
(defmacro effective-slot-definition.allocation-class    (arg) `(iref ,arg 9))

(let ((class (find-class-1 'standard-direct-slot-definition)))
  (setf (class.layout class) (make-layout class
                                          '(name initargs initform initfunction
                                            allocation %type %class %documentation
                                            readers writers)
                                          nil)))

(let ((class (find-class-1 'standard-effective-slot-definition)))
  (setf (class.layout class) (make-layout class
                                          '(name initargs initform initfunction
                                            allocation %type %class %documentation
                                            location allocation-class)
                                          nil)))

(fmakunbound 'initialize-built-in-class)
(unintern 'initialize-built-in-class)
(fmakunbound 'initialize-structure-class)
(unintern 'initialize-structure-class)
(fmakunbound 'initialize-standard-class)
(unintern 'initialize-standard-class)
(fmakunbound 'initialize-funcallable-standard-class)
(unintern 'initialize-funcallable-standard-class)
(fmakunbound 'initialize-condition-class)
(unintern 'initialize-condition-class)
