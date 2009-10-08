;;; canonicalize-type.lisp
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

(export '(canonicalize-type deftype-expander))

;; FIXME defglobal
(defparameter *canonical-types* (make-hash-table :test 'equal))

(eval-when (:compile-toplevel)
  (declaim (inline deftype-expander)))
(defun deftype-expander (name)
  (get name 'deftype-expander))

(defun set-deftype-expander (name expander)
  (put name 'deftype-expander expander)
  (clrhash *canonical-types*))

(assign-setf-inverse 'deftype-expander 'set-deftype-expander)

(set-deftype-expander 'index (lambda () '(integer 0 (#.most-positive-fixnum))))

;; (defconstant +atomic-types+ (make-hash-table :test 'eq))

;; (puthash 'bit +atomic-types+ '(integer 0 1))
;; (puthash 'cons +atomic-types+ 'cons)
;; (puthash 'fixnum +atomic-types+ '(integer #.most-negative-fixnum #.most-positive-fixnum))
;; (puthash 'signed-byte +atomic-types+ 'integer)
;; (puthash 'unsigned-byte +atomic-types+ '(integer 0 *))
;; (puthash 'base-char +atomic-types+ 'base-char)
;; (puthash 'short-float +atomic-types+ 'single-float)
;; (puthash 'long-float +atomic-types+ 'double-float)
;; (puthash 'array +atomic-types+ '(array * *))
;; (puthash 'simple-array +atomic-types+ '(simple-array * *))
;; (puthash 'vector +atomic-types+ '(array * (*)))
;; (puthash 'simple-vector +atomic-types+ '(simple-array t (*)))
;; (puthash 'bit-vector +atomic-types+ '(bit-vector *))
;; (puthash 'simple-bit-vector +atomic-types+ '(simple-bit-vector *))
;; (puthash 'base-string +atomic-types+ '(array base-char (*)))
;; (puthash 'simple-base-string +atomic-types+ '(simple-array base-char (*)))
;; (puthash 'string +atomic-types+
;;          '(or (array character (*)) (array base-char (*)) (array nil (*))))
;; (puthash 'simple-string +atomic-types+
;;          '(or (simple-array character (*)) (simple-array base-char (*)) (simple-array nil (*))))

(defun expand-deftype (type)
  (let (name args)
    (loop
      (if (consp type)
          (setq name (%car type)
                args (%cdr type))
          (setq name type
                args nil))
      (let ((expander (and (symbolp name) (deftype-expander name))))
        (if expander
            (setq type (apply expander args))
            (return)))))
  type)

(defun canonicalize-and-type (type)
  (declare (type cons type))
  (when (length-eql type 3)
    (let ((type1 (canonicalize-type (%cadr type)))
          (type2 (canonicalize-type (%caddr type))))
      (when (and (eq type1 'SYMBOL)
                 (eq type2 'LIST))
        (return-from canonicalize-and-type 'NULL))
      (when (and (eq type1 'LIST)
                 (eq type2 'SYMBOL))
        (return-from canonicalize-and-type 'NULL))
      (when (and (consp type1) (eq (%car type1) 'INTEGER))
        (when (and (consp type2) (eq (%car type2) 'INTEGER))
          (let ((low1  (%cadr  type1))
                (high1 (%caddr type1))
                (low2  (%cadr  type2))
                (high2 (%caddr type2))
                low-result
                high-result)
            (cond ((and (integerp high1)
                        (integerp low2)
                        (> low2 high1))
                   (return-from canonicalize-and-type nil))
                  ((and (integerp low1)
                        (integerp high2)
                        (> low1 high2))
                   (return-from canonicalize-and-type nil)))
            (cond ((eq low1 '*)
                   (setq low-result low2))
                  ((eq low2 '*)
                   (setq low-result low1))
                  (t
                   (setq low-result (max low1 low2))))
            (cond ((eq high1 '*)
                   (setq high-result high2))
                  ((eq high2 '*)
                   (setq high-result high1))
                  (t
                   (setq high-result (min high1 high2))))
            (return-from canonicalize-and-type (list 'INTEGER low-result high-result)))))))
    ;; otherwise...
    (list* 'AND (mapcar 'canonicalize-type (%cdr type))))

(defun canonicalize-atomic-type (type)
  (if (deftype-expander type)
      (canonicalize-type (expand-deftype type))
      (case type
        (BIT
         '(integer 0 1))
        (FIXNUM
         '(integer #.most-negative-fixnum #.most-positive-fixnum))
        (INTEGER
         '(integer * *))
        (SIGNED-BYTE
         '(integer * *))
        (UNSIGNED-BYTE
         '(integer 0 *))
        (BASE-CHAR
         'base-char)
        (SHORT-FLOAT
         'single-float)
        (LONG-FLOAT
         'double-float)
        (ARRAY
         '(array * *))
        (SIMPLE-ARRAY
         '(simple-array * *))
        (VECTOR
         '(array * (*)))
        (SIMPLE-VECTOR
         '(simple-array t (*)))
        (BIT-VECTOR
         '(array (integer 0 1) (*)))
        (SIMPLE-BIT-VECTOR
         '(simple-array (integer 0 1) (*)))
        (BASE-STRING
         '(array base-char (*)))
        (SIMPLE-BASE-STRING
         '(simple-array base-char (*)))
        (STRING
         '(or
           (array character (*))
           (array base-char (*))
           (array nil       (*))))
        (SIMPLE-STRING
         '(or
           (simple-array character (*))
           (simple-array base-char (*))
           (simple-array nil       (*))))
        (CONS
         '(cons t t))
        (t
         type))))

(defun canonicalize-eql-type (type)
  (declare (type list type))
  (when (length-eql type 2)
    (let ((object (%cadr type)))
      (when (integerp object)
        (return-from canonicalize-eql-type (list 'INTEGER object object)))))
  type)

(defun canonicalize-or-type (type)
  (declare (type cons type))
  (when (length-eql type 3)
    (let ((type1 (%cadr type))
          (type2 (%caddr type)))
      (when (and (eq type1 'CONS)
                 (eq type2 'NULL))
        (return-from canonicalize-or-type 'LIST))
      (when (and (eq type1 'NULL)
                 (eq type2 'CONS))
        (return-from canonicalize-or-type 'LIST))))
  (list* (%car type) (mapcar '%canonicalize-type (%cdr type))))

(defun %canonicalize-type (type)
  (cond ((classp type)
         (return-from %canonicalize-type type))
        ((symbolp type)
         (return-from %canonicalize-type (canonicalize-atomic-type type)))
        ((and (consp type)
              (memq (%car type) '(and or not eql member satisfies mod values)))
         (cond ((eq (%car type) 'AND)
                (return-from %canonicalize-type (canonicalize-and-type type)))
               ((eq (%car type) 'OR)
                (return-from %canonicalize-type (canonicalize-or-type type)))
               ((eq (%car type) 'MOD)
                (return-from %canonicalize-type (list 'INTEGER 0 (1- (cadr type)))))
               ((eq (%car type) 'EQL)
                (return-from %canonicalize-type (canonicalize-eql-type type)))
               (t
                (return-from %canonicalize-type type)))))
  ;; fall through...
  (let (name args)
    (loop
      (if (consp type)
          (setq name (%car type)
                args (%cdr type))
          (setq name type
                args nil))
      (let ((expander (and (symbolp name) (deftype-expander name))))
        (if expander
            (setq type (apply expander args))
            (return))))
    (case name
      (INTEGER
       (if args
           (let ((low (%car args))
                 (high (if (endp (%cdr args)) '* (%cadr args))))
             (when (consp low)
               (setq low (1+ (%car low))))
             (when (consp high)
               (setq high (1- (%car high))))
             (return-from %canonicalize-type (list name low high)))
           (return-from %canonicalize-type name)))
      (CONS
       (let* ((len (length args))
              (car-typespec (if (> len 0) (%car args) t))
              (cdr-typespec (if (> len 1) (%cadr args) t)))
         (unless (and car-typespec cdr-typespec)
           (return-from %canonicalize-type nil))
         (when (eq car-typespec '*)
           (setq car-typespec t))
         (when (eq cdr-typespec '*)
           (setq cdr-typespec t))
         (return-from %canonicalize-type (cons name (list car-typespec cdr-typespec)))))
      (SIGNED-BYTE
       (if (or (null args) (eq (car args) '*))
           (return-from %canonicalize-type '(integer * *))
           (return-from %canonicalize-type
                        (list 'integer
                              (- (expt 2 (1- (%car args))))
                              (1- (expt 2 (1- (%car args))))))))
      (UNSIGNED-BYTE
       (if (or (null args) (eq (car args) '*))
           (return-from %canonicalize-type '(integer 0 *)))
           (return-from %canonicalize-type (list 'integer 0 (1- (expt 2 (%car args))))))
      ((ARRAY SIMPLE-ARRAY)
       (unless args
         (return-from %canonicalize-type (list name '* '*)))
       (when (eql (length args) 1)
         (setq args (append args '(*))))
       (setf (car args) (%canonicalize-type (car args)))
       (return-from %canonicalize-type (cons name args)))
      (VECTOR
       (case (length args)
         (0
          (return-from %canonicalize-type '(array * (*))))
         (1
          (%setcar args (%canonicalize-type (%car args)))
          (return-from %canonicalize-type (list 'array (%car args) '(*))))
         (2
          (%setcar args (%canonicalize-type (%car args)))
          (return-from %canonicalize-type (list 'array (%car args) (list (%cadr args)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (SIMPLE-VECTOR
       (case (length args)
         (0
          (return-from %canonicalize-type '(simple-array t (*))))
         (1
          (return-from %canonicalize-type (list 'simple-array t (list (%car args)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (BIT-VECTOR
       (case (length args)
         (0
          (return-from %canonicalize-type '(array (integer 0 1) (*))))
         (1
          (return-from %canonicalize-type (list 'array '(integer 0 1) (list (%car args)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (SIMPLE-BIT-VECTOR
       (case (length args)
         (0
          (return-from %canonicalize-type '(simple-array (integer 0 1) *)))
         (1
          (return-from %canonicalize-type (list 'simple-array '(integer 0 1) (list (%car args)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (STRING
       (if args
           (return-from %canonicalize-type
                        (list 'or
                              (list 'array 'character (list (car args)))
                              (list 'array 'base-char (list (%car args)))
                              (list 'array nil        (list (%car args)))))
           (return-from %canonicalize-type
                        (list 'or
                              '(array character (*))
                              '(array base-char (*))
                              '(array nil (*))))))
      (SIMPLE-STRING
       (if args
           (return-from %canonicalize-type
                        (list 'or
                              (list 'simple-array 'character (list (car args)))
                              (list 'simple-array 'base-char (list (%car args)))
                              (list 'simple-array nil        (list (%car args)))))
           (return-from %canonicalize-type
                        (list 'or
                              '(simple-array character (*))
                              '(simple-array base-char (*))
                              '(simple-array nil       (*))))))
      (BASE-STRING
       (if args
           (return-from %canonicalize-type (list 'array 'base-char (list (car args))))
           (return-from %canonicalize-type '(array base-char (*)))))
      (SIMPLE-BASE-STRING
       (if args
           (return-from %canonicalize-type (list 'simple-array 'base-char (list (car args))))
           (return-from %canonicalize-type '(simple-array base-char (*)))))
      (SHORT-FLOAT
       (setq name 'single-float))
      (LONG-FLOAT
       (setq name 'double-float))
      (COMPLEX
        (cond ((null args)
               (return-from %canonicalize-type 'complex))
              ((eq (car args) 'short-float)
               (return-from %canonicalize-type '(complex single-float)))
              ((eq (car args) 'long-float)
               (return-from %canonicalize-type '(complex double-float))))))
    (if args (cons name args) name)))

(defun canonicalize-type (type)
  (let ((canonical-type (gethash2-1 type *canonical-types*)))
    (unless canonical-type
      (setq canonical-type (%canonicalize-type type))
      (when canonical-type
        ;; We don't want to put EQL or MEMBER type specifiers in an EQUAL hashtable!
        ;; (subtypep.eql.1, subtypep.eql.2, subtypep.member.18)
        (unless (and (consp canonical-type) (memq (%car canonical-type) '(EQL MEMBER)))
          (setf (gethash type *canonical-types*) canonical-type))))
    canonical-type))
