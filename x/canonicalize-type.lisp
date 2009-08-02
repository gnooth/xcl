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

(defconstant +canonical-types+ (make-hash-table :test 'equal))

(defun deftype-expander (name)
  (get name 'deftype-expander))

(defun set-deftype-expander (name expander)
  (put name 'deftype-expander expander)
  (clrhash +canonical-types+))

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
  (let (tp i)
    (loop
      (if (consp type)
          (setq tp (%car type)
                i  (%cdr type))
          (setq tp type
                i  nil))
      (let ((expander (and (symbolp tp) (deftype-expander tp))))
        (if expander
            (setq type (apply expander i))
            (return)))))
  type)

(defun canonicalize-and-type (type)
  (declare (type cons type))
  (when (eql (length type) 3)
    (let ((type1 (canonicalize-type (%cadr type)))
          (type2 (canonicalize-type (%caddr type))))
      (when (and (consp type1) (eq (%car type1) 'INTEGER))
        (when (and (consp type2) (eq (%car type2) 'INTEGER))
          (let ((low1  (%cadr  type1))
                (high1 (%caddr type1))
                (low2  (%cadr  type2))
                (high2 (%caddr type2))
                low-result
                high-result)
            (cond ((and (numberp high1)
                        (numberp low2)
                        (> low2 high1))
                   (return-from canonicalize-and-type nil))
                  ((and (numberp low1)
                        (numberp high2)
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
  (when (eql (length type) 2)
    (let ((object (%cadr type)))
      (when (integerp object)
        (return-from canonicalize-eql-type (list 'INTEGER object object)))))
  type)

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
                (return-from %canonicalize-type (list* (%car type) (mapcar '%canonicalize-type (%cdr type)))))
               ((eq (%car type) 'MOD)
                (return-from %canonicalize-type (list 'INTEGER 0 (1- (cadr type)))))
               ((eq (%car type) 'EQL)
                (return-from %canonicalize-type (canonicalize-eql-type type)))
               (t
                (return-from %canonicalize-type type)))))
  ;; fall through...
  (let (tp i)
    (loop
      (if (consp type)
          (setq tp (%car type)
                i  (%cdr type))
          (setq tp type
                i  nil))
      (let ((expander (and (symbolp tp) (deftype-expander tp))))
        (if expander
            (setq type (apply expander i))
            (return))))
    (case tp
      (INTEGER
       (if i
           (let ((low (%car i))
                 (high (if (endp (%cdr i)) '* (%cadr i))))
             (when (consp low)
               (setq low (1+ (%car low))))
             (when (consp high)
               (setq high (1- (%car high))))
             (return-from %canonicalize-type (list tp low high)))
           (return-from %canonicalize-type tp)))
      (CONS
       (let* ((len (length i))
              (car-typespec (if (> len 0) (%car i) t))
              (cdr-typespec (if (> len 1) (%cadr i) t)))
         (unless (and car-typespec cdr-typespec)
           (return-from %canonicalize-type nil))
         (when (eq car-typespec '*)
           (setq car-typespec t))
         (when (eq cdr-typespec '*)
           (setq cdr-typespec t))
         (return-from %canonicalize-type (cons tp (list car-typespec cdr-typespec)))))
      (SIGNED-BYTE
       (if (or (null i) (eq (car i) '*))
           (return-from %canonicalize-type '(integer * *))
           (return-from %canonicalize-type
                        (list 'integer
                              (- (expt 2 (1- (%car i))))
                              (1- (expt 2 (1- (%car i))))))))
      (UNSIGNED-BYTE
       (if (or (null i) (eq (car i) '*))
           (return-from %canonicalize-type '(integer 0 *)))
           (return-from %canonicalize-type (list 'integer 0 (1- (expt 2 (%car i))))))
      ((ARRAY SIMPLE-ARRAY)
       (unless i
         (return-from %canonicalize-type (list tp '* '*)))
       (when (eql (length i) 1)
         (setq i (append i '(*))))
       (setf (car i) (%canonicalize-type (car i)))
       (return-from %canonicalize-type (cons tp i)))
      (VECTOR
       (case (length i)
         (0
          (return-from %canonicalize-type '(array * (*))))
         (1
          (%setcar i (%canonicalize-type (%car i)))
          (return-from %canonicalize-type (list 'array (%car i) '(*))))
         (2
          (%setcar i (%canonicalize-type (%car i)))
          (return-from %canonicalize-type (list 'array (%car i) (list (%cadr i)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (SIMPLE-VECTOR
       (case (length i)
         (0
          (return-from %canonicalize-type '(simple-array t (*))))
         (1
          (return-from %canonicalize-type (list 'simple-array t (list (%car i)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (BIT-VECTOR
       (case (length i)
         (0
          (return-from %canonicalize-type '(array (integer 0 1) (*))))
         (1
          (return-from %canonicalize-type (list 'array '(integer 0 1) (list (%car i)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (SIMPLE-BIT-VECTOR
       (case (length i)
         (0
          (return-from %canonicalize-type '(simple-array (integer 0 1) *)))
         (1
          (return-from %canonicalize-type (list 'simple-array '(integer 0 1) (list (%car i)))))
         (t
          (error "Invalid type specifier ~S." type))))
      (STRING
       (if i
           (return-from %canonicalize-type
                        (list 'or
                              (list 'array 'character (list (car i)))
                              (list 'array 'base-char (list (%car i)))
                              (list 'array nil        (list (%car i)))))
           (return-from %canonicalize-type
                        (list 'or
                              '(array character (*))
                              '(array base-char (*))
                              '(array nil (*))))))
      (SIMPLE-STRING
       (if i
           (return-from %canonicalize-type
                        (list 'or
                              (list 'simple-array 'character (list (car i)))
                              (list 'simple-array 'base-char (list (%car i)))
                              (list 'simple-array nil        (list (%car i)))))
           (return-from %canonicalize-type
                        (list 'or
                              '(simple-array character (*))
                              '(simple-array base-char (*))
                              '(simple-array nil       (*))))))
      (BASE-STRING
       (if i
           (return-from %canonicalize-type (list 'array 'base-char (list (car i))))
           (return-from %canonicalize-type '(array base-char (*)))))
      (SIMPLE-BASE-STRING
       (if i
           (return-from %canonicalize-type (list 'simple-array 'base-char (list (car i))))
           (return-from %canonicalize-type '(simple-array base-char (*)))))
      (SHORT-FLOAT
       (setq tp 'single-float))
      (LONG-FLOAT
       (setq tp 'double-float))
      (COMPLEX
        (cond ((null i)
               (return-from %canonicalize-type 'complex))
              ((eq (car i) 'short-float)
               (return-from %canonicalize-type '(complex single-float)))
              ((eq (car i) 'long-float)
               (return-from %canonicalize-type '(complex double-float))))))
    (if i (cons tp i) tp)))

(defun canonicalize-type (type)
  (let ((canonical-type (gethash2-1 type +canonical-types+)))
    (unless canonical-type
      (setq canonical-type (%canonicalize-type type))
      (when canonical-type
        ;; We don't want to put EQL or MEMBER type specifiers in an EQUAL hashtable!
        ;; (subtypep.eql.1, subtypep.eql.2, subtypep.member.18)
        (unless (and (consp canonical-type) (memq (%car canonical-type) '(eql member)))
          (setf (gethash type +canonical-types+) canonical-type))))
    canonical-type))
