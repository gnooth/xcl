;;; instruction.lisp
;;;
;;; Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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

;; Maybe one byte, maybe many bytes, maybe a label, maybe some abstraction...
;; (defstruct instruction
;;   kind ; :BYTE, :BYTES, :CALL, :LABEL
;;   size
;;   data
;;   )

(eval-when (:compile-toplevel)
  (declaim (inline make-instruction)))
(defknown make-instruction (*) simple-vector)
(defun make-instruction (kind size data)
  (declare (optimize speed (safety 0)))
  (declare (type keyword kind))
  (declare (type fixnum size))
  (vector kind size data))

(eval-when (:compile-toplevel)
  (declaim (inline instruction-kind)))
(defknown instruction-kind (t) t)
(defun instruction-kind (instruction)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector instruction))
  (aref instruction 0))

(eval-when (:compile-toplevel)
  (declaim (inline set-instruction-kind)))
(defknown set-instruction-kind (t t) t)
(defun set-instruction-kind (instruction kind)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector instruction))
  (setf (aref instruction 0) kind))

;; (sys::assign-setf-inverse 'instruction-kind 'set-instruction-kind)

(eval-when (:compile-toplevel)
  (declaim (inline instruction-size)))
(defknown instruction-size (t) t)
(defun instruction-size (instruction)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector instruction))
  (aref instruction 1))

(defknown set-instruction-size (t t) t)
(defun set-instruction-size (instruction size)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector instruction))
  (setf (aref instruction 1) size))

;; (sys::assign-setf-inverse 'instruction-size 'set-instruction-size)

;; (defknown instruction-code (t) t)
;; (defun instruction-code (instruction)
;;   (third instruction))

(eval-when (:compile-toplevel)
  (declaim (inline instruction-data)))
(defknown instruction-data (t) t)
(defun instruction-data (instruction)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector instruction))
  (aref instruction 2))

(defknown set-instruction-data (t t) t)
(defun set-instruction-data (instruction data)
  (declare (optimize speed (safety 0)))
  (declare (type simple-vector instruction))
  (setf (aref instruction 1) data))

(defknown instruction-jump-test (intruction) t)
(defun instruction-jump-test (instruction)
  (when (memq (instruction-kind instruction) '(:jmp :jmp-short))
    (%car (instruction-data instruction))))

(defknown instruction-jump-target (instruction) t)
(defun instruction-jump-target (instruction)
  (when (memq (instruction-kind instruction) '(:jmp :jmp-short))
    (%cadr (instruction-data instruction))))

(defun print-instruction (instruction)
  (let ((kind (instruction-kind instruction))
        (size (instruction-size instruction))
        (data (instruction-data instruction)))
    (case kind
      (:label
       (format t "~S~%" (list :label data)))
      (t
       (format t "~S~%" (list kind size data))))))

(defknown calculate-code-vector-length (t) index)
(defun calculate-code-vector-length (instructions)
  (let ((length 0))
    (dolist (instruction instructions length)
      (let ((instruction-kind (instruction-kind instruction)))
        (cond ((eq instruction-kind :label)
               (let ((symbol (instruction-data instruction)))
                 (setf (symbol-global-value symbol) length)))
              ((eq instruction-kind :dead))
              (t
               (incf length (instruction-size instruction))))))))

(defknown opcode-for-test (t) (unsigned-byte 8))
(defun opcode-for-test (test)
  (declare (optimize speed (safety 0)))
  (declare (type keyword test))
  (case test
    ((:z :e)   #x74)
    ((:nz :ne) #x75)
    (:l        #x7c)
    (:le       #x7e)
    ((:nl :ge) #x7d)
    (:g        #x7f)
    (:ng       #x7e)
    (:o        #x70)
    (:no       #x71)
    (:s        #x78)
    (t
     (error "unsupported test ~S" test))))

#+x86
(defun generate-code-vector (instructions constants)
  (declare (optimize speed (safety 0)))
  (tagbody
   top
   (let* ((length (calculate-code-vector-length instructions))
          (code-vector (make-code-vector length))
          (i 0))
     (declare (type index i))
     (macrolet ((emit (x)
                      `(progn (setf (aref code-vector i) ,x) (incf i)))
                (emit-32bit-displacement (x)
                                         (let ((var (gensym)))
                                           `(let ((,var ,x))
                                              (emit (ldb (byte 8  0) ,var))
                                              (emit (ldb (byte 8  8) ,var))
                                              (emit (ldb (byte 8 16) ,var))
                                              (emit (ldb (byte 8 24) ,var))))))
       (dolist (instruction instructions)
         (case (instruction-kind instruction)
           (:dead
            )
           (:byte
            (setf (aref code-vector i) (instruction-data instruction))
            (incf i))
           (:bytes
            (dolist (byte (instruction-data instruction))
              (setf (aref code-vector i) byte)
              (incf i)))
           (:constant
            (let* ((form (instruction-data instruction))
                   (x (value-to-ub32 form)))
              (unless (and (symbolp form)
                           (eq (symbol-package (truly-the symbol form)) +cl-package+))
                (pushnew form constants :test 'eq))
              (emit (ldb (byte 8  0) x))
              (emit (ldb (byte 8  8) x))
              (emit (ldb (byte 8 16) x))
              (emit (ldb (byte 8 24) x))))
           (:function                                           ; REVIEW
            (let* ((symbol (instruction-data instruction))
                   (function (symbol-function symbol))
                   (x (value-to-ub32 function)))
              (unless (kernel-function-p function)
                (pushnew function constants :test 'eq))
              (emit (ldb (byte 8  0) x))
              (emit (ldb (byte 8  8) x))
              (emit (ldb (byte 8 16) x))
              (emit (ldb (byte 8 24) x))))
           (:call
            (let* ((here (+ (vector-data code-vector) i))
                   (what (instruction-data instruction))
                   (address (etypecase what
                              (integer what)
                              (string
                               (or (gethash2-1 what *runtime-names*)
                                   (error "Unknown runtime name ~S." what)))
                              (symbol
                               (cond ((symbol-package (truly-the symbol what))
                                      (function-code (symbol-function what)))
                                     ;; an uninterned symbol is a label
                                     (t
                                      (+ (vector-data code-vector) (symbol-global-value what)))))))
                   (displacement (- address (+ here 5))))
              (emit #xe8)
              (emit-32bit-displacement displacement)))
           (:recurse
            (let* ((displacement (- 0 (+ i 5))))
              (emit #xe8)
              (emit-32bit-displacement displacement)))
           (:exit
;;             (when (eql (instruction-size instruction) 2)
;;               (emit #xc9)) ; leave
;;             (emit #xc3)
            (dolist (byte (instruction-data instruction))
              (setf (aref code-vector i) byte)
              (incf i)))
           ((:jmp :jmp-short)
            (let* ((short (eq (instruction-kind instruction) :jmp-short))
                   (here (+ (vector-data code-vector) i))
                   (args (instruction-data instruction))
                   (test (car args))
                   (label (cadr args))
                   (size (instruction-size instruction))
                   (displacement (- (+ (vector-data code-vector) (symbol-global-value label)) (+ here size))))
              (aver (eql (length args) 2))
              (aver (or (keywordp test) (eq test t)))
              (when short
                (when (or (< displacement -128) (> displacement 127))
                  ;; displacement is out of range for short jump
                  (set-instruction-kind instruction :jmp)
                  (set-instruction-size instruction (if (eq test t) 5 6))
                  (go top))) ; start over
              (cond ((or (eq test t) ; unconditional jump
                         (eq test :jump-table))
                     (emit (if short #xeb #xe9)))
                    (t
                     (let ((byte (opcode-for-test test)))
                       (declare (type (unsigned-byte 8) byte))
                       (cond (short
                              (emit byte))
                             (t
                              (emit #x0f)
                              (emit (+ byte #x10)))))))
              (cond (short
                     (emit (ldb (byte 8 0) displacement)))
                    (t
                     (emit-32bit-displacement displacement)))))
           (:label
            )
           (t
            (error "unsupported")))))
     (aver (eql i length))
     (return-from generate-code-vector (values code-vector constants)))))

#+x86-64
(defun generate-code-vector (instructions constants)
  (declare (optimize speed (safety 0)))
  (tagbody
   top
   (let* ((length (calculate-code-vector-length instructions))
          (code-vector (make-code-vector length))
          (i 0))
     (declare (type index i))
     (macrolet ((emit (x)
                      `(progn (setf (aref code-vector i) ,x) (incf i)))
                (emit-32bit-displacement (x)
                                         (let ((var (gensym)))
                                           `(let ((,var ,x))
                                              (emit (ldb (byte 8  0) ,var))
                                              (emit (ldb (byte 8  8) ,var))
                                              (emit (ldb (byte 8 16) ,var))
                                              (emit (ldb (byte 8 24) ,var))))))
       (dolist (instruction instructions)
         ;;            (when *debug-compiler*
         ;;              (format t "instruction = ")
         ;;              (print-instruction instruction))
         (case (instruction-kind instruction)
           (:dead
            )
           (:byte
            (setf (aref code-vector i) (instruction-data instruction))
            (incf i))
           (:bytes
            (dolist (byte (instruction-data instruction))
              (setf (aref code-vector i) byte)
              (incf i)))
           (:constant
            (let* ((form (instruction-data instruction))
                   (x (value-to-ub64 form)))
              (unless (and (symbolp form)
                           (eq (symbol-package (truly-the symbol form)) +cl-package+))
                (pushnew form constants :test 'eq))
              (emit (ldb (byte 8  0) x))
              (emit (ldb (byte 8  8) x))
              (emit (ldb (byte 8 16) x))
              (emit (ldb (byte 8 24) x))
              (emit (ldb (byte 8 32) x))
              (emit (ldb (byte 8 40) x))
              (emit (ldb (byte 8 48) x))
              (emit (ldb (byte 8 56) x))))
           (:constant-32
            (let* ((form (instruction-data instruction))
                   (x (value-to-ub64 form)))
              (declare (type (integer 0 #x7fffffff) x)) ; small data model
              (unless (and (symbolp form)
                           (eq (symbol-package (truly-the symbol form)) +cl-package+))
                (pushnew form constants :test 'eq))
              (emit (ldb (byte 8  0) x))
              (emit (ldb (byte 8  8) x))
              (emit (ldb (byte 8 16) x))
              (emit (ldb (byte 8 24) x))))
           (:function                                           ; REVIEW
            (let* ((symbol (instruction-data instruction))
                   (function (symbol-function symbol))
                   (x (value-to-ub64 function)))
              (unless (kernel-function-p function)
                (pushnew function constants :test 'eq))
              (emit (ldb (byte 8  0) x))
              (emit (ldb (byte 8  8) x))
              (emit (ldb (byte 8 16) x))
              (emit (ldb (byte 8 24) x))))
           (:call
            (let* ((here (+ (vector-data code-vector) i))
                   (what (instruction-data instruction))
                   (address (etypecase what
                              (integer what)
                              (string
                               (or (gethash2-1 what *runtime-names*)
                                   (error "Unknown runtime name ~S." what)))
                              (symbol
                               (cond ((symbol-package (truly-the symbol what))
                                      (function-code (symbol-function what)))
                                     ;; an uninterned symbol is a label
                                     (t
                                      (+ (vector-data code-vector) (symbol-global-value what)))))))
                   (displacement (- address (+ here 5))))
              (emit #xe8)
              (emit-32bit-displacement displacement)))
           (:recurse
            (let* ((displacement (- 0 (+ i 5))))
              (emit #xe8)
              (emit-32bit-displacement displacement)))
           (:exit
            ;;               (unless (compiland-omit-frame-pointer *current-compiland*)
            ;;                 (emit #xc9)) ; leave
            ;;               (emit #xc3)

            ;;               (let ((epilog (compiland-epilog *current-compiland*)))
            ;;                 (dotimes (index (length epilog))
            ;;                   (emit (aref epilog index))))
            (dolist (byte (instruction-data instruction))
              (setf (aref code-vector i) byte)
              (incf i))
            )
           ((:jmp :jmp-short)
            (let* ((short (eq (instruction-kind instruction) :jmp-short))
                   (here (+ (vector-data code-vector) i))
                   (args (instruction-data instruction))
                   (test (car args))
                   (label (cadr args))
                   (size (instruction-size instruction))
                   (displacement (- (+ (vector-data code-vector) (symbol-global-value label)) (+ here size))))
;;               (aver (eql (length args) 2))
;;               (aver (or (keywordp test) (eq test t)))
              (when short
                (when (or (< displacement -128) (> displacement 127))
                  ;; displacement is out of range for short jump
                  (set-instruction-kind instruction :jmp)
                  (set-instruction-size instruction (if (eq test t) 5 6))
                  (go top))) ; start over
              (cond ((or (eq test t) ; unconditional jump
                         (eq test :jump-table))
                     (emit (if short #xeb #xe9)))
                    (t
                     (let ((byte (opcode-for-test test)))
                       (declare (type (unsigned-byte 8) byte))
                       (cond (short
                              (emit byte))
                             (t
                              (emit #x0f)
                              (emit (+ byte #x10)))))))
              (cond (short
                     (emit (ldb (byte 8 0) displacement)))
                    (t
                     (emit-32bit-displacement displacement)))))
           (:label
            )
           (t
            (error "unsupported")))))
;;      (aver (eql i length))
     (return-from generate-code-vector (values code-vector constants)))))
