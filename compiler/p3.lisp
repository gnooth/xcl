;;; p3-common.lisp
;;;
;;; Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

(declaim (inline add-ir2-instruction))
(defun add-ir2-instruction (instruction code)
  (vector-push-extend instruction code))

(defun add-instruction (instruction code compile-file-p)
  (when (and compile-file-p
             (eq (instruction-kind instruction) ':bytes)
             (plusp (length code)))
    (let ((last-instruction (aref code (1- (length code)))))
      (when (eq (instruction-kind last-instruction) :bytes)
        (let* ((new-size (+ (instruction-size instruction)
                            (instruction-size last-instruction)))
               (new-data (nconc (instruction-data last-instruction)
                                (instruction-data instruction))))
          (set-instruction-size last-instruction new-size)
          (set-instruction-data last-instruction new-data)
          (return-from add-instruction)))))
  (vector-push-extend instruction code))

(defun convert-binary-data ()
  (let ((code *code*))
    (declare (type simple-vector code))
    (dotimes (i (length code))
      (let ((instruction (aref code i)))
        (when (eq (instruction-kind instruction) :bytes)
          (setf (aref code i)
                (coerce (the list (instruction-data instruction))
                        '(simple-array (unsigned-byte 8) 1))))))))

(defun fix-jumps ()
  (finish-output)
  (let* ((code *code*))
    (declare (type simple-vector code))
    (loop
      (let ((length 0))
        (dotimes (i (length code))
          (let ((instruction (aref code i)))
            (if (instruction-p instruction)
                (let ((instruction-kind (instruction-kind instruction)))
                  (cond ((eq instruction-kind :label)
                         (let ((symbol (instruction-data instruction)))
                           (setf (symbol-global-value symbol) length)))
                        ((eq instruction-kind :dead))
                        (t
                         (incf length (instruction-size instruction)))))
                (progn
                  ;; binary data
                  (incf length (length instruction)))))))
      (let ((changed nil)
            (here 0))
        (dotimes (i (length code))
          (let ((instruction (aref code i)))
            (cond ((instruction-p instruction)
                   (cond ((eq (instruction-kind instruction) :jmp-short)
                          (let* ((args (instruction-data instruction))
                                 (test (first args))
                                 (label (second args))
                                 (size (instruction-size instruction))
                                 (displacement (- (symbol-global-value label) (+ here size))))
                            (when (or (< displacement -128) (> displacement 127))
                              (set-instruction-kind instruction :jmp)
                              (set-instruction-size instruction (if (eq test t) 5 6))
                              (setq changed t)))
                          (incf here (instruction-size instruction)))
                         ((memq (instruction-kind instruction) '(:label :dead))
                          )
                         (t
                          (incf here (instruction-size instruction)))))
                  (t
                   ;; binary data
                   (incf here (length instruction))))))
        (unless changed
          (return))))))

(defun p3 ()
  (finalize-ir2)
  (assemble-ir2)
  (when (compile-file-p)
    (convert-binary-data))
  (fix-jumps))
