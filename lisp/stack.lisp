;;; stack.lisp
;;;
;;; Copyright (C) 2010 Peter Graves <gnooth@gmail.com>
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

(defstruct syminfo address size name)

(defvar *kernel-symbols* nil)

(defvar *lisp-symbols* nil)

(defvar *lisp-code-addresses*)

(defun load-kernel-symbols ()
  (let ((symbols nil))
    (with-open-file (stream (merge-pathnames "kernel/xcl.nm" *xcl-home*))
      (loop
        (let ((line (read-line stream nil)))
          (when (null line)
            (return))
          ;;         (write-line line)
          (multiple-value-bind (address pos1)
              (parse-integer line :radix 16 :junk-allowed t)
            (when address
              (multiple-value-bind (size pos2)
                  (parse-integer line :start pos1 :radix 16 :junk-allowed t)
                (let ((name (and size (subseq line (+ pos2 3)))))
                  (when (and address size name)
                    (push (make-syminfo :address address
                                        :size size
                                        :name name)
                          symbols))))))))
      (setq *kernel-symbols* symbols))))

(defun load-lisp-symbols ()
  (setq *lisp-code-addresses* (make-hash-table))
  (let ((symbols nil))
    (flet ((process-symbol (symbol)
           (when (and (fboundp symbol)
                      (not (autoloadp symbol))
                      (not (macro-function symbol)))
             (let ((function (symbol-function symbol)))
               (when (function-code-address function)
                 (push (make-syminfo :address (function-code-address function)
                                     :size (function-code-size function)
                                     :name symbol)
                     symbols)
                 (setf (gethash (function-code-address function) *lisp-code-addresses*) symbol))))))
        (dolist (package (list-all-packages))
          (dolist (symbol (package-external-symbols package))
            (process-symbol symbol))
          (dolist (symbol (package-internal-symbols package))
            (process-symbol symbol))))
    (setq *lisp-symbols* (sort symbols #'< :key #'syminfo-address))))

;; only needed for debugging
(defun dump-symbols (symbols)
  (dolist (info symbols)
    (format t "~X ~X ~S~%" (syminfo-address info) (syminfo-size info) (syminfo-name info))))

(defun name-from-code-address (address)
  (when (null *kernel-symbols*)
    (load-kernel-symbols))
  (dolist (info *kernel-symbols*)
    (when (and (<= (syminfo-address info) address)
               (< address (+ (syminfo-address info) (syminfo-size info))))
      ;; found kernel symbol
      (let ((lisp-name (gethash (syminfo-address info) *lisp-code-addresses*)))
        (return-from name-from-code-address (or lisp-name (syminfo-name info))))))
  ;; not a kernel address
  (dolist (info *lisp-symbols*)
    (when (> (syminfo-address info) address)
      (return nil))
    (when (and (syminfo-size info)
               (< address (+ (syminfo-address info) (syminfo-size info))))
      (return (syminfo-name info)))))

(defun test-stack ()
  (assert (eq (name-from-code-address (+ (function-code-address #'car) 3)) 'car))
  t)

(defun print-saved-stack (&optional (limit most-positive-fixnum))
  (load-lisp-symbols)
  (let ((count 0))
    (dolist (entry (reverse *saved-stack*))
      (let ((name (name-from-code-address (cdr entry))))
        (format t "0x~8,'0X 0x~8,'0X ~A~%"
                (car entry)
                (cdr entry)
                (if name name "")))
      (incf count)
      (when (>= count limit)
        (return)))))
