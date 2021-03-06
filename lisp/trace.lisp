;;; trace.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves <peter@armedbear.org>
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

(defvar *traced-names* nil)

(defvar *trace-info-hashtable* (make-hash-table :test #'equal))

(defstruct trace-info name untraced-function breakp)

(defvar *trace-depth* 0)

(defun list-traced-functions ()
  (copy-list *traced-names*))

(defmacro trace (&rest args)
  (if args
      (expand-trace args)
      `(list-traced-functions)))

(defun expand-trace (args)
  (let ((results ())
        (breakp nil))
    (let ((index (position :break args)))
      (when index
        (setf breakp (nth (1+ index) args))
        (setf args (append (subseq args 0 index) (subseq args (+ index 2))))))
    (dolist (arg args)
      (let ((info (make-trace-info :name arg
                                   :breakp breakp)))
        (push `(trace-1 ',arg ,info) results)))
    `(list ,@(nreverse results))))

(defun trace-1 (name info)
  (unless (fboundp name)
    (error "~S is not the name of a function." name))
  (if (member name *traced-names* :test #'equal)
      (format t "~S is already being traced.~%" name)
      (let* ((untraced-function (fdefinition name))
             (traced-function
              (traced-function name info untraced-function)))
        (setf (trace-info-untraced-function info) untraced-function)
        (let (;(*warn-on-redefinition* nil)
              )
          (setf (fdefinition name) traced-function))
        (setf (gethash name *trace-info-hashtable*) info)
        (push name *traced-names*)))
  name)

(defun traced-function (name info untraced-function)
  (let ((breakp (trace-info-breakp info))
	(*trace-depth* *trace-depth*))
    (lambda (&rest args)
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*print-array* nil)
              (*print-structure* nil))
          (format *trace-output* (indent "~D: ~S~%") *trace-depth*
                  (cons name args))))
      (when breakp
        (break))
      (incf *trace-depth*)
      (let ((results (multiple-value-list (apply untraced-function args))))
        (decf *trace-depth*)
        (with-standard-io-syntax
          (let ((*print-readably* nil)
                (*print-array* nil)
                (*print-structure* nil))
            (format *trace-output* (indent "~D: ~S returned") *trace-depth* name)
            (if results
                (dolist (result results)
                  (format *trace-output* " ~S" result))
                (format *trace-output* " no values"))
            (terpri *trace-output*)))
        (values-list results)))))

(defun untraced-function (name)
  (let ((info (gethash name *trace-info-hashtable*)))
    (and info (trace-info-untraced-function info))))

(defun trace-redefined-update (name untraced-function)
  (when (and *traced-names* (find name *traced-names* :test #'equal))
    (let* ((info (gethash name *trace-info-hashtable*))
           (traced-function (traced-function name info untraced-function)))
      (setf (trace-info-untraced-function info) untraced-function)
      (let ((*traced-names* nil))
        (set-fdefinition name traced-function)))))

(defun indent (string)
  (concatenate 'string
               (make-string (* (1+ *trace-depth*) 2) :initial-element #\space)
               string))

(defmacro untrace (&rest args)
  (cond ((null args)
         `(untrace-all))
        (t
         `(progn ,@(mapcar (lambda (arg) `(untrace-1 ',arg)) args) t))))

(defun untrace-all ()
  (dolist (arg *traced-names*)
    (untrace-1 arg))
  t)

(defun untrace-1 (name)
  (cond ((member name *traced-names* :test #'equal)
         (let* ((trace-info (gethash name *trace-info-hashtable*))
                (untraced-function (trace-info-untraced-function trace-info))
;;                 (*warn-on-redefinition* nil)
                )
           (remhash name *trace-info-hashtable*)
           (setq *traced-names* (remove name *traced-names*))
           (setf (fdefinition name) untraced-function)))
        (t
         (format t "~S is not being traced.~%" name)))
  nil)
