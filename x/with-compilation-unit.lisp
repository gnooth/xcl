;;; with-compilation-unit.lisp
;;;
;;; Copyright (C) 2007 Peter Graves <peter@armedbear.org>
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

(export '(*style-warnings*
          *warnings*
          *errors*
          *suppress-compiler-warnings*))

(defvar *style-warnings* 0)

(defvar *warnings* 0)

(defvar *errors* 0)

(defvar *suppress-compiler-warnings* nil)

(defvar *in-compilation-unit* nil)

(defun note-error-context ()
  (let ((context *compiler-error-context*)
        (stream *error-output*))
    (cond ((and context (neq context *last-error-context*))
;;       (fresh-line stream)
;;       (princ "; in " stream)
;;       (write-line (namestring *compile-file-truename*))
;;       (princ "; in " stream)
;;       (let ((*print-length* 2)
;;             (*print-level* 2)
;;             (*print-pretty* nil))
;;         (prin1 context stream))
;;       (terpri stream)
;; ;;       (terpri stream)
;;       (write-line ";" stream)
           (let ((*print-length* 2)
                 (*print-level* 2)
                 (*print-pretty* nil))
             (format stream "~&~%; in ~A~%" (namestring *compile-file-truename*))
             (format stream "; in ~S~%;~%" context))
           (setq *last-error-context* context))
          (t
           (format stream ";~%")
           )
          )))

(defun handle-style-warning (condition)
  (unless *suppress-compiler-warnings*
;;     (fresh-line *error-output*)
    (note-error-context)
    (format *error-output* ";   Caught ~A:~%;     ~A~%" (type-of condition) condition))
  (incf *style-warnings*)
  (muffle-warning))

(defun handle-warning (condition)
  (unless *suppress-compiler-warnings*
;;     (fresh-line *error-output*)
    (note-error-context)
    (format *error-output* ";   Caught ~A:~%;     ~A~%" (type-of condition) condition))
  (incf *warnings*)
  (muffle-warning))

(defun handle-compiler-error (condition)
  (fresh-line *error-output*)
  (note-error-context)
  (format *error-output* ";   Caught ERROR:~%;     ~A~%" condition)
  (incf *errors*)
;;   (throw 'compile-defun-abort (funcall *compiler-error-bailout*))
  )

(defun %with-compilation-unit (fn &key override)
  (handler-bind ((style-warning 'handle-style-warning)
                 (warning 'handle-warning)
                 (compiler-error 'handle-compiler-error))
    (if (and *in-compilation-unit* (not override))
        (funcall fn)
        (let ((*style-warnings* 0)
              (*warnings* 0)
              (*errors* 0)
              (*defined-functions* nil)
              (*undefined-functions* nil)
              (*undefined-function-warnings* nil)
              (*in-compilation-unit* t))
          (unwind-protect
              (funcall fn)
            (unless (or (and *suppress-compiler-warnings* (zerop *errors*))
                        (and (zerop (+ *errors* *warnings* *style-warnings*))
                             (null *undefined-functions*)))
              (format *error-output* "~&~%; Compilation unit finished~%")
              (unless (zerop *errors*)
                (format *error-output* ";   Caught ~D ERROR condition~P~%"
                        *errors* *errors*))
              (unless *suppress-compiler-warnings*
                (unless (zerop *warnings*)
                  (format *error-output* ";   Caught ~D WARNING condition~P~%"
                          *warnings* *warnings*))
                (unless (zerop *style-warnings*)
                  (format *error-output* ";   Caught ~D STYLE-WARNING condition~P~%"
                          *style-warnings* *style-warnings*))
;;                 (when *undefined-functions*
;;                   (format *error-output* ";   The following functions were used but not defined:~%")
;;                   (dolist (name *undefined-functions*)
;;                     (format *error-output* ";     ~S~%" name)))
                (when *undefined-functions*
                  (format *error-output* ";   The following functions were used but not defined:~%")
                  (dolist (entry (reverse *undefined-function-warnings*))
                    (let ((name (car entry))
                          (context (cdr entry)))
                      (unless (memq name *defined-functions*)
                        (format *error-output* ";     ~S" name)
                        (when context
                          (format *error-output* " (in ~S)" context))
                        (terpri *error-output*)))))
                )
              (terpri *error-output*)))))))

(defmacro with-compilation-unit (options &body body)
  `(%with-compilation-unit (lambda () ,@body) ,@options))

(provide 'with-compilation-unit)
