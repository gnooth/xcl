;;; profiler.lisp
;;;
;;; Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

(in-package "PROFILER")

(export '(with-profiling show-call-counts))

;; (defun list-called-objects ()
;;   (let ((result nil))
;;     (dolist (pkg (list-all-packages))
;;       (dolist (sym (append (package-internal-symbols pkg)
;;                            (package-external-symbols pkg)))
;;         (when (fboundp sym)
;;           (let* ((function (symbol-function sym))
;;                  (count (if (functionp function)
;;                             (function-call-count function)
;;                             0)))
;;               (unless (zerop count)
;;                 (push (cons sym count) result))))))
;;     (remove-duplicates result :key 'car :test 'eq)))

;; (defun show-call-counts ()
;;   (let ((list (list-called-objects)))
;;     (setf list (sort list #'< :key 'cdr))
;;     (dolist (item list)
;;       (let* ((name (car item))
;;              (count (cdr item))
;;              (interpreted-p (and (fboundp name)
;;                                  (not (compiled-function-p (symbol-function name))))))
;;         (format t "~S~A ~D~%" name (if interpreted-p " [interpreted function]" "") count))))
;;   (values))

(defstruct (profile-info (:constructor make-profile-info (object count)))
  object
  count)

;; Returns list of all symbols with non-zero call counts.
(defun list-called-objects ()
  (let ((result nil))
    (dolist (pkg (list-all-packages))
      (dolist (sym (package-symbols pkg))
        (when (fboundp sym)
          (let* ((definition (fdefinition sym))
                 count)
            (cond ((typep definition 'generic-function)
;;                    (format t "~&considering ~S~%" definition)
                   (setq count (function-call-count (funcallable-instance-function definition)))
                   (unless (eql count 0)
                     (push (make-profile-info definition count) result))
                   (dolist (method (generic-function-methods definition))
;;                      (format t "~&considering ~S~%" method)
                     (setq count (function-call-count (method-function method)))
                     (unless (eql count 0)
                       (push (make-profile-info method count) result))))
                  ((functionp definition)
                   (setq count (function-call-count definition))
                   (unless (zerop count)
                     (push (make-profile-info sym count) result))))))))
    (remove-duplicates result :key 'profile-info-object :test 'eq)))

(defun object-name (object)
  (cond ((symbolp object)
         object)
        ((typep object 'generic-function)
;;          (generic-function-name object)
         object
         )
        ((typep object 'method)
;;          (list 'METHOD
;;                (generic-function-name (method-generic-function object))
;;                (method-specializers object))
         object
         )))

(defun object-compiled-function-p (object)
  (cond ((symbolp object)
         (compiled-function-p (fdefinition object)))
        ((typep object 'generic-function)
         (compiled-function-p (funcallable-instance-function object)))
        ((typep object 'method)
         (compiled-function-p (method-function object)))
        (t
         (compiled-function-p object))))

(defun show-call-count (info max-count)
  (declare (ignorable max-count))
  (let* ((object (profile-info-object info))
         (count  (profile-info-count  info)))
    (if max-count
        (format t "~5,1F ~8D ~S~A~%"
                (/ (* count 100.0) max-count)
                count
                (object-name object)
                (if (object-compiled-function-p object)
                    ""
                    " [interpreted function]"))
        (format t "~8D ~S~A~%"
                count
                (object-name object)
                (if (object-compiled-function-p object)
                    ""
                    " [interpreted function]")))
;;     (format t "~8D ~S~A~%"
;;             count
;;             (object-name object)
;;             (if (object-compiled-function-p object)
;;                 ""
;;                 " [interpreted function]"))
    ))

(defun show-call-counts ()
  (let ((list (list-called-objects)))
    (setf list (sort list #'< :key 'profile-info-count))
    (let ((max-count (sample-count)))
;;     (let ((max-count nil))
;;       (let ((last-info (car (last list))))
;;         (setq max-count (if last-info
;;                             (profile-info-count last-info)
;;                             nil))
;;         (when (eql max-count 0)
;;           (setq max-count nil)))
      (dolist (info list)
        (show-call-count info max-count))))
  (values))

(defmacro with-profiling ((&key (max-depth most-positive-fixnum)) &body body)
  `(unwind-protect (progn (start-profiler ,max-depth) ,@body)
     (stop-profiler)))

(provide "PROFILER")
