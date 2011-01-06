;;; profiler.lisp
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

(in-package "PROFILER")

(export '(with-profiling report))

(defstruct (profile-info (:constructor make-profile-info (object count)))
  object
  count)

;; returns list of all symbols with non-zero call counts
(defun list-called-objects ()
  (let ((result nil))
    (dolist (pkg (list-all-packages))
      (dolist (sym (package-symbols pkg))
        (when (fboundp sym)
          (let* ((definition (fdefinition sym))
                 count)
            (cond ((typep definition 'generic-function)
                   (setq count (function-call-count (funcallable-instance-function definition)))
                   (unless (eql count 0)
                     (push (make-profile-info definition count) result))
                   (dolist (method (generic-function-methods definition))
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
                    " [interpreted function]")))))

(defun show-call-counts ()
  (let ((list (list-called-objects)))
    (setf list (sort list #'> :key 'profile-info-count))
    (let ((max-count (sample-count)))
      (dolist (info list)
        (show-call-count info max-count))))
  (values))

(defun process-samples ()
  (sys::load-lisp-symbols)
  (let ((ht (make-hash-table :test 'equal))
        (results nil)
        (nsamples (length *samples*))
        (i 0)
        (total-count 0))
    (dotimes (i nsamples)
      (let* ((address (aref *samples* i))
             (name (sys::name-from-code-address address)))
        (when name
          (let ((count (gethash name ht)))
            (if count
                (setf (gethash name ht) (1+ count))
                (setf (gethash name ht) 1))))))
    (maphash (lambda (key value)
               (push (make-profile-info key value) results))
             ht)
    (setq results (sort results #'> :key 'profile-info-count))
    (format t "~2&XCL version:           ~A~%" (lisp-implementation-version))
    (format t "~&Mode:                  ~A~%" *sampling-mode*)
    (format t "~&Sample interval:       ~D millisecond~:P~%" *sample-interval*)
    (format t "~&Number of samples:     ~D~2%" nsamples)
    (format t "~&           Self        Cumul~%")
    (format t "~&      Count     %  Count     %    Function~%")
    (format t "~&----------------------------------------------------~%")
    (dolist (entry results)
      (let* ((name (profile-info-object entry))
             (count (profile-info-count entry))
             (percent (/ (* count 100.0) nsamples)))
        (format t "~4D ~6D ~5,1F ~6D ~5,1F "
                (incf i)
                count
                percent
                (incf total-count count)
                (/ (* total-count 100.0) nsamples))
        (format t (if (stringp name)
                      "~A~%"
                      "~S~%")
                name)))))

(defun report ()
  (cond ((null *sampling-mode*)
         (show-call-counts))
        (t
         (process-samples))))

(defmacro with-profiling ((&key (sample-interval nil)
                                (max-depth 1)
                                (mode nil)
                                (max-samples nil))
                          &body body)
  `(progn
     (setq *sampling-mode* ,mode)
     (setq *sample-interval* (or ,sample-interval
                                 (if (eq *sampling-mode* :cpu) 10 1)))
     (setq *max-samples* (or ,max-samples 32768))
     (unwind-protect (progn (start-profiler ,max-depth) ,@body)
       (stop-profiler))))
