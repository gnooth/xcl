;;; invoke-debugger.lisp
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

(in-package "SYSTEM")

(export 'show-restarts)

(defvar *debugger-hook* nil)

(defun show-restarts (restarts stream)
  (when restarts
    (format stream "~&Restarts (invokable by number):~%")
    (let ((max-name-len 0))
      (dolist (restart restarts)
        (let ((name (restart-name restart)))
          (when name
            (let ((len (length (princ-to-string name))))
              (when (> len max-name-len)
                (setf max-name-len len))))))
      (let ((count 0))
        (dolist (restart restarts)
          (let ((name (restart-name restart))
                (report-function (restart-report-function restart)))
            (format stream "  ~D: ~A" count name)
            (when (functionp report-function)
              (dotimes (i (1+ (- max-name-len (length (princ-to-string name)))))
                (write-char #\space stream))
              (funcall report-function stream))
            (terpri stream))
          (incf count))))))

(defun debug-loop ()
  (let* ((level *debug-level*)
         (*debug-level* (1+ *debug-level*)))
    (cond ((> level 0)
           (with-simple-restart (abort "Return to debug level ~D." level)
             (show-restarts (compute-restarts) *debug-io*)
             (top-level::repl))
           (values))
          (t
           (show-restarts (compute-restarts) *debug-io*)
           (top-level::repl)))))

(defun invoke-debugger (condition)
  (let ((*saved-backtrace* (backtrace-as-list))
        (*saved-stack* (current-stack-as-list)))
    (let ((old-hook *invoke-debugger-hook*))
      (when old-hook
        (let ((*invoke-debugger-hook* nil))
          (funcall old-hook condition old-hook))))
    (let ((old-hook *debugger-hook*))
      (when old-hook
        (let ((*debugger-hook* nil))
          (funcall old-hook condition old-hook))))
    (let ((original-package *package*))
      (with-standard-io-syntax
        (let ((*print-readably* nil)
              (*print-structure* nil)
              (*package* original-package)
              (*debug-condition* condition))
          (let ((*enable-autocompile* nil))
            (format *debug-io* "~&Debugger invoked on condition of type ~A:~%  ~A~%"
                    (type-of condition) condition))
          (cond ((and (fboundp 'compute-restarts)
                      (fboundp 'top-level::repl))
                 (clear-input)
                 (debug-loop))
                (t
                 (let ((n 0))
                   (dolist (frame *saved-backtrace*)
                     (format *debug-io* "~3D: ~S~%" n frame)
                     (incf n)))
                 (quit))))))))
