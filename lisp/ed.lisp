;;; ed.lisp
;;;
;;; Copyright (C) 2004-2009 Peter Graves <peter@armedbear.org>
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

;; FIXME export ext:*ed-functions*
(defvar *ed-functions* nil)

(defun ed (&optional x)
  "Starts the editor (on a file or a function if named).  Functions
from the list *ED-FUNCTIONS* are called in order with X as an argument
until one of them returns non-NIL; these functions are responsible for
signalling a FILE-ERROR to indicate failure to perform an operation on
the file system."
  (dolist (fun *ed-functions*
	   (error 'simple-error
		  :format-control "Don't know how to ~S ~A"
		  :format-arguments (list 'ed x)))
    (when (funcall fun x)
      (return)))
  (values))

(defun default-ed-function (what)
  (let ((portfile (merge-pathnames ".j/port"
                                   #+windows
                                   (if (ext:probe-directory "C:\\.j")
                                       "C:\\"
                                       (ext:probe-directory (pathname (ext:getenv "APPDATA"))))
                                   #-windows
                                   (user-homedir-pathname)))
        stream)
    (when (probe-file portfile)
      (let* ((port (with-open-file (s portfile) (read s nil nil))))
        (when (integerp port)
          (setq stream (make-socket :remote-host "127.0.0.1"
                                    :remote-port port)))))
    (unwind-protect
     (cond ((stringp what)
            (if stream
                (progn
                  (write-string (namestring (user-homedir-pathname)) stream)
                  (terpri stream)
                  (write-string (format nil "~S~%" what) stream))
                ;; FIXME we don't have RUN-SHELL-COMMAND
;;                 (run-shell-command (format nil "j ~S" what))
                ))
           ((and what (symbolp what))
            (when (autoloadp what)
              (let ((*load-verbose* nil)
                    (*load-print* nil)
                    (*autoload-verbose* nil))
                (resolve what)))
            (cond ((source what)
                   (let ((file (namestring (source-pathname what)))
                         (position (source-file-position what))
                         (line-number 1)
                         (pattern (string what)))
                     (with-open-file (s file)
                       (dotimes (i position)
                         (let ((c (read-char s nil s)))
                           (cond ((eq c s)
                                  (return))
                                 ((eql c #\newline)
                                  (incf line-number)))))
                       (dotimes (i 10)
                         (let ((text (read-line s nil s)))
                           (cond ((eq text s)
                                  (return))
                                 ((search pattern text :test 'string-equal)
                                  (return))
                                 (t
                                  (incf line-number))))))
                     (if stream
                         (progn
                           (write-string (namestring (user-homedir-pathname)) stream)
                           (terpri stream)
                           (write-string (format nil "+~D~%~S~%" line-number file) stream))
                         (run-program "j" (list (format nil "+~D" line-number)
                                                (format nil "~S" file))))))
                  ((not (null *xcl-home*))
                   (let ((tagfile (merge-pathnames "kernel/tags" *xcl-home*)))
                     (when (and tagfile (probe-file tagfile))
                       (with-open-file (s tagfile)
                         (loop
                           (let ((text (read-line s nil s)))
                             (cond ((eq text s)
                                    (return))
                                   ((eq what (read-from-string text nil nil))
                                    ;; found it!
                                    (with-input-from-string (string-stream text)
                                      (let* ((symbol (read string-stream text nil nil)) ; Ignored.
                                             (file (read string-stream text nil nil))
                                             (line-number (read string-stream text nil nil)))
                                        (declare (ignore symbol))
                                        (when (pathnamep file)
                                          (setf file (namestring file)))
                                        (if stream
                                            (progn
                                              (write-string (namestring (user-homedir-pathname)) stream)
                                              (terpri stream)
                                              (write-string (format nil "+~D~%~S~%" line-number file) stream))
                                            (run-program "j" (list (format nil "+~D" line-number)
                                                                   (format nil "~S" file)))))))))))))))))
     (when stream
       (close stream))))
  t)

(pushnew 'default-ed-function *ed-functions*)
