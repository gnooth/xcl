;;; pathname-match-p.lisp
;;;
;;; Copyright (C) 2003-2009 Peter Graves <peter@armedbear.org>
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

(defun component-match-p (thing wild ignore-case)
  (cond ((eq wild :wild)
         t)
        ((null wild)
         t)
        ((and (stringp wild) (string-find #\* wild))
         (error "Unsupported wildcard pattern: ~S" wild))
        (ignore-case
         (equalp thing wild))
        (t
         (equal thing wild))))

(defun directory-match-components (thing wild ignore-case)
  (loop
    (cond ((endp thing)
           (return (or (endp wild) (equal wild '(:wild-inferiors)))))
          ((endp wild)
           (return nil)))
    (let ((x (%car thing))
          (y (%car wild)))
      (when (eq y :wild-inferiors)
        (return t))
      (unless (component-match-p x y ignore-case)
        (return nil))
      (setq thing (%cdr thing)
            wild  (%cdr wild)))))

(defun directory-match-p (thing wild ignore-case)
  (cond ((eq wild :wild)
         t)
        ((null wild)
         t)
        ((and ignore-case (equalp thing wild))
         t)
        ((equal thing wild)
         t)
        ((and (null thing) (equal wild '(:absolute :wild-inferiors)))
         t)
        ((and (consp thing) (consp wild))
         (if (eq (%car thing) (%car wild))
             (directory-match-components (%cdr thing) (%cdr wild) ignore-case)
             nil))
        (t
         nil)))

(defun pathname-match-p (pathname wildcard)
  (setq pathname (pathname pathname)
        wildcard (pathname wildcard))
  (unless (component-match-p (pathname-host pathname) (pathname-host wildcard) nil)
    (return-from pathname-match-p nil))
  (let* ((windows-p (featurep :windows))
         (ignore-case (or windows-p (logical-pathname-p pathname))))
    (cond ((and windows-p
                (not (component-match-p (pathname-device pathname)
                                        (pathname-device wildcard)
                                        ignore-case)))
           nil)
          ((not (directory-match-p (pathname-directory pathname)
                                   (pathname-directory wildcard)
                                   ignore-case))
           nil)
          ((not (component-match-p (pathname-name pathname)
                                   (pathname-name wildcard)
                                   ignore-case))
           nil)
          ((not (component-match-p (pathname-type pathname)
                                   (pathname-type wildcard)
                                   ignore-case))
           nil)
          (t
           t))))
