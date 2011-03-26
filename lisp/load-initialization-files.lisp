;;; load-initialization-files.lisp
;;;
;;; Copyright (C) 2011 Peter Graves <gnooth@gmail.com>
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

(export 'load-initialization-files)

(defun load-initialization-files ()
  (let ((args (cdr sys:*argv*))
        (no-sysinit nil)
        (no-userinit nil))
    (when args
      (dolist (arg args)
        (cond ((equal arg "--no-sysinit")
               (setq no-sysinit t))
              ((equal arg "--no-userinit")
               (setq no-userinit t)))))
    (unless no-sysinit
      (let ((sysinit (or (probe-file (merge-pathnames "xclrc" *xcl-home*))
                         #-win32
                         (probe-file "/etc/xclrc"))))
        (when sysinit
          (load sysinit))))
    (unless no-userinit
      (let ((userinit (probe-file (merge-pathnames ".xclrc" (user-homedir-pathname)))))
        (when userinit
          (load userinit))))))
