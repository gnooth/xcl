;;; ensure-directories-exist.lisp
;;;
;;; Copyright (C) 2004-2007 Peter Graves <peter@armedbear.org>
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

(defun ensure-directories-exist (pathspec &key verbose)
  (let ((pathname (pathname pathspec))
	(created-p nil))
    (when (wild-pathname-p pathname)
      (error 'file-error
	     :format-control "Bad place for a wild pathname."
	     :pathname pathname))
    (let ((dir (pathname-directory pathname)))
      (loop for i from 1 upto (length dir)
        do (let ((newpath (make-pathname :host (pathname-host pathname)
                                         :device (pathname-device pathname)
                                         :directory (subseq dir 0 i))))
             (unless (probe-file newpath)
               (let ((namestring (namestring newpath)))
                 #+windows
                 (let ((device (pathname-device pathname)))
                   (when device
                     (setq namestring (concatenate 'string device ":" namestring))))
                 (when verbose
                   (fresh-line)
                   (format *standard-output*
                           "Creating directory: ~A~%"
                           namestring))
                 (mkdir namestring)
                 (unless (probe-file namestring)
                   (error 'file-error
                          :pathname pathspec
                          :format-control "Can't create directory ~A."
                          :format-arguments (list namestring)))
                 (setq created-p t)))))
      (values pathname created-p))))
