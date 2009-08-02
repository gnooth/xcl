;;; load.lisp
;;;
;;; Copyright (C) 2004-2008 Peter Graves <peter@armedbear.org>
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

(defun load (filespec
             &key
             (verbose *load-verbose*)
             (print *load-print*)
             (if-does-not-exist t)
             (external-format :default))
  (declare (ignore external-format)) ; FIXME
  (loop
    (restart-case
      (return
        (let ((*speed* *speed*)
              (*space* *space*)
              (*safety* *safety*)
              (*debug* *debug*))
          (if (streamp filespec)
              (let ((*load-pathname* nil)
                    (*load-truename* nil))
                (load-stream filespec nil verbose print))
              (let* ((pathname (merge-pathnames (pathname filespec)))
                     (probed-file (probe-file pathname)))
                (if probed-file
                    (let ((stream (open pathname)))
                      (unwind-protect
                          (let ((*load-pathname* pathname)
                                (*load-truename* probed-file)
                                (*source-file* probed-file)
                                (*source-position* 0))
                            (load-stream stream pathname verbose print))
                        (close stream)))
                    (if if-does-not-exist
                        (error 'file-error
                               :format-control "Unable to open ~S."
                               :format-arguments (list pathname)
                               :pathname pathname)
                        nil))))))
      (retry ()
        :report (lambda (stream) (format stream "Retry loading ~s" filespec)))
      (skip ()
        :report (lambda (stream) (format stream "Skip loading ~s" filespec))
        (return nil)))))
