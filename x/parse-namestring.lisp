;;; parse-namestring.lisp
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

(in-package "SYSTEM")

(defun parse-namestring (thing
                         &optional host (default-pathname *default-pathname-defaults*)
                         &key (start 0) end junk-allowed)
  (declare (ignore junk-allowed)) ; FIXME
  (cond ((eq host :unspecific)
         (setq host nil))
        (host
         (setq host (canonicalize-logical-host host))))
  (typecase thing
    (stream
     (values (pathname thing) start))
    (pathname
     (values thing start))
    (string
     (unless end
       (setq end (length thing)))
     (%parse-namestring (subseq thing start end) host default-pathname))
    (t
     (error 'type-error
            :format-control "~S cannot be converted to a pathname."
            :format-arguments (list thing)))))
