;;; translate-logical-pathname.lisp
;;;
;;; Copyright (C) 2009 Peter Graves <peter@armedbear.org>
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

(defun translate-logical-pathname (pathname &key)
  (typecase pathname
    (logical-pathname
     (let* ((host (pathname-host pathname))
            (translations (logical-pathname-translations host)))
       (dolist (translation translations
                            (error 'file-error
                                   :pathname pathname
                                   :format-control "No translation for ~S"
                                   :format-arguments (list pathname)))
         (let ((from-wildcard (car translation))
               (to-wildcard (cadr translation)))
           (when (pathname-match-p pathname from-wildcard)
             (return (translate-logical-pathname
                      (translate-pathname pathname from-wildcard to-wildcard))))))))
    (pathname pathname)
    (t
     (translate-logical-pathname (pathname pathname)))))
