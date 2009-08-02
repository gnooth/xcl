;;; logical-pathname-translations.lisp
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

(defun logical-host-p (canonical-host)
  (multiple-value-bind (translations present)
      (gethash canonical-host +logical-pathname-translation-table+)
    (declare (ignore translations))
    present))

(defun logical-pathname-translations (host)
  (multiple-value-bind (translations present)
      (gethash (canonicalize-logical-host host) +logical-pathname-translation-table+)
    (unless present
      (error 'type-error
             :datum host
             :expected-type '(and string (satisfies logical-host-p))))
    translations))

(defun canonicalize-logical-pathname-translations (translations host)
  (let (result)
    (dolist (translation translations (nreverse result))
      (let ((from (car translation))
            (to (cadr translation)))
        (push (list (if (logical-pathname-p from)
                        from
                        (parse-namestring from host))
                    (pathname to))
              result)))))

(defun %set-logical-pathname-translations (host translations)
  (setf host (canonicalize-logical-host host))
  ;; Avoid undefined host error in CANONICALIZE-LOGICAL-PATHNAME-TRANSLATIONS.
  (unless (logical-host-p host)
    (setf (gethash host +logical-pathname-translation-table+) nil))
  (setf (gethash host +logical-pathname-translation-table+)
        (canonicalize-logical-pathname-translations translations host)))

(defsetf logical-pathname-translations %set-logical-pathname-translations)
