;;; translate-pathname.lisp
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

(defun wild-p (component)
  (or (eq component :wild)
      (and (stringp component)
           (position #\* component))))

(defun casify (thing case)
  (typecase thing
    (string
     (case case
       (:upcase (string-upcase thing))
       (:downcase (string-downcase thing))
       (t thing)))
    (list
     (let (result)
       (dolist (component thing (nreverse result))
         (push (casify component case) result))))
    (t
     thing)))

(defun split-directory-components (directory)
  (declare (optimize safety))
  (declare (type list directory))
  (unless (memq (car directory) '(:absolute :relative))
    (error "Ill-formed directory list: ~S" directory))
  (let (result sublist)
    (push (car directory) result)
    (dolist (component (cdr directory))
      (cond ((memq component '(:wild :wild-inferiors))
             (when sublist
               (push (nreverse sublist) result)
               (setf sublist nil))
             (push component result))
            (t
             (push component sublist))))
    (when sublist
      (push (nreverse sublist) result))
    (nreverse result)))

(defun translate-component (source from to &optional case)
  (declare (ignore from))
  (cond ((or (eq to :wild) (null to))
         ;; "If the piece in TO-WILDCARD is :WILD or NIL, the piece in source
         ;; is copied into the result."
         (casify source case))
        ((and to (not (wild-p to)))
         ;; "If the piece in TO-WILDCARD is present and not wild, it is copied
         ;; into the result."
         to)
        (t
         ;; "Otherwise, the piece in TO-WILDCARD might be a complex wildcard
         ;; such as "foo*bar" and the piece in FROM-WILDCARD should be wild;
         ;; the portion of the piece in SOURCE that matches the wildcard
         ;; portion of the piece in FROM-WILDCARD replaces the wildcard portion
         ;; of the piece in TO-WILDCARD and the value produced is used in the
         ;; result."
         ;; FIXME
         (error "Unsupported wildcard pattern: ~S" to))))

(defun translate-directory-components (source from to case)
  (cond ((null to)
         nil
         )
        ((memq (car to) '(:absolute :relative))
         (cons (car to)
               (translate-directory-components (cdr source) (cdr from) (cdr to) case))
         )
        ((eq (car to) :wild)
         (if (eq (car from) :wild)
             ;; Grab the next chunk from SOURCE.
             (append (casify (car source) case)
                     (translate-directory-components (cdr source) (cdr from) (cdr to) case))
             (error "Unsupported case 1: ~S ~S ~S" source from to))
         )
        ((eq (car to) :wild-inferiors)
         ;; Grab the next chunk from SOURCE.
         (append (casify (car source) case)
                 (translate-directory-components (cdr source) (cdr from) (cdr to) case))
         )
        (t
         ;; "If the piece in TO-WILDCARD is present and not wild, it is copied
         ;; into the result."
         (append (casify (car to) case)
                 (translate-directory-components source from (cdr to) case))
         )
        ))

(defun translate-directory (source from to case)
  ;; FIXME The IGNORE-CASE argument to DIRECTORY-MATCH-P should not be nil on
  ;; Windows or if the source pathname is a logical pathname.
  ;; FIXME We can canonicalize logical pathnames to upper case, so we only need
  ;; IGNORE-CASE for Windows.
  (cond ((null source)
         to)
        ((equal source '(:absolute))
         (remove :wild-inferiors to))
        (t
         (translate-directory-components (split-directory-components source)
                                         (split-directory-components from)
                                         (split-directory-components to)
                                         case))))

;; "The resulting pathname is TO-WILDCARD with each wildcard or missing field
;; replaced by a portion of SOURCE."
(defun translate-pathname (source from-wildcard to-wildcard &key)
  (unless (pathname-match-p source from-wildcard)
    (error "~S and ~S do not match." source from-wildcard))
  (let* ((source (pathname source))
         (from   (pathname from-wildcard))
         (to     (pathname to-wildcard))
         (device (if (logical-pathname-p to)
                     :unspecific
                     (translate-component (pathname-device source)
                                          (pathname-device from)
                                          (pathname-device to))))
         (case   (and (logical-pathname-p source)
                      (or (featurep :unix) (featurep :windows))
                      :downcase)))
    (make-pathname :host      (pathname-host to)
                   :device    (cond ((logical-pathname-p to)
                                     :unspecific)
                                    ((eq device :unspecific)
                                     nil)
                                    (t
                                     device))
                   :directory (translate-directory (pathname-directory source)
                                                   (pathname-directory from)
                                                   (pathname-directory to)
                                                   case)
                   :name      (translate-component (pathname-name source)
                                                   (pathname-name from)
                                                   (pathname-name to)
                                                   case)
                   :type      (translate-component (pathname-type source)
                                                   (pathname-type from)
                                                   (pathname-type to)
                                                   case)
                   :version   (if (null (pathname-host from))
                                  (if (eq (pathname-version to) :wild)
                                      (pathname-version from)
                                      (pathname-version to))
                                  (translate-component (pathname-version source)
                                                       (pathname-version from)
                                                       (pathname-version to))))))
