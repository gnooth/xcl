;;; deftype.lisp
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

(in-package "SYSTEM")

(defmacro deftype (name lambda-list &rest body)
;;   (when (eq (symbol-package name) +cl-package+)
;;     (error :format-control "Attempt to define ~S, a symbol in the COMMON-LISP package, as a type specifier."
;;            :format-arguments (list name)))
;;   (check-declaration-type name)
  ;; Optional and keyword parameters default to * rather than NIL.
  (when (or (memq '&optional lambda-list)
            (memq '&key lambda-list))
    (let ((new-lambda-list nil)
          (state nil))
      (dolist (thing lambda-list)
        (cond ((eq thing '&optional)
               (setq state '&optional))
              ((eq thing '&key)
               (setq state '&key))
              ((memq thing lambda-list-keywords)
               (setq state nil))
              ((eq state '&optional)
               (when (symbolp thing)
                 (setq thing (list thing ''*))))
              ((eq state '&key)
               (when (symbolp thing)
                 (setq thing (list thing ''*)))))
        (push thing new-lambda-list))
      (setq lambda-list (nreverse new-lambda-list))))
  (multiple-value-bind (body decls doc)
      (parse-body body)
    `(progn
;;        (setf (get ',name 'deftype-definition)
       (set-deftype-expander
        ',name
        #'(lambda ,lambda-list ,@decls ,doc (block ,name ,@body)))
       ',name)))
