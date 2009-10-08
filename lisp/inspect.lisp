;;; inspect.lisp
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

(require "CLOS")

(defun leader (name)
  (let ((size (max 0 (- 16 (length (string name))))))
    (concatenate 'string (make-string size :initial-element #\-) "->")))

(defun inspected-description (object)
  (with-output-to-string (s)
    (format s "An object of type ~S at #x~X" object (address-of object))))

(defun safe-length (x)
  (do ((n 0 (+ n 2))
       (fast x (cddr fast))
       (slow x (cdr slow)))
      (())
    (when (null fast)
      (return (values n :proper)))
    (when (atom fast)
      (return (values n :dotted)))
    (when (null (cdr fast))
      (return (values (+ n 1) :proper)))
    (when (atom (cdr fast))
      (return (values (+ n 1) :dotted)))
    (when (and (eq fast slow) (> n 0))
      (return (values nil :circular)))))

(defun old-inspected-parts (object)
  (cond ((typep object 'standard-object)
         (let ((names (layout-slot-names (std-instance-layout object)))
               (parts nil))
           (dolist (name names (nreverse parts))
             (push
              (if (slot-boundp object name)
                  (slot-value object name)
                  +unbound-marker+)
              parts))))
        ((typep object 'structure-object)
         (let ((slots (%class-slots (class-of object)))
               (parts nil)
               (i 0))
           (dolist (slot slots (nreverse parts))
             (declare (ignore slot))
             (push (structure-ref object i) parts)
             (incf i))))
        ((typep object 'layout)
         (list (layout-class object)
               (layout-slot-names object)
               (layout-invalid-p object)))
        (t
         nil)))

(defgeneric inspected-parts (object))

(defmethod inspected-parts ((object t))
  (%inspected-parts object))

(defmethod inspected-parts ((object standard-object))
  (let ((names (layout-slot-names (std-instance-layout object)))
        (elements nil))
    (dolist (name names)
      (let ((value (if (slot-boundp object name)
                       (slot-value object name)
                       +unbound-marker+)))
        (push (cons name value) elements)))
    (values (format nil "~S~%" object)
            t
            (nreverse elements))))

(defmethod inspected-parts ((object layout))
  (values (format nil "~S~%" object)
          t
          (list (cons "CLASS" (layout-class object))
                (cons "SLOT-NAMES" (layout-slot-names object))
                (cons "INVALID-P"  (layout-invalid-p object)))))

(defmethod inspected-parts ((object structure-object))
  (values (format nil "~S~%" object)
          t
          (let ((elements nil))
            (dolist (slot (class-slots (class-of object)))
              (let ((name (slot-name slot))
                    (value (structure-ref object (slot-index slot))))
                (push (cons name value) elements)))
            (nreverse elements))))

(defun display-object (object)
  (let ((*print-length* 2)
        (*print-level* 2)
        (*print-readably* nil)
        (*print-array* nil)
        (*print-structure* nil))
    (multiple-value-bind (description named-p elements)
        (inspected-parts object)

    (cond (
;;            (or (typep object 'standard-object)
;;                (typep object 'layout)
;;                (typep object 'hash-table))
             description
;;            (format t "~S~%" object)
;;            (let ((names (layout-slot-names (std-instance-layout object)))
;;                  (i 0))
;;              (dolist (name names)
;;                (format t "~4D ~A ~A"
;;                        i name (leader name))
;;                (if (slot-boundp object name)
;;                    (format t " ~S~%" (slot-value object name))
;;                    (format t " unbound~%"))
;;                (incf i)))
;;            (multiple-value-bind (description named-p elements)
;;                (inspected-parts object)
             (write-string description)
             (let ((i 0))
               (cond (named-p
                      (dolist (element elements)
                        (format t "~4D ~A ~A"
                                i (car element) (leader (car element)))
                        (format t " ~S~%" (cdr element))
                        (incf i)))
                     (t
                      nil))))
          ((typep object 'structure-object)
           (format t "~S~%" object)
           (let* ((slots (%class-slots (class-of object)))
                  (names (mapcar #'slot-name slots))
                  (i 0))
             (dolist (name names)
               (format t "~4D ~A ~A ~S~%"
                       i name (leader name) (structure-ref object i))
               (incf i))))
;;           ((typep object 'layout)
;;            (format t "~S~%" object)
;;            (let ((name "CLASS")
;;                  (i 0))
;;              (format t "~4D ~A ~A" i name (leader name))
;;              (format t " ~S~%" (layout-class object)))
;;            (let ((name "SLOT-NAMES")
;;                  (i 1))
;;              (format t "~4D ~A ~A" i name (leader name))
;;              (format t " ~S~%" (layout-slot-names object)))
;;            (let ((name "INVALID-P")
;;                  (i 2))
;;              (format t "~4D ~A ~A" i name (leader name))
;;              (format t " ~S~%" (layout-invalid-p object)))
;;            )
          ((consp object)
           (multiple-value-bind (len kind) (safe-length object)
             (case kind
               (:proper
                (format t "A proper list with ~D elements at #x~X~%" len (address-of object))
                (let ((i 0))
                  (dolist (item object)
                    (cond ((< i 25)
                           (format t "~4D-> ~S~%" i item))
                          ((eql i 25)
                           (format t "    ...~%"))
                          ((eql i (1- len))
                           (format t "~4D-> ~S~%" i item)))
                    (incf i))))
               (:dotted
                (format t "A dotted list with ~D elements~%"
                        len
                        (address-of object))
                (let* ((rest object)
                       (item (car rest))
                       (i 0))
                  (loop
                    (cond ((< i 25)
                           (format t "~4D-> ~S~%" i item))
                          ((= i 25)
                           (format t "    ...~%")))
                    (incf i)
                    (setf rest (cdr rest))
                    (when (atom rest)
                      (return))
                    (setf item (car rest)))
                  (format t "tail-> ~S~%" rest)))
               (:circular
                (format t "A circular list~%")))))
          ((fixnump object)
           (format t "The fixnum ~D~%" object))
          ((vectorp object)
           (format t "~S~%" object)
           (let ((limit (min (length object) 25)))
             (dotimes (i limit)
               (format t "~4D-> ~A~%" i (aref object i)))))
;;           ((consp obj)
;;            (multiple-value-bind (len kind) (safe-length obj)
;;              (case kind
;;                (:proper
;;                 (format t "A proper list with ~D elements at #x~X~%"
;;                         len
;;                         (address-of obj))
;;                 (let ((i 0))
;;                   (dolist (item obj)
;;                     (cond ((< i 25)
;;                            (format t "~4D-> ~S~%" i item))
;;                           ((= i 25)
;;                            (format t "    ...~%"))
;;                           ((= i (1- len))
;;                            (format t "~4D-> ~S~%" i item)))
;;                     (incf i))))
;;                (:dotted
;;                 (format t "A dotted list with ~D elements at #x~X~%"
;;                         len
;;                         (address-of obj))
;;                 (let* ((rest obj)
;;                        (item (car rest))
;;                        (i 0))
;;                   (loop
;;                     (cond ((< i 25)
;;                            (format t "~4D-> ~S~%" i item))
;;                           ((= i 25)
;;                            (format t "    ...~%")))
;;                     (incf i)
;;                     (setf rest (cdr rest))
;;                     (when (atom rest)
;;                       (return))
;;                     (setf item (car rest)))
;;                   (format t "tail-> ~S~%" rest)))
;;                (:circular
;;                 (format t "A circular list at #x~X~%" (address-of obj))))))
;;           (t
;;            (format t "~A~%" (inspected-description obj))
;;            (let ((parts (inspected-parts obj))
;;                  (i 0)
;;                  (limit 25))
;;              (dolist (part parts)
;;                (let ((name (string (car part)))
;;                      (value (cdr part)))
;;                  (format t "~4D ~A ~A ~S~%" i
;;                          name
;;                          (leader name)
;;                          value)
;;                  (incf i)
;;                  (when (> i limit)
;;                    (return))))))
          (t
           (format t "~S~%" object)
           )
          ))
    )
  (values))

(defun display-current ()
  (if *inspect-break*
      (display-object *inspected-object*)
      (format t "No object is being inspected.")))

(defun inspect (obj)
  (when *inspected-object*
    (push *inspected-object* *inspected-object-stack*))
  (setq *inspected-object* obj)
  (let* ((*inspect-break* t)
         (*debug-level* (1+ *debug-level*)))
    (setq *** **
          **  *
          *   obj)
    (display-current)
    (catch 'inspect-exit
      (tpl::repl)))
  (setq *** **
        **  *
        *   obj)
  (values))

(defun istep (args)
  (if (null args)
      (display-current)
      (let* ((pos (position #\space args))
             (option-string (if pos (subseq args 0 pos) args))
             (option (read-from-string option-string)))
        (cond ((string= option-string "-")
               (if *inspected-object-stack*
                   (progn
                     (setq *inspected-object* (pop *inspected-object-stack*))
                     (setq *** **
                           **  *
                           *   *inspected-object*)
                     (display-current))
                   (format t "Object has no parent.")))
              ((string= option-string "q")
               (setq *inspected-object* nil
                     *inspected-object-stack* nil
                     *inspect-break* nil)
               (throw 'inspect-exit nil))
              ((fixnump option)
               (let* ((index option)
;;                       (parts (old-inspected-parts *inspected-object*))
                      )
                 (multiple-value-bind (description named-p elements)
                     (inspected-parts *inspected-object*)
                   (declare (ignore description)) ;; REVIEW
                   (cond ((null elements)
                          (cond ((typep *inspected-object* 'sequence)
                                 (cond ((or (minusp index)
                                            (>= index (length *inspected-object*)))
                                        (format t "Invalid index (~D)." index))
                                       (t
                                        (push *inspected-object* *inspected-object-stack*)
                                        (setq *inspected-object*
                                              (elt *inspected-object* index))
                                        (setq * *inspected-object*)
                                        (display-current))))
                                (t
                                 (format t "Object has no selectable components.")
                                 )))
                         ((or (minusp index)
                              (>= index (length elements)))
                          (format t "Invalid index (~D)." index))
                         (t
                          (push *inspected-object* *inspected-object-stack*)
                          (setq *inspected-object*
                                (if named-p (cdr (elt elements index)) (elt elements index)))
                          (setq * *inspected-object*)
                          (display-current))))))))))
