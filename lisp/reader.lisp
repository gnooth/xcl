;;; reader.lisp
;;;
;;; Copyright (C) 2006-2007 Peter Graves <peter@armedbear.org>
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

;;; Reading circular data: the #= and ## reader macros (from SBCL)

(in-package "SYSTEM")

;;; Objects already seen by CIRCLE-SUBST.
;; (defvar *sharp-equal-circle-table*)

;; This function is kind of like NSUBLIS, but checks for circularities and
;; substitutes in arrays and structures as well as lists. The first arg is an
;; alist of the things to be replaced assoc'd with the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((not (typep tree
                     '(or cons (array t) structure-object standard-object)))
         (let ((entry (find tree old-new-alist :key #'second)))
           (if entry (third entry) tree)))
        ((null (gethash tree *sharp-equal-circle-table*))
         (setf (gethash tree *sharp-equal-circle-table*) t)
         (cond
;;           ((typep tree 'structure-object)
;;            (do ((i 0 (1+ i))
;;                 (end (structure-length tree)))
;;                ((= i end))
;;              (let* ((old (structure-ref tree i))
;;                     (new (circle-subst old-new-alist old)))
;;                (unless (eq old new)
;;                  (structure-set tree i new)))))
;;           ((typep tree 'standard-object)
;;            (do ((i 1 (1+ i))
;;                 (end (%instance-length tree)))
;;                ((= i end))
;;              (let* ((old (%instance-ref tree i))
;;                     (new (circle-subst old-new-alist old)))
;;                (unless (eq old new)
;;                  (setf (%instance-ref tree i) new)))))
          ((arrayp tree)
           (do ((i 0 (1+ i))
                (end (array-total-size tree)))
               ((>= i end))
             (let* ((old (row-major-aref tree i))
                    (new (circle-subst old-new-alist old)))
               (unless (eq old new)
                 (setf (row-major-aref tree i) new)))))
          (t
           (let ((a (circle-subst old-new-alist (car tree)))
                 (d (circle-subst old-new-alist (cdr tree))))
             (unless (eq a (car tree))
               (rplaca tree a))
             (unless (eq d (cdr tree))
               (rplacd tree d)))))
         tree)
        (t
         tree)))

;;; Sharp-equal works as follows. When a label is assigned (i.e. when
;;; #= is called) we GENSYM a symbol is which is used as an
;;; unforgeable tag. *SHARP-SHARP-ALIST* maps the integer tag to this
;;; gensym.
;;;
;;; When SHARP-SHARP encounters a reference to a label, it returns the
;;; symbol assoc'd with the label. Resolution of the reference is
;;; deferred until the read done by #= finishes. Any already resolved
;;; tags (in *SHARP-EQUAL-ALIST*) are simply returned.
;;;
;;; After reading of the #= form is completed, we add an entry to
;;; *SHARP-EQUAL-ALIST* that maps the gensym tag to the resolved
;;; object. Then for each entry in the *SHARP-SHARP-ALIST, the current
;;; object is searched and any uses of the gensysm token are replaced
;;; with the actual value.

;; (defvar *sharp-sharp-alist* nil)

(defun sharp-equal (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless label
    (error 'reader-error
           :stream stream
           :format-control "Missing label for #="))
  (when (or (assql label *sharp-sharp-alist*)
            (assql label *sharp-equal-alist*))
    (error 'reader-error
           :stream stream
           :format-control "Multiply defined label: #~D="
           :format-arguments (list label)))
  (let* ((tag (gensym))
         (*sharp-sharp-alist* (acons label tag *sharp-sharp-alist*))
         (obj (read stream t nil t)))
    (when (eq obj tag)
      (error 'reader-error
             :stream stream
             :format-control "Must tag something more than just #~D#"
             :format-arguments (list label)))
    (push (list label tag obj) *sharp-equal-alist*)
    (let ((*sharp-equal-circle-table* (make-hash-table :test 'eq :size 20)))
      (circle-subst *sharp-equal-alist* obj))))

(defun sharp-sharp (stream ignore label)
  (declare (ignore ignore))
  (when *read-suppress* (return-from sharp-sharp nil))
  (unless label
    (error 'reader-error :stream stream :format-control "Missing label for ##"))
  (let ((entry (assql label *sharp-equal-alist*)))
    (if entry
        (third entry)
        (let ((pair (assoc label *sharp-sharp-alist*)))
          (unless pair
            (error 'reader-error
                   :stream stream
                   :format-control "Object is not labelled #~S#"
                   :format-arguments (list label)))
          (cdr pair)))))

(set-dispatch-macro-character #\# #\= #'sharp-equal *standard-readtable*)
(set-dispatch-macro-character #\# #\# #'sharp-sharp *standard-readtable*)

(copy-readtable *standard-readtable* *readtable*)
