;;; delete.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves <peter@armedbear.org>
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

;;; Adapted from CMUCL.

(in-package "SYSTEM")

(defmacro real-count (count)
  `(cond ((null ,count) most-positive-fixnum)
         ((fixnump ,count) (if (minusp ,count) 0 ,count))
         ((integerp ,count) (if (minusp ,count) 0 most-positive-fixnum))
         (t ,count)))

(defmacro mumble-delete (pred)
  `(do ((index start (1+ index))
        (jndex start)
        (number-zapped 0))
       ((or (eql index end) (eql number-zapped count))
        (do ((index index (1+ index))		; copy the rest of the vector
             (jndex jndex (1+ jndex)))
            ((eql index length)
             (shrink-vector sequence jndex))
          (aset sequence jndex (aref sequence index))))
     (aset sequence jndex (aref sequence index))
     (if ,pred
         (setq number-zapped (1+ number-zapped))
         (setq jndex (1+ jndex)))))

(defmacro mumble-delete-from-end (pred)
  `(do ((index (1- end) (1- index)) ; find the losers
        (number-zapped 0)
        (losers ())
        this-element
        (terminus (1- start)))
     ((or (eql index terminus) (eql number-zapped count))
      (do ((losers losers)			 ; delete the losers
           (index start (1+ index))
           (jndex start))
        ((or (null losers) (eql index end))
         (do ((index index (1+ index))	 ; copy the rest of the vector
              (jndex jndex (1+ jndex)))
           ((eql index length)
            (shrink-vector sequence jndex))
           (aset sequence jndex (aref sequence index))))
        (aset sequence jndex (aref sequence index))
        (if (eql index (car losers))
            (pop losers)
            (setq jndex (1+ jndex)))))
     (setq this-element (aref sequence index))
     (when ,pred
       (setq number-zapped (1+ number-zapped))
       (push index losers))))

(defmacro normal-mumble-delete ()
  `(mumble-delete
    (if test-not
        (not (funcall test-not item (apply-key key (aref sequence index))))
        (funcall test item (apply-key key (aref sequence index))))))

(defmacro normal-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(defmacro list-delete (pred)
  `(let ((handle (cons nil sequence)))
     (do ((current (nthcdr start sequence) (cdr current))
          (previous (nthcdr start handle))
          (index start (1+ index))
          (number-zapped 0))
       ((or (eql index end) (eql number-zapped count))
        (cdr handle))
       (cond (,pred
              (rplacd previous (cdr current))
              (setq number-zapped (1+ number-zapped)))
             (t
              (setq previous (cdr previous)))))))

(defmacro list-delete-from-end (pred)
  `(let* ((reverse (nreverse sequence))
          (handle (cons nil reverse)))
     (do ((current (nthcdr (- length end) reverse)
                   (cdr current))
          (previous (nthcdr (- length end) handle))
          (index start (1+ index))
          (number-zapped 0))
       ((or (eql index end) (eql number-zapped count))
        (nreverse (cdr handle)))
       (cond (,pred
              (rplacd previous (cdr current))
              (setq number-zapped (1+ number-zapped)))
             (t
              (setq previous (cdr previous)))))))

(defmacro normal-list-delete ()
  '(list-delete
    (if test-not
        (not (funcall test-not item (apply-key key (car current))))
        (funcall test item (apply-key key (car current))))))

(defmacro normal-list-delete-from-end ()
  '(list-delete-from-end
    (if test-not
        (not (funcall test-not item (apply-key key (car current))))
        (funcall test item (apply-key key (car current))))))

(defun delete (item sequence &key from-end (test #'eql) test-not (start 0)
                    end count key)
  (when key
    (setq key (coerce-to-function key)))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (if (listp sequence)
        (if from-end
            (normal-list-delete-from-end)
            (normal-list-delete))
        (if from-end
            (normal-mumble-delete-from-end)
            (normal-mumble-delete)))))

(defmacro if-mumble-delete ()
  `(mumble-delete
    (funcall predicate (apply-key key (aref sequence index)))))

(defmacro if-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (funcall predicate (apply-key key this-element))))

(defmacro if-list-delete ()
  '(list-delete
    (funcall predicate (apply-key key (car current)))))

(defmacro if-list-delete-from-end ()
  '(list-delete-from-end
    (funcall predicate (apply-key key (car current)))))

(defun delete-if (predicate sequence &key from-end (start 0) key end count)
  (when key
    (setq key (coerce-to-function key)))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (if (listp sequence)
        (if from-end
            (if-list-delete-from-end)
            (if-list-delete))
        (if from-end
            (if-mumble-delete-from-end)
            (if-mumble-delete)))))

(defmacro if-not-mumble-delete ()
  `(mumble-delete
    (not (funcall predicate (apply-key key (aref sequence index))))))

(defmacro if-not-mumble-delete-from-end ()
  `(mumble-delete-from-end
    (not (funcall predicate (apply-key key this-element)))))

(defmacro if-not-list-delete ()
  '(list-delete
    (not (funcall predicate (apply-key key (car current))))))

(defmacro if-not-list-delete-from-end ()
  '(list-delete-from-end
    (not (funcall predicate (apply-key key (car current))))))

(defun delete-if-not (predicate sequence &key from-end (start 0) end key count)
  (when key
    (setq key (coerce-to-function key)))
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (if (listp sequence)
        (if from-end
            (if-not-list-delete-from-end)
            (if-not-list-delete))
        (if from-end
            (if-not-mumble-delete-from-end)
            (if-not-mumble-delete)))))
