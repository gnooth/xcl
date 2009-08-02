;;; bit-array-ops.lisp
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

(defun bit-array-same-dimensions-p (array1 array2)
  (declare (type (array bit) array1 array2))
  (let ((rank (array-rank array1)))
    (and (eql rank (array-rank array2))
         (dotimes (index rank t)
           (declare (type index index))
           (declare (optimize speed (safety 0)))
           (unless (eql (array-dimension array1 index)
                        (array-dimension array2 index))
             (return nil))))))

(defun require-same-dimensions (array1 array2)
  (unless (bit-array-same-dimensions-p array1 array2)
    (error 'program-error
           "~S and ~S do not have the same dimensions."
           array1 array2)))

(defun pick-result-array (result-bit-array bit-array-1)
  (case result-bit-array
    ((t) bit-array-1)
    ((nil) (make-array (array-dimensions bit-array-1)
		       :element-type 'bit
		       :initial-element 0))
    (t
     (require-same-dimensions bit-array-1 result-bit-array)
     result-bit-array)))

(defun bit-and (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (declare (type index i))
          (declare (optimize speed (safety 0)))
          (setf (row-major-aref result-bit-array i)
                (logand (row-major-aref bit-array-1 i)
                        (row-major-aref bit-array-2 i))))))

(defun bit-ior (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logior (row-major-aref bit-array-1 i)
                    (row-major-aref bit-array-2 i))))))

(defun bit-xor (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logxor (row-major-aref bit-array-1 i)
                    (row-major-aref bit-array-2 i))))))

(defun bit-eqv (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (logeqv (row-major-aref bit-array-1 i)
                            (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-nand (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (lognand (row-major-aref bit-array-1 i)
                             (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-nor (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (lognor (row-major-aref bit-array-1 i)
                            (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-andc1 (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (logandc1 (row-major-aref bit-array-1 i)
                              (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-andc2 (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (logandc2 (row-major-aref bit-array-1 i)
                              (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-orc1 (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (logorc1 (row-major-aref bit-array-1 i)
                             (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-orc2 (bit-array-1 bit-array-2 &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logand (logorc2 (row-major-aref bit-array-1 i)
                             (row-major-aref bit-array-2 i))
                    1)))))

(defun bit-not (bit-array &optional result-bit-array)
  (declare (type (array bit) bit-array-1 bit-array-2))
  (let ((result-bit-array (pick-result-array result-bit-array bit-array)))
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (declare (type index i))
      (declare (optimize speed (safety 0)))
      (setf (row-major-aref result-bit-array i)
            (logxor (row-major-aref bit-array i) 1)))))
