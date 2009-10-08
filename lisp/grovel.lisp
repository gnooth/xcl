;;; grovel.lisp
;;;
;;; Copyright (C) 2006-2009 Peter Graves <peter@armedbear.org>
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

(defun grovel-cpp-definitions-in-file (file out)
  (with-open-file (in file)
    (declare (type stream in))
    (let ((system-package (find-package "SYSTEM"))
          (line-number 1))
      (loop
        (let ((text (read-line in nil)))
          (when (null text)
            (return))
          (let ((position (search "### " text)))
            (when position
                 (let* ((start-index (+ position 4))
                        (end-index (or (position #\space text :start start-index) (length text)))
                        (name (string-upcase (subseq text start-index end-index)))
                        (symbol (or (find-symbol name system-package) ; uses CL and EXT
                                    (find-symbol name (find-package "MOP")))))
                   (when symbol
                     ;; Force the symbol's package prefix to be written out
                     ;; with "::" instead of ":" so there won't be a reader
                     ;; error if a symbol that's external now is no longer
                     ;; external when we read the tags file.
                     (format out "~A::~A ~S ~S~%"
                             (package-name (symbol-package symbol))
                             name
                             file line-number)))))
          (incf line-number))))))

(defun grovel-cpp-definitions ()
  (let ((files (directory (merge-pathnames "kernel/*.cpp" *xcl-home*))))
    (with-open-file (stream (merge-pathnames "kernel/tags" *xcl-home*)
                            :direction :output :if-exists :supersede)
      (dolist (file files)
        (grovel-cpp-definitions-in-file file stream)))))
