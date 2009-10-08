;;; load-compiler.lisp
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

(require "CLOS")

(require "WITH-COMPILATION-UNIT")

(load-system-file "compiler/known-functions")

(declaim (type (integer 0 3) *speed* *space* *safety* *debug* *compilation-speed*))

(defvar *enable-compiler* t)

(export '*enable-compiler*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require #+x86    "X86"
           #+x86-64 "X86-64"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "ASSEMBLER"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (find-package "COMPILER")
      (use-package #+x86    "X86"
                   #+x86-64 "X86-64"
                   "COMPILER")
      (make-package "COMPILER"
                    :nicknames '("C")
                    :use '("COMMON-LISP" "EXTENSIONS" "SYSTEM"
                           #+x86    "X86"
                           #+x86-64 "X86-64"))))

(load-system-file "compiler/derive-type")

(load-system-file "compiler/ir2-defs")

(load-system-file "compiler/p2")

#+x86
(progn
  (load-system-file "compiler/x86/p2-x86")
  (load-system-file "compiler/x86/p3-x86"))

#+x86-64
(progn
  (load-system-file "compiler/x86-64/p2-x86-64")
  (load-system-file "compiler/x86-64/p3-x86-64"))

(load-system-file "compiler/install-p2-handlers")

(load-system-file "compiler/source-transforms")

(load-system-file "compiler/compiler")

;; (dolist (sym '(allocate-instance
;;                make-instance
;;                initialize-instance
;;                shared-initialize))
;;   (let ((gf (and (fboundp sym) (fdefinition sym))))
;;     (when (typep gf 'generic-function)
;;       (unless (compiled-function-p gf)
;;         (finalize-generic-function gf)))))

(provide "COMPILER")
