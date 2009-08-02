(in-package "SYSTEM")

(require "CLOS")

(require 'with-compilation-unit)

(load-system-file "known-functions")

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

(load-system-file "derive-type")

(load-system-file "p2")

#+x86
(load-system-file "p2-x86")

#+x86-64
(load-system-file "p2-x86-64")

(load-system-file "source-transforms")

(load-system-file "compiler")

;; (dolist (sym '(allocate-instance
;;                make-instance
;;                initialize-instance
;;                shared-initialize))
;;   (let ((gf (and (fboundp sym) (fdefinition sym))))
;;     (when (typep gf 'generic-function)
;;       (unless (compiled-function-p gf)
;;         (finalize-generic-function gf)))))

(provide "COMPILER")
