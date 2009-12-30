;;; make-host-2.lisp

(defparameter *sbcl-directory*
  #+windows #p"c:\\cygwin\\home\\peter\\sbcl\\"
  #-windows #p"/home/peter/sbcl/")

(setq *default-pathname-defaults* *sbcl-directory*)
;; (setq *load-verbose* t)
(setq *compile-verbose* t)

#+xcl
(setq *print-structure* nil)

(load "make-host-2.lisp")
