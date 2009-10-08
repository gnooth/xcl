;;; make-host-1.lisp

(defparameter *sbcl-directory*
  #+windows #p"c:\\cygwin\\home\\peter\\sbcl\\"
  #-windows #p"/home/peter/sbcl/")

(setq *default-pathname-defaults* *sbcl-directory*)
(setq *load-verbose* t)
(setq *compile-verbose* t)

#+xcl
(setq *print-structure* nil)

;; (setq *force-full-calls* t)
;; (setq *force-type-checks* t)

(load "make-host-1.lisp")
