;;; compiler-tests.lisp

#+xcl
(resolve 'compile)

#-xcl
(defmacro aver (form)
  `(assert ',form))

(defmacro defun-compile (name lambda-list &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)
     (compile ',name)))

(let (#+xcl
      (c::*catch-errors* nil)
      #+xcl
      (sys:*speed* sys:*speed*)
      #+xcl
      (sys:*debug* sys:*debug*))

  (declaim (optimize (speed 1) (debug 1)))
  #+xcl
  (format t "~%;;; speed ~D debug ~D~%~%" sys:*speed* sys:*debug*)
  (load (merge-pathnames "compiler-tests-aux.lisp" *load-truename*))

  (declaim (optimize (speed 2) (debug 1)))
  #+xcl
  (format t "~%;;; speed ~D debug ~D~%~%" sys:*speed* sys:*debug*)
  (load (merge-pathnames "compiler-tests-aux.lisp" *load-truename*))

  (declaim (optimize (speed 3) (debug 1)))
  #+xcl
  (format t "~%;;; speed ~D debug ~D~%~%" sys:*speed* sys:*debug*)
  (load (merge-pathnames "compiler-tests-aux.lisp" *load-truename*))

  (declaim (optimize (speed 1) (debug 2)))
  #+xcl
  (format t "~%;;; speed ~D debug ~D~%~%" sys:*speed* sys:*debug*)
  (load (merge-pathnames "compiler-tests-aux.lisp" *load-truename*))

  (declaim (optimize (speed 1) (debug 3)))
  #+xcl
  (format t "~%;;; speed ~D debug ~D~%~%" sys:*speed* sys:*debug*)
  (load (merge-pathnames "compiler-tests-aux.lisp" *load-truename*))
  )
