(in-package "CL-USER")

#+(or :xcl :sbcl :openmcl)
(require "ASDF")
#+clisp
(load "asdf.lisp")
#+xcl
(require "PROFILER")

(defun run-other-tests ()
  (declaim (optimize speed)) ; yes, this is intended to be DECLAIM, not DECLARE!
  (let ((cl-ppcre-directory
         #+windows #p"c:/cygwin/home/peter/cl-ppcre-1.3.0/"
         #-windows #p"/home/peter/cl-ppcre-1.3.0/")
        (ironclad-directory
         #+windows #p"c:/cygwin/home/peter/ironclad_0.22/"
         #-windows #p"/home/peter/ironclad_0.22/"))
    ;; cl-ppcre
    (let ((*default-pathname-defaults* cl-ppcre-directory))
      (map nil #'delete-file (directory "*.xcl"))
      (map nil #'delete-file (directory "*.fasl"))
      (map nil #'delete-file (directory "*.lx64fsl"))
      (map nil #'delete-file (directory "*.fas"))
      (map nil #'delete-file (directory "*.lib"))
      (load "load.lisp")
      #+xcl (ext:gc)
      (time (funcall (find-symbol "TEST" "CL-PPCRE-TEST"))))
    ;; ironclad
    (let ((*default-pathname-defaults* ironclad-directory))
      (map nil #'delete-file (directory "*.xcl"))
      (map nil #'delete-file (directory "*.fasl"))
      (map nil #'delete-file (directory "*.lx64fsl"))
      (map nil #'delete-file (directory "*.fas"))
      (map nil #'delete-file (directory "*.lib"))
      (map nil #'delete-file (directory "test-vectors/*.xcl"))
      (map nil #'delete-file (directory "test-vectors/*.fasl"))
      (map nil #'delete-file (directory "test-vectors/*.lx64fsl"))
      (map nil #'delete-file (directory "test-vectors/*.fas"))
      (map nil #'delete-file (directory "test-vectors/*.lib"))
      (asdf:oos 'asdf:load-op :ironclad :force t)
      #+xcl (ext:gc)
      (time (asdf:oos 'asdf:test-op  :ironclad)))))

#+xcl
(defun profile-cl-ppcre ()
  (let* ((cl-ppcre-directory
          #+windows #p"c:/cygwin/home/peter/cl-ppcre-1.3.0/"
          #-windows #p"/home/peter/cl-ppcre-1.3.0/")
         (*default-pathname-defaults* cl-ppcre-directory))
    (map nil #'delete-file (directory "*.xcl"))
    (load "load.lisp")
    (time (prof:with-profiling () (funcall (find-symbol "TEST" "CL-PPCRE-TEST"))))))

#+xcl
(defun profile-ironclad ()
  (let* ((ironclad-directory
          #+windows #p"c:/cygwin/home/peter/ironclad_0.22/"
          #-windows #p"/home/peter/ironclad_0.22/")
         (*default-pathname-defaults* ironclad-directory))
    (map nil #'delete-file (directory "*.xcl"))
    (map nil #'delete-file (directory "test-vectors/*.xcl"))
    (asdf:oos 'asdf:load-op :ironclad :force t)
    (time (prof:with-profiling () (asdf:oos 'asdf:test-op  :ironclad)))))

(defun run-mop-tests ()
  (let ((lw-compat-directory
         #+windows #p"c:/cygwin/home/peter/lw-compat/"
         #-windows #p"/home/peter/lw-compat/")
        (mop-features-directory
         #+windows #p"c:/cygwin/home/peter/mop-features/"
         #-windows #p"/home/peter/mop-features/"))
    (let ((*default-pathname-defaults* lw-compat-directory))
      (map nil #'delete-file (directory "*.xcl"))
      (asdf:oos 'asdf:load-op :lw-compat :force t))
    (let ((*default-pathname-defaults* mop-features-directory))
      (map nil #'delete-file (directory "*.xcl"))
      (asdf:oos 'asdf:load-op :mop-feature-tests :force t))
    (funcall (find-symbol "RUN-FEATURE-TESTS" "MOP-FEATURE-TESTS"))))
