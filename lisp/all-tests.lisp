(in-package "CL-USER")

(let* ((directory (pathname-directory (merge-pathnames "lisp/" *xcl-home*)))
       (defaults (make-pathname :directory directory :type "lisp")))
  (dolist (x '("tests" "compiler-tests" "compile-file-tests" "assembler-tests" "clos-tests"))
    (let ((pathname (merge-pathnames x defaults)))
      (load pathname))))
