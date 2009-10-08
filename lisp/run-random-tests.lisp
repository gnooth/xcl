;;; run-random-tests.lisp

(in-package "CL-USER")

(defun run-random-tests (size nvars count)
  (let* ((ansi-tests-directory
          (pathname (directory-namestring (merge-pathnames "ansi-tests/" #.*load-pathname*)))))
    (let ((*default-pathname-defaults* ansi-tests-directory))
      (load "gclload1.lsp")
      (load "random-int-form.lsp")
      (let ((f (find-symbol "TEST-RANDOM-INTEGER-FORMS" "CL-TEST")))
        (when f
          (let (#+abcl
                (*suppress-compiler-warnings* t)
                (*package* (find-package "CL-TEST"))
                (*random-state* (make-random-state t)))
            (funcall f size nvars count)))))))
