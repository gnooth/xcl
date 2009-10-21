;; (eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct method-combination
    name
    operator
    identity-with-one-argument
    documentation)

  (defun expand-short-defcombin (whole)
    (let* ((name (cadr whole))
           (documentation
            (getf (cddr whole) :documentation ""))
           (identity-with-one-arg
            (getf (cddr whole) :identity-with-one-argument nil))
           (operator
            (getf (cddr whole) :operator name)))
      `(progn
         (setf (get ',name 'method-combination-object)
               (make-method-combination :name ',name
                                        :operator ',operator
                                        :identity-with-one-argument ',identity-with-one-arg
                                        :documentation ',documentation))
         ',name)))

  (defun expand-long-defcombin (whole)
    (declare (ignore whole))
    (error "The long form of DEFINE-METHOD-COMBINATION is not implemented."))
;; )

(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  (if (and (cddr form)
           (listp (caddr form)))
      (expand-long-defcombin form)
      (expand-short-defcombin form)))

