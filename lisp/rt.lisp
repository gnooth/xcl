;; rt.lisp

(unless (find-package "REGRESSION-TEST")
  (make-package "REGRESSION-TEST" :nicknames '("RT") :use '("COMMON-LISP")))

(unless (find-package "CL-TEST")
  (make-package "CL-TEST" :use '("REGRESSION-TEST" "COMMON-LISP")))

(in-package "RT")

(export '(my-aref deftest *failed-tests* *ansi-tests-directory*))

(defparameter *ansi-tests-directory*
  (pathname (directory-namestring (merge-pathnames "ansi-tests/" ext:*xcl-home*))))

(defvar *precompile-tests* t)

(defvar *compile-tests* nil)

(defun my-row-major-aref (a index)
  (row-major-aref a index))

(defun my-aref (a &rest args)
  (apply #'aref a args))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters.
Currently doesn't work on arrays of dimension > 2."
  (cond
   ((eq x y) t)
   ((consp x)
    #+xcl
    (and (consp y)
	 (equalp-with-case (sys:%car x) (sys:%car y))
	 (equalp-with-case (sys:%cdr x) (sys:%cdr y)))
    #-xcl
    (and (consp y)
	 (equalp-with-case (car x) (car y))
	 (equalp-with-case (cdr x) (cdr y))))
;;    ((and (typep x 'array)
;; 	 (= (array-rank x) 0))
;;     (equalp-with-case (my-aref x) (my-aref y)))
   ((vectorp x)
    (and (vectorp y)
	 (let ((x-len (length x))
	       (y-len (length y)))
	   (and (eql x-len y-len)
;; 		(loop
;;                   for i from 0 below x-len
;;                   for e1 = (my-aref x i)
;;                   for e2 = (my-aref y i)
;;                   always (equalp-with-case e1 e2))
                (dotimes (i x-len t)
                  (unless (equalp-with-case (aref x i) (aref y i))
                    (return nil)))))))
;;    ((and (typep x 'array)
;; 	 (typep y 'array)
;; 	 (not (equal (array-dimensions x)
;; 		     (array-dimensions y))))
;;     nil)

   ((arrayp x)
    (and (arrayp y)
	 (let ((size (array-total-size x)))
;; 	   (loop for i from 0 below size
;;              always (equalp-with-case (my-row-major-aref x i)
;;                                       (my-row-major-aref y i)))
           (dotimes (i size t)
             (unless (equalp-with-case (my-row-major-aref x i) (my-row-major-aref y i))
               (return nil)))
           )))
   ((typep x 'pathname)
    (equal x y))
   (t (eql x y))))

(defvar *passed* 0)
(defvar *failed* 0)
(defvar *failed-tests* nil)

(defmacro deftest (name &rest body)
  (fresh-line)
  (format t "Test ~S~%" `,name)
  (finish-output)
  (let* ((p body)
	 (properties
	  (loop while (keywordp (first p))
            unless (cadr p)
            do (error "Poorly formed deftest: ~S~%"
                      (list* 'deftest name body))
            append (list (pop p) (pop p))))
	 (form (pop p))
	 (values p))
    (declare (ignore properties))
    (let* ((aborted nil)
           (r (handler-case (multiple-value-list
                             (cond (*compile-tests*
                                    (funcall (compile nil `(lambda () ,form))))
                                   #+xcl
                                   (*precompile-tests*
                                    (eval (ext:precompile-form `,form)))
                                   (t
                                    (eval `,form))))
                (error (c) (setf aborted t) (list c))))
           (passed (and (not aborted) (equalp-with-case r `,values))))
      (unless passed
        (let ((*print-pretty* t))
          (fresh-line)
          (format t "Form: ~S~%" `,form)
          (format t "Expected value: ~S~%"
                  (if (= (length `,values) 1)
                      (car `,values)
                      `,values))
          (let ((r (if (= (length r) 1) (car r) r)))
            (format t "Actual value: ~S" r)
            (when (typep r 'condition)
              (format t " [\"~A\"]" r))
            (terpri))
          (finish-output)))
      (if passed
          (incf *passed*)
          (progn (push name *failed-tests*) (incf *failed*))))))

(in-package "CL-TEST")

(defun notnot (x) (not (not x)))

(defmacro notnot-mv (form)
  `(notnot-mv-fn (multiple-value-list ,form)))

(defun notnot-mv-fn (results)
  (if (null results)
      (values)
      (apply #'values
             (not (not (first results)))
             (rest results))))

(defmacro not-mv (form)
  `(not-mv-fn (multiple-value-list ,form)))

(defun not-mv-fn (results)
  (if (null results)
      (values)
      (apply #'values
             (not (first results))
             (rest results))))

(defun eqt (x y)
  "Like EQ, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eq x y)))))

(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eql x y)))))

(defun equalt (x y)
  "Like EQUAL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equal x y)))))

(defun equalpt (x y)
  "Like EQUALP, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equalp x y)))))

(defun =t (x &rest args)
  "Like =, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (apply #'=  x args)))))

(defun evendigitp (c)
  (notnot (find c "02468")))

(defun odddigitp (c)
  (notnot (find c "13579")))

(defun make-int-list (n)
;;   (loop for i from 0 below n collect i))
  (let ((result ()))
    (dotimes (i n (nreverse result))
      (setq result (cons i result)))))

;; (load "/home/peter/x/ansi-tests/ansi-aux-macros.lsp")

(defparameter *report-and-ignore-errors-break* nil
  "When true, REPORT-AND-IGNORE-ERRORS breaks instead of discarding the error condition.")

(defmacro report-and-ignore-errors (&body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (#+sbcl let #+sbcl () #-sbcl progn
             (handler-case
                 (progn ,@body)
               (error (condition)
                      (princ condition)
                      (terpri)
                      (when *report-and-ignore-errors-break* (break))
                      (values nil condition))))))


(defmacro signals-error (form error-name)
;;   `(locally (declare (optimize safety))
;;      (handler-case ,form
;;        (condition (c) (typep c ,error-name))
;;        (:no-error (&rest ignored) (declare (ignore ignored)) nil))))
  `(handler-case ,form
     (condition (c) (typep c ',error-name))
     (:no-error (&rest ignored) nil)
     ))

(defmacro def-macro-test (test-name macro-form)
  (let ((macro-name (car macro-form)))
    (assert (symbolp macro-name))
    `(deftest ,test-name
       (values
	(signals-error (funcall (macro-function ',macro-name))
		       program-error)
	(signals-error (funcall (macro-function ',macro-name)
				',macro-form)
		       program-error)
	(signals-error (funcall (macro-function ',macro-name)
				',macro-form nil nil)
		       program-error))
       t t t)))

(defun sequencep (x) (typep x 'sequence))

(defmacro def-fold-test (name form)
  "Create a test that FORM, which should produce a fresh value,
does not improperly introduce sharing during constant folding."
  `(deftest ,name
     (flet ((%f () (declare (optimize (speed 3) (safety 0) (space 0)
				      (compilation-speed 0) (debug 0)))
              ,form))
       (eq (%f) (%f)))
     nil))

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

(defvar *compiled-and-loaded-files* (make-hash-table :test 'equal))

(defun compile-and-load (pathspec &key force)
  (unless (gethash pathspec *compiled-and-loaded-files*)
    (load (concatenate 'string
                       (namestring *ansi-tests-directory*)
                       pathspec))
    (setf (gethash pathspec *compiled-and-loaded-files*) t)))

;;; Macros for avoiding dead code warnings

(defvar *should-always-be-true* t)

(declaim (notinline should-never-be-called))

(defun should-never-be-called () nil)

(defmacro normally (form &optional (default-form
				     '(should-never-be-called)))
  `(if *should-always-be-true* ,form ,default-form))

#+xcl
(sys:load-system-file "lisp/universe.lisp")
#-xcl
(load "x/universe.lisp")
(compile-and-load "ansi-aux.lsp")
(compile-and-load "char-aux.lsp")
(compile-and-load "cl-symbol-names.lsp")
(compile-and-load "pathnames-aux.lsp")
(compile-and-load "packages-00.lsp")
(compile-and-load "defclass-aux.lsp")
(compile-and-load "random-aux.lsp")
(compile-and-load "times-aux.lsp")
(compile-and-load "cons-aux.lsp")
(compile-and-load "remove-aux.lsp")
(compile-and-load "search-aux.lsp")

(defun set-up-packages ()
  (safely-delete-package "A")
  (safely-delete-package "B")
  (safely-delete-package "Q")
  (defpackage "A"
    (:use)
    (:nicknames "Q")
    (:export "FOO"))
  (defpackage "B"
    (:use "A")
    (:export "BAR")))

;; This is defined in do-symbols.lsp but needed in do-external-symbols.lsp too.
(defun collect-symbols (pkg)
  (remove-duplicates
   (sort-symbols
    (let ((all nil))
      (do-symbols (x pkg all) (push x all))))))

;; FIXME for now we need to override the definition of this in ansi-aux.lsp
(defmacro defharmless (&rest ignored)
  )

;; FIXME
(defun printable-p (obj)
  "Returns T iff obj can be printed to a string."
;;   (with-standard-io-syntax
;;     (let ((*print-readably* nil)
;;           (*print-escape* nil))
;;       (declare (optimize safety))
;;       (handler-case (and (stringp (write-to-string obj)) t)
;;         (condition (c) (declare (ignore c)) nil))))
  t
  )

(defmacro signals-type-error (var datum-form form &key (safety 3) (inline nil))
  (let ((lambda-form
	 `(lambda (,var)
	    (declare (optimize (safety ,safety)))
	    ,form)))
    `(let ((,var ,datum-form))
;;        (declare (optimize safety))
       (handler-bind
         ((warning #'(lambda (c)
;;                       (declare (ignore c))
;; 		      (muffle-warning)
                      nil ;; is this right?
                      )))
         ; (proclaim '(optimize (safety 3)))
         (handler-case
             (apply #'values
                    nil
                    (multiple-value-list
                     (funcall
                      ,(cond
                        (inline `(function ,lambda-form))
;;                         (regression-test::*compile-tests*
;;                          `(compile nil ',lambda-form))
                        (t `(eval ',lambda-form)))
                      ,var)))
           (type-error
            (c)
            (let ((datum (type-error-datum c))
                  (expected-type (type-error-expected-type c)))
              (cond
               ((not (eql ,var datum))
                (list :datum-mismatch ,var datum))
               ((typep datum expected-type)
                (list :is-typep datum expected-type))
               (t (printable-p c))
               ))))))))

(defun check-type-predicate (P TYPE)
  "Check that a predicate P is the same as #'(lambda (x) (typep x TYPE))
by applying both to all elements of *UNIVERSE*.  Print message
when a mismatch is found, and return number of mistakes."

  (when (symbolp p)
    (assert (fboundp p))
    (setf p (symbol-function p)))
  (assert (typep p 'function))

  (loop
    for x in *universe*
    when
    (block failed
      (let ((p1 (handler-case
                    (funcall (the function p) x)
                  (error () (format t "(FUNCALL ~S ~S) failed~%"
                                    P x)
                         (return-from failed t))))
            (p2 (handler-case
                    (typep x TYPE)
                  (error () (format t "(TYPEP ~S '~S) failed~%"
                                    x TYPE)
                         (return-from failed t)))))
        (when (or (and p1 (not p2))
                  (and (not p1) p2))
          (format t "(FUNCALL ~S ~S) = ~S, (TYPEP ~S '~S) = ~S~%"
                  P x p1 x TYPE p2)
          t)))
    collect x))

;; (declaim (special *mini-universe*))
(defvar *mini-universe*)

(defun check-type-error* (pred-fn guard-fn &optional (universe *mini-universe*))
  "Check that for all elements in some set, either guard-fn is true or
pred-fn signals a type error."
  (let (val)
    (loop for e in universe
      unless (or (funcall guard-fn e)
                 (equal
                  (setf val (multiple-value-list
                             (signals-type-error x e (funcall pred-fn x) :inline t)))
                  '(t)))
      collect (list e val))))

(defmacro check-type-error (&body args)
  `(locally (declare (optimize safety)) (check-type-error* ,@args)))

(defvar *universe*)

(defun check-predicate (predicate &optional guard (universe *universe*))
  "Return all elements of UNIVERSE for which the guard (if present) is false
and for which PREDICATE is false."
  (remove-if #'(lambda (e) (or (and guard (funcall guard e))
			       (funcall predicate e)))
	     universe))

(defun random-fixnum ()
  (+ (random (1+ (- most-positive-fixnum most-negative-fixnum)))
     most-negative-fixnum))

(defparameter *use-random-byte* t)
(defparameter *random-readable* nil)

(defun make-random-string (size-spec &key simple)
  (let*
    ((size (if (eql size-spec '*) (random 30) size-spec))
     (use-random-byte nil)
     (etype 'character)
     (s (random-case
         (progn
           (setf use-random-byte *use-random-byte*)
           (make-string size :element-type 'character))
         (progn
           (setf use-random-byte *use-random-byte*)
           (make-array size :element-type 'character
                       :initial-element #\a))
         (make-array size :element-type (setf etype (if *random-readable* 'character 'standard-char))
                     :adjustable (and (not simple) (not *random-readable*) (rcase (3 nil) (1 t)))
                     :fill-pointer (and (not simple) (not *random-readable*) (rcase (3 nil) (1 (random (1+ size)))))
                     :initial-element #\a)
         (make-array size :element-type (setf etype (if *random-readable* 'character 'base-char))
                     :adjustable (and (not simple) (not *random-readable*) (rcase (3 nil) (1 t)))
                     :fill-pointer (and (not simple) (not *random-readable*) (rcase (3 nil) (1 (random (1+ size)))))
                     :initial-element #\a))))
    (if (coin)
	(dotimes (i size)
	  (setf (char s i) (elt #(#\a #\b #\A #\B) (random 4))))
        (dotimes (i size)
          (setf (char s i)
                (or (and (eql etype 'character)
                         use-random-byte
                         (or (code-char (random (min char-code-limit (ash 1 16))))
                             (code-char (random 256))))
                    (elt "abcdefghijklmnopqrstuvwyxzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
                         (random 62))))))
    (when (and (not simple) (not *random-readable*) (coin 5))
      (let ((len (+ (random (1+ size)) size)))
	(setq s (make-random-string len))
	(setq etype (array-element-type s))
	(setq s (make-array size
			    :element-type etype
			    :displaced-to s
			    :displaced-index-offset (random (1+ (- len size)))))))

    s))

(defun random-from-interval (upper &optional (lower (- upper)))
  (+ (random (- upper lower)) lower))

(defun coin (&optional (n 2))
  "Flip an n-sided coin."
  (eql (random n) 0))

;;; Randomly permute a sequence
(defun random-permute (seq)
  (setq seq (copy-seq seq))
  (let ((len (length seq)))
    (loop for i from len downto 2
      do (let ((r (random i)))
           (rotatef (elt seq r) (elt seq (1- i))))))
  seq)

(defun rational-safely (x)
  "Rational a floating point number, making sure the rational
number isn't 'too big'.  This is important in implementations such
as clisp where the floating bounds can be very large."
  (assert (floatp x))
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float x)
    (let ((limit 1000)
	  (radix (float-radix x)))
      (cond
       ((< exponent (- limit))
	(* significand (expt radix (- limit)) sign))
       ((> exponent limit)
	(* significand (expt radix limit) sign))
       (t (rational x))))))

;;; Approximate equality function
(defun approx= (x y &optional (eps (epsilon x)))
  (<= (abs (/ (- x y) (max (abs x) 1))) eps))

(defun epsilon (number)
;;   (etypecase number
  (typecase number
    (complex (* 2 (epsilon (realpart number)))) ;; crude
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)
    (rational 0)
    (t
     (error "Shouldn't happen!"))))

(defun negative-epsilon (number)
;;   (etypecase number
  (typecase number
    (complex (* 2 (negative-epsilon (realpart number)))) ;; crude
    (short-float short-float-negative-epsilon)
    (single-float single-float-negative-epsilon)
    (double-float double-float-negative-epsilon)
    (long-float long-float-negative-epsilon)
    (rational 0)
    (t
     (error "Shouldn't happen!"))))

(defun sequencep (x) (typep x 'sequence))

(defun typef (type) #'(lambda (x) (typep x type)))

;; ;; cons-aux.lsp
;; (defun subsetp-with-check (x y &key (key 'no-key) test test-not)
;;   (let ((xcopy (make-scaffold-copy x))
;; 	(ycopy (make-scaffold-copy y)))
;;     (let ((result
;; 	   (apply #'subsetp x y
;; 		  `(,@(unless (eqt key 'no-key)
;; 			`(:key ,key))
;;                     ,@(when test `(:test ,test))
;;                     ,@(when test-not `(:test-not ,test-not))))))
;;       (cond
;;        ((and (check-scaffold-copy x xcopy)
;; 	     (check-scaffold-copy y ycopy))
;; 	(not (not result)))
;;        (t 'failed)))))

;; (sys:load-system-file "universe.lisp")

#+xcl
(when (fboundp 'c::%compile)
  (let ((sys:*speed* 3)
        (c::*catch-errors* nil))
    (mapc 'c::%compile '(rt::equalp-with-case
                         cl-test::check-scaffold-copy
                         cl-test::make-scaffold-copy
                         cl-test::nat-times
                         cl-test::notnot
                         cl-test::split-list
                         cl-test::delete-all-versions
                         cl-test::make-random-element
                         cl-test::make-random-remove-input
;;                          cl-test::random-test-delete-if
                         cl-test::random-test-remove-args
;;                          cl-test::random-test-remove-if
                         cl-test::search-check
                         cl-test::subseq-equalp
                         cl-test::shuffle
                         cl-test::split-list
                         cl-test::equiv
                         ))))

(in-package "CL-USER")

(defun compile-and-load (&rest args)
  (apply #'cl-test::compile-and-load args))

(defparameter *default-tests*
  '("abort"
    "abs"
    "acos"
    "acosh"
    "acons"
    "adjoin"
    "adjust-array"
    "adjustable-array-p"
    "and"
    "append"
    "apply"
    "apropos"
    "apropos-list"
    "aref"
    "array"
    "array-as-class"
    "array-dimension"
    "array-dimensions"
    "array-displacement"
    "array-element-type"
    "array-has-fill-pointer-p"
    "array-in-bounds-p"
    "array-misc"
    "array-rank"
    "array-row-major-index"
    "array-t"
    "array-total-size"
    "arrayp"
    "ash"
    "asin"
    "asinh"
    "assoc"
    "assoc-if"
    "assoc-if-not"
    "atan"
    "atanh"
    "atom"
    "base-string"
    "bit"
    "bit-and"
    "bit-andc1"
    "bit-andc2"
    "bit-eqv"
    "bit-ior"
    "bit-nand"
    "bit-nor"
    "bit-not"
    "bit-orc1"
    "bit-orc2"
    "bit-vector"
    "bit-vector-p"
    "bit-xor"
    "block"
    "boole"
    "boundp"
    "broadcast-stream-streams"
    "butlast"
    "byte"
    "call-arguments-limit"
    "case"
    "catch"
    "ccase"
    "ceiling"
    "cell-error-name"
    "char-compare"
    "char-schar"
    "character"
    "check-type"
    "cis"
    "class-of"
    "class-precedence-lists"
    "clear-input"
    "clear-output"
    "clrhash"
    "coerce"
    "complement"
    "complexp"
    "compute-restarts"
    "concatenate"
    "concatenated-stream-streams"
    "cond"
    "conjugate"
    "consp"
    "constantly"
    "constantp"
    "continue"
    "copy-alist"
    "copy-list"
    "copy-readtable"
    "copy-seq"
    "copy-symbol"
    "cos"
    "cosh"
    "count"
    "count-if"
    "count-if-not"
    "ctypecase"
    "cxr"
    "decf"
    "declaim"
    "decode-universal-time"
    "defconstant"
    "define-compiler-macro"
    "defpackage"
    "defparameter"
    "defsetf"
    "deftype"
    "defun"
    "defvar"
    "delete-file"
    "delete-package"
    "deposit-field"
    "directory"
    "directory-namestring"
    "dispatch-macro-characters"
    "divide"
    "do"
    "do-all-symbols"
    "do-external-symbols"
    "do-symbols"
    "dolist"
    "dostar"
    "dotimes"
    "dpb"
    "ecase"
    "echo-stream-input-stream"
    "echo-stream-output-stream"
    "elt"
    "encode-universal-time"
    "enough-namestring"
    "environment-functions"
    "eql"
    "equal"
    "equalp"
    "etypecase"
    "evenp"
    "every"
    "exp"
    "export"
    "expt"
    "fboundp"
    "fceiling"
    "fdefinition"
    "ffloor"
    "file-author"
    "file-length"
    "file-namestring"
    "file-position"
    "file-string-length"
    "file-write-date"
    "fill"
    "fill-pointer"
    "find"
    "find-all-symbols"
    "find-if"
    "find-if-not"
    "find-package"
    "find-symbol"
    "finish-output"
    "flet"
    "float"
    "floatp"
    "floor"
    "fmakunbound"
    "force-output"
    "fresh-line"
    "fround"
    "ftruncate"
    "funcall"
    "function"
    "functionp"
    "gcd"
    "gensym"
    "gentemp"
    "get"
    "get-macro-character"
    "get-output-stream-string"
    "get-properties"
    "get-universal-time"
    "getf"
    "gethash"
    "handler-bind"
    "handler-case"
    "hash-table-count"
    "hash-table-test"
    "host-namestring"
    "identity"
    "if"
    "ignore-errors"
    "imagpart"
    "import"
    "in-package"
    "incf"
    "input-stream-p"
    "integer-length"
    "integerp"
    "interactive-stream-p"
    "intern"
    "intersection"
    "isqrt"
    "keyword"
    "keywordp"
    "labels"
    "lambda"
    "lambda-list-keywords"
    "lambda-parameters-limit"
    "last"
    "lcm"
    "ldb"
    "ldiff"
    "length"
    "let"
    "letstar"
    "list"
    "list-all-packages"
    "list-length"
    "listen"
    "listp"
    "load-structures"
    "locally"
    "log"
    "logand"
    "logandc1"
    "logandc2"
    "logbitp"
    "logcount"
    "logeqv"
    "logior"
    "lognand"
    "lognor"
    "lognot"
    "logorc1"
    "logorc2"
    "logtest"
    "logxor"
    "loop"
    "loop1"
    "loop10"
    "loop11"
    "loop12"
    "loop13"
    "loop14"
    "loop15"
    "loop16"
    "loop17"
    "loop2"
    "loop3"
    "loop4"
    "loop5"
    "loop6"
    "loop7"
    "loop8"
    "loop9"
    "macro-function"
    "macroexpand"
    "macrolet"
    "make-array"
    "make-broadcast-stream"
    "make-concatenated-stream"
    "make-echo-stream"
    "make-hash-table"
    "make-list"
    "make-package"
    "make-random-state"
    "make-sequence"
    "make-string"
    "make-string-input-stream"
    "make-string-output-stream"
    "make-symbol"
    "make-synonym-stream"
    "make-two-way-stream"
    "makunbound"
    "map-into"
    "mapc"
    "mapcan"
    "mapcar"
    "mapcon"
    "mapl"
    "maplist"
    "mask-field"
    "max"
    "member"
    "member-if"
    "member-if-not"
    "merge"
    "merge-pathnames"
    "min"
    "minus"
    "minusp"
    "mismatch"
    "modules"
    "muffle-warning"
    "multiple-value-bind"
    "multiple-value-call"
    "multiple-value-list"
    "multiple-value-prog1"
    "multiple-value-setq"
    "namestring"
    "nbutlast"
    "nconc"
    "nil"
    "nintersection"
    "not-and-null"
    "notany"
    "notevery"
    "nreconc"
    "nreverse"
    "nset-difference"
    "nset-exclusive-or"
    "nstring-capitalize"
    "nstring-downcase"
    "nstring-upcase"
    "nsublis"
    "nsubst"
    "nsubst-if"
    "nsubst-if-not"
    "nsubstitute"
    "nsubstitute-if"
    "nsubstitute-if-not"
    "nth"
    "nth-value"
    "nthcdr"
    "number-comparison"
    "numberp"
    "nunion"
    "oddp"
    "open"
    "open-stream-p"
    "or"
    "output-stream-p"
    "package-error"
    "package-error-package"
    "package-name"
    "package-nicknames"
    "package-shadowing-symbols"
    "package-use-list"
    "package-used-by-list"
    "packagep"
    "pairlis"
    "parse-integer"
    "parse-namestring"
    "pathname"
    "pathname-device"
    "pathname-directory"
    "pathname-host"
    "pathname-match-p"
    "pathname-name"
    "pathname-type"
    "pathname-version"
    "pathnamep"
    "peek-char"
    "phase"
    "places"
    "plus"
    "plusp"
    "pop"
    "position"
    "position-if"
    "position-if-not"
    "prin1"
    "princ"
    "print"
    "print-integers"
    "print-unreadable-object"
    "probe-file"
    "proclaim"
    "prog"
    "prog1"
    "prog2"
    "progn"
    "progv"
    "psetf"
    "psetq"
    "push"
    "pushnew"
    "random"
    "random-state-p"
    "rassoc"
    "rassoc-if"
    "rassoc-if-not"
    "rational"
    "rationalize"
    "rationalp"
    "read"
    "read-byte"
    "read-char"
    "read-char-no-hang"
    "read-delimited-list"
    "read-from-string"
    "read-line"
    "read-preserving-whitespace"
    "read-sequence"
    "read-suppress"
    "reader-test"
    "readtable-case"
    "readtablep"
    "realp"
    "realpart"
    "reduce"
    "remf"
    "remhash"
    "remove"
    "remove-duplicates"
    "remprop"
    "rename-file"
    "rename-package"
    "replace"
    "rest"
    "restart-bind"
    "restart-case"
    "return"
    "return-from"
    "revappend"
    "reverse"
    "round"
    "row-major-aref"
    "rplaca"
    "rplacd"
    "sbit"
    "search-bitvector"
    "search-list"
    "search-string"
    "search-vector"
    "set"
    "set-difference"
    "set-exclusive-or"
    "set-macro-character"
    "set-syntax-from-char"
    "shadow"
    "shadowing-import"
    "signum"
    "simple-array"
    "simple-array-t"
    "simple-base-string"
    "simple-bit-vector"
    "simple-bit-vector-p"
    "simple-string"
    "simple-string-p"
    "simple-vector-p"
    "sin"
    "sinh"
    "sleep"
    "sort"
    "sqrt"
    "stable-sort"
    "store-value"
    "stream-element-type"
    "stream-error-stream"
    "stream-external-format"
    "streamp"
    "string"
    "string-capitalize"
    "string-comparisons"
    "string-downcase"
    "string-left-trim"
    "string-right-trim"
    "string-trim"
    "string-upcase"
    "stringp"
    "sublis"
    "subseq"
    "subsetp"
    "subst"
    "subst-if"
    "subst-if-not"
    "substitute"
    "substitute-if"
    "substitute-if-not"
    "subtypep"
    "subtypep-array"
    "subtypep-integer"
    "svref"
    "symbol-function"
    "symbol-macrolet"
    "symbol-name"
    "symbolp"
    "synonym-stream-symbol"
    "syntax"
    "syntax-tokens"
    "tagbody"
    "tailp"
    "tan"
    "tanh"
    "terpri"
    "times"
    "tree-equal"
    "truename"
    "truncate"
    "two-way-stream-input-stream"
    "two-way-stream-output-stream"
    "type-of"
    "typecase"
    "typep"
    "unexport"
    "unintern"
    "union"
    "unless"
    "unread-char"
    "unuse-package"
    "unwind-protect"
    "upgraded-array-element-type"
    "use-package"
    "use-value"
    "values"
    "values-list"
    "vector"
    "vector-pop"
    "vector-push"
    "vector-push-extend"
    "vectorp"
    "when"
    "wild-pathname-p"
    "with-condition-restarts"
    "with-hash-table-iterator"
    "with-input-from-string"
    "with-open-file"
    "with-open-stream"
    "with-output-to-string"
    "with-package-iterator"
    "with-simple-restart"
    "with-standard-io-syntax"
    "write"
    "write-char"
    "write-line"
    "write-sequence"
    "write-string"
    "write-to-string"
    "zerop"
    ))

(defun do-tests (&rest tests)
  (let* ((rt::*passed* 0)
         (rt::*failed* 0)
         (*default-pathname-defaults* rt::*ansi-tests-directory*))
    (setq rt::*failed-tests* nil)
    (unless tests
      (setq tests *default-tests*))
    (dolist (test tests)
      (let ((filename (concatenate 'string
                                   test ".lsp")))
        (load filename)))
    (format t "~A tests: ~A passed, ~A failed~%"
            (+ rt::*passed* rt::*failed*)
            rt::*passed*
            rt::*failed*)
    (values)))

(defun do-compiled-tests (&rest tests)
;;   (require "COMPILER")
  (let ((rt::*compile-tests* t))
    (apply 'do-tests tests)))
