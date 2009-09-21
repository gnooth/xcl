;;; boot.lisp
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

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (sys:%in-package "SYSTEM"))

(setq *load-verbose*     nil)
(setq *autoload-verbose* nil)

(load-system-file "backquote.lisp")
(load-system-file "early-macros.lisp")
(load-system-file "featurep.lisp")
(load-system-file "read-conditional.lisp")

(defun caaaar (list) (car (car (car (car list)))))
(defun caaadr (list) (car (car (car (cdr list)))))
(defun caaddr (list) (car (car (cdr (cdr list)))))
(defun cadddr (list) (car (cdr (cdr (cdr list)))))
(defun cddddr (list) (cdr (cdr (cdr (cdr list)))))
(defun cdaaar (list) (cdr (car (car (car list)))))
(defun cddaar (list) (cdr (cdr (car (car list)))))
(defun cdddar (list) (cdr (cdr (cdr (car list)))))
(defun caadar (list) (car (car (cdr (car list)))))
(defun cadaar (list) (car (cdr (car (car list)))))
(defun cadadr (list) (car (cdr (car (cdr list)))))
(defun caddar (list) (car (cdr (cdr (car list)))))
(defun cdaadr (list) (cdr (car (car (cdr list)))))
(defun cdadar (list) (cdr (car (cdr (car list)))))
(defun cdaddr (list) (cdr (car (cdr (cdr list)))))
(defun cddadr (list) (cdr (cdr (car (cdr list)))))

(defun fifth   (list) (car (cddddr list)))
(defun sixth   (list) (cadr (cddddr list)))
(defun seventh (list) (caddr (cddddr list)))
(defun eighth  (list) (cadddr (cddddr list)))
(defun ninth   (list) (car (cddddr (cddddr list))))
(defun tenth   (list) (cadr (cddddr (cddddr list))))

(defun mapcan (function &rest lists)
  (apply #'nconc (apply #'mapcar function lists)))

(autoload 'make-list)

(autoload 'make-string)

(defun make-package (package-name &key nicknames use)
  (%make-package package-name nicknames use))

;; redefined in define-modify-macro.lisp
(defmacro incf (place &optional (delta 1))
  `(setq ,place (+ ,place ,delta)))

;; redefined in define-modify-macro.lisp
(defmacro decf (place &optional (delta 1))
  `(setq ,place (- ,place ,delta)))

;; redefined in push.lisp
(defmacro push (item place)
  `(setq ,place (cons ,item ,place)))

(defun ext:precompile (name &optional definition)
  (or name definition))

;; redefined in pprint.lisp
(defun ext:charpos (stream)
  (%stream-charpos stream))

;; redefined in pprint.lisp
(defun (setf ext:charpos) (new-value stream)
  (%stream-set-charpos stream new-value))

(load-system-file "defmacro.lisp")

(load-system-file "defconstant.lisp")

(defconstant +nil-symbol-name+ "NIL")
(export '+nil-symbol-name+)

(load-system-file "defparameter.lisp")

(autoload-macro 'destructuring-bind)

;; (set-symbol-function 'defmacro (macro-function 'defmacro))
;; (set-symbol-plist 'defmacro nil)
;; (set-macro-function 'defmacro (make-macro 'defmacro (symbol-function 'defmacro)))

(let ((x (macro-function 'defmacro)))
  (set-symbol-function 'defmacro x) ; will be overwritten, but clears special operator flag
  (set-symbol-plist 'defmacro nil)
  (set-macro-function 'defmacro x))

(defun ansi-loop (exps)
  (load-system-file "loop")
  (fmakunbound 'ansi-loop)
  `(loop ,@exps))

(defmacro loop (&rest exps)
  (dolist (exp exps)
    (when (atom exp)
      (return-from loop (ansi-loop exps))))
  (let ((tag (gensym)))
    `(block nil (tagbody ,tag ,@exps (go ,tag)))))

;; redefined in restart.lisp
(defun warn (&rest args)
  (format t "WARNING: ~S~%" args))

(%defconstant 'lambda-list-keywords
  '(&optional &rest &key &aux &body &whole &allow-other-keys &environment))

(load-system-file "inline-expansion.lisp")
(load-system-file "setf.lisp")
(load-system-file "compiler-error.lisp")
(load-system-file "precompiler.lisp")

(defun make-thread (function &key name)
  (%make-thread function name))

(defun make-mutex (&key name)
  (%make-mutex name))

(defun mapappend (function &rest lists)
  (apply #'append (apply #'mapcar function lists)))
(export 'mapappend)

(autoload-macro 'ecase)
(autoload-macro 'typecase)
(autoload-macro 'etypecase)

(load-system-file "member.lisp")

(autoload-macro 'apply-key)

;; needed for reader.lisp
(load-system-file "make-hash-table.lisp")

;; needed for reader.lisp
(load-system-file "canonicalize-type.lisp")

;; needed for reader.lisp
(load-system-file "typep.lisp")

;; needed for reader.lisp
(load-system-file "apply-key.lisp")
(load-system-file "find.lisp")

(load-system-file "reader.lisp")

(load-system-file "adjoin.lisp")
(load-system-file "pushnew.lisp")

(load-system-file "ldb.lisp")
(load-system-file "typecase.lisp")
(load-system-file "etypecase.lisp")
(load-system-file "defknown.lisp")
(load-system-file "instruction.lisp")
(load-system-file "local-variable-information.lisp")

(maybe-load-system-file "ldb.xcl")
(maybe-load-system-file "defknown.xcl")
(maybe-load-system-file "instruction.xcl")
(maybe-load-system-file "local-variable-information.xcl")
(maybe-load-system-file "inline-expansion.xcl")
(maybe-load-system-file "setf.xcl")
(maybe-load-system-file "compiler-error.xcl")
(maybe-load-system-file "precompiler.xcl")
(maybe-load-system-file "canonicalize-type.xcl")
(maybe-load-system-file "typep.xcl")
(maybe-load-system-file "find.xcl")
(maybe-load-system-file "defmacro.xcl")
(maybe-load-system-file "defconstant.xcl")
(maybe-load-system-file "defparameter.xcl")
(maybe-load-system-file "backquote.xcl")
(maybe-load-system-file "featurep.xcl")
(maybe-load-system-file "read-conditional.xcl")
(maybe-load-system-file "adjoin.xcl")
(maybe-load-system-file "pushnew.xcl")
(maybe-load-system-file "member.xcl")
(maybe-load-system-file "make-hash-table.xcl")

(maybe-load-system-file "typecase.xcl")
(maybe-load-system-file "etypecase.xcl")

(maybe-load-system-file "early-macros.xcl")
(maybe-load-system-file "apply-key.xcl")

(autoload-macro 'defsetf)

(load-system-file "invoke-debugger")
(load-system-file "signal")
(load-system-file "concatenate")
(autoload '(make-array adjust-array))
(load-system-file "copy-seq")

(autoload 'parse-lambda-list)
(autoload-macro 'defstruct)

(export '(source-transform define-source-transform expand-source-transform))
(autoload-macro 'define-source-transform "define-source-transform")
(autoload '(source-transform set-source-transform expand-source-transform)
          "define-source-transform")

(autoload 'proclaim)
(autoload-macro 'declaim)

(autoload '(string= string/= string< string<= string> string>= string-equal
            string-not-equal string-greaterp string-not-greaterp string-lessp
            string-not-lessp)
          "strings")

(defun complement (f)
  #'(lambda (&rest x) (not (apply f x))))

(defun constantly (x)
  #'(lambda (&rest args) (declare (ignore args)) x))

(autoload 'revappend)
(autoload 'upgraded-array-element-type)
(autoload '(%subtypep subtypep) "subtypep")

(load-system-file "require-type")

(autoload 'make-iterator)
(autoload-macro 'do-subsequence)

(autoload '(every every2) "every")
(autoload 'notevery)
(autoload '(some notany) "some")

(load-system-file "late-setf")

(autoload '(dpb %dpb) "dpb")

(autoload-macro 'psetq)
(autoload-macro '(make-string-output-stream with-output-to-string))

;; (load-system-file "find.lisp")
;; (autoload '(position position-if position-if-not find find-if find-if-not
;;             list-find* vector-find*)
;;           "find")

;; (load-system-file "assoc.lisp")
(autoload '(assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
            pairlis copy-alist)
          "assoc")

(autoload 'subsetp)
(autoload 'list-length)
(autoload 'ldiff)
(autoload 'mapl)

;; REVIEW
(defun shrink-vector (vector new-size)
  (subseq vector 0 new-size))

(autoload 'make-sequence-of-type)

;; (defmacro make-sequence-like (sequence length)
;;   `(make-sequence-of-type (type-of ,sequence) ,length))

(autoload-macro 'deftype)
(autoload 'make-sequence)
(autoload '(set-difference nset-difference))
(autoload '(remove-duplicates delete-duplicates))
(autoload '(intersection nintersection))
(autoload-macro '(psetf shiftf rotatef))
(autoload '(sort stable-sort merge))
(autoload 'replace)
(autoload '(string-trim string-right-trim string-left-trim))
(autoload '(delete delete-if delete-if-not) "delete")
(autoload '(remove remove-if remove-if-not) "remove")
(autoload 'reduce)
(autoload 'map)
(autoload 'enough-namestring)
(autoload 'parse-namestring)
(autoload 'pathname-match-p)
(autoload 'directory)
(autoload '(string-downcase string-upcase nstring-downcase nstring-upcase
            string-capitalize nstring-capitalize))
(autoload 'disassemble)
(autoload '(provide require))
(autoload '(count count-if count-if-not) "count")
(autoload 'search)
(autoload '(sublis nsublis))
(autoload '(subst nsubst subst-if subst-if-not nsubst-if nsubst-if-not))
(autoload 'bit)
(autoload '(sbit set-sbit) "sbit")
(autoload '(butlast nbutlast))
(autoload '(union nunion))
(autoload 'parse-integer)
;; (autoload 'mod)
(autoload 'gcd)
(autoload '(two-arg-lcm lcm) "lcm")
(autoload '(mask-field deposit-field))
(autoload 'round)
(autoload 'conjugate)
(autoload-macro '(trace untrace) "trace")

(autoload '%print-unreadable-object "print-unreadable-object")
(autoload-macro 'print-unreadable-object)
(autoload-macro 'with-standard-io-syntax)

(autoload 'read-from-string)
(autoload-macro 'with-input-from-string)

(autoload-macro 'and)
(autoload-macro 'or)
(autoload-macro 'case)
(autoload-macro 'cond)
(autoload-macro '(do do*) "do")
(autoload-macro '(dolist dotimes))
(autoload-macro 'multiple-value-list)

(autoload-macro 'aver)
(autoload '%failed-aver "aver")

(autoload '(open write-byte read-byte write-sequence read-sequence))
(autoload-macro 'with-open-file)

(load-system-file "define-modify-macro")

(autoload-macro 'with-hash-table-iterator)
(autoload 'hash-table-iterator-function "with-hash-table-iterator")

(defvar *mumble* t)
(export '(*mumble* mumble))
(autoload 'mumble "mumble")

(autoload 'compile "load-compiler")

;; FIXME
(defmacro defgeneric (&rest args))
(defmacro defmethod (&rest args))
(load-system-file "defvar")
(load-system-file "defconstant")
(defmacro define-condition (&rest args))

(autoload 'assign-setf-macro "defsetf")
(load-system-file "initialize-classes")

(load-system-file "restart")
(load-system-file "top-level")

(export '(coerce-list-to-vector coerce-vector-to-list))
(autoload '(coerce coerce-list-to-vector coerce-vector-to-list) "coerce")
(autoload 'delete-package)
(autoload-macro 'defpackage)
(autoload-macro 'do-external-symbols)
(autoload 'find-all-symbols)
(autoload-macro 'do-symbols)
(autoload-macro 'do-all-symbols)
(autoload 'package-iterator-function "with-package-iterator")
(autoload-macro 'with-package-iterator)
(autoload '(set-exclusive-or nset-exclusive-or))
(autoload 'check-type-error "check-type")
(autoload-macro '(ccase ctypecase check-type))
(load-system-file "define-compiler-macro")

(autoload-macro 'defclass)
(autoload-macro '(defgeneric defmethod) "clos")
(autoload '(ensure-class documentation) "clos")
(autoload 'gentemp)
(autoload 'mismatch)
(autoload '(substitute substitute-if substitute-if-not) "substitute")
(autoload-macro 'with-slots)
(autoload '(write-string write-line))
(autoload 'compile-file-pathname)
(autoload-macro '(prog prog*) "prog")
(autoload-macro 'multiple-value-bind)
(autoload '(bit-array-same-dimensions-p require-same-dimensions pick-result-array
            bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor
            bit-andc1 bit-andc2 bit-orc1 bit-orc2 bit-not)
          "bit-array-ops")
(autoload-macro 'remf)
(autoload-macro 'with-open-stream)
(autoload-macro 'deftype)
(autoload 'boole)
(autoload '(member-if member-if-not))
(autoload 'tailp)
(autoload 'tree-equal)
(autoload '(map-into maplist mapcon))
(autoload '(nsubstitute nsubstitute-if nsubstitute-if-not))
(autoload 'fill)
(autoload-macro 'with-accessors)
;; (autoload 'write)
(autoload 'write-to-string)
(autoload 'copy-symbol)

(autoload 'ext:make-socket)
(autoload 'inspect)
(autoload '(compile-file compile-file-if-needed) "compile-file")
(autoload-macro 'with-compilation-unit)
(autoload '%with-compilation-unit "with-compilation-unit")
(autoload 'copy-file)
(autoload '(ftruncate ffloor fceiling fround))
(autoload '(decode-universal-time get-decoded-time encode-universal-time) "universal-time")
(autoload-macro 'time)
(autoload '%time "time")

(autoload '(phase scale-float))

(export 'grovel-cpp-definitions)
(autoload 'grovel-cpp-definitions "grovel")

(autoload 'rebuild-lisp)

(export '(dump-form dump-top-level-form)) ; REVIEW also in dump-form.lisp
(autoload '(dump-form dump-top-level-form) "dump-form")

;; (autoload-macro 'defknown "known-functions")
;; (autoload '%defknown "known-functions")

(autoload '(source source-pathname source-file-position) "source")

(autoload 'ed)

(autoload 'make-load-form-saving-slots)

(autoload '(asin complex-asin) "asin")
(autoload '(acos complex-acos) "acos")

(autoload '(sinh cosh tanh))

(autoload '(asinh complex-asinh) "asinh")
(autoload '(acosh complex-acosh) "acosh")
(autoload '(atanh complex-atanh) "atanh")

(autoload 'file-author)
(autoload 'upgraded-complex-part-type)
(autoload 'dribble)
(autoload 'ensure-directories-exist)

(autoload-macro 'assert)

;; (load-system-file "reader.lisp")

(make-package "XP" :use '("CL"))

;; (autoload '(pprint
;;             copy-pprint-dispatch
;;             xp::maybe-initiate-xp-printing
;;             xp::write+
;;             xp::write-string++
;;             )
;;           "pprint")

(in-package "XP")

;; redefined in pprint.lisp
(defun xp-structure-p (ignored) nil)

(in-package "SYSTEM")

(load-system-file "loop")
(load-system-file "print")
(load-system-file "defstruct") ; needed for pprint.xcl
(load-system-file "pprint") ; REVIEW

(load-system-file "open")
(load-system-file "load")

(autoload 'apropos)
(autoload 'describe)
(autoload 'describe-compiler-policy)

(autoload 'logical-pathname)
(autoload 'translate-pathname)
(autoload 'translate-logical-pathname)
(autoload 'logical-pathname-translations)
(autoload 'load-logical-pathname-translations)

(autoload 'float-precision)

(autoload '(query-readline y-or-n-p yes-or-no-p) "query")

(let ((software-type (software-type)))
  (cond ((equal software-type "Linux")
         (pushnew :linux *features*)
         (pushnew :unix *features*))
        ((equal software-type "Windows")
         (pushnew :windows *features*))))

(load-system-file "epsilons.lisp")

(in-package "EXTENSIONS")
(export 'dump-ir2)
(autoload 'dump-ir2 "load-compiler")
(export 'with-mutex)
(autoload-macro 'with-mutex)

(in-package "CL-USER")

(autoload '(do-tests do-compiled-tests) "rt")
(autoload 'run-random-tests)
(autoload 'run-other-tests)

(defun run-ansi-tests (&key (compile-tests t) (mumble t))
;;   (declaim (optimize speed))
  (let* ((ansi-tests-directory
          (pathname (directory-namestring (merge-pathnames "ansi-tests/" #.*load-pathname*))))
         (*default-pathname-defaults* ansi-tests-directory)
         (*print-structure* t)
         (sys:*mumble* mumble))
    (cond (compile-tests
           (load "compileit.lsp"))
          (t
           (load "gclload1.lsp")
           (load "gclload2.lsp")
           (let ((f (find-symbol "DO-TESTS" "RT")))
             (when f
               (eval `(time (funcall ',f)))))))))

(when (cdr sys:*argv*)
  (let ((args (cdr sys:*argv*)))
    (loop
      (when (null args)
        (return))
      (let ((arg (car args)))
        (setq args (cdr args))
        (cond ((equal arg "--load")
               (setq arg (pop args))
               (when arg
                 (load arg))))))))

;; REVIEW
(when (probe-file "grovel.xcl")
  (make-thread #'sys:grovel-cpp-definitions))
