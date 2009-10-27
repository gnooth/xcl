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

(load-system-file "lisp/backquote.lisp")
(load-system-file "lisp/early-macros.lisp")
(load-system-file "lisp/featurep.lisp")
(load-system-file "lisp/read-conditional.lisp")
(load-system-file "lisp/autoloads.lisp")

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

(defun make-package (package-name &key nicknames use)
  (%make-package package-name nicknames use))

;; redefined in define-modify-macro.lisp
(defmacro incf (place &optional (delta 1))
  `(setq ,place (+ ,place ,delta)))

;; redefined in define-modify-macro.lisp
(defmacro decf (place &optional (delta 1))
  `(setq ,place (- ,place ,delta)))

;; redefined in setf.lisp
(defmacro push (item place)
  `(setq ,place (cons ,item ,place)))

;; redefined in precompiler.lisp
(defun ext:precompile (name &optional definition)
  (or name definition))

;; redefined in pprint.lisp
(defun ext:charpos (stream)
  (%stream-charpos stream))

;; redefined in pprint.lisp
(defun (setf ext:charpos) (new-value stream)
  (%stream-set-charpos stream new-value))

(load-system-file "lisp/defmacro.lisp")

(load-system-file "lisp/defconstant.lisp")

(defconstant +nil-symbol-name+ "NIL")
(export '+nil-symbol-name+)

(load-system-file "lisp/defparameter.lisp")

;; (set-symbol-function 'defmacro (macro-function 'defmacro))
;; (set-symbol-plist 'defmacro nil)
;; (set-macro-function 'defmacro (make-macro 'defmacro (symbol-function 'defmacro)))

(let ((x (macro-function 'defmacro)))
  (set-symbol-function 'defmacro x) ; will be overwritten, but clears special operator flag
  (set-symbol-plist 'defmacro nil)
  (set-macro-function 'defmacro x))

(defun ansi-loop (exps)
  (load-system-file "lisp/loop")
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

(load-system-file "lisp/inline-expansion.lisp")
(load-system-file "lisp/setf.lisp")
(load-system-file "lisp/compiler-error.lisp")
(load-system-file "lisp/precompiler.lisp")

(defun make-thread (function &key name)
  (%make-thread function name))

(defun make-mutex (&key name)
  (%make-mutex name))

(defun mapappend (function &rest lists)
  (apply #'append (apply #'mapcar function lists)))
(export 'mapappend)

(load-system-file "lisp/member.lisp")

;; needed for reader.lisp
(load-system-file "lisp/make-hash-table.lisp")

;; needed for reader.lisp
(load-system-file "lisp/canonicalize-type.lisp")

;; needed for reader.lisp
(load-system-file "lisp/typep.lisp")

;; needed for reader.lisp
(load-system-file "lisp/apply-key.lisp")
(load-system-file "lisp/find.lisp")

(load-system-file "lisp/reader.lisp")

(load-system-file "lisp/adjoin.lisp")
(load-system-file "lisp/pushnew.lisp")

(load-system-file "lisp/ldb.lisp")
(load-system-file "lisp/typecase.lisp")
(load-system-file "lisp/etypecase.lisp")
(load-system-file "lisp/defknown.lisp")
(load-system-file "lisp/instruction.lisp")
(load-system-file "lisp/local-variable-information.lisp")

(maybe-load-system-file "lisp/ldb.xcl")
(maybe-load-system-file "lisp/defknown.xcl")
(maybe-load-system-file "lisp/instruction.xcl")
(maybe-load-system-file "lisp/local-variable-information.xcl")
(maybe-load-system-file "lisp/inline-expansion.xcl")
(maybe-load-system-file "lisp/setf.xcl")
(maybe-load-system-file "lisp/compiler-error.xcl")
(maybe-load-system-file "lisp/precompiler.xcl")
(maybe-load-system-file "lisp/canonicalize-type.xcl")
(maybe-load-system-file "lisp/typep.xcl")
(maybe-load-system-file "lisp/find.xcl")
(maybe-load-system-file "lisp/defmacro.xcl")
(maybe-load-system-file "lisp/defconstant.xcl")
(maybe-load-system-file "lisp/defparameter.xcl")
(maybe-load-system-file "lisp/backquote.xcl")
(maybe-load-system-file "lisp/featurep.xcl")
(maybe-load-system-file "lisp/read-conditional.xcl")
(maybe-load-system-file "lisp/adjoin.xcl")
(maybe-load-system-file "lisp/pushnew.xcl")
(maybe-load-system-file "lisp/member.xcl")
(maybe-load-system-file "lisp/make-hash-table.xcl")

(maybe-load-system-file "lisp/typecase.xcl")
(maybe-load-system-file "lisp/etypecase.xcl")

(maybe-load-system-file "lisp/early-macros.xcl")
(maybe-load-system-file "lisp/apply-key.xcl")

(load-system-file "lisp/invoke-debugger")
(load-system-file "lisp/signal")
(load-system-file "lisp/concatenate")
(load-system-file "lisp/copy-seq")

(defun complement (f)
  #'(lambda (&rest x) (not (apply f x))))

(defun constantly (x)
  #'(lambda (&rest args) (declare (ignore args)) x))

(load-system-file "lisp/require-type")

(load-system-file "lisp/late-setf")

;; REVIEW
(defun shrink-vector (vector new-size)
  (subseq vector 0 new-size))

(load-system-file "lisp/define-modify-macro")

(defvar *mumble* nil)
(export '(*mumble* mumble))

(load-system-file "lisp/defvar")
(load-system-file "lisp/defconstant")

(load-system-file "clos/initialize-classes")

(load-system-file "lisp/restart")
(load-system-file "lisp/top-level")

(load-system-file "lisp/define-compiler-macro")

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

(load-system-file "lisp/loop")
(load-system-file "lisp/print")
(load-system-file "lisp/defstruct") ; needed for pprint.xcl
(load-system-file "lisp/pprint") ; REVIEW

(load-system-file "lisp/open")
(load-system-file "lisp/load")

(load-system-file "lisp/epsilons.lisp")

(defvar ext:*load-path* nil)

(in-package "CL-USER")

(defun run-ansi-tests (&key (compile-tests t) (mumble t))
;;   (declaim (optimize speed))
  (let* ((ansi-tests-directory
          (pathname (directory-namestring (merge-pathnames "ansi-tests/" *xcl-home*))))
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
(when (probe-file (merge-pathnames "lisp/grovel.xcl" *xcl-home*))
  (make-thread #'sys:grovel-cpp-definitions))

;; REVIEW
(let ((initialization-file (merge-pathnames ".xclrc" (user-homedir-pathname))))
  (when (probe-file initialization-file)
    (load initialization-file)))
