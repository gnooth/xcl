;;; rebuild-lisp.lisp
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

(in-package "SYSTEM")

(resolve 'compile)

(defun rebuild-lisp ()
  (let ((*default-pathname-defaults* *xcl-home*))
    (with-compilation-unit ()
      (load (compile-file-if-needed "precompiler.lisp"))
      (load (compile-file-if-needed "dump-form.lisp"))
      (load (compile-file-if-needed "instruction.lisp"))
      (load (compile-file-if-needed "local-variable-information.lisp"))
      (load (compile-file-if-needed "canonicalize-type.lisp"))
      (load (compile-file-if-needed "typep.lisp"))
      (load (compile-file-if-needed "print.lisp"))
      (load (compile-file-if-needed "dpb.lisp"))
      (load (compile-file-if-needed "member.lisp"))
      (load (compile-file-if-needed "mumble.lisp"))
      #+x86
      (load (compile-file-if-needed "x86.lisp"))
      #+x86-64
      (load (compile-file-if-needed "x86-64.lisp"))
      (load (compile-file-if-needed "derive-type.lisp"))
      (load (compile-file-if-needed "p2.lisp"))
      #+x86
      (load (compile-file-if-needed "p2-x86.lisp"))
      #+x86-64
      (load (compile-file-if-needed "p2-x86-64.lisp"))
      #+x86
      (load (compile-file-if-needed "p3-x86.lisp"))
      #+x86-64
      (load (compile-file-if-needed "p3-x86-64.lisp"))
      (load (compile-file-if-needed "assembler.lisp"))
      #+x86
      (load (compile-file-if-needed "asm-x86.lisp"))
      #+x86-64
      (load (compile-file-if-needed "asm-x86-64.lisp"))
      (load (compile-file-if-needed "source-transforms.lisp"))
      (load (compile-file-if-needed "compiler.lisp"))
      (load (compile-file-if-needed "compile-file.lisp"))
      (load (compile-file-if-needed "backquote.lisp"))
      (load (compile-file-if-needed "find.lisp"))
      (load (compile-file-if-needed "coerce.lisp"))
      (load (compile-file-if-needed "delete.lisp"))
      (load (compile-file-if-needed "with-compilation-unit.lisp"))
      (load (compile-file-if-needed "format.lisp"))
      (dolist (filespec '("acos"
                          "acosh"
                          "adjoin"
                          "adjust-array"
                          "and"
                          "apropos"
;;                           "asdf"
                          "asin"
                          "asinh"
                          "assert"
                          "assoc"
                          "atanh"
                          "aver"
                          "bit"
                          "bit-array-ops"
                          "boole"
                          "butlast"
                          "case"
                          "ccase"
                          "check-type"
                          "clos"
                          "compile-file-pathname"
                          "compiler-error"
                          "concatenate"
                          "cond"
                          "conjugate"
                          "copy-file"
                          "copy-seq"
                          "copy-symbol"
                          "cosh"
                          "count"
                          "ctypecase"
                          "declaim"
                          "defclass"
                          "defconstant"
                          "define-compiler-macro"
                          "define-modify-macro"
                          "define-source-transform"
                          "defknown"
                          "defmacro"
                          "defpackage"
                          "defsetf"
                          "defstruct"
                          "deftype"
                          "defvar"
                          "delete-duplicates"
                          "delete-package"
                          "deposit-field"
                          "describe"
                          "describe-compiler-policy"
                          "destructuring-bind"
                          "directory"
                          "do"
                          "do-all-symbols"
                          "do-external-symbols"
                          "do-subsequence"
                          "do-symbols"
                          "dolist"
                          "dotimes"
                          "dribble"
                          "ecase"
                          "ed"
                          "enough-namestring"
                          "ensure-directories-exist"
                          "etypecase"
                          "every"
                          "fceiling"
                          "featurep"
                          "ffloor"
                          "file-author"
                          "fill"
                          "find-all-symbols"
                          "float-precision"
                          "fround"
                          "ftruncate"
                          "gcd"
                          "gentemp"
                          "grovel"
                          "initialize-classes"
                          "inline-expansion"
                          "inspect"
                          "install-p2-handlers"
                          "intersection"
                          "invoke-debugger"
                          "known-functions"
                          "late-setf"
                          "lcm"
                          "ldb"
                          "ldiff"
                          "list-length"
                          "load"
                          "load-logical-pathname-translations"
                          "logical-pathname"
                          "logical-pathname-translations"
                          "loop"
                          "make-array"
                          "make-hash-table"
                          "make-iterator"
                          "make-list"
                          "make-load-form-saving-slots"
                          "make-sequence"
                          "make-sequence-of-type"
                          "make-socket"
                          "make-string"
                          "make-string-output-stream"
                          "map"
                          "map-into"
                          "mapcon"
                          "mapl"
                          "maplist"
                          "mask-field"
                          "member-if"
                          "member-if-not"
                          "merge"
                          "mismatch"
                          "multiple-value-bind"
                          "multiple-value-list"
                          "nbutlast"
                          "nintersection"
                          "notevery"
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
                          "nunion"
                          "open"
                          "or"
                          "parse-integer"
                          "parse-lambda-list"
                          "parse-namestring"
                          "pathname-match-p"
                          "phase"
                          "pprint"
                          "print-unreadable-object"
                          "proclaim"
                          "profiler"
                          "prog"
                          "provide"
                          "psetf"
                          "psetq"
                          "pushnew"
                          "query"
                          "read-byte"
                          "read-conditional"
                          "read-from-string"
                          "read-sequence"
                          "reader"
                          "reduce"
                          "remf"
                          "remove"
                          "remove-duplicates"
                          "replace"
                          "require"
                          "require-type"
                          "restart"
                          "revappend"
                          "rotatef"
                          "round"
                          "sbit"
                          "scale-float"
                          "search"
                          "set-difference"
                          "set-exclusive-or"
                          "setf"
                          "shiftf"
                          "signal"
                          "sinh"
                          "some"
                          "sort"
                          "source"
                          "stable-sort"
                          "string-capitalize"
                          "string-downcase"
                          "string-left-trim"
                          "string-right-trim"
                          "string-trim"
                          "string-upcase"
                          "strings"
                          "sublis"
                          "subsetp"
                          "subst"
                          "subst-if"
                          "subst-if-not"
                          "substitute"
                          "subtypep"
                          "tailp"
                          "tanh"
                          "time"
                          "top-level"
                          "trace"
                          "translate-logical-pathname"
                          "translate-pathname"
                          "tree-equal"
                          "typecase"
                          "union"
                          "universal-time"
                          "upgraded-array-element-type"
                          "upgraded-complex-part-type"
                          "with-accessors"
                          "with-hash-table-iterator"
                          "with-input-from-string"
                          "with-open-file"
                          "with-open-stream"
                          "with-output-to-string"
                          "with-package-iterator"
                          "with-slots"
                          "with-standard-io-syntax"
                          "write"
                          "write-byte"
                          "write-line"
                          "write-sequence"
                          "write-string"
                          "write-to-string"

                          ;; order matters!
                          "disassemble"
                          #+x86 "disasm-x86"
                          #+x86-64 "disasm-x86-64"
                          )
                        t)
        (compile-file filespec)))))
