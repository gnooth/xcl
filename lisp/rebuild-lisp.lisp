;;; rebuild-lisp.lisp
;;;
;;; Copyright (C) 2006-2010 Peter Graves <gnooth@gmail.com>
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

(defun clean ()
  (dolist (dir '("lisp/" "clos/" "compiler/" #+x86 "compiler/x86/" #+x86-64 "compiler/x86-64/"))
    (let ((*default-pathname-defaults* (merge-pathnames dir *xcl-home*)))
      (dolist (file (directory "*.xcl"))
        (delete-file file))))
  t)

(defun write-version ()
  (let* ((*default-pathname-defaults* *xcl-home*)
         version
         dirty)
    (when (probe-directory ".git")
      (run-shell-command "git describe --tags > version")
      (with-open-file (stream "version" :direction :input)
        (setq version (read-line stream)))
      (when version
        (run-shell-command "git status > status")
        (with-open-file (stream "status")
          (loop
            (let ((line (read-line stream nil nil)))
              (when (null line)
                (return))
              (when (or (string= line "# Changed but not updated:")
                        (string= line "# Changes to be committed:"))
                (setq dirty t)
                (return)))))
        (when dirty
          (setq version (concatenate 'string version "*"))
          (with-open-file (stream "version" :direction :output :if-exists :supersede)
            (write-line version stream)))))))

(defun write-build ()
  (let* ((*default-pathname-defaults* *xcl-home*))
    #+unix
    (run-shell-command "date > build")
    #-unix
    (multiple-value-bind (sec min hour date month year day daylight-p zone)
        (get-decoded-time)
      (setq day (case day
                  (0 "Mon")
                  (1 "Tue")
                  (2 "Wed")
                  (3 "Thu")
                  (4 "Fri")
                  (5 "Sat")
                  (6 "Sun")))
      (setq month (case month
                    (1 "Jan")
                    (2 "Feb")
                    (3 "Mar")
                    (4 "Apr")
                    (5  "May")
                    (6 "Jun")
                    (7 "Jul")
                    (8 "Aug")
                    (9 "Sep")
                    (10 "Oct")
                    (11 "Nov")
                    (12 "Dec")))
      (case zone
        (5
         (setq zone (if daylight-p "EDT" "EST")))
        (6
         (setq zone (if daylight-p "CDT" "CST")))
        (7
         (setq zone (if daylight-p "MDT" "MST")))
        (8
         (setq zone (if daylight-p "PDT" "PST")))
        (t
         (setq zone (format nil "~A~4,'0D"
                            (if (plusp zone) "-" "+")
                            (* (if daylight-p
                                   (1- zone)
                                   zone)
                               100)))))
      (with-open-file (stream "build" :direction :output :if-exists :supersede)
        (format stream "~A ~A ~D ~D:~2,'0D:~2,'0D ~A ~D~%"
                day
                month
                date
                hour
                min
                sec
                zone
                year)))))

(defun rebuild-lisp ()
  (clean)
  (load-system-file "compiler/load-compiler.lisp")
  (with-compilation-unit ()
    (let ((*default-pathname-defaults* *xcl-home*))
      (load (compile-file "lisp/precompiler.lisp"))
      (load (compile-file "compiler/dump-form.lisp"))
      (load (compile-file "lisp/instruction.lisp"))
      (load (compile-file "lisp/local-variable-information.lisp"))
      (load (compile-file "lisp/canonicalize-type.lisp"))
      (load (compile-file "lisp/typep.lisp"))
      (load (compile-file "lisp/print.lisp"))
      (load (compile-file "lisp/dpb.lisp"))
      (load (compile-file "lisp/member.lisp"))
      (load (compile-file "lisp/mumble.lisp"))
      #+x86
      (load (compile-file "lisp/x86.lisp"))
      #+x86-64
      (load (compile-file "lisp/x86-64.lisp"))
      (load (compile-file "compiler/derive-type.lisp"))
      (load (compile-file "compiler/ir2-defs.lisp"))
      (load (compile-file "compiler/p2.lisp"))
      #+x86
      (load (compile-file "compiler/x86/p2-x86.lisp"))
      #+x86-64
      (load (compile-file "compiler/x86-64/p2-x86-64.lisp"))
      (load (compile-file "compiler/p3.lisp"))
      #+x86
      (load (compile-file "compiler/x86/p3-x86.lisp"))
      #+x86-64
      (load (compile-file "compiler/x86-64/p3-x86-64.lisp"))
      (load (compile-file "compiler/assembler.lisp"))
      #+x86
      (load (compile-file "compiler/x86/asm-x86.lisp"))
      #+x86-64
      (load (compile-file "compiler/x86-64/asm-x86-64.lisp"))
      (load (compile-file "compiler/known-functions.lisp"))
      (load (compile-file "compiler/source-transforms.lisp"))
      (load (compile-file "compiler/compiler.lisp"))
      (load (compile-file "compiler/compile-file.lisp"))
      (load (compile-file "lisp/backquote.lisp"))
      (load (compile-file "lisp/find.lisp"))
      (load (compile-file "lisp/coerce.lisp"))
      (load (compile-file "lisp/delete.lisp"))
      (load (compile-file "compiler/with-compilation-unit.lisp"))
      (load (compile-file "lisp/subtypep.lisp"))
      (load (compile-file "lisp/format.lisp")))
    (let ((*default-pathname-defaults* (merge-pathnames "lisp/" *xcl-home*)))
      (dolist (filespec '("acos"
                          "acosh"
                          "adjoin"
                          "adjust-array"
                          "and"
                          "apply-key"
                          "apropos"
                          "asdf"
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
                          "check-sequence-bounds"
                          "check-type"
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
                          "defconstant"
                          "define-compiler-macro"
                          "define-modify-macro"
                          "define-source-transform"
                          "define-symbol-macro"
                          "defknown"
                          "defmacro"
                          "defpackage"
                          "defparameter"
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
                          "early-macros"
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
                          "inline-expansion"
                          "inspect"
                          "intersection"
                          "invoke-debugger"
                          "late-setf"
                          "lcm"
                          "ldb"
                          "ldiff"
                          "list-length"
                          ;; FIXME load.xcl is broken Oct 26 2010 5:05 AM
                          #+x86-64 "load"
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
                          "run-shell-command"
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
                          "stack"
                          "step"
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
                          "with-mutex"
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
                          ))
        (compile-file filespec)))
    (let ((*default-pathname-defaults* (merge-pathnames "compiler/" *xcl-home*)))
      (compile-file "install-p2-handlers"))
    (let ((*default-pathname-defaults* (merge-pathnames "clos/" *xcl-home*)))
      (dolist (filespec '("initialize-classes"
                          "clos"
                          "define-method-combination"
                          "defclass"
                          "defmethod"))
        (compile-file filespec)))
    (write-version)
    (write-build)
    t))
