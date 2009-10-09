;;; autoloads.lisp
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

(autoload 'make-list)

(autoload 'make-string)

(autoload-macro 'destructuring-bind)

(autoload-macro 'ecase)
(autoload-macro 'typecase)
(autoload-macro 'etypecase)

(autoload-macro 'apply-key)

(autoload-macro 'defsetf)

(autoload '(make-array adjust-array))

(autoload 'parse-lambda-list)
(autoload-macro 'defstruct)

(export '(source-transform define-source-transform expand-source-transform))
(autoload-macro 'define-source-transform "lisp/define-source-transform")
(autoload '(source-transform set-source-transform expand-source-transform)
          "lisp/define-source-transform")

(autoload 'proclaim)
(autoload-macro 'declaim)

(autoload '(string= string/= string< string<= string> string>= string-equal
            string-not-equal string-greaterp string-not-greaterp string-lessp
            string-not-lessp)
          "lisp/strings")

(autoload 'revappend)
(autoload 'upgraded-array-element-type)
(autoload '(%subtypep subtypep) "lisp/subtypep")

(autoload 'make-iterator)
(autoload-macro 'do-subsequence)

(autoload '(every every2) "lisp/every")
(autoload 'notevery)
(autoload '(some notany) "lisp/some")

(autoload '(dpb %dpb) "lisp/dpb")

(autoload-macro 'psetq)
(autoload-macro '(make-string-output-stream with-output-to-string))

(autoload '(assoc assoc-if assoc-if-not rassoc rassoc-if rassoc-if-not
            pairlis copy-alist)
          "lisp/assoc")

(autoload 'subsetp)
(autoload 'list-length)
(autoload 'ldiff)
(autoload 'mapl)

(autoload 'make-sequence-of-type)

(autoload-macro 'deftype)
(autoload 'make-sequence)
(autoload '(set-difference nset-difference))
(autoload '(remove-duplicates delete-duplicates))
(autoload '(intersection nintersection))
(autoload-macro '(psetf shiftf rotatef))
(autoload '(sort stable-sort merge))
(autoload 'replace)
(autoload '(string-trim string-right-trim string-left-trim))
(autoload '(delete delete-if delete-if-not) "lisp/delete")
(autoload '(remove remove-if remove-if-not) "lisp/remove")
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
(autoload '(count count-if count-if-not) "lisp/count")
(autoload 'search)
(autoload '(sublis nsublis))
(autoload '(subst nsubst subst-if subst-if-not nsubst-if nsubst-if-not))
(autoload 'bit)
(autoload '(sbit set-sbit) "lisp/sbit")
(autoload '(butlast nbutlast))
(autoload '(union nunion))
(autoload 'parse-integer)
;; (autoload 'mod)
(autoload 'gcd)
(autoload '(two-arg-lcm lcm) "lisp/lcm")
(autoload '(mask-field deposit-field))
(autoload 'round)
(autoload 'conjugate)
(autoload-macro '(trace untrace) "lisp/trace")

(autoload '%print-unreadable-object "lisp/print-unreadable-object")
(autoload-macro 'print-unreadable-object)
(autoload-macro 'with-standard-io-syntax)

(autoload 'read-from-string)
(autoload-macro 'with-input-from-string)

(autoload-macro 'and)
(autoload-macro 'or)
(autoload-macro 'case)
(autoload-macro 'cond)
(autoload-macro '(do do*) "lisp/do")
(autoload-macro '(dolist dotimes))
(autoload-macro 'multiple-value-list)

(autoload-macro 'aver)
(autoload '%failed-aver "lisp/aver")

(autoload '(open write-byte read-byte write-sequence read-sequence))
(autoload-macro 'with-open-file)

(autoload-macro 'with-hash-table-iterator)
(autoload 'hash-table-iterator-function "lisp/with-hash-table-iterator")

(autoload 'mumble "lisp/mumble")

(autoload 'compile "lisp/load-compiler")

(autoload 'assign-setf-macro "lisp/defsetf")

(export '(coerce-list-to-vector coerce-vector-to-list))
(autoload '(coerce coerce-list-to-vector coerce-vector-to-list) "lisp/coerce")
(autoload 'delete-package)
(autoload-macro 'defpackage)
(autoload-macro 'do-external-symbols)
(autoload 'find-all-symbols)
(autoload-macro 'do-symbols)
(autoload-macro 'do-all-symbols)
(autoload 'package-iterator-function "lisp/with-package-iterator")
(autoload-macro 'with-package-iterator)
(autoload '(set-exclusive-or nset-exclusive-or))
(autoload 'check-type-error "lisp/check-type")
(autoload-macro '(ccase ctypecase check-type))
(autoload-macro 'defclass)
(autoload-macro '(defgeneric defmethod define-condition) "lisp/clos")
(autoload '(ensure-class documentation) "lisp/clos")
(autoload 'gentemp)
(autoload 'mismatch)
(autoload '(substitute substitute-if substitute-if-not) "lisp/substitute")
(autoload-macro 'with-slots)
(autoload '(write-string write-line))
(autoload 'compile-file-pathname)
(autoload-macro '(prog prog*) "lisp/prog")
(autoload-macro 'multiple-value-bind)
(autoload '(bit-array-same-dimensions-p require-same-dimensions pick-result-array
            bit-and bit-ior bit-xor bit-eqv bit-nand bit-nor
            bit-andc1 bit-andc2 bit-orc1 bit-orc2 bit-not)
          "lisp/bit-array-ops")
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
(autoload '(compile-file compile-file-if-needed) "compiler/compile-file")
(autoload-macro 'with-compilation-unit)
(autoload '%with-compilation-unit "lisp/with-compilation-unit")
(autoload 'copy-file)
(autoload '(ftruncate ffloor fceiling fround))
(autoload '(decode-universal-time get-decoded-time encode-universal-time) "lisp/universal-time")
(autoload-macro 'time)
(autoload '%time "lisp/time")

(autoload '(phase scale-float))

(export 'grovel-cpp-definitions)
(autoload 'grovel-cpp-definitions "lisp/grovel")

(autoload 'rebuild-lisp)

(autoload '(source source-pathname source-file-position) "lisp/source")

(autoload 'ed)

(autoload 'make-load-form-saving-slots)

(autoload '(asin complex-asin) "lisp/asin")
(autoload '(acos complex-acos) "lisp/acos")

(autoload '(sinh cosh tanh))

(autoload '(asinh complex-asinh) "lisp/asinh")
(autoload '(acosh complex-acosh) "lisp/acosh")
(autoload '(atanh complex-atanh) "lisp/atanh")

(autoload 'file-author)
(autoload 'upgraded-complex-part-type)
(autoload 'dribble)
(autoload 'ensure-directories-exist)

(autoload-macro 'assert)

(autoload 'apropos)
(autoload 'describe)
(autoload 'describe-compiler-policy)

(autoload 'logical-pathname)
(autoload 'translate-pathname)
(autoload 'translate-logical-pathname)
(autoload 'logical-pathname-translations)
(autoload 'load-logical-pathname-translations)

(autoload 'float-precision)

(autoload '(query-readline y-or-n-p yes-or-no-p) "lisp/query")

(in-package "EXTENSIONS")
(export 'dump-ir2)
(autoload 'dump-ir2 "lisp/lisp/load-compiler")
(export 'with-mutex)
(autoload-macro 'with-mutex)

;; REVIEW
(in-package "CL-USER")

(autoload '(do-tests do-compiled-tests) "lisp/rt")
(autoload 'run-random-tests)
(autoload 'run-other-tests)
