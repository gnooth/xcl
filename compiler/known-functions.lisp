;;; known-functions.lisp
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

(dolist (name '(*
                /
                /=
                <
                <=
                =
                >
                >=
                abs
                acos
                acosh
                adjust-array
                adjustable-array-p
                alpha-char-p
                alphanumericp
                apropos-list
                aref
                array-dimension
                array-dimensions
                array-element-type
                array-has-fill-pointer-p
                array-in-bounds-p
                array-rank
                array-row-major-index
                array-total-size
                ash
                asin
                asinh
                atan
                atanh
                bit-and
                bit-andc1
                bit-andc2
                bit-eqv
                bit-ior
                bit-nand
                bit-nor
                bit-orc1
                bit-orc2
                bit-xor
                bit-not
                boole
                both-case-p
                boundp
                byte
                caadr
                caar
                cadar
                caddr
                cadr
                car
                cdar
                cddddr
                cdddr
                cddr
                cdr
                char-equal
                char-greaterp
                char-lessp
                char-not-equal
                char-not-greaterp
                char-not-lessp
                char<
                char<=
                char>
                char>=
                characterp
                class-of
                clear-output
                close
                clrhash
                code-char
                coerce
                compiled-function-p
                complement
                complex
                conjugate
                constantp
                copy-readtable
                copy-tree
                cos
                cosh
                count
                count-if
                count-if-not
                delete-duplicates
                delete-file
                delete-package
                denominator
                deposit-field
                digit-char
                digit-char-p
                directory
                eighth
                elt
                endp
                error
                eq
                eql
                evenp
                every
                expt
                fboundp
                fdefinition
                fifth
                fill-pointer
                find
                find-class
                find-if
                find-if-not
                find-package
                find-restart
                finish-output
                first
                float
                float-radix
                fmakunbound
                force-output
                format
                fourth
                fresh-line
                function
                gcd
                gensym
                get
                getf
                go
                graphic-char-p
                hash-table-count
                identity
                imagpart
                integer-length
                isqrt
                keywordp
                last
                lcm
                ldb-test
                ldiff
                list
                list*
                list-all-packages
                list-length
                logand
                logandc1
                logandc2
                logbitp
                logcount
                logeqv
                logior
                lognand
                lognor
                logorc1
                logorc2
                logxor
                lower-case-p
                macro-function
                make-array
                make-condition
                make-hash-table
                make-package
                make-pathname
                make-symbol
                mask-field
                makunbound
                map
                mapc
                mapcar
                max
                merge-pathnames
                min
                minusp
                mismatch
                mod
                multiple-value-list
                nbutlast
                nconc
                ninth
                notany
                notevery
                nreconc
                nreverse
                nset-exclusive-or
                nsublis
                nsubst
                nsubst-if
                nsubst-if-not
                nth
                nthcdr
                numerator
                nunion
                oddp
                package-name
                package-shadowing-symbols
                package-use-list
                package-used-by-list
                pathname
                pathname-device
                pathname-directory
                pathname-host
                pathname-match-p
                pathname-name
                pathname-type
                pathname-version
                pathnamep
                plusp
                position
                position-if
                position-if-not
                prin1
                princ
                print
                probe-file
                proclaim
                random
                rational
                rationalize
                read
                read-byte
                read-char
                read-preserving-whitespace
                realpart
                reduce
                rem
                remhash
                remove-duplicates
                rest
                room
                row-major-aref
                search
                second
                set-exclusive-or
                setq
                seventh
                signum
                sin
                sinh
                sixth
                slot-value
                some
                special-operator-p
                sqrt
                standard-char-p
                stream-element-type
                string
                string-left-trim
                string-right-trim
                string-trim
                string=
                sublis
                subst
                subst-if
                subst-if-not
                svref
                symbol-function
                symbol-name
                symbol-package
                symbol-plist
                symbol-value
                synonym-stream-symbol
                unuse-package
                use-package
                tailp
                tan
                tanh
                tenth
                terpri
                third
                type-of
                typep
                union
                unread-char
                upper-case-p
                vector
                warn
                wild-pathname-p
                write
                zerop

                ;; ext
                autoloadp
                classp
                featurep
                file-directory-p
                memq
                memql
                neq
                resolve
                string-find

                ;; sys
                %caddr
                %cadr
                %car
                %cddr
                %cdr
                %cddr
                %class-name
                %failed-aver
                %member
                %rplaca
                %rplacd
                %stream-terpri
                %type-error
                aset
                assert-error
                backq-list
                backq-list*
                backq-append
                backq-nconc
                backq-cons
                check-subsequence
                designator-input-stream
                designator-list
                designator-output-stream
                every2
                find-eql
                function-arity
                function-code
                get2
                get3
                gethash2-1
                kernel-function-p
                list-copy-seq
                list-directory
                make-code-vector
                mumble
                package-external-symbols
                package-internal-symbols
                package-symbols
                pathname-as-file
                putf
                puthash
                read-8-bits
                require-type
                row-major-aset
                set-cadr
                setcar
                setcdr
                setelt
                special-variable-p
                structure-ref
                structure-set
                svset
                symbol-global-value
                two-arg-*
                two-arg-+
                two-arg--
                two-arg-/
                two-arg-/=
                two-arg-<
                two-arg-<=
                two-arg-=
                two-arg->
                two-arg->=
;;                 two-arg-append
                two-arg-char=
                two-arg-lcm
                two-arg-logand
                two-arg-logior
                two-arg-logxor
                value-to-ub64
                vector-copy-seq
                vector-data
                vector-ref
                vector-set
                write-8-bits

                ;; compiler
                c::instruction-data
                c::instruction-kind
                c::instruction-size
                c::make-instruction
                ))
  (set-operator-single-valued-p name t))

(defknown %canonicalize-type (t) t)
(defknown %ldb (t integer) (integer 0 *))
(defknown %list-length (list) (integer 0 #.most-positive-fixnum))
(defknown %setcar (cons t) t)
(defknown %standard-char-p (t) boolean (:safe))
(defknown %stream-write-char (stream character) character)
(defknown %svref (simple-vector index) t)
(defknown %svset (simple-vector index t) t)
(defknown %typep (t t) t)
(defknown %vector-length (vector) (integer 0 #.most-positive-fixnum))
(defknown %write-to-string (t) simple-string)
(defknown (* two-arg-*) (*) number (:flushable))
(defknown (+ two-arg-+) (*) number (:flushable))
(defknown (- two-arg--) (*) number (:flushable))
(defknown (/ two-arg-/) (*) number)
(defknown acons (*) cons)
(defknown adjoin (*) list)
(defknown append (*) list)
(defknown arrayp (t) boolean (:safe))
(defknown array-rank (array) (integer 0 #.array-rank-limit))
(defknown array-dimension (array t) index)
(defknown array-dimensions (array) list)
(defknown array-total-size (array) index)
(defknown ash (integer integer) integer)
(defknown assoc (*) list)
(defknown assoc-if (*) list)
(defknown assoc-if-not (*) list)
(defknown assq (t t) list)
(defknown assql (t t) list)
(defknown atom (t) boolean (:safe))
(defknown bignump (t) boolean (:safe))
(defknown (bit sbit sbit1) (*) (integer 0 1))
(defknown bit-vector-p (t) boolean (:safe))
(defknown (butlast nbutlast) (*) list)
(defknown byte-position (*) (integer 0 *))
(defknown byte-size (*) (integer 0 *))
(defknown canonicalize-type (t) t)
(defknown (floor ceiling truncate round) (*) (values integer real))
(defknown char (string index) character)
(defknown (char/= two-arg-char/=) (character character) boolean)
(defknown (char= two-arg-char=) (character character) boolean)
(defknown char-code (character) (integer 0 255))
(defknown char-downcase (character) character)
(defknown char-upcase (character) character)
(defknown character (t) character)
(defknown characterp (t) boolean (:safe))
(defknown check-fixnum-bounds (t t t) fixnum)
(defknown classp (t) boolean (:safe))
(defknown coerce-to-function (t) function)
(defknown complexp (t) boolean (:safe))
(defknown concatenate (*) sequence)
(defknown conditionp (t) boolean (:safe))
(defknown cons (t t) cons (:safe :flushable))
(defknown consp (t) boolean (:safe))
(defknown copy-alist (list) list)
(defknown copy-list (list) list)
(defknown copy-seq (sequence) sequence)
(defknown copy-symbol (*) symbol)
(defknown (count count-if count-if-not) (*) index)
(defknown (delete delete-if delete-if-not) (*) sequence)
(defknown delete-duplicates (*) sequence)
(defknown (dpb %dpb) (*) integer)
(defknown double-float-p (t) boolean (:safe))
(defknown eq (t t) boolean (:safe))
(defknown eql (t t) boolean (:safe))
(defknown equal (t t) boolean (:safe))
(defknown equalp (t t) boolean (:safe))
(defknown exp (number) number)
(defknown expt (number number) number)
(defknown fdefinition-block-name (t) symbol)
(defknown fill (*) sequence)
(defknown fixnump (t) boolean (:safe))
(defknown fixnum-typep (t fixnum fixnum) boolean (:safe))
(defknown float-string (float) string)
(defknown floatp (t) boolean (:safe))
(defknown functionp (t) boolean (:safe))
(defknown gensym (*) symbol)
(defknown hash-table-count (hash-table) (integer 0 *))
(defknown hash-table-p (t) boolean (:safe))
(defknown integer-decode-float (float) (integer integer integer))
(defknown integerp (t) boolean (:safe))
(defknown intern (*) (values symbol (member :internal :external :inherited nil)))
(defknown keywordp (t) boolean (:safe))
(defknown last1 (list) list)
(defknown ldb (t integer) (integer 0 *))
(defknown length (sequence) index)
(defknown length-eql (sequence (integer #.most-negative-fixnum #.most-positive-fixnum)) boolean)
(defknown list (*) list (:safe))
(defknown list-delete-eq (t list) list)
(defknown list-delete-eql (t list) list)
(defknown list-fill (list t t t) list)
(defknown list-position-eql (t list) t)
(defknown list1 (t) cons (:safe))
(defknown list2 (t t) cons (:safe))
(defknown list3 (t t t) cons (:safe))
(defknown list4 (t t t t) cons (:safe))
(defknown list5 (t t t t t) cons (:safe))
(defknown listp (t) boolean (:safe))
(defknown log (*) number)
(defknown logand (*) integer)
(defknown (logandc1 logandc2 logorc1 logorc2) (integer integer) integer)
(defknown logior (*) integer)
(defknown lognot (integer) integer)
(defknown logtest (integer integer) t)
(defknown logxor (*) integer)
(defknown make-array (*) array)
(defknown make-code-vector (index) (simple-array (unsigned-byte 8) (*)))
(defknown make-hash-table (*) hash-table)
(defknown make-list (*) list)
(defknown make-sequence (*) sequence)
(defknown make-simple-vector (index) simple-vector)
(defknown make-string (*) simple-string)
(defknown make-string-output-stream (*) stream)
(defknown make-symbol (string) symbol)
(defknown mapcar (*) list)
(defknown mapcar2 (t t) list)
(defknown (mapc2 fast-mapc2) (t list) list)
(defknown maphash (t hash-table) null)
(defknown mask-field (t integer) (integer 0 *))
(defknown max (*) real)
(defknown member (*) list)
(defknown member-if (*) list)
(defknown member-if-not (*) list)
(defknown memq (*) list)
(defknown memql (*) list)
(defknown min (*) real)
(defknown not (t) boolean (:safe))
(defknown nreverse (t) sequence)
(defknown nset-difference (*) list)
(defknown nstring-capitalize (*) string)
(defknown nstring-downcase (*) string)
(defknown nstring-upcase (*) string)
(defknown nsubstitute (*) sequence)
(defknown nsubstitute-if (*) sequence)
(defknown nsubstitute-if-not (*) sequence)
(defknown null (t) boolean (:safe))
(defknown numberp (t) boolean (:safe))
(defknown open (*) stream)
(defknown package-nicknames (package) list)
(defknown packagep (t) boolean (:safe))
(defknown pathnamep (t) boolean (:safe))
(defknown position-eql (t sequence) t)
(defknown prin1-to-string (t) string)
(defknown princ-to-string (t) string)
(defknown quoted-form-p (t) boolean (:safe))
(defknown rassoc (*) list)
(defknown rassoc-if (*) list)
(defknown rassoc-if-not (*) list)
(defknown rationalp (t) boolean (:safe))
(defknown ratiop (t) boolean (:safe))
(defknown readtablep (t) boolean (:safe))
(defknown realp (t) boolean (:safe))
(defknown remove (*) sequence)
(defknown remove-duplicates (*) sequence)
(defknown remove-if (*) sequence)
(defknown remove-if-not (*) sequence)
(defknown rename-package (*) package)
(defknown replace (*) sequence)
(defknown require-boolean (t) boolean)
(defknown require-character (t) character)
(defknown require-cons (t) cons)
(defknown require-fixnum (t) (integer #.most-negative-fixnum #.most-positive-fixnum))
(defknown require-hash-table (t) hash-table)
(defknown require-integer (t) integer)
(defknown require-keyword (t) keyword)
(defknown require-list (t) list)
(defknown require-number (t) number)
(defknown require-simple-string (t) simple-string)
(defknown require-simple-vector (t) simple-vector)
(defknown require-stream (t) stream)
(defknown require-string (t) string)
(defknown require-structure-type (t symbol) structure-object)
(defknown require-symbol (t) symbol)
(defknown require-ub32 (t) (unsigned-byte 32))
(defknown require-vector (t) vector)
(defknown reverse (t) sequence)
(defknown rplaca (cons t) cons)
(defknown rplacd (cons t) cons)
(defknown schar (simple-string index) character)
(defknown sequencep (t) boolean (:safe))
(defknown set-difference (*) list)
(defknown (set-sbit set-sbit1) (*) bit)
(defknown set-subseq (*) sequence)
(defknown setcar (cons t) t)
(defknown setcdr (cons t) t)
(defknown simple-array-p (t) boolean)
(defknown simple-bit-vector-p (t) boolean (:safe))
(defknown simple-string-p (t) boolean (:safe))
(defknown simple-vector-p (t) boolean (:safe))
(defknown single-float-p (t) boolean (:safe))
(defknown standard-object-p (t) boolean (:safe))
(defknown streamp (t) boolean (:safe))
(defknown string (t) string)
(defknown string-capitalize (*) simple-string)
(defknown string-cmp (*) (values fixnum fixnum))
(defknown string-compare (*) (values fixnum fixnum))
(defknown string-downcase (*) simple-string)
(defknown string-upcase (*) simple-string)
(defknown stringp (t) boolean (:safe))
(defknown structure-object-p (t) boolean (:safe))
(defknown structure-typep (t) boolean (:safe))
(defknown subseq (*) sequence)
(defknown subseq2 (sequence index) sequence)
(defknown subseq3 (sequence index t) sequence)
(defknown subsetp-eql (list list) t)
(defknown substitute (*) sequence)
(defknown substitute-if (*) sequence)
(defknown substitute-if-not (*) sequence)
(defknown sxhash (t) (integer 0 #.most-positive-fixnum))
(defknown symbol-name (symbol) simple-string)
(defknown symbolp (t) boolean (:safe))
(defknown two-arg-append (*) list)
(defknown two-arg-logior (integer integer) integer)
(defknown two-arg-logxor (integer integer) integer)
(defknown two-arg-max (real real) real)
(defknown two-arg-min (real real) real)
(defknown value-to-ub32 (t) (integer 0 #.(1- (expt 2 32))))
(defknown value-to-ub64 (t) (integer 0 #.(1- (expt 2 64))))
(defknown values (*) * (:safe :flushable))
(defknown vector (*) simple-vector)
(defknown vector2 (t t) (simple-array t (2)))
(defknown vector3 (t t t) (simple-array t (3)))
(defknown vector-fill (vector t t t) vector)
(defknown vector-length (vector) index)
(defknown vector-pop (vector) t)
(defknown vector-position-eql (t vector) t)
(defknown vector-push (t vector) t)
(defknown vector-push-extend (*) index)
(defknown vector-push-extend-2 (t vector) index)
(defknown vector-push-extend-3 (t vector index) index)
(defknown vectorp (t) boolean (:safe))
(defknown write-char (*) character)
(defknown write-line (*) string)
(defknown write-string (*) string)
(defknown write-to-string (*) string)
