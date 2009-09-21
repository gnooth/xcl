;;; describe.lisp
;;;
;;; Copyright (C) 2009 Peter Graves <peter@armedbear.org>
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

(require "CLOS")

(defun describe (object &optional stream)
  (describe-object object (designator-output-stream stream))
  (values))

(defgeneric object-self-string (x))

(defmethod object-self-string (x)
  (prin1-to-string x))

(defmethod object-self-string ((x symbol))
  (let ((*package* +KEYWORD-PACKAGE+))
    (prin1-to-string x)))

(defmethod object-self-string ((x symbol))
  (let ((*package* (find-package :keyword)))
    (prin1-to-string x)))

(defgeneric object-type-string (x))

(defmethod object-type-string (x)
  (string-downcase (class-name (class-of x))))

(defmethod object-type-string ((x integer))
  (if (fixnump x) "fixnum" "bignum"))

(defgeneric object-type-string ((x float))
  (if (double-float-p x) "double-float" "single-float"))

(defmethod describe-object ((object t) stream)
  (format stream "~A~%  [~A]~%"
          (object-self-string object)
          (object-type-string object)))

(defun normalize (x)
  (if (minusp x)
      (+ x (expt 2 (max 32 (* 4 (round (+ (integer-length x) 4) 4)))))
      x))

(defmethod describe-object ((object integer) stream)
  (format stream "~A   ~S~%" (if (fixnump object) "Fixnum:" "Bignum:") object)
  (cond ((minusp object)
         (let ((n (normalize object)))
           (format stream "Binary:   #b...~B~%" n)
           (format stream "Octal:    #o...~O~%" n)
           (format stream "Decimal:  ~D~%" object)
           (format stream "Hex:      #x...~X~%" n)))
        (t
         (format stream "Binary:   #b~B~%" object)
         (format stream "Octal:    #o~O~%" object)
         (format stream "Decimal:  ~D~%" object)
         (format stream "Hex:      #x~X~%" object)))
  (when (fixnump object)
    (let ((tagged (ldb (byte +bits-per-word+ 0) (ash object +fixnum-shift+))))
      (format stream "Tagged:   ~D #x~X~%" tagged tagged)))
  (when (and #+nil (typep object '(INTEGER 0 #.(ash most-positive-fixnum +fixnum-shift+)))
             (typep object '(UNSIGNED-BYTE #.+bits-per-word+))
             (zerop (logand object +fixnum-tag-mask+)))
    (let ((untagged (ash object (- +fixnum-shift+)))
          (minusp (logbitp (1- +bits-per-word+) object)))
      (when minusp
        (setq untagged
              (logior untagged (lognot (1- (expt 2 (- sys:+bits-per-word+ sys:+fixnum-shift+)))))))
      (format stream "Untagged: ~D " untagged)
      (format stream (if minusp "#x...~X~%" "#x~X~%") (normalize untagged)))))

(defmethod describe-object ((object symbol) stream)
  (let ((*package* +keyword-package+))
    (format stream "Symbol:   ~S~%" object))
  (when (boundp object)
    (format stream "Value:    ~S~%" (symbol-value object)))
  (when (fboundp object)
    (format stream "Function: ~S~%" (symbol-function object)))
  (let ((plist (symbol-plist object)))
    (when plist
      (format stream "Plist:    (")
      (loop
        (let ((key (car plist))
              (value (cadr plist)))
          (format stream "~S ~S" key value)
          (setq plist (cddr plist))
          (unless plist
            (write-char #\) stream)
            (terpri stream)
            (return))
          (format stream "~%           ")))))
  (let ((flags (symbol-flags object)))
    (format stream "Flags:    #x~X~%" flags)
    (when (logbitp 0 flags)
      (format stream "          FLAG_SPECIAL_VARIABLE"))
    (when (logbitp 1 flags)
      (format stream "          FLAG_SPECIAL_OPERATOR"))
    (when (logbitp 2 flags)
      (format stream "          FLAG_MACRO"))
    (when (logbitp 3 flags)
      (format stream "          FLAG_CONSTANT"))
    (when (logbitp 4 flags)
      (format stream "          FLAG_AUTOLOAD"))
    (when (logbitp 5 flags)
      (format stream "          FLAG_KERNEL_FUNCTION"))))

(defmethod describe-object ((object pathname) stream)
  (call-next-method object stream)
  (terpri stream)
  (format stream "HOST: ~S~%" (pathname-host object))
  (format stream "DEVICE: ~S~%" (pathname-device object))
  (format stream "DIRECTORY: ~S~%" (pathname-directory object))
  (format stream "NAME: ~S~%" (pathname-name object))
  (format stream "TYPE: ~S~%" (pathname-type object))
  (format stream "VERSION: ~S~%" (pathname-version object)))
