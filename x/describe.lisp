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

(defmethod describe-object ((object t) stream)
  (format stream "~S~%" object))

(defun normalize (x)
  (if (minusp x)
      (+ x (expt 2 (max 32 (* 4 (round (+ (integer-length x) 4) 4)))))
      x))

(defmethod describe-object ((object integer) stream)
  (format stream "~A~12T~S~%" (if (fixnump object) "Fixnum:" "Bignum:") object)
  (cond ((minusp object)
         (let ((n (normalize object)))
           (format stream "Binary:~12T#b...~B~%" n)
           (format stream "Octal:~12T#o...~O~%" n)
           (format stream "Decimal:~12T~D~%" object)
           (format stream "Hex:~12T#x...~X~%" n)))
        (t
         (format stream "Binary:~12T#b~B~%" object)
         (format stream "Octal:~12T#o~O~%" object)
         (format stream "Decimal:~12T~D~%" object)
         (format stream "Hex:~12T#x~X~%" object)))
  (when (fixnump object)
    (let ((tagged (ldb (byte +bits-per-word+ 0) (ash object +fixnum-shift+))))
      (format stream "Tagged:~12T~D #x~X~%" tagged tagged)))
  (when (and (typep object '(UNSIGNED-BYTE #.+bits-per-word+))
             (zerop (logand object +fixnum-tag-mask+)))
    (let ((untagged (ash object (- +fixnum-shift+)))
          (minusp (logbitp (1- +bits-per-word+) object)))
      (when minusp
        (setq untagged
              (logior untagged (lognot (1- (expt 2 (- sys:+bits-per-word+ sys:+fixnum-shift+)))))))
      (format stream "Untagged:~12T~D " untagged)
      (format stream (if minusp "#x...~X~%" "#x~X~%") (normalize untagged)))))

(defun symbol-flags-bit-name (bit)
  ;; need to keep this in sync with Symbols.hpp
  (case bit
    (0 "FLAG_SPECIAL_VARIABLE")
    (1 "FLAG_SPECIAL_OPERATOR")
    (2 "FLAG_MACRO")
    (3 "FLAG_CONSTANT")
    (4 "FLAG_AUTOLOAD")
    (5 "FLAG_KERNEL_FUNCTION")
    (t
     (format nil "UNKNOWN_BIT_~D" bit))))

(defmethod describe-object ((object symbol) stream)
  (let ((*package* +keyword-package+))
    (format stream "Symbol:~12T~S~%" object))
  (when (boundp object)
    (format stream "Value:~12T~S~%" (symbol-value object)))
  (when (fboundp object)
    (format stream "Function:~12T~S~%" (symbol-function object)))
  (let ((plist (symbol-plist object)))
    (when plist
      (format stream "Plist:~12T(")
      (loop
        (let ((key (car plist))
              (value (cadr plist)))
          (format stream "~13,0T~S ~S" key value)
          (setq plist (cddr plist))
          (unless plist
            (write-char #\) stream)
            (terpri stream)
            (return))
          (terpri stream)))))
  (let ((flags (symbol-flags object)))
    (when flags
      (format stream "Flags:" flags)
      (dotimes (i +bits-per-word+)
        (when (logbitp i flags)
          (format stream "~12T~A~%" (symbol-flags-bit-name i)))))))

(defmethod describe-object ((object pathname) stream)
  (format stream "Pathname: ~12T~S~%" object)
  (format stream "Host: ~12T~S~%" (pathname-host object))
  (format stream "Device: ~12T~S~%" (pathname-device object))
  (format stream "Directory: ~12T~S~%" (pathname-directory object))
  (format stream "Name: ~12T~S~%" (pathname-name object))
  (format stream "Type: ~12T~S~%" (pathname-type object))
  (format stream "Version: ~12T~S~%" (pathname-version object)))
