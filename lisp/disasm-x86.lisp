;;; disasm-x86.lisp
;;;
;;; Copyright (C) 2006-2011 Peter Graves <gnooth@gmail.com>
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

(in-package "DISASSEMBLER")

;; (defparameter *disassemblers* (make-hash-table))

;; (defun install-disassembler (byte disassembler)
;;   (setf (gethash byte *disassemblers*) disassembler))

;; (defun find-disassembler (byte)
;;   (gethash2-1 byte *disassemblers*))

(defparameter *disassemblers* (make-array 256 :initial-element nil))

(declaim (type (simple-array t (256)) *disassemblers*))

(defun install-disassembler (byte disassembler)
  (declare (type (integer 0 255) byte))
  (declare (type symbol disassembler))
  (setf (svref *disassemblers* byte) disassembler))

(defun find-disassembler (byte)
  (declare (type (integer 0 255) byte))
  (svref *disassemblers* byte))

(defmacro define-disassembler (byte-or-bytes &body body)
  (let* ((bytes (designator-list byte-or-bytes))
         (name (intern (format nil "DIS~{-~2,'0X~}" bytes)))
         (args '(byte1 start)))
    `(progn
       (defun ,name ,args
         (declare (ignorable ,@args))
         (let (mnemonic length operand1 operand2 annotation)
           ,@body
           (make-instruction :start start
                             :length length
                             :mnemonic mnemonic
                             :operand1 operand1
                             :operand2 operand2
                             :annotation annotation)))
       (dolist (byte ',bytes)
         (install-disassembler byte ',name)))))

(define-disassembler #x01
  (with-modrm-byte (mref-8 start 1)
   (case mod
     (#b11
      (setq length 2
            mnemonic :add
            operand1 (make-register-operand (register reg))
            operand2 (make-register-operand (register rm))))
     (t
      (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x05
  (let* ((immediate-value (mref-32 start 1)))
    (setq length 5
          mnemonic :add
          operand1 (make-immediate-operand immediate-value)
          operand2 (make-register-operand :eax))))

(define-disassembler #x09
  ;; /r or dword register to r/m dword
  (setq mnemonic :or)
  (with-modrm-byte (mref-8 start 1)
    (case mod
      (#b01
       (setq length   3
             operand1 (make-register-operand (register reg))
             operand2 (make-operand :kind :relative
                                    :register (register rm)
                                    :data (mref-8 start 2))))
      (#b11
       (setq length   2
             operand1 (make-register-operand (register reg))
             operand2 (make-register-operand (register rm))))
      (t
       (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x0c
  ;; OR immediate byte to AL
  (let ((data (mref-8 start 1)))
    (setq length 2
          mnemonic :or
          operand1 (make-immediate-operand data)
          operand2 (make-register-operand :al))))

(define-disassembler #x0f
  (let ((byte2 (mref-8 start 1)))
    (cond ((eql byte2 #x31)
           (setq length 2
                 mnemonic :rdtsc))
          ((eql byte2 #x92)
           (with-modrm-byte (mref-8 start 2)
             (cond ((and (eql mod #b11)
                         (eql reg 0))
                    (setq length 3
                          mnemonic :setb
                          operand1 (make-register-operand (reg8 (register rm)))))
                   (t
                    (error "unhandled byte sequence #x~2,'0x #x~2,'0x #x~2,'0x"
                           byte1 byte2 modrm-byte)))))
          ((eql byte2 #xa2)
           (setq length 2
                 mnemonic :cpuid))
          ((eql byte2 #xab)
           (with-modrm-byte (mref-8 start 2)
             (cond ((eql mod #b01)
                    (setq length 4
                          mnemonic :bts
                          operand1 (make-register-operand (register reg))
                          operand2 (make-operand :kind :relative
                                                 :register (register rm)
                                                 :data (mref-8-signed start 3))))
                   (t
                    (unsupported)))))
          ((eql byte2 #xaf)
           (with-modrm-byte (mref-8 start 2)
             (setq length 3
                   mnemonic :imul
                   operand1 (make-register-operand (register rm))
                   operand2 (make-register-operand (register reg)))))
          ((eql byte2 #xb3)
           (with-modrm-byte (mref-8 start 2)
             (cond ((eql mod #b01)
                    (setq length 4
                          mnemonic :btr
                          operand1 (make-register-operand (register reg))
                          operand2 (make-operand :kind :relative
                                                 :register (register rm)
                                                 :data (mref-8-signed start 3))))
                   (t
                    (unsupported)))))
          ((eql byte2 #xb6)
           (let ((byte3 (mref-8 start 2)))
             (with-modrm-byte byte3
               (cond ((eql byte3 #x02)
                      (setq length 3
                            mnemonic :movzbl
                            operand1 (make-operand :kind :relative
                                                   :register :edx
                                                   :data 0)
                            operand2 (make-register-operand :eax)))
                     ((eql byte3 #xc0)
                      (setq length 3
                            mnemonic :movzbl
                            operand1 (make-register-operand :al)
                            operand2 (make-register-operand :eax)))
                     ((eql rm #b100)
                      ;; SIB byte follows
                      (with-sib-byte (mref-8 start 3)
                        (setq length 5
                              mnemonic :movzbl
                              operand1 (make-operand :kind :indexed
                                                     :register (register base)
                                                     :index (register index)
                                                     :scale scale
                                                     :data (mref-8-signed start 4))
                              operand2 (make-register-operand (register reg)))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x #x~2,'0x"
                             byte1 byte2 byte3))))))
          ((eql byte2 #xb7)
           (let ((byte3 (mref-8 start 2)))
             (with-modrm-byte byte3
               (cond ((eql rm #b100)
                      ;; SIB byte follows
                      (with-sib-byte (mref-8 start 3)
                        (setq length 5
                              mnemonic :movzwl
                              operand1 (make-operand :kind :indexed
                                                     :register (register base)
                                                     :index (register index)
                                                     :scale scale
                                                     :data (mref-8-signed start 4))
                              operand2 (make-register-operand (reg16 (register reg))))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x #x~2,'0x"
                             byte1 byte2 byte3))))))
          ((eql byte2 #xa3)
           (with-modrm-byte (mref-8 start 2)
             (cond ((eql mod #b01)
                    (setq length 4
                          mnemonic :bt
                          operand1 (make-register-operand (register reg))
                          operand2 (make-operand :kind :relative
                                                 :register (register rm)
                                                 :data (mref-8-signed start 3))))
                   (t
                    (unsupported)))))
;;           ((eql (ldb (byte 4 4) byte2) 8)
          ((memq byte2 '(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87
                         #x88 #x89 #x8a #x8b #x8c #x8d #x8e #x8f))
           ;;                           (setq instruction (process-jcc (+ start offset)))
           (let* ((displacement (mref-32 start 2))
                  (absolute-address (ldb (byte 32 0) (+ start 6 displacement))))
             (setq length 6
                   mnemonic (case byte2
                              (#x80 :jo)
                              (#x81 :jno)
                              (#x82 :jb)
                              (#x83 :jae)
                              (#x84 :je)
                              (#x85 :jne)
                              (#x86 :jbe)
                              (#x87 :ja)
                              (#x88 :js)
                              (#x89 :jns)
                              (#x8a :jpe)
                              (#x8b :jpo)
                              (#x8c :jl)
                              (#x8d :jge)
                              (#x8e :jle)
                              (#x8f :jg))
                   operand1 (make-absolute-operand absolute-address))
             (push (make-disassembly-block :start-address absolute-address) *blocks*)
             (push absolute-address *labels*)))
;;           ((eql (ldb (byte 4 4) byte2) 4)
          ((memq byte2 '(#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47
                         #x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f))
           ;; cmov
           (setq mnemonic (case (ldb (byte 4 0) byte2)
                            (#x0 :cmovo)
                            (#x1 :cmovno)
                            (#x2 :cmovb)
                            (#x3 :cmovae)
                            (#x4 :cmove)
                            (#x5 :cmovne)
                            (#x6 :cmovbe)
                            (#x7 :cmova)
                            (#x8 :cmovs)
                            (#x9 :cmovns)
                            (#xa :cmovpe)
                            (#xb :cmovpo)
                            (#xc :cmovl)
                            (#xd :cmovge)
                            (#xe :cmovle)
                            (#xf :cmovg)))
           (with-modrm-byte (mref-8 start 2)
             (cond ((= modrm-byte #x05)
                    (setq length 7
                          operand1 (make-operand :kind :absolute
                                                 :data (mref-32 start 3))
                          operand2 (make-register-operand (register reg))))
                   (t
                    (unsupported-byte-sequence byte1 modrm-byte))))))))

(define-disassembler #x21
  ;; /r and dword register to r/m dword
  (setq mnemonic :and)
  (with-modrm-byte (mref-8 start 1)
    (case mod
      (#b01
       (setq length   3
             operand1 (make-register-operand (register reg))
             operand2 (make-operand :kind :relative
                                    :register (register rm)
                                    :data (mref-8 start 2))))
      (#b11
       (setq length   2
             operand1 (make-register-operand (register reg))
             operand2 (make-register-operand (register rm))))
      (t
       (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x24
  (setq length   2
        mnemonic :and
        operand1 (make-register-operand :al)
        operand2 (make-immediate-operand (mref-8 start 1))))

(define-disassembler #x25
  (setq length   5
        mnemonic :and
        operand1 (make-immediate-operand (mref-32 start 1))
        operand2 (make-register-operand :eax)))

(define-disassembler #x29
  (with-modrm-byte (mref-8 start 1)
    (case mod
      (#b11
       (setq length   2
             mnemonic :sub
             operand1 (make-register-operand (register reg))
             operand2 (make-register-operand (register rm))))
      (t
       (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x2c
  (let ((immediate-value (mref-8 start 1)))
    (setq length   2
          mnemonic :sub
          operand1 (make-immediate-operand immediate-value)
          operand2 (make-register-operand :al))))

(define-disassembler #x2d
  (let* ((immediate-value (mref-32 start 1)))
    (setq length   5
          mnemonic :sub
          operand1 (make-immediate-operand immediate-value)
          operand2 (make-register-operand :eax))))

(define-disassembler #x31
  (setq mnemonic :xor)
  (with-modrm-byte (mref-8 start 1)
    (case mod
      (#b11
       (setq length   2
             operand1 (make-register-operand (register reg))
             operand2 (make-register-operand (register rm))))
      (t
       (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x3b
  ;; /r compare r/m dword to dword register
  (setq mnemonic :cmp)
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod #b00) (eql rm  #b101))
           (setq length 6
                 operand1 (make-operand :kind :absolute
                                        :data (mref-32-signed start 2))
                 operand2 (make-register-operand (register reg)))))))

(define-disassembler #x3c
  (setq mnemonic :cmp)
  (let ((byte2 (mref-8-signed start 1)))
    (setq length   2
          operand1 (make-immediate-operand byte2)
          operand2 (make-register-operand :al))))

(define-disassembler #x39
  ;; /r compare dword register to r/m dword
  (setq mnemonic :cmp)
  (with-modrm-byte (mref-8 start 1)
    (case mod
      (#b01
       (setq length 3
             operand1 (make-register-operand (register reg))
             operand2 (make-operand :kind :relative
                                    :register (register rm)
                                    :data (mref-8 start 2))))
      (#b11
       (setq length 2
             operand1 (make-register-operand (register reg))
             operand2 (make-register-operand (register rm))))
      (t
       (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x3d
  ;; compare immediate dword to eax
  (let* ((immediate-value (mref-32 start 1)))
    (setq length     5
          mnemonic   :cmp
          operand1   (make-immediate-operand immediate-value)
          operand2   (make-register-operand :eax)
          annotation immediate-value)))

(define-disassembler (#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47)
  (setq length   1
        mnemonic :inc
        operand1 (make-register-operand (register (- byte1 #x40)))))

(define-disassembler (#x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f)
  (setq length   1
        mnemonic :dec
        operand1 (make-register-operand (register (- byte1 #x48)))))

(define-disassembler (#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57)
  (setq length   1
        mnemonic :push
        operand1 (make-register-operand (register (- byte1 #x50)))))

(define-disassembler (#x58 #x59 #x5a #x5b #x5c #x5d #x5e #x5f)
  (setq length   1
        mnemonic :pop
        operand1 (make-register-operand (register (- byte1 #x58)))))

(define-disassembler #x60
  (setq length   1
        mnemonic :pusha))

(define-disassembler #x61
  (setq length   1
        mnemonic :popa))

(define-disassembler #x66
  (setq length   1
        mnemonic :data16))

(define-disassembler #x68
  ;; push immediate dword
  (let* ((immediate-value (mref-32 start 1)))
    (setq length     5
          mnemonic   :push
          operand1   (make-immediate-operand immediate-value)
          annotation immediate-value)))

(define-disassembler #x6a
  ;; push immediate byte
  (let* ((immediate-value (mref-8 start 1)))
    (setq length   2
          mnemonic :push
          operand1 (make-immediate-operand immediate-value))))

(define-disassembler #x6b
  (let ((byte2 (mref-8 start 1)))
    (cond ((eql byte2 #xc0)
           (let ((byte3 (mref-8 start 2)))
             (setq length 3
                   mnemonic :imul
                   operand1 (make-immediate-operand byte3)
                   operand2 (make-register-operand :eax))))
          (t
           (unsupported-byte-sequence #x6b byte2)))))

(define-disassembler (#x70 #x71 #x72 #x73 #x74 #x75 #x76 #x77
                      #x78 #x79 #x7a #x7b #x7c #x7d #x7e #x7f)
  (let* ((displacement (mref-8-signed start 1))
         (absolute-address (+ start 2 displacement)))
    (push (make-disassembly-block :start-address absolute-address) *blocks*)
    (push absolute-address *labels*)
    (setq length 2
          mnemonic (ecase byte1
                     (#x70 :jo)
                     (#x71 :jno)
                     (#x72 :jb)
                     (#x73 :jnb)
                     (#x74 :je)
                     (#x75 :jne)
                     (#x76 :jbe)
                     (#x77 :ja)
                     (#x78 :js)
                     (#x79 :jns)
                     (#x7a :jp)
                     (#x7b :jnp)
                     (#x7c :jl)
                     (#x7d :jnl)
                     (#x7e :jle)
                     (#x7f :jg))
          operand1 (make-absolute-operand absolute-address))))

(define-disassembler #x80
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod 3)
                (eql reg 4))
           (setq length 3
                 mnemonic :and
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (byte-register rm))))
          ((and (eql mod 3)
                (eql reg 7))
           (setq length 3
                 mnemonic :cmp
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (byte-register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x81
 (with-modrm-byte (mref-8 start 1)
   (cond ((and (eql mod #b11)
               (eql reg 0))
          (setq length 6
                mnemonic :add
                operand1 (make-immediate-operand (mref-32 start 2))
                operand2 (make-register-operand (register rm))))
         ((and (eql mod #b11)
               (eql reg 4))
          (setq length 6
                mnemonic :and
                operand1 (make-immediate-operand (mref-32 start 2))
                operand2 (make-register-operand (register rm))))
         ((and (eql mod #b11)
               (eql reg 5))
          (setq length 6
                mnemonic :sub
                operand1 (make-immediate-operand (mref-32 start 2))
                operand2 (make-register-operand (register rm))))
         ((and (eql mod #b01)
               (eql reg 7))
          (setq length 7
                mnemonic :cmpl
                operand1 (make-immediate-operand (mref-32 start 3))
                operand2 (make-operand :kind :relative
                                       :register (register rm)
                                       :data (mref-8 start 2))))
         (t
          (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x83
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql modrm-byte #x3d)
           ;; 83  /7 ib       CMP r/m32,imm8     2/5      Compare sign extended immediate byte to r/m dword
           (setq length 7
                 mnemonic :cmp
                 operand1 (make-immediate-operand (mref-8 start 6))
                 operand2 (make-operand :kind :absolute
                                        :data (mref-32-signed start 2))))
          ((and (eql mod #b01) (eql reg #b111))
           (setq length 4
                 mnemonic :cmpl
                 operand1 (make-immediate-operand (mref-8 start 3))
                 operand2 (make-operand :kind :relative
                                        :register (register rm)
                                        :data (mref-8 start 2))))
          ((and (eql mod #b11) (eql reg 0))
           (setq length 3
                 mnemonic :add
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          ((and (eql mod #b11) (eql reg #b101))
           (setq length 3
                 mnemonic :sub
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          ((and (eql mod #b11) (eql reg #b100))
           (setq length 3
                 mnemonic :and
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          ((and (eql mod #b11) (eql reg #b110))
           (setq length 3
                 mnemonic :xor
                 operand1 (make-immediate-operand (ldb (byte 32 0) (mref-8-signed start 2)))
                 operand2 (make-register-operand (register rm))))
          ((and (eql mod #b11)
                (eql reg #b111))
           (setq length 3
                 mnemonic :cmp
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          ((and (eql mod #b11)
                (eql reg 1))
           (setq length 3
                 mnemonic :or
                 operand1 (make-immediate-operand (mref-8-signed start 2))
                 operand2 (make-register-operand (register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x84
  ;; /r AND byte register with r/m byte
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod #b11)
           (setq length   2
                 mnemonic :test
                 operand1 (make-register-operand (byte-register reg))
                 operand2 (make-register-operand (byte-register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x85
  ;; /r AND dword register with r/m dword
  (setq mnemonic :test)
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod #b11)
           (setq length 2
                 operand1 (make-register-operand (register reg))
                 operand2 (make-register-operand (register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x88
  ;; /r MOV r/m8,r8
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod 3)
           (setq length 2
                 mnemonic :mov
                 operand1 (make-register-operand (byte-register reg))
                 operand2 (make-register-operand (byte-register rm))))
          ((eql mod 0)
           (setq length 2
                 mnemonic :mov
                 operand1 (make-register-operand (byte-register reg))
                 operand2 (make-operand :kind :relative
                                        :register (register rm)
                                        :data 0)))
          (t
           (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                   modrm-byte mod reg rm)
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x89
  ;; /r move dword register to r/m dword
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod #b01)
           (let ((byte3 (mref-8 start 2)))
             (cond ((eql byte3 #x24)
                    (setq length 4
                          mnemonic :mov
                          operand1 (make-register-operand (register reg))
                          operand2 (make-operand :kind :relative
                                                 :register (register rm)
                                                 :data (mref-8 start 3))))
                   (t
                    (let ((displacement (mref-8-signed start 2)))
                      (setq length 3
                            mnemonic :mov
                            operand1 (make-register-operand (register reg))
                            operand2 (make-operand :kind :relative
                                                   :register (register rm)
                                                   :data displacement))
                      (when (eq (register rm) :ebp)
                        (let ((index (/ displacement 4)))
                          (setq annotation (cdr (assoc index *locals*))))))))))
          ((eql mod #b10)
           (let ((displacement (mref-32-signed start 2)))
             (setq length 6
                   mnemonic :mov
                   operand1 (make-register-operand (register reg))
                   operand2 (make-operand :kind :relative
                                          :register (register rm)
                                          :data displacement))
             (when (eq (register rm) :ebp)
               (let ((index (/ displacement 4)))
                 (setq annotation (cdr (assoc index *locals*)))))))
          ((eql mod #b11)
           (setq length 2
                 mnemonic :mov
                 operand1 (make-register-operand (register reg))
                 operand2 (make-register-operand (register rm))))
          ((eql mod #b00)
           (cond ((and (eql modrm-byte #x04)
                       (eql (mref-8 start 2) #x24))
                  (setq length 3))
                 (t
                  (setq length 2)))
           (setq mnemonic :mov
                 operand1 (make-register-operand (register reg))
                 operand2 (make-indirect-operand (register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x8a
  ;; /r move r/m byte to byte register
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod #b01)
           ;; 1-byte displacement
           (cond ((eql rm #b100)
                  (let ((sib-byte (mref-8 start 2))
                        (displacement-byte (mref-8-signed start 3)))
                    (cond ((eql sib-byte #x24)
                           (setq length 4
                                 mnemonic :movb
                                 operand1 (make-operand :kind :relative
                                                        :register (register rm)
                                                        :data displacement-byte)
                                 operand2 (make-register-operand (byte-register reg))))
                          (t
                           (unsupported)))))
                 (t
                  (let ((displacement-byte (mref-8-signed start 2)))
                    (setq length 3
                          mnemonic :movb
                          operand1 (make-operand :kind :relative
                                                 :register (register rm)
                                                 :data displacement-byte)
                          operand2 (make-register-operand (byte-register reg)))))))
          (t
           (unsupported)))))

(define-disassembler #x8b
  ;; /r move r/m dword to dword register
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql modrm-byte 0)
           (setq length 2
                 mnemonic :mov
                 operand1 (make-operand :kind :relative
                                        :register :eax
                                        :data 0)
                 operand2 (make-register-operand :eax)))
          ((eql modrm-byte #x09)
           (setq length 2
                 mnemonic :mov
                 operand1 (make-operand :kind :relative
                                        :register :ecx
                                        :data 0)
                 operand2 (make-register-operand :ecx)))
          ((eql mod #b00)
           (cond ((eql rm #b101)
                  (setq length 6
                        mnemonic :mov
                        operand1 (make-absolute-operand (mref-32 start 2))
                        operand2 (make-register-operand (register reg))))
                 (t
                  (setq length 2
                        mnemonic :mov
                        operand1 (make-indirect-operand (register rm))
                        operand2 (make-register-operand (register reg))))))
          ((eql mod #b01)
           (cond ((eql rm #b100)
                  (let ((sib-byte (mref-8 start 2))
                        (displacement (mref-8-signed start 3)))
                    (with-sib-byte sib-byte
                      (setq length 4
                            mnemonic :mov
                            operand1 (make-operand :kind :indexed
                                                   :register (register base)
                                                   :index (register index)
                                                   :scale scale
                                                   :data displacement)
                            operand2 (make-register-operand (register reg))))))
                 (t
                  (let ((displacement (mref-8-signed start 2)))
                    (setq length 3
                          mnemonic :mov
                          operand1 (make-operand :kind :relative
                                                 :register (register rm)
                                                 :data displacement)
                          operand2 (make-register-operand (register reg)))
                    (when (eq (register rm) :ebp)
                      (let ((index (/ displacement 4)))
                        (setq annotation (cdr (assoc index *locals*)))))))))
          ((eql mod #b10)
           (let ((displacement (mref-32-signed start 2)))
             (setq length 6
                   mnemonic :mov
                   operand1 (make-operand :kind :relative
                                          :register (register rm)
                                          :data displacement)
                   operand2 (make-register-operand (register reg)))
             (when (eq (register rm) :ebp)
               (let ((index (/ displacement 4)))
                 (setq annotation (cdr (assoc index *locals*)))))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x8d
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod #b01)
           (cond ((eql rm #b100)
                  ;; SIB follows
                  (with-sib-byte (mref-8 start 2)
                    (cond ((eql sib #x26)
                           (setq length   4
                                 mnemonic :lea
                                 operand1 (make-operand :kind :relative
                                                        :register (register base)
                                                        :data (mref-8-signed start 3))
                                 operand2 (make-register-operand (register reg))))
                          (t
                           (error "unhandled byte sequence #x~2,'0x #x~2,'0x #x~2,'0x" byte1 modrm-byte sib)))))
                 (t
                  (setq length   3
                        mnemonic :lea
                        operand1 (make-operand :kind :relative
                                               :register (register rm)
                                               :data (mref-8-signed start 2))
                        operand2 (make-register-operand (register reg))))))
          ((eql mod #b10)
           (cond ((eql rm #b100)
                  ;; SIB follows
                  (with-sib-byte (mref-8 start 2)
                    (cond ((eql sib #x26)
                           (setq length   7
                                 mnemonic :lea
                                 operand1 (make-operand :kind :relative
                                                        :register (register base)
                                                        :data (mref-32 start 3))
                                 operand2 (make-register-operand (register reg))))
                          (t
                           (error "unhandled byte sequence #x~2,'0x #x~2,'0x #x~2,'0x" byte1 modrm-byte sib)))))
                 (t
                  (unsupported-byte-sequence byte1 modrm-byte))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #x8f
  (setq mnemonic :pop)
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod #b00) (eql reg 0))
           (setq length   2
                 operand1 (make-operand :kind :relative
                                        :register (register rm)
                                        :data 0)))
          ((and (eql mod #b01) (eql reg 0))
           (setq length   3
                 operand1 (make-operand :kind :relative
                                        :register (register rm)
                                        :data (mref-8-signed start 2))))
          (t
           (unsupported)))))

(define-disassembler #x90
  (setq length   1
        mnemonic :nop))

(define-disassembler #xa1
  ;; move dword at (seg:offset) to EAX
  (setq length 5
        mnemonic :mov
        operand1 (make-operand :kind :absolute
                               :data (mref-32 start 1))
        operand2 (make-register-operand :eax)))

(define-disassembler #xa8
  (let* ((immediate-value (mref-8 start 1)))
    (setq length 2
          mnemonic :test
          operand1 (make-immediate-operand immediate-value)
          operand2 (make-register-operand :al))))

(define-disassembler #xad
  (setq length   1
        mnemonic :lods)) ; FIXME lods %ds:(%esi),%eax

(define-disassembler (#xb8 #xb9 #xba #xbb #xbc #xbd #xbe #xbf)
  ;; move immediate dword to register
  (let ((immediate-value (mref-32 start 1)))
    (setq length     5
          mnemonic   :mov
          operand1   (make-immediate-operand immediate-value)
          operand2   (make-register-operand (register (- byte1 #xb8)))
          annotation immediate-value)))

(define-disassembler #xc1
  ;; C1   /7 ib      SAR r/m32,imm8
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql reg 7)
           (setq length 3
                 mnemonic :sar
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          ((eql reg 5)
           (setq length 3
                 mnemonic :shr
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          ((eql reg 4)
           (setq length 3
                 mnemonic :shl
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #xc3
  (setq length   1
        mnemonic :ret))

(define-disassembler #xc6
  ;; C6       MOV r/m8,imm8     2/2           Move immediate byte to r/m byte
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod #b01)
           (setq length   4
                 mnemonic :mov
                 operand1 (make-immediate-operand (mref-8 start 1))
                 operand2 (make-operand :kind :relative
                                        :register (register rm)
                                        :data (mref-8 start 2))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #xc7
  ;; C7       MOV r/m32,imm32
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql modrm-byte #x40)
           (setq length 7
                 mnemonic :mov
                 operand1 (make-immediate-operand (mref-32 start 3))
                 operand2 (make-operand :kind :relative
                                        :register :eax
                                        :data (mref-8 start 2))))
          (t
           (unsupported)))))

(define-disassembler #xc9
  (setq length   1
        mnemonic :leave))

(define-disassembler #xcc
  (setq length   1
        mnemonic :int3))

(define-disassembler #xd1
  (with-modrm-byte (mref-8 start 1)
    (case reg
      ((4 5)
       (setq length   2
             mnemonic (if (eql reg 4) :shl :shr)
             operand1 (make-register-operand (register rm))))
      (t
       (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #xe8
  ;; call near, displacement relative to next instruction
  (let* ((displacement (mref-32 start 1))
         (absolute-address (ldb (byte 32 0) (+ start 5 displacement))))
    (setq length   5
          mnemonic :call
          operand1 (make-absolute-operand absolute-address)
          annotation absolute-address)
    (push absolute-address *labels*)))

(define-disassembler #xe9
  (let* ((displacement (mref-32-signed start 1))
         (absolute-address (ldb (byte 32 0) (+ start 5 displacement))))
    (setq length   5
          mnemonic :jmpq
          operand1 (make-absolute-operand absolute-address))
    (push (make-disassembly-block :start-address absolute-address) *blocks*)
    (push absolute-address *labels*)))

(define-disassembler #xeb
  (let* ((displacement (mref-8-signed start 1))
         (absolute-address (+ start 2 displacement)))
    (setq length   2
          mnemonic :jmp
          operand1 (make-absolute-operand absolute-address))
    (push absolute-address *labels*)
    (push (make-disassembly-block :start-address absolute-address) *blocks*)))

(define-disassembler #xf3
  (setq length   1
        mnemonic :repz))

(define-disassembler #xf6
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod #b01) (eql reg 0))
           (setq length 4
                 mnemonic :testb
                 operand1 (make-immediate-operand (mref-8 start 3))
                 operand2 (make-operand :kind :relative
                                        :register (register rm)
                                        :data (mref-8 start 2))))
          ((and (eql mod #b11) (eql reg 0))
           (setq length 3
                 mnemonic :test
                 operand1 (make-immediate-operand (mref-8 start 2))
                 operand2 (make-register-operand (byte-register rm))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #xf7
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod #b11) (eql reg 2))
           (setq length 2
                 mnemonic :not
                 operand1 (make-register-operand (register rm))))
          ((and (eql mod #b11) (eql reg 3))
           (setq length 2
                 mnemonic :negate
                 operand1 (make-register-operand (register rm))))
          ((and (eql mod #b11) (eql reg 6))
           (setq length 2
                 mnemonic :div
                 operand1 (make-register-operand (register rm))))
          ((and (eql mod #b01) (eql reg 0))
           (setq length 7
                 mnemonic :testl
                 operand1 (make-immediate-operand (mref-32 start 3))
                 operand2 (make-operand :kind :relative
                                        :register (register rm)
                                        :data (mref-8 start 2))))
          (t
           (unsupported-byte-sequence byte1 modrm-byte)))))

(define-disassembler #xff
  (with-modrm-byte (mref-8 start 1)
   (cond ((eql modrm-byte #xd0) ; call *%eax
          (setq length   2
                mnemonic :call
                operand1 (make-register-operand :eax)))
         ((eql modrm-byte #xe0) ; jmp *%eax
          (setq length   2
                mnemonic :jmp
                operand1 (make-register-operand :eax)))
         ((eql reg 1)
          (setq length   2
                mnemonic :dec
                operand1 (make-register-operand (register rm))))
         ((eql reg 6)
          (case mod
            (#b01
             (setq length   3
                   mnemonic :push
                   operand1 (make-operand :kind     :relative
                                          :register (register rm)
                                          :data     (mref-8-signed start 2))))
            (#b10
             (setq length   6
                   mnemonic :push
                   operand1 (make-operand :kind     :relative
                                          :register (register rm)
                                          :data     (mref-32-signed start 2))))
            (t
             (unsupported))))
         (t
          (unsupported-byte-sequence byte1 modrm-byte)))))

;; (defun process-block (block)
;;   (let ((block-start (block-start-address block))
;;         (offset 0)
;;         done)
;;     (loop
;;       (let (start
;;             length
;;             mnemonic
;;             operand1
;;             operand2
;;             annotation
;;             (byte1 (mref-8 block-start offset))
;;             instruction)
;;         (when (memq byte1 '(#xc3 #xe9 #xeb))
;;           ;; last time through the loop for this block
;;           (setq done t))
;;         ;; instruction start
;;         (setq start (+ block-start offset))
;;         (let ((disassembler (find-disassembler byte1)))
;;           (unless disassembler
;;             (mumble "no disassembler found for #x~2,'0x~%" byte1))
;;           (if disassembler
;;               (setq instruction (funcall disassembler byte1 start))
;;               (case byte1
;;                 (t
;;                  (error "unhandled opcode #x~2,'0x at #x~X" byte1 (+ block-start offset))))))
;;         (when (null instruction)
;;           (setq instruction
;;                 (make-instruction :start start
;;                                   :length length
;;                                   :mnemonic mnemonic
;;                                   :operand1 operand1
;;                                   :operand2 operand2
;;                                   :annotation annotation)))
;;         ;;                (print-instruction instruction)
;;         (push instruction *instructions*)
;;         (when done
;;           (setf (block-end-address block) (+ (instruction-start instruction) (instruction-length instruction)))
;;           (return))
;;         (setq offset (- (+ (instruction-start instruction) (instruction-length instruction)) block-start)))))
;;   nil)

(defun process-block (block)
  (let ((block-start (block-start-address block))
        (offset 0)
        done)
    (loop
      (let (start
            ;;             length
            #+x86-64 prefix-byte
            ;;             mnemonic
            ;;             operand1
            ;;             operand2
            ;;             annotation
            (byte1 (mref-8 block-start offset))
            instruction)
        (when (memq byte1 '(#xc3 #xe9 #xeb))
          ;; last time through the loop for this block
          (setq done t))
        ;; instruction start
        (setq start (+ block-start offset))
        ;; prefix byte?
        #+x86-64
        (when (<= #x40 byte1 #x4f)
          (setq prefix-byte byte1)
          (setq byte1 (mref-8 block-start (incf offset))))
        (let ((disassembler (find-disassembler byte1)))
          (if disassembler
              (setq instruction (funcall disassembler
                                         byte1
                                         #+x86    start
                                         #+x86-64 (if prefix-byte (1+ start) start)
                                         #+x86-64 prefix-byte))
              (error "No disassembler for opcode #x~2,'0x at #x~X" byte1 start)))
        ;;         (aver (not (null instruction)))
        ;;         (when (null instruction)
        ;;           (when prefix-byte
        ;;             (incf length))
        ;;           (setq instruction
        ;;                 (make-instruction :start start
        ;;                                   :length length
        ;;                                   :mnemonic mnemonic
        ;;                                   :operand1 operand1
        ;;                                   :operand2 operand2
        ;;                                   :annotation annotation)))

        ;;                (print-instruction instruction)

        (push instruction *instructions*)
        (when done
          (setf (block-end-address block) (+ (instruction-start instruction) (instruction-length instruction)))
          (return))
        (setq offset (- (+ (instruction-start instruction) (instruction-length instruction)) block-start)))))
  nil)
