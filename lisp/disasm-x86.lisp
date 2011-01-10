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

(defparameter *disassemblers* (make-hash-table))

(defun install-disassembler (byte disassembler)
  (setf (gethash byte *disassemblers*) disassembler))

(defun find-disassembler (byte)
  (gethash2-1 byte *disassemblers*))

(defmacro define-disassembler (byte-or-bytes &body body)
  (let* ((bytes (designator-list byte-or-bytes))
         (name (intern (format nil "DIS~{-~X~}" bytes)))
         (args '(byte1 start)))
    `(progn
       (defun ,name ,args
         (declare (ignorable ,@args))
         ,@body)
       (dolist (byte ',bytes)
         (install-disassembler byte ',name)))))

(define-disassembler #x25
  (make-instruction :start start
                    :length 5
                    :mnemonic :and
                    :operand1 (make-immediate-operand (mref-32 start 1))
                    :operand2 (make-register-operand :eax)))

(define-disassembler #x3c
  (let ((byte2 (mref-8-signed start 1)))
    (make-instruction :start start
                      :length 2
                      :mnemonic :cmp
                      :operand1 (make-immediate-operand byte2)
                      :operand2 (make-register-operand :al))))

(define-disassembler #x80
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod 3)
                (eql reg 4))
           (make-instruction :start start
                             :length 3
                             :mnemonic :and
                             :operand1 (make-immediate-operand (mref-8 start 2))
                             :operand2 (make-register-operand (byte-register rm))))
          ((and (eql mod 3)
                (eql reg 7))
           (make-instruction :start start
                             :length 3
                             :mnemonic :cmp
                             :operand1 (make-immediate-operand (mref-8 start 2))
                             :operand2 (make-register-operand (byte-register rm))))
          (t
           (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(define-disassembler #x88
  ;; /r MOV r/m8,r8
  (with-modrm-byte (mref-8 start 1)
    (cond ((eql mod 3)
           (make-instruction :start start
                             :length 2
                             :mnemonic :mov
                             :operand1 (make-register-operand (byte-register reg))
                             :operand2 (make-register-operand (byte-register rm))))
          (t
           (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                   modrm-byte mod reg rm)
           (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(define-disassembler #x8f
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod #b00)
                (eql reg 0))
           (make-instruction :start start
                             :length 2
                             :mnemonic :pop
                             :operand1 (make-operand :kind :relative
                                                     :register (register rm)
                                                     :data 0)))
          ((and (eql mod #b01)
                (eql reg 0))
           (make-instruction :start start
                             :length 3
                             :mnemonic :pop
                             :operand1 (make-operand :kind :relative
                                                     :register (register rm)
                                                     :data (mref-8-signed start 2))))
          (t
           (unsupported)))))

(define-disassembler #xd1
  (with-modrm-byte (mref-8 start 1)
    (case reg
      ((4 5)
       (make-instruction :start start
                         :length 2
                         :mnemonic (if (eql reg 4) :shl :shr)
                         :operand1 (make-register-operand (register rm))))
      (t
       (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(define-disassembler (#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57)
  (make-instruction :start start
                    :length 1
                    :mnemonic :push
                    :operand1 (make-register-operand (register (- byte1 #x50)))))

(define-disassembler (#x58 #x59 #x5a #x5b #x5c #x5d #x5e #x5f)
  (make-instruction :start start
                    :length 1
                    :mnemonic :pop
                    :operand1 (make-register-operand (register (- byte1 #x58)))))

(define-disassembler #x6b
  (let ((byte2 (mref-8 start 1)))
    (cond ((eql byte2 #xc0)
           (let ((byte3 (mref-8 start 2)))
             (make-instruction :start start
                               :length 3
                               :mnemonic :imul
                               :operand1 (make-immediate-operand byte3)
                               :operand2 (make-register-operand :eax))))
          (t
           (error "unhandled byte sequence #x~2,'0x #x~2,'0x" #x6b byte2)))))

(define-disassembler #x8b
   ;; /r move r/m dword to dword register
   (with-modrm-byte (mref-8 start 1)
     (let (length mnemonic operand1 operand2 annotation)
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
;;               (cond ((eql rm #b001)
;;                      (let* ((displacement (mref-32 start 2))
;;                             (absolute-address (ldb (byte 32 0) (+ start 5 displacement))))
;;                        (setq length 6
;;                              mnemonic :mov
;;                              operand1 (make-register-operand (register reg))
;;                              operand2 (make-absolute-operand absolute-address))))
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
              (let ((displacement (mref-8-signed start 2)))
                (setq length 3
                      mnemonic :mov
                      operand1 (make-operand :kind :relative
                                             :register (register rm)
                                             :data displacement)
                      operand2 (make-register-operand (register reg)))
                (when (eq (register rm) :ebp)
                  (let ((index (/ displacement 4)))
                    (setq annotation (cdr (assoc index *locals*)))))))
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
              (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))
       (make-instruction :start      start
                         :length     length
                         :mnemonic   mnemonic
                         :operand1   operand1
                         :operand2   operand2
                         :annotation annotation))))

(define-disassembler #x8d
  (with-modrm-byte (mref-8 start 1)
    (let (length mnemonic operand1 operand2 annotation)
      (cond ((eql mod #b01)
             (cond ((eql rm #b100)
                    ;; SIB follows
                    (with-sib (mref-8 start 2)
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
                                                 :data (mref-32-signed start 2))
                          operand2 (make-register-operand (register reg))))))
            ((eql mod #b10)
             (cond ((eql rm #b100)
                    ;; SIB follows
                    (with-sib (mref-8 start 2)
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
;;                     (setq length 3
;;                           mnemonic :lea
;;                           operand1 (make-operand :kind :relative
;;                                                  :register (register rm)
;;                                                  :data (mref-32-signed block-start (+ offset 2)))
;;                           operand2 (make-register-operand (register reg)))
                    (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte))))
            (t
             (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))
      (make-instruction :start      start
                        :length     length
                        :mnemonic   mnemonic
                        :operand1   operand1
                        :operand2   operand2
                        :annotation annotation))))

(define-disassembler #xf7
  (with-modrm-byte (mref-8 start 1)
    (cond ((and (eql mod #b11)
                (eql reg 2))
           (make-instruction :start start
                             :length 2
                             :mnemonic :not
                             :operand1 (make-register-operand (register rm))))
          ((and (eql mod #b11)
                (eql reg 3))
           (make-instruction :start start
                             :length 2
                             :mnemonic :negate
                             :operand1 (make-register-operand (register rm))))
          ((and (eql mod #b11)
                (eql reg 6))
           (make-instruction :start start
                             :length 2
                             :mnemonic :div
                             :operand1 (make-register-operand (register rm))))
          (t
           (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(define-disassembler #xff
  (with-modrm-byte (mref-8 start 1)
   (cond ((eql modrm-byte #xd0) ; call *%eax
          (make-instruction :start    start
                            :length   2
                            :mnemonic :call
                            :operand1 (make-register-operand :eax)))
         ((eql modrm-byte #xe0) ; jmp *%eax
          (make-instruction :start    start
                            :length   2
                            :mnemonic :jmp
                            :operand1 (make-register-operand :eax)))
         ((eql reg 1)
          (make-instruction :start    start
                            :length   2
                            :mnemonic :dec
                            :operand1 (make-register-operand (register rm))))
         ((eql reg 6)
          (case mod
            (#b01
             (make-instruction :start    start
                               :length   3
                               :mnemonic :push
                               :operand1 (make-operand :kind     :relative
                                                       :register (register rm)
                                                       :data     (mref-8-signed start 2))))
            (#b10
             (make-instruction :start    start
                               :length   6
                               :mnemonic :push
                               :operand1 (make-operand :kind     :relative
                                                       :register (register rm)
                                                       :data     (mref-32-signed start 2))))
            (t
             (unsupported))))
         (t
          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(defun process-block (block)
  (let ((block-start (block-start-address block))
        (offset 0)
        (done nil))
    (loop
      (let (start
            length
            mnemonic
            operand1
            operand2
            annotation
            (byte1 (mref-8 block-start offset))
            instruction)
        ;; instruction start
        (setq start (+ block-start offset))
        (let ((disassembler (find-disassembler byte1)))
          (if disassembler
              (setq instruction (funcall disassembler byte1 start))
              (case byte1
                (#x01
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :add
                            operand1 (make-register-operand (register reg))
                            operand2 (make-register-operand (register rm))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                      ))))
                (#x05
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :add
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand :eax))))
                (#x09
                 ;; /r or dword register to r/m dword
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b01
                      (setq length 3
                            mnemonic :or
                            operand1 (make-register-operand (register reg))
                            operand2 (make-operand :kind :relative
                                                   :register (register rm)
                                                   :data (mref-8 block-start (+ offset 2)))))
                     (#b11
                      (setq length 2
                            mnemonic :or
                            operand1 (make-register-operand (register reg))
                            operand2 (make-register-operand (register rm))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#x0f
                 (let ((byte2 (mref-8 block-start (1+ offset))))
                   (cond ((eql byte2 #xaf)
                          (with-modrm-byte (mref-8 block-start (+ offset 2))
                            (setq length 3
                                  mnemonic :imul
                                  operand1 (make-register-operand (register rm))
                                  operand2 (make-register-operand (register reg)))))
                         ((eql byte2 #xb6)
                          (let ((byte3 (mref-8 block-start (+ offset 2))))
                            (cond ((eql byte3 #x02)
                                   (setq length 3
                                         mnemonic :movzbl
                                         operand1 (make-operand :kind :relative
                                                                :register :edx
                                                                :data 0)
                                         operand2 (make-register-operand :eax)))
                                  (t
                                   (error "unhandled byte sequence #x~2,'0x #x~2,'0x #x~2,'0x"
                                          byte1 byte2 byte3)))))
                         ((eql (ldb (byte 4 4) byte2) 8)
                          (setq instruction (process-jcc (+ block-start offset))))
                         ((eql (ldb (byte 4 4) byte2) 4)
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
                          (with-modrm-byte (mref-8 block-start (+ offset 2))
                            (cond ((= modrm-byte #x05)
                                   (setq length 7
                                         operand1 (make-operand :kind :absolute
                                                                :data (mref-32 block-start (+ offset 3)))
                                         operand2 (make-register-operand (register reg))))
                                  (t
                                   (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                                   ))))
                         )))
                (#x21
                 ;; /r and dword register to r/m dword
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b01
                      (setq length 3
                            mnemonic :and
                            operand1 (make-register-operand (register reg))
                            operand2 (make-operand :kind :relative
                                                   :register (register rm)
                                                   :data (mref-8 block-start (+ offset 2)))))
                     (#b11
                      (setq length 2
                            mnemonic :and
                            operand1 (make-register-operand (register reg))
                            operand2 (make-register-operand (register rm))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#x24
                 (setq length 2
                       mnemonic :and
                       operand1 (make-register-operand :al)
                       operand2 (make-immediate-operand (mref-8 block-start (1+ offset))))
                 )
                (#x29
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :sub
                            operand1 (make-register-operand (register reg))
                            operand2 (make-register-operand (register rm))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#x2c
                 (let ((immediate-value (mref-8 block-start (1+ offset))))
                   (setq length 2
                         mnemonic :sub
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand :al))))
                (#x2d
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :sub
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand :eax))))
                (#x31
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   ;;              (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                   ;;                      modrm-byte mod reg rm)
                   ;;              (error "unsupported opcode #x31")
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :xor
                            operand1 (make-register-operand (register reg))
                            operand2 (make-register-operand (register rm))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                      ))))
                (#x39
                 ;; /r compare dword register to r/m dword
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b01
                      (setq length 3
                            mnemonic :cmp
                            operand1 (make-register-operand (register reg))
                            operand2 (make-operand :kind :relative
                                                   :register (register rm)
                                                   :data (mref-8 block-start (+ offset 2)))))
                     (#b11
                      (setq length 2
                            mnemonic :cmp
                            operand1 (make-register-operand (register reg))
                            operand2 (make-register-operand (register rm))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#x3b
                 ;; /r compare r/m dword to dword register
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (= mod #b00)
                               (= rm  #b101))
                          (setq length 6
                                mnemonic :cmp
                                operand1 (make-operand :kind :absolute
                                                       :data (mref-32-signed block-start (+ offset 2)))
                                operand2 (make-register-operand (register reg))))
                         )
                   ))
                (#x3d
                 ;; compare immediate dword to eax
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :cmp
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand :eax)
                         annotation immediate-value)))
                ((#x40 #x41 #x42 #x43 #x44 #x45 #x46 #x47)
                 (setq length 1
                       mnemonic :inc
                       operand1 (make-register-operand (register (- byte1 #x40)))))
                ((#x48 #x49 #x4a #x4b #x4c #x4d #x4e #x4f)
                 (setq length 1
                       mnemonic :dec
                       operand1 (make-register-operand (register (- byte1 #x48)))))
;;                 ((#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57)
;;                  (setq length 1
;;                        mnemonic :push
;;                        operand1 (make-register-operand (register (- byte1 #x50)))))
;;                 ((#x58 #x59 #x5a #x5b #x5c #x5d #x5e #x5f)
;;                  (setq length 1
;;                        mnemonic :pop
;;                        operand1 (make-register-operand (register (- byte1 #x58)))))
                (#x60
                 (setq length 1
                       mnemonic :pusha))
                (#x61
                 (setq length 1
                       mnemonic :popa))
                (#x66
                 (setq length 1
                       mnemonic :data16))
                (#x68
                 ;; push immediate dword
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :push
                         operand1 (make-immediate-operand immediate-value)
                         annotation immediate-value)))
                (#x6a
                 ;; push immediate byte
                 (let* ((immediate-value (mref-8 block-start (1+ offset))))
                   (setq length 2
                         mnemonic :push
                         operand1 (make-immediate-operand immediate-value))))
                (#x70
                 ;; jump short if overflow (OF=1), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jo
                         operand1 (make-absolute-operand absolute-address))))
                (#x71
                 ;; jump short if not overflow (OF=0), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jno
                         operand1 (make-absolute-operand absolute-address))))
                (#x74
                 ;; jump short if ZF=1, 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :je
                         operand1 (make-absolute-operand absolute-address))))
                (#x75
                 ;; jump short if ZF=0, 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jne
                         operand1 (make-absolute-operand absolute-address))))
                (#x77
                 ;; jump short if above (CF=0 and ZF=0), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :ja
                         operand1 (make-absolute-operand absolute-address))))
                (#x78
                 ;; jump short if SF=1, 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :js
                         operand1 (make-absolute-operand absolute-address))))
                (#x7c
                 ;; jump short if less (SF<>OF), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8-signed block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jl
                         operand1 (make-absolute-operand absolute-address))))
                (#x7d
                 ;; jump short if not less (SF=OF), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8 block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jnl
                         operand1 (make-absolute-operand absolute-address))))
                (#x7e
                 ;; jump short if not greater (ZF=1 or SF<>OF), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8 block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jng
                         operand1 (make-absolute-operand absolute-address))))
                (#x7f
                 ;; jump short if greater (ZF=0 and SF=OF), 1-byte displacement relative to next instruction
                 (let* ((displacement (mref-8 block-start (1+ offset)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq length 2
                         mnemonic :jg
                         operand1 (make-absolute-operand absolute-address))))
                (#x81
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (= mod #b11)
                               (= reg 0))
                          (setq length 6
                                mnemonic :add
                                operand1 (make-immediate-operand (mref-32 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((and (= mod #b11)
                               (= reg #b101))
                          (setq length 6
                                mnemonic :sub
                                operand1 (make-immediate-operand (mref-32 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x83
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql modrm-byte #x3d)
                          ;; 83  /7 ib       CMP r/m32,imm8     2/5      Compare sign extended immediate byte to r/m dword
                          (setq length 7
                                mnemonic :cmp
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 6)))
                                operand2 (make-operand :kind :absolute
                                                       :data (mref-32-signed block-start (+ offset 2))))
                          )
                         ((and (eql mod #b01)
                               (eql reg #b111))
                          (setq length 4
                                mnemonic :cmpl
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 3)))
                                operand2 (make-operand :kind :relative
                                                       :register (register rm)
                                                       :data (mref-8 block-start (+ offset 2)))))
                         ((and (eql mod #b11)
                               (eql reg 0))
                          (setq length 3
                                mnemonic :add
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((and (eql mod #b11)
                               (eql reg #b101))
                          (setq length 3
                                mnemonic :sub
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((and (eql mod #b11)
                               (eql reg #b100))
                          (setq length 3
                                mnemonic :and
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((and (eql mod #b11)
                               (eql reg #b111))
                          (setq length 3
                                mnemonic :cmp
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((and (eql mod #b11)
                               (eql reg 1))
                          (setq length 3
                                mnemonic :or
                                operand1 (make-immediate-operand (mref-8-signed block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         (t
                          (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                                  modrm-byte mod reg rm)
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x84
                 ;; /r AND byte register with r/m byte
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((= mod #b11)
                          (setq length 2
                                mnemonic :test
                                operand1 (make-register-operand (byte-register reg))
                                operand2 (make-register-operand (byte-register rm))))
                         (t
                          (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                                  modrm-byte mod reg rm)
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x85
                 ;; /r AND dword register with r/m dword
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((= mod #b11)
                          (setq length 2
                                mnemonic :test
                                operand1 (make-register-operand (register reg))
                                operand2 (make-register-operand (register rm))))
                         (t
                          (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                                  modrm-byte mod reg rm)
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x89
                 ;; /r move dword register to r/m dword
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond #+nil
                         ((eql modrm-byte #x01)
                          (setq length 2
                                mnemonic :mov
                                operand1 (make-register-operand :eax)
                                operand2 (make-indirect-operand :ecx)))
                         #+nil
                         ((eql modrm-byte #x03)
                          (setq length 2
                                mnemonic :mov
                                operand1 (make-register-operand :eax)
                                operand2 (make-indirect-operand :ebx)))
                         ((eql mod #b01)
                          (let ((byte3 (mref-8 block-start (+ offset 2))))
                            (cond ((= byte3 #x24)
                                   (setq length 4
                                         mnemonic :mov
                                         operand1 (make-register-operand (register reg))
                                         operand2 (make-operand :kind :relative
                                                                :register (register rm)
                                                                :data (mref-8 block-start (+ offset 3)))))
                                  (t
                                   (let ((displacement (mref-8-signed block-start (+ offset 2))))
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
                          (let (
                                #+nil (byte3 (mref-8 block-start (+ offset 2)))
                                )
                            (cond #+nil
                                  ((= byte3 #x24) ;; REVIEW
                                   (setq length 4
                                         mnemonic :mov
                                         operand1 (make-register-operand (register reg))
                                         operand2 (make-operand :kind :relative
                                                                :register (register rm)
                                                                :data (mref-8 block-start (+ offset 3)))))
                                  (t
                                   (let ((displacement (mref-32-signed block-start (+ offset 2))))
                                     (setq length 6
                                           mnemonic :mov
                                           operand1 (make-register-operand (register reg))
                                           operand2 (make-operand :kind :relative
                                                                  :register (register rm)
                                                                  :data displacement))
                                     (when (eq (register rm) :ebp)
                                       (let ((index (/ displacement 4)))
                                         (setq annotation (cdr (assoc index *locals*))))))))))
                         ((eql mod #b11)
                          (setq length 2
                                mnemonic :mov
                                operand1 (make-register-operand (register reg))
                                operand2 (make-register-operand (register rm))))
                         ((and (eql mod #b00)
                               ;;                          (= reg #b010)
                               ;;                          (= rm  #b100)
                               )
                          (cond ((and (eql modrm-byte #x04)
                                      (eql (mref-8 block-start (+ offset 2)) #x24))
                                 (setq length 3))
                                (t
                                 (setq length 2)))
                          (setq mnemonic :mov
                                operand1 (make-register-operand (register reg))
                                operand2 (make-indirect-operand (register rm))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x90
                 (setq length 1
                       mnemonic :nop))
                (#xa1
                 ;; move dword at (seg:offset) to EAX
                 (setq length 5
                       mnemonic :mov
                       operand1 (make-operand :kind :absolute
                                              :data (mref-32 block-start (+ offset 1)))
                       operand2 (make-register-operand :eax)))
                (#xa8
                 (let* ((immediate-value (mref-8 block-start (1+ offset))))
                   (setq length 2
                         mnemonic :test
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand :al))))
                (#xad
                 (setq length 1
                       mnemonic :lods)) ; FIXME lods %ds:(%esi),%eax
                ((#xb8 #xb9 #xba #xbb #xbc #xbd #xbe #xbf)
                 ;; move immediate dword to register
                 (let ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :mov
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand (register (- byte1 #xb8)))
                         annotation immediate-value)))
                (#xc1
                 ;; C1   /7 ib      SAR r/m32,imm8
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql reg 7)
                          (setq length 3
                                mnemonic :sar
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((eql reg 5)
                          (setq length 3
                                mnemonic :shr
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((eql reg 4)
                          (setq length 3
                                mnemonic :shl
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xc3
                 (setq length 1
                       mnemonic :ret)
                 (setf (block-end-address block) (+ block-start offset 1))
                 (setq done t))
                (#xc6
                 ;; C6       MOV r/m8,imm8     2/2           Move immediate byte to r/m byte
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql mod #b01)
                          (setq length 4
                                mnemonic :mov
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 3)))
                                operand2 (make-operand :kind :relative
                                                       :register (register rm)
                                                       :data (mref-8 block-start (+ offset 2)))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xc7
                 ;; C7       MOV r/m32,imm32
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql modrm-byte #x40)
                          (setq length 7
                                mnemonic :mov
                                operand1 (make-immediate-operand (mref-32 block-start (+ offset 3)))
                                operand2 (make-operand :kind :relative
                                                       :register :eax
                                                       :data (mref-8 block-start (+ offset 2)))))
                         (t
                          (unsupported)))))
                (#xc9
                 (setq length 1
                       mnemonic :leave))
                (#xcc
                 (setq length 1
                       mnemonic :int3))
                (#xe8
                 ;; call near, displacement relative to next instruction
                 (let* ((displacement (mref-32 block-start (1+ offset)))
                        (absolute-address (ldb (byte 32 0) (+ block-start offset 5 displacement))))
                   (setq length 5
                         mnemonic :call
                         operand1 (make-absolute-operand absolute-address)
                         annotation absolute-address)
                   (push absolute-address *labels*)))
                (#xe9
                 (let* ((displacement (mref-32-signed block-start (+ offset 1)))
                        (absolute-address (ldb (byte 32 0) (+ block-start offset 5 displacement))))
                   (setq length 5
                         mnemonic :jmpq
                         operand1 (make-absolute-operand absolute-address))
                   (setf (block-end-address block) (+ block-start offset 5))
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (push absolute-address *labels*)
                   (setq done t)))
                (#xeb
                 (let* ((displacement (mref-8-signed block-start (+ offset 1)))
                        (absolute-address (+ block-start offset 2 displacement)))
                   (setq length 2
                         mnemonic :jmp
                         operand1 (make-absolute-operand absolute-address))
                   (setf (block-end-address block) (+ block-start offset 2))
                   (push absolute-address *labels*)
                   (push (make-disassembly-block :start-address absolute-address) *blocks*)
                   (setq done t)))
                (#xf3
                 (setq length 1
                       mnemonic :repz))
                (#xf6
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (eql mod #b01)
                               (eql reg 0))
                          (setq length 4
                                mnemonic :testb
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 3)))
                                operand2 (make-operand :kind :relative
                                                       :register (register rm)
                                                       :data (mref-8 block-start (+ offset 2)))))
                         ((and (eql mod #b11)
                               (eql reg 0))
                          (setq length 3
                                mnemonic :test
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (byte-register rm))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xff
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((= modrm-byte #xd0)
                          (setq length 2
                                mnemonic :call
                                operand1 (make-register-operand :eax)))
                         ((= reg 1)
                          (setq length 2
                                mnemonic :dec
                                operand1 (make-register-operand (register rm))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (t
                 (error "unhandled opcode #x~2,'0x at #x~X" byte1 (+ block-start offset))))))
        (when (null instruction)
          (setq instruction
                (make-instruction :start start
                                  :length length
                                  :mnemonic mnemonic
                                  :operand1 operand1
                                  :operand2 operand2
                                  :annotation annotation)))
;;         (format t "~%instruction = ~s~%" instruction)
        (cond (instruction
;;                (print-instruction instruction)
               (push instruction *instructions*)
;;                (incf offset (instruction-length instruction))
               (setq offset
                     (- (+ (instruction-start instruction) (instruction-length instruction))
                        block-start))
;;                (format t "~%new offset = 0x~X~%" offset)
               )
              (t
               (error "shouldn't happen")))
        (when done
          (return))
        )
      )
    )
  nil)
