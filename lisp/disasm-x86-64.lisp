;;; disasm-x86-64.lisp
;;;
;;; Copyright (C) 2006-2010 Peter Graves <peter@armedbear.org>
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

(defparameter *handlers* (make-hash-table))

(defun install-handler (byte handler)
  (setf (gethash byte *handlers*) handler))

(defun find-handler (byte)
  (gethash2-1 byte *handlers*))

(defmacro define-handler (byte-or-bytes &body body)
  (let* ((bytes (designator-list byte-or-bytes))
         (name (intern (format nil "DIS~{-~X~}" bytes)))
         (args '(byte1 start offset prefix-byte)))
    `(progn
       (defun ,name ,args
         (declare (ignorable ,@args))
         ,@body)
       (dolist (byte ',bytes)
         (setf (gethash byte *handlers*) ',name)))))

(define-handler #x3c
  ;; compare immediate byte to AL
  (let ((data (mref-8 start (1+ offset))))
    (make-instruction :start start
                      :length 2
                      :mnemonic :cmp
                      :operand1 (make-immediate-operand data)
                      :operand2 (make-register-operand :al))))

(define-handler #x77
  ;; jump short if above (CF=0 and ZF=0)
 (let* ((displacement (mref-8-signed start (1+ offset)))
        (absolute-address (+ start offset 2 displacement)))
   (push (make-disassembly-block :start-address absolute-address) *blocks*)
   (push absolute-address *labels*)
   (make-instruction :start start
                     :length 2
                     :mnemonic :ja
                     :operand1 (make-absolute-operand absolute-address))))

(define-handler #x80
  (with-modrm-byte (mref-8 start (1+ offset))
  (cond ((and (eql mod 3)
              (eql reg 4))
         (make-instruction :start start
                           :length 3
                           :mnemonic :and
                           :operand1 (make-immediate-operand (mref-8 start (+ offset 2)))
                           :operand2 (make-register-operand (byte-register rm))))
        ((and (eql mod 3)
              (eql reg 7))
         (make-instruction :start start
                           :length 3
                           :mnemonic :cmp
                           :operand1 (make-immediate-operand (mref-8 start (+ offset 2)))
                           :operand2 (make-register-operand (byte-register rm))))
        (t
         (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(define-handler #xd1
  (with-modrm-byte (mref-8 start (1+ offset))
;;     (format t "mod = ~S reg = ~S rm = ~S~%" mod reg rm)
    (case reg
      ((4 5)
       (let ((length 2))
         (when prefix-byte
           (incf length))
         (make-instruction :start start
                           :length length
                           :mnemonic (if (eql reg 4) :shl :shr)
                           :operand1 (make-register-operand (register rm prefix-byte)))))
      (t
       (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))

(define-handler #x25
  (let ((data (mref-32 start (1+ offset)))
        (length 5))
    (when prefix-byte
      (incf length))
    (make-instruction :start start
                      :length length
                      :mnemonic :and
                      :operand1 (make-immediate-operand data)
                      :operand2 (make-register-operand (register 0 prefix-byte)))))

(define-handler #x2c
  (make-instruction :start start
                    :length 2
                    :mnemonic :sub
                    :operand1 (make-immediate-operand (mref-8 start (1+ offset)))
                    :operand2 (make-register-operand :al)))

(define-handler #x04
  (make-instruction :start start
                    :length 2
                    :mnemonic :add
                    :operand1 (make-immediate-operand (mref-8 start (1+ offset)))
                    :operand2 (make-register-operand :al)))

(define-handler #x98
  (make-instruction :start start
                    :length (if prefix-byte 2 1)
                    :mnemonic :cwtl))

(define-handler #x8b
  ;; /r move r/m dword to dword register
  (let (length operand1 operand2 annotation)
    (with-modrm-byte (mref-8 start (1+ offset))
      (cond ((eql modrm-byte 0)
             (setq length 2
                   operand1 (make-operand :kind :relative
                                          :register :rax
                                          :data 0)
                   operand2 (make-register-operand (register 0 prefix-byte))))
            ((eql modrm-byte 2)
             (setq length 2
                   operand1 (make-operand :kind :relative
                                          :register :rdx
                                          :data 0)
                   operand2 (make-register-operand :rax)))
            ((eql mod #b00)
             (let ((source (register reg)))
               (cond ((eql rm #b001)
                      (let* ((displacement (mref-32 start (+ offset 2)))
                             (absolute-address (ldb (byte 32 0) (+ start offset 5 displacement))))
                        (setq length 6
                              operand1 (make-register-operand source)
                              operand2 (make-absolute-operand absolute-address))))
                     ((eql rm #b101)
                      (let* ((displacement (mref-32 start (+ offset 2))))
                        (setq length 6
                              operand1 (make-operand :kind :relative
                                                     :register :rip
                                                     :data displacement)
                              operand2 (make-register-operand (register reg prefix-byte)))))
                     (t
                      (setq length 2
                            operand1 (make-indirect-operand (register-rm rm prefix-byte))
                            operand2 (make-register-operand (register-reg reg prefix-byte)))))))
            ((eql mod #b01)
             ;; 1-byte displacement
             (cond ((eql rm #b100)
                    (let ((sib-byte (mref-8 start (+ offset 2)))
                          (displacement-byte (mref-8-signed start (+ offset 3))))
                      (cond ((eql sib-byte #x24)
                             (setq length 4
                                   operand1 (make-operand :kind :relative
                                                          :register (register-rm rm prefix-byte)
                                                          :data displacement-byte)
                                   operand2 (make-register-operand (register-reg reg prefix-byte))))
                            (t
                             (unsupported)))))
                   (t
                    (let ((displacement (mref-8-signed start (+ offset 2))))
                      (setq length 3
                            operand1 (make-operand :kind :relative
                                                   :register (register-rm rm prefix-byte)
                                                   :data displacement)
                            operand2 (make-register-operand (register-reg reg prefix-byte)))
                      (when (eq (register-rm rm prefix-byte) :rbp)
                        (let ((index (/ (+ displacement 8) -8)))
                          (setq annotation (cdr (assoc index *locals*)))))))))
            ((eql mod #b10)
             (let ((displacement (mref-32-signed start (+ offset 2))))
               (setq length 6
                     operand1 (make-operand :kind :relative
                                            :register (register rm prefix-byte)
                                            :data displacement)
                     operand2 (make-register-operand (register reg prefix-byte)))
               (when (eq (register rm prefix-byte) :rbp)
                 (let ((index (/ (+ displacement 8) -8)))
                   (setq annotation (cdr (assoc index *locals*)))))))
            (t
             (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte))))
    (when prefix-byte
      (incf length))
    (make-instruction :start start
                      :length length
                      :mnemonic :mov
                      :operand1 operand1
                      :operand2 operand2
                      :annotation annotation)))

(define-handler #xc9
  (make-instruction :start start
                    :length 1
                    :mnemonic :leave))

(define-handler (#x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57)
  ;; "In 64-bit mode, the operand size of all PUSH instructions defaults to 64
  ;; bits, and there is no prefix available to encode a 32-bit operand size."
  (make-instruction :start start
                    :length (if prefix-byte 2 1)
                    :mnemonic :push
                    :operand1 (make-register-operand (register (- byte1 #x50)
                                                               (or prefix-byte #x48)))))

(define-handler #x6b
   (let ((byte2 (mref-8 start (1+ offset))))
     (cond ((and (eql prefix-byte #x48)
                 (eql byte2 #xc0))
            (let ((byte3 (mref-8 start (+ offset 2))))
              (make-instruction :start start
                                :length 4
                                :mnemonic :imul
                                :operand1 (make-immediate-operand byte3)
                                :operand2 (make-register-operand :rax))
              ))
           (t
            (error "unhandled byte sequence #x~2,'0x #x~2,'0x" #x6b byte2)))))

(define-handler #xff
  (with-modrm-byte (mref-8 start (1+ offset))
    (cond ((eql modrm-byte #xd0) ; call *%eax
           (make-instruction :start start
                             :length 2
                             :mnemonic :call
                             :operand1 (make-register-operand :eax)))
          ((eql modrm-byte #xe0) ; jmp *%eax
           (make-instruction :start start
                             :length 2
                             :mnemonic :jmp
                             :operand1 (make-register-operand :eax)))
          ((eql reg 1)
           (make-instruction :start start
                             :length (if prefix-byte 3 2)
                             :mnemonic :dec
                             :operand1 (make-register-operand (register rm))))
          ((eql reg 6)
           (case mod
             (#b01
              (let ((displacement (mref-8-signed start 2))
                    annotation)
                (when (eq (register-rm rm #x48) :rbp)
                  (let ((index (/ (+ displacement 8) -8)))
                    (setq annotation (cdr (assoc index *locals*)))))
                (make-instruction :start start
                                  :length 3
                                  :mnemonic :push
                                  ;; "In 64-bit mode, the operand size of all
                                  ;; PUSH instructions defaults to 64 bits, and
                                  ;; there is no prefix available to encode a 32-
                                  ;; bit operand size."
                                  :operand1 (make-operand :kind :relative
                                                          :register (register rm #x48) ; force 64-bit reg
                                                          :data (mref-8-signed start 2))
                                  :annotation annotation)))
             (#b10
              (make-instruction :start start
                                :length 6
                                :mnemonic :push
                                :operand1 (make-operand :kind :relative
                                                        :register (register rm)
                                                        :data (mref-32-signed start 2))))
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
            prefix-byte
            mnemonic
            operand1
            operand2
            annotation
            (byte1 (mref-8 block-start offset))
            instruction)
        ;; instruction start
        (setq start (+ block-start offset))
        ;; prefix byte?
        (when (<= #x40 byte1 #x4f)
;;           (format t "~%prefix byte 0x~2,'0x~%" byte1)
          (setq prefix-byte byte1)
          (setq byte1 (mref-8 block-start (incf offset))))
;;         (format t "calling find-handler byte1 = ~x~%" byte1)
        (let ((handler (find-handler byte1)))
          (if handler
              (let ((local-offset (if prefix-byte 1 0)))
;;                 (format t "calling handler for ~x~%" byte1)
                (setq instruction (funcall handler byte1 start local-offset prefix-byte)))
              (case byte1
                (#x01
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :add
                            operand1 (make-register-operand (register reg prefix-byte))
                            operand2 (make-register-operand (register rm prefix-byte))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                      ))))
                (#x05
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :add
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand (register 0 prefix-byte)))))
                (#x09
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :or
                            operand1 (make-register-operand (register reg prefix-byte))
                            operand2 (make-register-operand (register rm prefix-byte))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                      ))))
                (#x0f
                 (let ((byte2 (mref-8 block-start (1+ offset))))
                   (cond ((eql byte2 #xaf)
                          (with-modrm-byte (mref-8 block-start (+ offset 2))
                            (setq length 3
                                  mnemonic :imul
                                  operand1 (make-register-operand (register rm prefix-byte))
                                  operand2 (make-register-operand (register reg prefix-byte)))))
                         ((eql byte2 #xb6)
                          (let ((byte3 (mref-8 block-start (+ offset 2))))
                            (cond ((and (eql byte3 #x00)
                                        (eql prefix-byte #x48))
                                   (setq length 3
                                         mnemonic :movzbq
                                         operand1 (make-operand :kind :relative
                                                                :register :rax
                                                                :data 0)
                                         operand2 (make-register-operand :rax)))
                                  ((and (eql byte3 #x02)
                                        (eql prefix-byte #x48))
                                   (setq length 3
                                         mnemonic :movzbq
                                         operand1 (make-operand :kind :relative
                                                                :register :rdx
                                                                :data 0)
                                         operand2 (make-register-operand :rax)))
                                  ((eql byte3 #xc0)
                                   (setq length 3
                                         mnemonic :movzbl
                                         operand1 (make-register-operand :al)
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
                            (cond ((eql modrm-byte #x05)
                                   (setq length 7
                                         operand1 (make-operand :kind :relative
                                                                :register :rip
                                                                :data (mref-32 block-start (+ offset 3)))
                                         operand2 (make-register-operand (register reg prefix-byte))))
                                  (t
                                   (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 byte2)))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 byte2)))))
                (#x21
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :and
                            operand1 (make-register-operand (register reg prefix-byte))
                            operand2 (make-register-operand (register rm prefix-byte))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                      ))))
                (#x24
                 (setq length 2
                       mnemonic :and
                       operand1 (make-immediate-operand (mref-8 block-start (1+ offset)))
                       operand2 (make-register-operand :al))
                 )
                (#x29
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :sub
                            operand1 (make-register-operand (register reg prefix-byte))
                            operand2 (make-register-operand (register rm prefix-byte))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                      ))))
                (#x2b
                 ;; subtract dword register from r/m dword
                 )
                (#x2d
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :sub
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand :eax))))
                (#x31
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (case mod
                     (#b11
                      (setq length 2
                            mnemonic :xor
                            operand1 (make-register-operand (register reg prefix-byte))
                            operand2 (make-register-operand (register rm prefix-byte))))
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
                            operand1 (make-register-operand (register reg prefix-byte))
                            operand2 (make-register-operand (register rm prefix-byte))))
                     (t
                      (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#x3b
                 ;; /r compare r/m dword to dword register
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (eql mod #b00)
                               ;;                          (= reg #b010)
                               (eql rm  #b101))
                          (setq length 6
                                mnemonic :cmp
                                operand1 (make-operand :kind :relative
                                                       :register :rip
                                                       :data (mref-32-signed block-start (+ offset 2)))
                                operand2 (make-register-operand (register reg prefix-byte)))))))
                (#x3d
                 ;; compare immediate dword to eax
                 (let* ((immediate-value (mref-32 block-start (1+ offset))))
                   (setq length 5
                         mnemonic :cmp
                         operand1 (make-immediate-operand immediate-value)
                         operand2 (make-register-operand (if (eql prefix-byte #x48) :rax :eax))
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
;;                  ;; "In 64-bit mode, the operand size of all PUSH instructions defaults to 64
;;                  ;; bits, and there is no prefix available to encode a 32-bit operand size."
;;                  (setq length 1
;;                        mnemonic :push
;;                        operand1 (make-register-operand (register (- byte1 #x50)
;;                                                                  (or prefix-byte #x48)))))
                ((#x58 #x59 #x5a #x5b #x5c #x5d #x5e #x5f)
                 ;; "In 64-bit mode, the POP operand size defaults to 64 bits and there is no
                 ;; prefix available to encode a 32-bit operand size."
                 (setq length 1
                       mnemonic :pop
                       operand1 (make-register-operand (register (- byte1 #x58)
                                                                 (or prefix-byte #x48)))))
                (#x66
                 ;; FIXME
                 (let ((byte1 (mref-8 block-start (+ offset 1)))
                       (byte2 (mref-8 block-start (+ offset 2)))
                       (byte3 (mref-8 block-start (+ offset 3)))
                       (byte4 (mref-8 block-start (+ offset 4)))
                       (byte5 (mref-8 block-start (+ offset 5))))
                   (cond ((and (eql byte1 #x0f)
                               (eql byte2 #x1f)
                               (eql byte3 #x44)
                               (eql byte4 #x00)
                               (eql byte5 #x00))
                          (setq length 6
                                mnemonic :nopw)) ; nopw 0x0(%rax,%rax,1)
                         (t
                          (setq length 1
                                mnemonic :data16)))))
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
;;                 (#x6b
;;                  (let ((byte1 (mref-8 block-start (1+ offset))))
;;                    (cond ((eql byte1 #xc0)
;;                           (let ((byte2 (mref-8 block-start (+ offset 2))))
;;                             (setq length 3
;;                                   mnemonic :imul
;;                                   operand1 (make-immediate-operand byte2)
;;                                   oeprand2 (make-register-operand :eax))))
;;                          (t
;;                           (error "unhandled byte sequence #x~2,'0x #x~2,'0x" #x6b byte1)))))
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
                         mnemonic :jle
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
                   (cond ((and (eql mod #b11)
                               (eql reg 0))
                          (setq length 6
                                mnemonic :add
                                operand1 (make-immediate-operand (mref-32 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm))))
                         ((and (eql mod #b11)
                               (eql reg #b101))
                          (setq length 6
                                mnemonic :sub
                                operand1 (make-immediate-operand (mref-32 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((and (eql mod #b11)
                               (eql reg 7))
                          (setq length 6
                                mnemonic :cmp
                                operand1 (make-immediate-operand (mref-32-signed block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x83
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (= mod #b01)
                               (= reg #b111))
                          (setq length 4
                                mnemonic :cmpq
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 3)))
                                operand2 (make-operand :kind :relative
                                                       :register (register rm)
                                                       :data (mref-8 block-start (+ offset 2)))))
                         ((and (= mod #b11)
                               (= reg 0))
                          (setq length 3
                                mnemonic :add
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((and (= mod #b11)
                               (= reg #b101))
                          (setq length 3
                                mnemonic :sub
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((and (= mod #b11)
                               (= reg #b100))
                          (setq length 3
                                mnemonic :and
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((and (= mod #b11)
                               (= reg #b111))
                          (setq length 3
                                mnemonic :cmp
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((and (= mod #b11)
                               (= reg 1))
                          (setq length 3
                                mnemonic :or
                                operand1 (make-immediate-operand (mref-8-signed block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         (t
                          (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                                  modrm-byte mod reg rm)
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x84
                 ;; /r AND byte register with r/m byte
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql mod #b11)
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
                   (cond ((eql mod #b11)
                          (setq length 2
                                mnemonic :test
                                operand1 (make-register-operand (register reg prefix-byte))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         (t
                          (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                                  modrm-byte mod reg rm)
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x88
                 ;; /r MOV r/m8,r8
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql mod 0)
                          (setq length 2
                                mnemonic :mov
                                operand1 (make-register-operand (byte-register reg))
                                operand2 (make-operand :kind :relative
                                                       :register (register rm #x48)
                                                       :data 0)))
                         ((eql mod 3)
                          (setq length 2
                                mnemonic :mov
                                operand1 (make-register-operand (byte-register reg))
                                operand2 (make-register-operand (byte-register rm))))
                         (t
                          (format t "~%modrm-byte = #x~x mod = ~s reg = ~s rm = ~s~%"
                                  modrm-byte mod reg rm)
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#x89
                 ;; /r move dword register to r/m dword
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (eql modrm-byte #x04)
                               (eql (mref-8 block-start (+ offset 2)) #x24))
                          (setq length 3 ; REVIEW
                                mnemonic :mov
                                operand1 (make-register-operand (register reg prefix-byte))
                                operand2 (make-indirect-operand (register rm prefix-byte))))
                         ((eql mod #b01)
                          ;; 1-byte displacement
                          (let ((byte3 (mref-8 block-start (+ offset 2))))
                            (cond ((and (eql rm #b100) (eql byte3 #x24))
                                   (setq length 4
                                         mnemonic :mov
                                         operand1 (make-register-operand (register-reg reg prefix-byte))
                                         operand2 (make-operand :kind :relative
                                                                :register (register-rm rm prefix-byte)
                                                                :data (mref-8-signed block-start (+ offset 3)))))
                                  (t
                                   (let ((displacement (mref-8-signed block-start (+ offset 2))))
                                     (setq length 3
                                           mnemonic :mov
                                           operand1 (make-register-operand (register-reg reg prefix-byte))
                                           operand2 (make-operand :kind :relative
                                                                  :register (register-rm rm prefix-byte)
                                                                  :data displacement))
                                     (when (eq (register-rm rm prefix-byte) :rbp)
                                       (let ((index (/ (+ displacement 8) -8)))
                                         (setq annotation (cdr (assoc index *locals*)))))))
                                  ;;                             (t
                                  ;;                              (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte))
                                  )
                            ))
                         ((eql mod #b10)
                          ;; 4-byte displacement
                          (let ((displacement (mref-32-signed block-start (+ offset 2))))
                            (setq length 6
                                  mnemonic :mov
                                  operand1 (make-register-operand (register reg prefix-byte))
                                  operand2 (make-operand :kind :relative
                                                         :register (register rm prefix-byte)
                                                         :data displacement))
                            (when (eq (register rm prefix-byte) :rbp)
                              (let ((index (/ (+ displacement 8) -8)))
                                (setq annotation (cdr (assoc index *locals*)))))))
                         ((eql mod #b11)
                          (setq length 2
                                mnemonic :mov
                                operand1 (make-register-operand (register-reg reg prefix-byte))
                                operand2 (make-register-operand (register-rm rm prefix-byte))
                                ))
                         ((and (= mod #b00)
                               ;;                          (= reg #b010)
                               ;;                          (= rm  #b100)
                               )
                          (setq length 2 ; REVIEW
                                mnemonic :mov
                                operand1 (make-register-operand (register-reg reg prefix-byte))
                                operand2 (make-indirect-operand (register-rm rm prefix-byte))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (#x8d
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((= mod #b01)
                          (setq length 3
                                mnemonic :lea
                                operand1 (make-operand :kind :relative
                                                       :register (register rm prefix-byte)
                                                       :data (mref-8-signed block-start (+ offset 2)))
                                operand2 (make-register-operand (register reg prefix-byte))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
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
                       mnemonic :lods)) ; FIXME lods %ds:(%rsi),%rax
                ((#xb8 #xb9 #xba #xbb #xbc #xbd #xbe #xbf)
                 ;; move immediate dword to register
                 (cond ((eql prefix-byte #x48)
                        (let ((immediate-value (mref-64 block-start (1+ offset))))
                          (setq length 9
                                mnemonic :mov
                                operand1 (make-immediate-operand immediate-value)
                                operand2 (make-register-operand (register (- byte1 #xb8) prefix-byte))
                                annotation immediate-value)))
                       ((eql prefix-byte #x49)
                        (let ((immediate-value (mref-64 block-start (1+ offset))))
                          (setq length 9
                                mnemonic :mov
                                operand1 (make-immediate-operand immediate-value)
                                operand2 (make-register-operand (register (- byte1 #xb8) prefix-byte))
                                annotation immediate-value)))
                       (t
                        (let ((immediate-value (mref-32 block-start (1+ offset))))
                          (setq length 5
                                mnemonic :mov
                                operand1 (make-immediate-operand immediate-value)
                                operand2 (make-register-operand (register (- byte1 #xb8) prefix-byte))
                                annotation immediate-value)))))
                (#xc1
                 ;; C1   /7 ib      SAR r/m32,imm8
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql reg 7)
                          (setq length 3
                                mnemonic :sar
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((eql reg 4)
                          (setq length 3
                                mnemonic :shl
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((eql reg 5)
                          (setq length 3
                                mnemonic :shr
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xc3
                 (setq length 1
                       mnemonic :retq)
                 (setf (block-end-address block) (+ block-start offset 1))
                 (setq done t))
                (#xc6
                 ;; C6       MOV r/m8,imm8     2/2           Move immediate byte to r/m byte
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((and (eql prefix-byte #x41)
                               (eql modrm-byte #x44)
                               (eql (mref-8 block-start (+ offset 2)) #x24))
                          (setq length 5
                                mnemonic :movb
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 4)))
                                operand2 (make-operand :kind :relative
                                                       :register (register-rm rm prefix-byte)
                                                       :data (mref-8 block-start (+ offset 3)))))
                         ((eql mod #b01)
                          (setq length 4
                                mnemonic :movb
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 3)))
                                operand2 (make-operand :kind :relative
                                                       :register (register-rm rm prefix-byte)
                                                       :data (mref-8 block-start (+ offset 2)))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xc7
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond
                         ((eql mod #b11)
                          (let ((immediate-value (mref-32 block-start (+ offset 2))))
                            (setq length 6
                                  mnemonic :mov
                                  operand1 (make-immediate-operand immediate-value)
                                  operand2 (make-register-operand (register rm prefix-byte))
                                  annotation immediate-value)))
                         ((eql mod #b01) ; 8-bit displacement
                          (let ((sib-byte (mref-8 block-start (+ offset 2))))
                            (cond ((eql sib-byte #x24)
                                   (setq length 8
                                         mnemonic :movq
                                         operand1 (make-immediate-operand (mref-32-signed block-start (+ offset 4)))
                                         operand2 (make-operand :kind :relative
                                                                :register (register-rm rm prefix-byte)
                                                                :data (mref-8 block-start (+ offset 3)))))
                                  (t
                                   (setq length 7
                                         mnemonic :movq
                                         operand1 (make-immediate-operand (mref-32-signed block-start (+ offset 3)))
                                         operand2 (make-operand :kind :relative
                                                                :register (register-rm rm prefix-byte)
                                                                :data (mref-8 block-start (+ offset 2))))))))
                    ((eql mod #b00) ; relative, no displacement
                     (let ((sib-byte (mref-8 block-start (+ offset 2))))
                       (cond ((eql sib-byte #x24)
                              (setq length 7
                                    mnemonic :movq
                                    operand1 (make-immediate-operand (mref-32-signed block-start (+ offset 3)))
                                    operand2 (make-operand :kind :relative
                                                           :register (register-rm rm prefix-byte)
;;                                                            :data (mref-8 block-start (+ offset 3))
                                                           :data 0
                                                           )))
                             (t
                              (setq length 6
                                    mnemonic :movq
                                    operand1 (make-immediate-operand (mref-32-signed block-start (+ offset 2)))
                                    operand2 (make-operand :kind :relative
                                                           :register (register-rm rm prefix-byte)
;;                                                            :data (mref-8 block-start (+ offset 2))
                                                           :data 0
                                                           ))))))
                    )))
;;                 (#xc9
;;                  (setq length 1
;;                        mnemonic :leave))
                (#xcc
                 (setq length 1
                       mnemonic :int3))
                (#xd3
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql reg 7)
                          ;; D3   /7 ib      SAR r/m32,CL
                          (setq length 2
                                mnemonic :sar
                                operand1 (make-register-operand :cl)
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((eql reg 4)
                          ;; D3   /4         SHL r/m32,CL
                          (setq length 2
                                mnemonic :shl
                                operand1 (make-register-operand :cl)
                                operand2 (make-register-operand (register rm prefix-byte))))
                         (t
                          (unsupported))
                         )))
                (#xe8
                 ;; call near, displacement relative to next instruction
                 (let* ((displacement (mref-32 block-start (1+ offset)))
                        (absolute-address (ldb (byte 32 0) (+ block-start offset 5 displacement))))
                   (setq length 5
                         mnemonic :callq
                         operand1 (make-absolute-operand absolute-address)
                         annotation absolute-address)
                   (push absolute-address *labels*)))
                (#xe9
                 (let* ((displacement (mref-32-signed block-start (+ offset 1)))
                        (absolute-address (ldb (byte 32 0) (+ block-start offset 5 displacement))))
                   (setq length 5
                         mnemonic :jmpq
                         operand1 (make-absolute-operand absolute-address)
                         annotation absolute-address)
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
                                mnemonic :test
                                operand1 (make-operand :kind :relative
                                                       :register (register rm)
                                                       :data (mref-8 block-start (+ offset 2)))
                                operand2 (make-immediate-operand (mref-8 block-start (+ offset 3)))))
                         ((and (eql mod #b11)
;;                                (eql reg 0)
                               )
                          (setq length 3
                                mnemonic :test
                                operand1 (make-immediate-operand (mref-8 block-start (+ offset 2)))
                                operand2 (make-register-operand (byte-register rm))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xf7
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql reg 3)
                          (setq length 2)
                          (setq mnemonic :neg)
                          (setq operand1 (make-register-operand (register rm prefix-byte))))
                         ((and (eql mod #b11)
                               (eql reg 0))
                          (setq length 6
                                mnemonic :test
                                operand1 (make-immediate-operand (mref-32 block-start (+ offset 2)))
                                operand2 (make-register-operand (register rm prefix-byte))))
                         ((and (eql mod #b11)
                               (eql reg 2))
                          (setq length 2
                                mnemonic :not
                                operand1 (make-register-operand (register rm prefix-byte))))
                         ((and (eql mod #b11)
                               (eql reg 6))
                          (setq length 2
                                mnemonic :div
                                operand1 (make-register-operand (register rm prefix-byte))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)))))
                (#xf8
                 (setq length 1
                       mnemonic :clc))
                (#xff
                 (with-modrm-byte (mref-8 block-start (1+ offset))
                   (cond ((eql modrm-byte #xd0) ; callq *%rax
                          (setq length 2
                                mnemonic :callq
                                operand1 (make-register-operand :rax)))
                         ((eql modrm-byte #xe0) ; jmpq *%rax
                          (setq length 2
                                mnemonic :jmpq
                                operand1 (make-register-operand :rax)))
                         ((eql reg 1)
                          (setq length 2
                                mnemonic :dec
                                operand1 (make-register-operand (register rm prefix-byte))))
                         (t
                          (error "unhandled byte sequence #x~2,'0x #x~2,'0x" byte1 modrm-byte)
                          ))))
                (t
                 (error "unhandled opcode #x~2,'0x" byte1)))))
        (when (null instruction)
          (when prefix-byte
            (incf length))
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
        (when (memq byte1 '(#xc3 #xe9 #xeb))
          (return))
        )))
  nil)
