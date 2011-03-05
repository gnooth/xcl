;;; stack.lisp
;;;
;;; Copyright (C) 2011 Peter Graves <gnooth@gmail.com>
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

(defstruct syminfo address size name)

(defvar *kernel-names* nil)

(defvar *lisp-names* nil)

(defvar *lisp-code-addresses*)

#-windows
(defun load-kernel-symbols ()
  (let ((symbols nil))
    (with-open-file (stream (merge-pathnames "kernel/xcl.nm" *xcl-home*))
      (loop
        (let ((line (read-line stream nil)))
          (when (null line)
            (return))
          ;;         (write-line line)
          (multiple-value-bind (address pos1)
              (parse-integer line :radix 16 :junk-allowed t)
            (when address
              (multiple-value-bind (size pos2)
                  (parse-integer line :start pos1 :radix 16 :junk-allowed t)
                (let ((name (and size (subseq line (+ pos2 3)))))
                  (when (and address size name)
                    (push (make-syminfo :address address
                                        :size size
                                        :name name)
                          symbols))))))))
      (setq *kernel-names* symbols))))

#+windows
(defun load-kernel-symbols ()
  (let ((symbols nil))
    (with-open-file (stream (merge-pathnames "kernel/xcl.nm" *xcl-home*))
      (loop
        (let ((line (read-line stream nil)))
          (when (null line)
            (return))
          ;;         (write-line line)
;;           (multiple-value-bind (address pos1)
;;               (parse-integer line :radix 16 :junk-allowed t)
;;             (when address
;;               (multiple-value-bind (size pos2)
;;                   (parse-integer line :start pos1 :radix 16 :junk-allowed t)
;;                 (let ((name (and size (subseq line (+ pos2 3)))))
;;                   (when (and address size name)
          (let* ((pos1 (position #\space line))
                 (token1 (subseq line 0 pos1))
                 (pos2 (position #\space line :start (1+ pos1)))
                 (token2 (subseq line (1+ pos1) pos2))
                 (address (parse-integer token1 :radix 16 :junk-allowed t))
                 (size (parse-integer token2 :radix 16 :junk-allowed t)))
;;             (write-line line)
;;             (format t "token1 = |~S| token2 = |~S|~%" token1 token2)
            (cond (size
                   (aver nil))
                  (t
                   (let ((name (subseq line (1+ pos2))))
                     (unless (and (>= (length name) 5)
                                  (string= (subseq name 0 5) ".text"))
                       (push (make-syminfo :address address
                                           :size size
                                           :name name)
                             symbols)))))))))
    (setq *kernel-names* (nreverse symbols))
    (let ((last-entry nil))
      (dolist (this-entry *kernel-names*)
        (when (and last-entry
                   (null (syminfo-size last-entry)))
          (setf (syminfo-size last-entry) (- (syminfo-address this-entry) (syminfo-address last-entry))))
        (setq last-entry this-entry)))
;;     (dolist (entry *kernel-names*)
;;       (format t "address = #x~X size = ~S name = |~A|~%"
;;               (syminfo-address entry)
;;               (syminfo-size entry)
;;               (syminfo-name entry)))
    ))

(defun specializer-name (specializer)
  (cond ((classp specializer)
         (class-name specializer))
        ((eq (class-of specializer) (find-class 'eql-specializer))
         (list 'EQL (eql-specializer-object specializer)))
        (t
         (aver nil))))

(defun load-lisp-names ()
  (setq *lisp-code-addresses* (make-hash-table))
  (let ((names nil))
    (flet ((process-symbol (symbol)
             (when (and (fboundp symbol)
                        (not (autoloadp symbol))
                        (not (macro-function symbol)))
               (let ((function (symbol-function symbol)))
                 (when (function-code-address function)
                   (push (make-syminfo :address (function-code-address function)
                                       :size (function-code-size function)
                                       :name symbol)
                         names)
                   (setf (gethash (function-code-address function) *lisp-code-addresses*) symbol)

                   (let ((constants (compiled-function-constants function)))
                     (when constants
;;                        (mumble "~S constants = ~S~%" symbol constants)
                       (dolist (thing constants)
                         (when (and (functionp thing) (function-code-address thing))
;;                            (when (listp (function-name thing))
;;                              (mumble "found local function ~S~%" thing)
                           (push (make-syminfo :address (function-code-address thing)
                                               :size (function-code-size thing)
                                               :name (function-name thing))
                                 names))))))
                 (when (typep function 'generic-function)
                   (let* ((dfun (funcallable-instance-function function))
                          (code-address (and dfun (function-code-address dfun))))
                     (when code-address
                       (push (make-syminfo :address code-address
                                           :size (function-code-size dfun)
                                           :name symbol)
                             names)
                       (setf (gethash (function-code-address function) *lisp-code-addresses*) symbol)))
                   (dolist (method (generic-function-methods function))
                     (let (
;;                            (name (format nil "(~S ~S ~S)"
;;                                          (class-name (class-of method))
;;                                          symbol
;;                                          (mapcar 'specializer-name (method-specializers method))))
                           )
                       (let ((method-function (method-function method)))
                         (cond ((function-code-address method-function)
                                (push (make-syminfo :address (function-code-address method-function)
                                                    :size (function-code-size method-function)
;;                                                     :name name
                                                    :name (function-name method-function)
                                                    )
                                      names)
                                (setf (gethash (function-code-address method-function) *lisp-code-addresses*) symbol)))
;;                          (method-function
;;                           (format t "method function no code address ~S~%" method-function))
                         )
                       (let ((method-fast-function (method-fast-function method)))
                         (cond ((and method-fast-function (function-code-address method-fast-function))
;;                                 (format t "method fast function ~S~%" method-fast-function)
                                (push (make-syminfo :address (function-code-address method-fast-function)
                                                    :size (function-code-size method-fast-function)
;;                                                     :name (format nil "~S [fast function]" name)
                                                    :name (function-name method-fast-function)
                                                    )
                                      names)
                                (setf (gethash (function-code-address method-fast-function) *lisp-code-addresses*) symbol))
;;                                (method-fast-function
;;                                 (format t "method fast function no code address ~S~%" method-fast-function))
                               ))
                       ))
                   )))))
        (dolist (package (list-all-packages))
          (dolist (symbol (package-external-symbols package))
            (process-symbol symbol))
          (dolist (symbol (package-internal-symbols package))
            (process-symbol symbol))))
    (setq *lisp-names* (sort names #'< :key #'syminfo-address))))

;; only needed for debugging
(defun dump-symbols (symbols)
  (dolist (info symbols)
    (format t "~X ~X ~S~%" (syminfo-address info) (syminfo-size info) (syminfo-name info))))

(defun name-from-code-address (address)
  (when (null *kernel-names*)
    (load-kernel-symbols))
  (dolist (info *kernel-names*)
;;     (when (null (syminfo-size info))
;;       (format t "~A has no size information~%" (syminfo-name info)))
    (when (and (<= (syminfo-address info) address)
               (syminfo-size info)
               (< address (+ (syminfo-address info) (syminfo-size info))))
      ;; found kernel symbol
      (let ((lisp-name (gethash (syminfo-address info) *lisp-code-addresses*)))
        (return-from name-from-code-address (or lisp-name (syminfo-name info))))))
  ;; not a kernel address
  (dolist (info *lisp-names*)
    (when (> (syminfo-address info) address)
      (return nil))
    (when (and (syminfo-size info)
               (< address (+ (syminfo-address info) (syminfo-size info))))
      (return (syminfo-name info)))))

(defun test-stack ()
  (assert (eq (name-from-code-address (+ (function-code-address #'car) 3)) 'car))
  t)

(defstruct stack-entry
  address
  contents
  name
  (args 0)
  bp
  flag
  annotation)

(defvar *stack-entry-vector* nil)

(defvar *bp* nil)

#+nil
(defun analyze-saved-stack ()
  (load-lisp-names)
  (let ((vector (make-array (length *saved-stack*)))
        (i 0))
    (dolist (entry (reverse *saved-stack*))
      (let* ((address (car entry))
             (contents (cdr entry))
             (name (name-from-code-address contents)))
        (setf (aref vector i) (make-stack-entry :address address :contents contents :name name))
        (incf i)))
    (dotimes (j (length vector))
      (let ((entry (aref vector j)))
        (when (address-is-in-saved-stack (stack-entry-contents entry))
          (setf (stack-entry-flag entry) "*"))))
    (setq *stack-entry-vector* vector))

;;   (let ((first-entry (aref *stack-entry-vector* 0))
;;         (last-entry (aref *stack-entry-vector* (1- (length *stack-entry-vector*)))))
;;     (format t "first = 0x~X last = 0x~X~%"
;;             (stack-entry-address first-entry)
;;             (stack-entry-address last-entry)))

  (let* ((first-entry (aref *stack-entry-vector* 0))
         (first-address (stack-entry-address first-entry)))
    (dolist (entry *saved-backtrace*)
      (let ((address (car entry))
            (name (car (cdr entry))))
        (when (address-is-in-saved-stack address)
          (let* ((i (/ (- address first-address) +bytes-per-word+)))
            (let ((e (aref *stack-entry-vector* i)))
;;               (mumble "entry-address = #x~X stack-entry-address = #x~X~%"
;;                       (car entry) (stack-entry-address e))
              (let ((c-f-p nil))
                (when (fboundp name)
                  (setq c-f-p (ignore-errors (compiled-function-p (coerce name 'function)))))
                (setf (stack-entry-annotation e) (list (cdr entry) c-f-p))))
            (loop
              (when (<= i 0)
                (return))
              (let ((e (aref *stack-entry-vector* i)))
                (when (eq name (stack-entry-name e))
                  (setf (stack-entry-args e) (cdr (cdr entry)))
                  (return)))
              (decf i)))))))

;;   #+x86-64
;;   (dotimes (i (length *stack-entry-vector*))
;;     (let* ((entry (aref *stack-entry-vector* i))
;;            (contents (stack-entry-contents entry)))
;;       (when (address-is-in-saved-stack contents)
;;         (setf (stack-entry-flag entry) "**")
;; ;;         (d (stack-entry-contents entry))
;;         (setf *bp* contents)
;;         (return))))

  #+x86
  (dotimes (i (length *stack-entry-vector*))
    (let* ((entry (aref *stack-entry-vector* i))
           (contents (stack-entry-contents entry)))
      (when (address-is-in-saved-stack contents)
        (let ((next-entry (if (< i (1- (length *stack-entry-vector*)))
                              (aref *stack-entry-vector* (1+ i)))))
          (when (eq (stack-entry-name next-entry) 'INVOKE-DEBUGGER)
            (setf (stack-entry-flag entry) "**")
            (setq *bp* contents)
            (return))))))
  #+x86-64
  (setq *bp* *saved-bp*)
  )

(defun print-saved-stack (&optional (limit most-positive-fixnum))
  (analyze-saved-stack)
  (let ((count 0))
;;     (dolist (entry (reverse *saved-stack*))
    (dotimes (i (length *stack-entry-vector*))
;;       (let ((name (name-from-code-address (cdr entry))))
      (let* ((entry (aref *stack-entry-vector* i))
             (address (stack-entry-address entry))
             (contents (stack-entry-contents entry))
             (name (stack-entry-name entry))
             (flag (stack-entry-flag entry))
             (args (stack-entry-args entry))
             (str (unless (eql args 0)
                    (format nil "args = ~S" args))))
        (format t
                #+x86-64
                "0x~8,'0X 0x~16,'0X ~A~A ~A~%"
                #+x86
                "0x~8,'0X 0x~8,'0X ~A~A ~A~%"
                address
                contents
                (if flag flag "")
                (if name name "")
                (if str str "")))
      (incf count)
      (when (>= count limit)
        (return)))))

(defun address-is-in-saved-stack (addr)
  (when (and *stack-entry-vector*
             (> (length *stack-entry-vector*) 0))
    (let ((first-entry (aref *stack-entry-vector* 0))
          (last-entry (aref *stack-entry-vector* (1- (length *stack-entry-vector*)))))
      (aver (<= (stack-entry-address first-entry) (stack-entry-address last-entry)))
      (<= (stack-entry-address first-entry) addr (stack-entry-address last-entry)))))

(defun next-named-entry (addr)
  (let* ((first-entry (aref *stack-entry-vector* 0))
         (first-address (stack-entry-address first-entry))
         (i (/ (- addr first-address) +bytes-per-word+)))
    (loop
      (unless (< i (length *stack-entry-vector*))
        (return))
      (let ((entry (aref *stack-entry-vector* i)))
        (when (stack-entry-name entry)
          (return entry)))
      (incf i))))

#+x86-64
(defun extract-args (bp count)
  (let* ((first-entry (aref *stack-entry-vector* 0))
         (first-address (stack-entry-address first-entry))
         (i (/ (- bp first-address) +bytes-per-word+))
         (args nil))
    (dotimes (j count)
      (decf i)
      (when (minusp i)
        (return))
      (let ((entry (aref *stack-entry-vector* i)))
        (push (lisp-object (stack-entry-contents entry)) args)))
;;     (format t "extract-args args = ~S~%" args)
    (nreverse args)))

#+x86
(defun extract-args (bp count)
;;   (format t "extract-args bp = 0x~X~%" bp)
  (when (> count 0)
    (let* ((length (length *stack-entry-vector*))
           (first-entry (aref *stack-entry-vector* 0))
           (first-address (stack-entry-address first-entry))
           (i (/ (- bp first-address) +bytes-per-word+))
           (args nil))
      (incf i) ; skip one entry
      (dotimes (j count)
        (incf i)
        (when (>= i length)
          (return))
        (let ((entry (aref *stack-entry-vector* i)))
;;           (format t "0x~X 0x~X~%" (stack-entry-address entry) (stack-entry-contents entry))
          (push (lisp-object (stack-entry-contents entry)) args)))
;;       (format t "extract-args args = ~S~%" args)
      (nreverse args))))

(defun bt2 (saved-bp)
;;   (analyze-saved-stack)
  (let* ((first-entry (aref *stack-entry-vector* 0))
         (first-address (stack-entry-address first-entry))
;;          (bp *bp*)
         (bp saved-bp)
         (result nil))
    (loop
      (unless (and bp (address-is-in-saved-stack bp))
        (return))
      (let* ((i (/ (- bp first-address) +bytes-per-word+))
             (entry (aref *stack-entry-vector* i)))
;;         (format t "address = 0x~X contents = 0x~X~%"
;;                 (stack-entry-address entry)
;;                 (stack-entry-contents entry))
        (let ((e (next-named-entry (stack-entry-address entry))))
;;           (format t "address = 0x~X name = ~A"
;;                   (stack-entry-address e)
;;                   (stack-entry-name e))
;;           (when (stack-entry-args e)
;;             (format t " args = ~S" (stack-entry-args e)))
;;           (terpri)

;;           (let ((name (stack-entry-name e))
;;                 (args (stack-entry-args e)))
;;             (if (stringp name)
;;                 (push name result)
;;                 (push (cons name args) result)))
          (setf (stack-entry-bp e) (stack-entry-contents entry))

          (let* ((name (stack-entry-name e))
                 (function (and (symbolp name)
                                (not (kernel-function-p name))
                                (fboundp name)
                                (symbol-function name))))
            (when function
              (let ((arity (function-arity function)))
                (cond ((and arity (> arity 0))
;;                        (format t "calling extract-args for ~S~%" name)
                       (setf (stack-entry-args e)
                             (extract-args (stack-entry-bp e) (function-arity function))))
                      ((eql arity 0)
                       (setf (stack-entry-args e) nil))))))

          (push e result)
          )
        (setq bp (stack-entry-contents entry))))

    (dotimes (i (length *stack-entry-vector*))
      (let ((entry (aref *stack-entry-vector* i)))
        (when (stack-entry-annotation entry)
;;           (mumble "pushing annotated entry~%")
          (push entry result))))

    (setq result (sort result '> :key 'stack-entry-address))

    (nreverse result)))

(defun print-frame (frame-number frame)
  (let ((contents (stack-entry-contents frame))
        (name (stack-entry-name frame))
        (args (stack-entry-args frame))
;;         (bp (stack-entry-bp frame))
        )
    (format t "0x~X " (stack-entry-address frame))
    (format t "~3D: " frame-number)
    (cond ((stack-entry-annotation frame)
           (format t "*** annotation: ~S" (stack-entry-annotation frame)))
          ((stringp name)
           (format t "~A at 0x~X" name contents))
          ((eql args 0)
           (format t "(~S ...) at 0x~X" name contents))
          (t
           (format t "~S at 0x~X" (list* name args) contents)))
;;     (format t " bp = 0x~X" (stack-entry-bp frame))
    (terpri)))

(defun nth-frame (n)
  (let* (;(frames (bt2))
         (frames *saved-backtrace*)
         (frame (nth n frames)))
    (let (;;(address (stack-entry-address frame))
          ;;(contents (stack-entry-contents frame))
          (name (stack-entry-name frame))
          (args (stack-entry-args frame)))
      (if (stringp name)
          name
          (list* name args)))))

(defun analyze-saved-stack (saved-stack saved-call-history)
;;   (load-lisp-names)
  (let ((vector (make-array (length saved-stack)))
        (i 0))
    (dolist (entry (reverse saved-stack))
      (let* ((address (car entry))
             (contents (cdr entry))
             (name (name-from-code-address contents)))
        (setf (aref vector i) (make-stack-entry :address address :contents contents :name name))
        (incf i)))
    (dotimes (j (length vector))
      (let ((entry (aref vector j)))
        (when (address-is-in-saved-stack (stack-entry-contents entry))
          (setf (stack-entry-flag entry) "*"))))
    (setq *stack-entry-vector* vector))

  ;;   (let ((first-entry (aref *stack-entry-vector* 0))
  ;;         (last-entry (aref *stack-entry-vector* (1- (length *stack-entry-vector*)))))
  ;;     (format t "first = 0x~X last = 0x~X~%"
  ;;             (stack-entry-address first-entry)
  ;;             (stack-entry-address last-entry)))

  (let* ((first-entry (aref *stack-entry-vector* 0))
         (first-address (stack-entry-address first-entry)))
    (dolist (entry saved-call-history)
      (let ((address (car entry))
            (name (car (cdr entry))))
        (when (address-is-in-saved-stack address)
          (let* ((i (/ (- address first-address) +bytes-per-word+)))
            (let ((e (aref *stack-entry-vector* i)))
              ;;               (mumble "entry-address = #x~X stack-entry-address = #x~X~%"
              ;;                       (car entry) (stack-entry-address e))
              (let ((c-f-p nil))
                (cond ((functionp name)
                       (setq c-f-p (compiled-function-p name)))
                      ((and (symbolp name) (fboundp name))
                       (setq c-f-p (ignore-errors (compiled-function-p (coerce name 'function))))))
                (setf (stack-entry-annotation e) (list (cdr entry) c-f-p))))
            (loop
              (when (<= i 0)
                (return))
              (let ((e (aref *stack-entry-vector* i)))
                (when (eq name (stack-entry-name e))
                  (setf (stack-entry-args e) (cdr (cdr entry)))
                  (return)))
              (decf i)))))))

  ;;   #+x86-64
  ;;   (dotimes (i (length *stack-entry-vector*))
  ;;     (let* ((entry (aref *stack-entry-vector* i))
  ;;            (contents (stack-entry-contents entry)))
  ;;       (when (address-is-in-saved-stack contents)
  ;;         (setf (stack-entry-flag entry) "**")
  ;; ;;         (d (stack-entry-contents entry))
  ;;         (setf *bp* contents)
  ;;         (return))))

  #+x86
  (dotimes (i (length *stack-entry-vector*))
    (let* ((entry (aref *stack-entry-vector* i))
           (contents (stack-entry-contents entry)))
      (when (address-is-in-saved-stack contents)
        (let ((next-entry (if (< i (1- (length *stack-entry-vector*)))
                              (aref *stack-entry-vector* (1+ i)))))
          (when (eq (stack-entry-name next-entry) 'INVOKE-DEBUGGER)
            (setf (stack-entry-flag entry) "**")
            (setq *bp* contents)
            (return))))))
  #+x86-64
  (setq *bp* *saved-bp*)
  )

(defun simplify-backtrace (backtrace)
  (declare (type list backtrace))
  (mumble "simplify-backtrace called~%")
  (let ((result nil))
    (dolist (frame backtrace)
      (let ((annotation (stack-entry-annotation frame)))
        (cond (annotation
               (let ((call (first annotation))
                     (c-f-p (second annotation)))
                 (unless c-f-p
                   (setf (stack-entry-name frame) (first call))
                   (setf (stack-entry-args frame) (rest call))
                   (setf (stack-entry-annotation frame) nil)
                   (push frame result))))
              ((stringp (stack-entry-name frame))
               )
              (t
               (push frame result)))))
    (nreverse result)))

(defun generate-backtrace ()
  (let ((saved-stack (current-stack-as-list))
        (saved-call-history (backtrace-as-list))
        (saved-bp (current-bp)))
    (load-lisp-names)
    (analyze-saved-stack saved-stack saved-call-history)
    (let ((backtrace (bt2 saved-bp)))
      (setq backtrace (simplify-backtrace backtrace))
      backtrace)))
