;;; top-level.lisp
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

(defvar *inspect-break* nil)

(defvar *inspected-object* nil)

(defvar *inspected-object-stack* nil)

(export '(*inspect-break* *inspected-object* *inspected-object-stack* istep))

(in-package "TOP-LEVEL")

(defvar *null-command* (gensym))

(defvar *history-list* nil)

(defvar *command-char* #\:)

(defvar *command-number* 1)

(defun prompt-package-name ()
  (let ((result (package-name *package*)))
    (dolist (nickname (package-nicknames *package*))
      (when (< (length nickname) (length result))
        (setf result nickname)))
    result))

(defun peek-char-non-whitespace (stream)
  (loop
    (let ((c (read-char stream nil)))
      (when (null c) ; control d
        (quit))
      (unless (eql c #\space)
        (unread-char c stream)
        (return c)))))

(defun read-command (stream)
  (let ((c (peek-char-non-whitespace stream)))
    (cond ((eql c #\newline)
           (read-line stream)
           *null-command*)
          ((eql c *command-char*)
           (let ((string (read-line stream)))
             (if (and (>= (length string) 2)
                      (eql (char string 0) *command-char*)
                      (digit-char-p (char string 1)))
                 (let* ((n (ignore-errors (parse-integer (subseq string 1))))
                        (entry (assoc n *history-list*)))
                   (cond (entry
                          (format t "; ~S~%" (cdr entry))
                          (cdr entry))
                         (t
                          (format t "Not found.")
                          *null-command*)))
                 string)))
          (t
           (read stream nil)))))

(defun read-form ()
  (loop
    (fresh-line)
    (when (> *debug-level* 0)
      (format t "[~D~A] "
              *debug-level*
              (if *inspect-break* "i" "")))
    (format t "~A(~D): " (prompt-package-name) *command-number*)
    (let ((form (read-command *standard-input*)))
      (setf (charpos *standard-output*) 0)
      (unless (eq form *null-command*)
        (push (cons *command-number* form) *history-list*)
        (incf *command-number*))
      (cond ((process-command form)
             ; Nothing to do.
             )
            ((and (> *debug-level* 0)
                  (fixnump form))
             (let ((n form)
                   (restarts (compute-restarts)))
               (cond ((< -1 n (length restarts))
                      (invoke-restart-interactively (nth n restarts)))
                     (t
                      (return form)))))
            (t
             (return form))))))

(defun apropos-command (arg)
  (when arg (apropos arg)))

(defun backtrace-command (arg)
  (let ((limit (or (and arg (ignore-errors (parse-integer arg)))
                   8))
        (len (length *saved-backtrace*))
        (n 0)
        (*print-pretty* t)
        (*print-array* t)
        (*print-structure* nil)
        (*enable-autocompile* nil))
    (dolist (frame *saved-backtrace*)
      (when (>= n limit)
        (when (< n len)
          (format t "; ~D frames~%" len)
          (return)))
      (ignore-errors (format t "~3D: ~S~%" n frame))
      (incf n)))
  (values))

(defun error-command (ignored)
  (declare (ignore ignored))
  (when *debug-condition*
    (let* ((s (format nil "~A" *debug-condition*))
           (len (length s)))
      (when (plusp len)
        (setf (schar s 0) (char-upcase (schar s 0)))
        (unless (eql (schar s (1- len)) #\.)
          (setq s (concatenate 'string s "."))))
      (format *debug-io* "~A~%" s))
    (show-restarts (compute-restarts) *debug-io*)))

(defun frame-command (arg)
  (let* ((n (or (and arg (ignore-errors (parse-integer arg)))
                0))
         (frame (nth n *saved-backtrace*)))
    (when frame
      (format t "~S~%" frame)
      (setq *** **)
      (setq **  *)
      (setq *   frame)))
  (values))

(defun describe-command (arg)
  (let ((obj (eval (read-from-string arg))))
    (describe obj)))

(defun inspect-command (arg)
  (let ((obj (eval (read-from-string arg))))
    (inspect obj)))

(defun istep-command (arg)
  (when (fboundp 'istep)
    (istep arg)))

(defun exit-command (ignored)
  (declare (ignore ignored))
  (exit))

(defvar *old-pwd* nil)

(defun cd-command (args)
  (cond ((null args)
         #+windows
         (setq args "C:\\")
         #-windows
         (setq args (namestring (user-homedir-pathname))))
        ((string= args "-")
         (if *old-pwd*
             (setf args (namestring *old-pwd*))
             (progn
               (format t "No previous directory.")
               (return-from cd-command))))
        ((and (> (length args) 1) (string= (subseq args 0 2) "~/")
              (setf args (concatenate 'string
                                      (namestring (user-homedir-pathname))
                                      (subseq args 2))))))
  (let ((dir (probe-directory args)))
    (cond (dir
           (unless (equal dir *default-pathname-defaults*)
             (setq *old-pwd* *default-pathname-defaults*)
             (setq *default-pathname-defaults* dir))
           (format t "~A" (namestring *default-pathname-defaults*)))
          (t
           (format t "Error: no such directory (~S).~%" args)))))

(defun tokenize (string)
  (do* ((res nil)
        (string (string-left-trim " " string)
                (string-left-trim " " (subseq string end)))
        (end (position #\space string) (position #\space string)))
       ((zerop (length string)) (nreverse res))
    (unless end
      (setq end (length string)))
    (push (subseq string 0 end) res)))

(defvar *last-files-loaded* nil)

(defun ld-command (args)
  (let ((files (if args (tokenize args) *last-files-loaded*)))
    (cond ((null files)
           (format t "No file specified.~%"))
          (t
           (setq *last-files-loaded* files)
           (dolist (file files)
             (setq file (merge-pathnames file))
             (let ((loaded-file nil)
                   (result nil))
               (cond ((setq loaded-file (probe-file file))
                      (setq result (load loaded-file)))
                     ((null (pathname-type file))
                      (let ((file.lisp (merge-pathnames (make-pathname :type "lisp"
                                                                       :defaults file))))
                        (when (setq loaded-file (probe-file file.lisp))
                          (setq result (load loaded-file))))))
               (cond (result
                      (format t "; Loaded ~A~%" (namestring (truename loaded-file))))
                     ((null loaded-file)
                      (format t "File not found.~%")))))))))

(defun cf-command (args)
;;   (let ((files (tokenize args)))
;;     (dolist (file files)
;;       (compile-file file))))
  (let ((results (multiple-value-list (compile-file args))))
    (fresh-line)
    (dolist (result results)
      (prin1 result)
      (terpri))))

(defvar *last-files-cloaded* nil)

(defun cl-command (args)
  (let ((files (if args (tokenize args) *last-files-cloaded*)))
    (setq *last-files-cloaded* files)
    (dolist (file files)
      (load (compile-file file)))))

(defun find-unique-name (arg)
  (setq arg (string-upcase arg)) ; FIXME readtable-case
  (when (position #\: arg)
    (return-from find-unique-name (values (ignore-errors (read-from-string arg)))))
  (let ((name (find-symbol arg))) ; look in current package first
    (when (or (null name)
              (not (fboundp name)))
      (let* ((symbols (remove-if-not #'fboundp (find-all-symbols arg)))
             (n (length symbols)))
        (case n
          (0)
          (1
           (setq name (car symbols)))
          (t
           (dolist (symbol symbols)
             (let ((*package* +keyword-package+))
               (format t "~S~%" symbol)))))))
    name))

(defun disassemble-command (arg)
  (let ((name (find-unique-name arg)))
    (when (and name (symbolp name) (fboundp name))
      (disassemble name)
      (return-from disassemble-command)))
  (let* ((form (values (ignore-errors (read-from-string arg))))
         (thing (values (ignore-errors (eval form)))))
    (when (and thing
               (or (functionp thing)
                   (and (symbolp thing) (fboundp thing))))
      (disassemble thing))))

(defun edit-command (arg)
  (let ((name (find-unique-name arg)))
    (when name
      (ed name))))

(defvar *old-package* nil)

(defun package-command (args)
  (cond ((null args)
         (format t "The current package is ~A.~%" (package-name *package*)))
        ((and *old-package* (string= args "-") (null (find-package "-")))
         (rotatef *old-package* *package*))
        (t
         (when (and (plusp (length args)) (eql (char args 0) #\:))
           (setf args (subseq args 1)))
         (setf args (string-upcase args))
         (let ((pkg (find-package args)))
           (if pkg
               (setf *old-package* *package*
                     *package* pkg)
               (format *standard-output* "Unknown package ~A.~%" args))))))

(defun pwd-command (ignored)
  (declare (ignore ignored))
  (format t "~A~%" (namestring *default-pathname-defaults*)))

(defun reset-command (ignored)
  (declare (ignore ignored))
  (reset))

(defconstant spaces (make-string 32 :initial-element #\space))

(defun pad (string width)
  (if (< (length string) width)
      (concatenate 'string string (subseq spaces 0 (- width (length string))))
      string))

(defun %help-command (prefix)
  (let ((prefix-len (length prefix)))
    (when (and (> prefix-len 0)
               (eql (schar prefix 0) *command-char*))
      (setf prefix (subseq prefix 1))
      (decf prefix-len))
    (format t "~%  COMMAND     ABBR DESCRIPTION~%~%")
    (dolist (entry *command-table*)
      (when (or (null prefix)
                (and (<= prefix-len (length (entry-name entry)))
                     (string-equal prefix (subseq (entry-name entry) 0 prefix-len))))
        (format t "  ~A~A~A~%"
                (pad (entry-name entry) 12)
                (pad (entry-abbreviation entry) 5)
                (entry-help entry))))
    (format t "~%Commands must be prefixed by the command character, which is '~A'~A.~%~%"
            *command-char* (if (eql *command-char* #\:) " by default" ""))))

(defun help-command (&optional ignored)
  (declare (ignore ignored))
  (%help-command nil))

(defun history-command (&optional ignored)
  (declare (ignore ignored))
  (dolist (item (reverse *history-list*))
    (format t "~3D   ~A~%" (car item) (cdr item))))

(defparameter *command-table*
  '(("apropos" "ap" apropos-command "apropos")
    ("bt" nil backtrace-command "backtrace n stack frames (default 8)")
    ("cd" nil cd-command "change default directory")
    ("cf" nil cf-command "compile file")
    ("cl" nil cl-command "compile and load file")
;;     ("continue" "cont" continue-command "invoke restart n")
    ("describe" "de" describe-command "describe an object")
    ("disassemble" "dis" disassemble-command "disassemble a function")
    ("edit" "ed" edit-command "edit a definition")
    ("error" "err" error-command "print the current error message")
    ("exit" "ex" exit-command "exit lisp")
    ("frame" "fr" frame-command "set the value of cl:* to be frame n (default 0)")
    ("help" "he" help-command "show this help")
    ("history" "hi" history-command "show input history")
    ("inspect" "in" inspect-command "inspect an object")
    ("istep" "i" istep-command "navigate within inspection of an object")
    ("ld" nil ld-command "load file")
;;     ("ls" nil ls-command "list directory")
;;     ("macroexpand" "ma" macroexpand-command "macroexpand an expression")
    ("package" "pa" package-command "change *PACKAGE*")
    ("pwd" "pw" pwd-command "print current directory")
    ("reset" "res" reset-command "return to top level")
;;     ("rq" nil rq-command "require a module")
;;     ("trace" "tr" trace-command "trace function(s)")
;;     ("untrace" "untr" untrace-command "untrace function(s)")
    ))

(defun entry-name (entry)
  (first entry))

(defun entry-abbreviation (entry)
  (second entry))

(defun entry-command (entry)
  (third entry))

(defun entry-help (entry)
  (fourth entry))

(defun find-command (string)
  (let ((len (length string)))
    (when (and (> len 0)
               (eql (schar string 0) *command-char*))
      (setf string (subseq string 1)
            len (1- len)))
    (dolist (entry *command-table*)
      (when (or (string-equal string (entry-abbreviation entry))
                (and (<= 2 len (length (entry-name entry)))
                     (string-equal string (subseq (entry-name entry) 0 len))))
        (return (entry-command entry))))))

(defun process-command (form)
  (when (eq form *null-command*)
    (return-from process-command t))
  (when (and (stringp form)
             (> (length form) 1)
             (eql (char form 0) *command-char*))
    (let* ((pos (or (position #\space form)
                    (position #\return form)))
           (command-string (subseq form 0 pos))
           (args (if pos (subseq form (1+ pos)) nil)))
      (let ((command (find-command command-string)))
        (cond ((null command)
               (format t "Unknown command ~A.~%" (string-upcase command-string))
               (format t "Type \"~Ahelp\" for a list of available commands.~%" *command-char*))
              (t
               (when args
                 (setf args (string-trim (list #\space #\return) args))
                 (when (zerop (length args))
                   (setf args nil)))
               (funcall command args)))))
    t))

(defun repl ()
  (loop
    (let* ((form (read-form))
           (results (multiple-value-list (interactive-eval form)))
           (*print-length* 10))
      (fresh-line)
      (dolist (result results)
        (prin1 result)
        (terpri)))))

(defvar *top-level-initialized-p* nil)

(defun top-level-loop ()
  (unless *top-level-initialized-p*
    (format t "Type \"~Ahelp\" for a list of available commands.~%" *command-char*)
    (setq *top-level-initialized-p* t))
  (loop
    (with-simple-restart (top-level "Return to top level.")
      (repl))))

(setq *top-level-read-eval-print-loop* #'top-level-loop)
