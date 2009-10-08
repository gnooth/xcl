(defvar *stak-x*)
(defvar *stak-y*)
(defvar *stak-z*)

(defun stak-aux ()
  (if (not (< (the fixnum *stak-y*) (the fixnum *stak-x*)))
      *stak-z*
      (let ((*stak-x* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-x*))))
                            (*stak-y* *stak-y*)
                            (*stak-z* *stak-z*))
                        (stak-aux)))
	    (*stak-y* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-y*))))
                            (*stak-y* *stak-z*)
                            (*stak-z* *stak-x*))
                        (stak-aux)))
	    (*stak-z* (let ((*stak-x* (the fixnum (1- (the fixnum *stak-z*))))
                            (*stak-y* *stak-x*)
                            (*stak-z* *stak-y*))
                        (stak-aux))))
	(stak-aux))))

(defun stak (*stak-x* *stak-y* *stak-z*)
  (stak-aux))

(defun run-stak ()
  (dotimes (i 200)
    (stak 18 12 6)))

(progn
;;   #+xcl
;;   (require "COMPILER")
  (compile 'stak-aux)
  (compile 'stak)
  (compile 'run-stak))
