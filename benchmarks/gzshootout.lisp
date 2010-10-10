;;;; gzshootout.lisp

;; Xach http://paste.lisp.org/display/115105#1

;(in-package #:gzshootout)

;;; "gzshootout" goes here. Hacks and glory await!

(defun time-fun (fun)
  (let ((start (get-internal-real-time)))
    (funcall fun)
    (/ (- (get-internal-real-time) start)
       internal-time-units-per-second)))

(defun time-gunzip (file gunzipper)
  (let ((sum 0))
    (dotimes (i 5 (/ sum 5))
      (incf sum
            (time-fun (lambda ()
                        (funcall gunzipper file "/tmp/gzshootout.dat")
                        (delete-file "/tmp/gzshootout.dat")))))))

(defun chipz-gunzipper (input output)
  (with-open-file (instream input :element-type '(unsigned-byte 8))
    (with-open-file (outstream output
                               :element-type '(unsigned-byte 8)
                               :direction :output
                               :if-exists :rename-and-delete)
      (chipz:decompress outstream 'chipz:gzip instream)))
  (probe-file output))

(defun deflate-gunzipper (input-file output-file)
  (with-open-file (input input-file
                         :element-type '(unsigned-byte 8))
    (with-open-file (output output-file
                            :direction :output
                            :if-exists :supersede
                            :element-type '(unsigned-byte 8))
      (deflate:inflate-gzip-stream input output)))
  (probe-file output-file))

(defun file-byte-count (file)
  (with-open-file (stream file)
    (file-length stream )))

(defun cl-user::shootout ()
  (with-open-file (stream "/tmp/shootout.txt"
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)
    (let ((*standard-output* (make-broadcast-stream *standard-output*
                                                    stream)))
      (format t "~&~%;; Implementation: ~A ~A ~A~%"
              (lisp-implementation-type)
              (lisp-implementation-version)
              (machine-type))
      (format t "; file size Deflate chipz~%")
      (dolist (file (sort (directory #p "/tmp/gzshootout/*.gz")
                          #'<
                          :key #'file-byte-count))
        (let ((dtime (time-gunzip file 'deflate-gunzipper))
              (ctime (time-gunzip file 'chipz-gunzipper)))
          (format t "~A ~A ~F ~F~%"
                  (file-namestring file)
                  (file-byte-count file )
                  (float dtime 1.0d0)
                  (float ctime 1.0d0)))))))
