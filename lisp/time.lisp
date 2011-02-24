;;; time.lisp
;;;
;;; Copyright (C) 2007-2011 Peter Graves <gnooth@gmail.com>
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

;;; Adapted from SBCL.

(in-package "SYSTEM")

(defmacro time (form)
  `(%time (lambda () ,form)))

(defun %time (fun)
  (declare (type function fun))
  (let (old-run-utime
        new-run-utime
        old-run-stime
        new-run-stime
        old-real-time
        new-real-time
        real-time-overhead
        run-utime-overhead
        run-stime-overhead
        old-cycle-count
        new-cycle-count
        old-gc-total-bytes
        new-gc-total-bytes
        gc-total-bytes-overhead
        old-total-cons-cells
        new-total-cons-cells
        total-cons-cells-overhead)
    ;; warm up
    (multiple-value-setq
      (old-run-utime old-run-stime)
      (get-process-times))
    (setq old-real-time (get-internal-real-time))
    (setq old-gc-total-bytes (gc-total-bytes))
    (setq old-total-cons-cells (gc-total-cons-cells))
    ;; calculate overhead
    (let ((dummy (lambda () nil)))
      (multiple-value-setq
        (old-run-utime old-run-stime)
        (get-process-times))
      (setq old-real-time (get-internal-real-time))
      (setq old-gc-total-bytes (gc-total-bytes))
      (setq old-total-cons-cells (gc-total-cons-cells))
      (multiple-value-prog1 (funcall dummy))
      (setq new-gc-total-bytes (gc-total-bytes))
      (setq new-total-cons-cells (gc-total-cons-cells))
      (multiple-value-setq
        (new-run-utime new-run-stime)
        (get-process-times))
      (setq new-real-time (get-internal-real-time)))
    (setq run-utime-overhead (- new-run-utime old-run-utime))
    (setq run-stime-overhead (- new-run-stime old-run-stime))
    (setq real-time-overhead (- new-real-time old-real-time))
    (when (> old-gc-total-bytes new-gc-total-bytes)
      ;; overflow
      #+x86-64 (setq new-gc-total-bytes (+ new-gc-total-bytes #.(expt 2 64)))
      #-x86-64 (setq new-gc-total-bytes (+ new-gc-total-bytes #.(expt 2 32))))
    (setq gc-total-bytes-overhead (- new-gc-total-bytes old-gc-total-bytes))
    (setq total-cons-cells-overhead (- new-total-cons-cells old-total-cons-cells))

    ;; REVIEW fudge
    #+x86-64
    (incf gc-total-bytes-overhead 32)
    #+x86
    (incf gc-total-bytes-overhead 16)

    ;; now get the initial times
    (multiple-value-setq
      (old-run-utime old-run-stime)
      (get-process-times))
    (setq old-real-time (get-internal-real-time))
    (setq old-gc-total-bytes (gc-total-bytes))
    (%cpuid)
    (%cpuid)
    (%cpuid)
    (setq old-cycle-count (rdtsc))
    (multiple-value-prog1
      ;; execute the form and return its values
      (funcall fun)
      (%cpuid)
      (setq new-cycle-count (rdtsc))
      (setq new-gc-total-bytes (gc-total-bytes))
      (setq new-total-cons-cells (gc-total-cons-cells))
      (multiple-value-setq
        (new-run-utime new-run-stime)
        (get-process-times))
      (setq new-real-time (get-internal-real-time))
      (when (> old-gc-total-bytes new-gc-total-bytes)
        ;; overflow
        #+x86-64 (setq new-gc-total-bytes (+ new-gc-total-bytes #.(expt 2 64)))
        #-x86-64 (setq new-gc-total-bytes (+ new-gc-total-bytes #.(expt 2 32))))
      (let ((out *trace-output*))
        (fresh-line out)
        (format out "Execution took:~%")
        (format out "  ~S seconds of real time~%"
                (max (/ (- new-real-time old-real-time real-time-overhead)
                        (float internal-time-units-per-second))
                     0.0))
        (format out "  ~S seconds of user run time~%"
                (max (/ (- new-run-utime old-run-utime run-utime-overhead)
                        (float internal-time-units-per-second))
                     0.0))
        (format out "  ~S seconds of system run time~%"
                (max (/ (- new-run-stime old-run-stime run-stime-overhead)
                        (float internal-time-units-per-second))
                     0.0))
        (format out "  ~:D processor cycles~%" (- new-cycle-count old-cycle-count))
        (format out "  ~:D bytes allocated~%"
                (max (- new-gc-total-bytes old-gc-total-bytes gc-total-bytes-overhead) 0))
        (let ((cons-cells (max (- new-total-cons-cells old-total-cons-cells total-cons-cells-overhead) 0)))
          (format out "  ~:D cons cell~:P~%" cons-cells))))))
