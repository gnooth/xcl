(defun fib (n)
  (cond ((= n 0)
         0)
        ((< n 2)
         1)
        (t
         (+ (fib (- n 1)) (fib (- n 2))))))
