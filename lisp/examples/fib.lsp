
(defun fib (n)
  (cond ((< n 2) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))


(defun factorial (n)
  (cond ((= n 0) 1)
        (t (* n (factorial (- n 1))))))



