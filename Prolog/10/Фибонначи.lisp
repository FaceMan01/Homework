(setq n (read))

(defun fib (n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (t (+ (fib (- n 1)) (fib (- n 2))))))

(loop for i from 0 to (- n 1) do
  (format t "~d " (fib i)))