(setq n (read))

(loop for i from 1 to n do
  (cond
    ((or (= i 1) (= i n))
     (loop for j from 1 to n do
       (format t "*"))
     (format t "~%"))
    (t
     (format t "*")
     (loop for j from 2 to (- n 1) do
       (format t " "))
     (format t "*~%"))))