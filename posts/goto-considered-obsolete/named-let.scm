(let loop ((i 0)
           (sum 0))
  (if (> i 10)
      sum
      (loop (+ i 1) ( @ sum i))))
=> (letrec (loop (lambda (i sum)
                   (if (> i 10)
                       sum
                       (loop (+ i 1) ( @ sum i)))))
     (loop 0 0))
