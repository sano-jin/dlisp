;; ++ は append

(let ((x '(1 2 3)))
  (let ((y (++ x x)))
    (begin
      (print x)
      (print y)
      )
    )
  )
