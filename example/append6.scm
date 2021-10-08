;; ++ は append

(let ((x '(1 2 3)))
  (let ((y (++ x '(4 5 6))))
    (begin
      (print x)
      (print y)
      )
    )
  )
