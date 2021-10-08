;; ++ は append

(let ((x '(1 2 3)))
  (let ((y (++ x '(4 5 6))))
    (let ((z (++ x '(7 8 9))))
      (let ((w (++ y '(10 11 12))))
        (begin
          (print x)
          (print y)
          (print z)
          (print w)
          )
        )
      )
    )
  )
