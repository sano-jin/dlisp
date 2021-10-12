(letrec
    ((foldl
      (lambda (func accum lst)
        (if (null? lst)
            accum
          (foldl func (func accum (car lst)) (cdr lst)))))
     (foldr
      (lambda (func end lst1)
        (if (null? lst1)
            end
          (func (car lst1) (foldr func end (cdr lst1))))))
     (map
      (lambda (func lst2)
        (foldr (lambda (x y) (cons (func x) y)) '() lst2)))
     (foldr2
      (lambda (func end lst1 lst2)
        (if (null? lst1)
            end
          (func (car lst1) (car lst2) (foldr2 func end (cdr lst1) (cdr lst2))))))
     (map2
      (lambda (func lst1 lst2)
        (foldr2 (lambda (x y t) (cons (func x y) t)) '() lst1 lst2)))
     (append
      (lambda (lst1 lst2)
        (foldr (lambda (x y) (cons x y)) lst2 lst1)))
     )
  (let ((x '(1 2 3)))
    (let ((y (++ x '(4 5 6))))
      (let ((z (++ x '(7 8 9))))
        (let ((w (++ y '(10 11 12))))
          (let ((u (++ z '(13 14 15))))
            (begin
             (print x)
             ;; ---> (1 2 3)

             (print y)
             ;; ---> (1 2 3 4 5 6)

             (print z)
             ;; ---> (1 2 3 7 8 9)

             (print w)
             ;; ---> (1 2 3 4 5 6 10 11 12)

             (print u)
             ;; ---> (1 2 3 7 8 9 13 14 15)

             (map2 (lambda (x y) (- x y)) u w)
             ;; ---> (0 0 0 3 3 3 3 3 3)
             )
            )
          )
        )
      )
    )
  )


