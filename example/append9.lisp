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
     (append
      (lambda (lst1 lst2)
        (foldr (lambda (x y) (cons x y)) lst2 lst1)))
     )
  (append '(1 2 3 4 5) '(6 7 8 9 10))
  )


