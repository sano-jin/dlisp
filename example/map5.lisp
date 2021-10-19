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
     (singleton
      (lambda (x) (cons x ())))
     (map
      (lambda (func lst2)
        (letrec
            ((helper
              (lambda (acc lst3)
                (if (null? lst3)
                    acc
                  (helper (++ acc (singleton (func (car lst3)))) (cdr lst3))))))
          (helper () lst2))))
     (generate
       (lambda (n)
         (letrec
             ((helper
               (lambda (i)
                 (if (>= i n)
                     ()
                   (cons i (helper (+ i 1)))))                  
               ))
           (helper 0))))
     )
  (car (map (lambda (x) (+ x 1)) (generate 30000)))
  )


