(letrec
    ((singleton
      (lambda (x) (cons x ())))
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
  (car (map (lambda (x) (+ x 1)) (generate 70000)))
  )
