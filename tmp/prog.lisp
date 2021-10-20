(letrec
    ((singleton
      (lambda (x) (cons x ())))
     (push_back_map
      (lambda (func lst2)
        (letrec
            ((helper
              (lambda (acc lst3)
                (if (null? lst3)
                    acc
                  (helper (append acc (singleton (func (car lst3)))) (cdr lst3))))))
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
  (car (map (lambda (x) (+ x 1)) (generate 60000)))
  )
