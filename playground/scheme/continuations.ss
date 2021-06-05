(define (prod ls)
  (call/cc
    (lambda (break)
      (let pr ((ls ls))
        (cond
          ((null? ls) 1)
          ((zero? (car ls)) (break 0))
          (#t (* (car ls) (pr (cdr ls)))))))))

(define (prod ls break)
  (let pr ((ls ls) (k break))
    (cond
      ((null? ls) (k 1))
      ((zero? (car ls)) (break 0))
      (#t (pr (cdr ls)
              (lambda (n)
                (k (* (car ls) n))))))))