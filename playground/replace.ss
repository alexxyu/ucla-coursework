(define (replace-in-list ls x y)
  (cond
    [(null? ls) '()]
    [(list? (car ls)) (cons (fnx-helper (car ls) x) (replace-in-list (cdr ls) x y))]
    [(equal? (car ls) x) (cons y (replace-in-list (cdr ls) x y))]
    [#t (cons (car ls) (replace-in-list (cdr ls) x y))]))

(define (replace ls x y)
  (cond
    [(list? ls) (replace-in-list ls x y)]
    [(equal? ls x) y]
    [#t ls]))

(define (fnx-helper ls to-replace)
  (cond
    [(equal? (car ls) 'lambda)
      (if (equal? (caadr ls) to-replace)
        ls
        (list 'lambda (cadr ls) (replace-in-list (caddr ls) (caadr ls) 'x)))]
    [(equal? (car ls) 'quote) ls]
    [#t (replace ls to-replace 'x)]
  )
)

(define (fnx ls)
  (list 'lambda (cadr ls) (replace-in-list (caddr ls) (caadr ls) 'x)))