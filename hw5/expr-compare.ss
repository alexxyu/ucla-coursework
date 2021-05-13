#lang racket
(provide expr-compare)

(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y))
         (if x `% `(not %))]
        [(or (not (list? x)) (not (list? y)))
         (list `if `% x y)]
        [(and (list? x) (list? y)
         (list-compare-start x y))]
  )
)

(define (list-compare-start x y)
  (cond [(not (= (length x) (length y)))
         (list `if `% x y)]
        [(or (equal? (car x) `quote) (equal? (car y) `quote))
         (list `if `% x y)]
        [(and (equal? (car x) (car y)) (equal? (car x) `if))
         (list-compare x y)]
        [(or (equal? (car x) `if) (equal? (car y) `if))
         (list `if `% x y)]
        [(and (is-lambda (car x)) (is-lambda (car y)))
         (lambda-compare x y)]
        [else (list-compare x y)]
  )
)

(define (is-lambda s)
  (or (equal? s `lambda) (equal? s #\u03BB))
)

(define (list-compare x y)
  (cond [(and (null? x) (null? y))
         `()]
        [(and (list? (car x)) (list? (car y)))
         (cons (list-compare-start (car x) (car y)) (list-compare (cdr x) (cdr y)))]
        [(not (equal? (car x) (car y)))
         (cons (list `if `% (car x) (car y)) (list-compare (cdr x) (cdr y)))]
        [(equal? (car x) (car y))
         (append (list (car x)) (list-compare (cdr x) (cdr y)))]
  )
)

(define (lambda-compare x y)
  (list (get-lambda-symbol (car x) (car y))
        (lambda-formals-compare (cadr x) (cadr y))
        (expr-compare (caddr x) (caddr y)))         ; TODO: handle variable bindings
)

(define (get-lambda-symbol a b)
  (if (equal? a b)
    a
    `λ
  )
)

(define (lambda-formals-compare x y)
  (cond [(or (null? x) (null? y)) `()]
        [(not (equal? (car x) (car y)))
         (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y)))) 
               (lambda-formals-compare (cdr x) (cdr y)))]
        [else (cons (car x) (lambda-formals-compare (cdr x) (cdr y)))]
  )
)