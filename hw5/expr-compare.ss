#lang racket
(provide expr-compare)

(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y))
         (if x '% '(not %))]
        [(or (not (list? x)) (not (list? y)))
         (list 'if '% x y)]
        [(and (list? x) (list? y)
         (list-compare-start x y))]))

(define (list-compare-start x y)
  (cond [(not (= (length x) (length y)))
         (list 'if '% x y)]
        [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
         (list 'if '% x y)]
        [(and (equal? (car x) (car y)) (equal? (car x) 'if))
         (list-compare x y)]
        [(or (equal? (car x) 'if) (equal? (car y) 'if))
         (list 'if '% x y)]
        [(and (is-lambda (car x)) (is-lambda (car y)))
         (lambda-compare x y (list (hash) (hash)))]
        [(or (is-lambda (car x)) (is-lambda (car y)))
         (list 'if '% x y)]
        [#t (list-compare x y)]))

(define (is-lambda s)
  (or (equal? s 'lambda) (equal? s 'λ)))

(define (list-compare x y)
  (cond [(and (null? x) (null? y))
         '()]
        [(and (boolean? (car x)) (boolean? (car y)))
         (cons (if (car x) '% '(not %))
               (list-compare (cdr x) (cdr y)))]
        [(and (list? (car x)) (list? (car y)))
         (cons (list-compare-start (car x) (car y))
               (list-compare (cdr x) (cdr y)))]
        [(not (equal? (car x) (car y)))
         (cons (list 'if '% (car x) (car y))
               (list-compare (cdr x) (cdr y)))]
        [#t (append (list (car x)) (list-compare (cdr x) (cdr y)))]))

(define (lambda-compare x y bindings)
  (if (= (length (cadr x)) (length (cadr y)))
      (list (get-lambda-symbol (car x) (car y))
            (lambda-formals-compare (cadr x) (cadr y))
            (lambda-body-compare-start (caddr x) (caddr y)
                                       (get-lambda-bindings (cadr x) (cadr y) bindings)))
      (list 'if '% x y)))

(define (get-lambda-symbol a b)
  (if (equal? a b)
    a
    'λ))

(define (lambda-formals-compare x y)
  (cond [(or (null? x) (null? y)) '()]
        [(not (equal? (car x) (car y)))
         (cons (format-binding (car x) (car y)) 
               (lambda-formals-compare (cdr x) (cdr y)))]
        [#t (cons (car x) (lambda-formals-compare (cdr x) (cdr y)))]))

(define (lambda-body-compare-start x y bindings)
  (cond [(and (boolean? x) (boolean? y))
         (if x '% '(not %))]
        [(and (not (list? x)) (not (list? y)))
         (lambda-binding-compare x y bindings)]
        [(or (not (list? x)) (not (list? y)))
         (list 'if '% x y)]
        [(not (= (length x) (length y)))
         (list 'if '% x y)]
        [(or (equal? (car x) 'quote) (equal? (car y) 'quote))
         (list 'if '% x y)]
        [(and (equal? (car x) (car y)) (equal? (car x) 'if))
         (lambda-body-compare x y bindings)]
        [(or (equal? (car x) 'if) (equal? (car y) 'if))
         (list 'if '% x y)]
        [(and (is-lambda (car x)) (is-lambda (car y)))
         (lambda-compare x y bindings)]
        [(or (is-lambda (car x)) (is-lambda (car y)))
         (list 'if '% x y)]
        [#t (lambda-body-compare x y bindings)]))

(define (lambda-body-compare x y bindings)
  (cond [(and (null? x) (null? y))
         '()]
        [(and (list? (car x)) (list? (car y)))
         (cons (lambda-body-compare-start (car x) (car y) bindings)
               (lambda-body-compare (cdr x) (cdr y) bindings))]
        [#t (cons (lambda-binding-compare (car x) (car y) bindings)
                  (lambda-body-compare (cdr x) (cdr y) bindings))]))

(define (lambda-binding-compare a b bindings)
  (let ([a-bind (get-binding a #t bindings)]
        [b-bind (get-binding b #f bindings)])
    (cond [(and (not a-bind) (not b-bind))
           (if (equal? a b)
               a
               (list 'if '% a b))]
          [(not a-bind)
           (list 'if '% a (format-binding b-bind b))]
          [(not b-bind)
           (list 'if '% (format-binding a a-bind) b)]
          [(equal? a-bind b-bind)
           (if (and (equal? a-bind a) (equal? b-bind b))
               a-bind
               (list 'if '% (format-binding a a-bind) (format-binding b-bind b)))]
          [(and (equal? a b-bind) (equal? b a-bind))
           (format-binding a a-bind)]
          [#t (list 'if '% (format-binding a a-bind) (format-binding b-bind b))])))

(define (format-binding a b)
  (if (equal? a b)
      a
      (string->symbol (string-append (symbol->string a) "!" (symbol->string b)))))

(define (get-lambda-bindings x y bindings)
  (cond [(or (null? x) (null? y)) bindings]
        [#t (get-lambda-bindings (cdr x) (cdr y)
                                 (list (hash-set (car bindings) (car x) (car y))
                                       (hash-set (cadr bindings) (car y) (car x))))]))

(define (get-binding id use-first bindings)
  (cond [use-first (hash-ref (car bindings) id #f)]
        [#t (hash-ref (cadr bindings) id #f)]))

(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '([% #t]) (expr-compare x y))))
       (equal? (eval y) (eval (list 'let '([% #f]) (expr-compare x y))))))