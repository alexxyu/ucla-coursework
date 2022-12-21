#lang racket
(provide (all-defined-out))

;; Compares two expressions
(define (expr-compare x y)
  (cond [(equal? x y) x]
        [(and (boolean? x) (boolean? y))
         (if x '% '(not %))]
        [(or (not (list? x)) (not (list? y)))
         (list 'if '% x y)]
        [#t (list-compare-start x y)]))

;; Compares the heads of lists within the expressions
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

;; Checks whether the symbol represents lambda
(define (is-lambda s)
  (or (equal? s 'lambda) (equal? s 'λ)))

;; Compares two general lists within the expressions
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

;; Compares lambda functions within the expressions
(define (lambda-compare x y bindings)
  (if (= (length (cadr x)) (length (cadr y)))
      (list (get-lambda-symbol (car x) (car y))
            (lambda-formals-compare (cadr x) (cadr y))
            (lambda-body-compare-start (caddr x) (caddr y)
                                       (get-lambda-bindings (cadr x) (cadr y) bindings)))
      (list 'if '% x y)))

;; Returns the appropriate lambda representation to use
(define (get-lambda-symbol a b)
  (if (equal? a b)
    a
    'λ))

;; Compares the formals of the lambda functions
(define (lambda-formals-compare x y)
  (cond [(or (null? x) (null? y)) '()]
        [(not (equal? (car x) (car y)))
         (cons (format-binding (car x) (car y)) 
               (lambda-formals-compare (cdr x) (cdr y)))]
        [#t (cons (car x) (lambda-formals-compare (cdr x) (cdr y)))]))

;; Compares the beginnings of the lambda bodies
(define (lambda-body-compare-start x y bindings)
  (cond [(and (boolean? x) (boolean? y))
         (if x '% '(not %))]
        [(and (not (list? x)) (not (list? y)))
         (lambda-binding-compare x y bindings)]
        [(or (not (list? x)) (not (list? y)))
         (list 'if '% x y)]
        [(not (= (length x) (length y)))
         (list 'if '% x y)]
        [(and (equal? (car x) 'quote) (equal? (car y) 'quote))
         (list-compare-start x y)]
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

;; Compares the bodies of the lambda sub-expressions
(define (lambda-body-compare x y bindings)
  (cond [(and (null? x) (null? y))
         '()]
        [(and (list? (car x)) (list? (car y)))
         (cons (lambda-body-compare-start (car x) (car y) bindings)
               (lambda-body-compare (cdr x) (cdr y) bindings))]
        [#t (cons (lambda-binding-compare (car x) (car y) bindings)
                  (lambda-body-compare (cdr x) (cdr y) bindings))]))

;; Compares two variables within a lambda body based on their bindings
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

;; Returns a formatted string representing the binding equivalency of a and b
(define (format-binding a b)
  (if (equal? a b)
      a
      (string->symbol (string-append (symbol->string a) "!" (symbol->string b)))))

;; Returns two symmetric hashmaps representing the arguments that correspond to each
;; other position-wise in the two sub-expressions
(define (get-lambda-bindings x y bindings)
  (cond [(or (null? x) (null? y)) bindings]
        [#t (get-lambda-bindings (cdr x) (cdr y)
                                 (list (hash-set (car bindings) (car x) (car y))
                                       (hash-set (cadr bindings) (car y) (car x))))]))

;; Gets the equivalent binding of id in the other expression
(define (get-binding id use-first bindings)
  (cond [use-first (hash-ref (car bindings) id #f)]
        [#t (hash-ref (cadr bindings) id #f)]))

;; Tests expr-compare by checking that x and expr-compare's output evaluate to the same value
;; with % bound to #t, and then does the same with y and expr-compare's output with % bound
;; to #f
;;
;; Credit to the TA hint code Github repo
(define (test-expr-compare x y) 
  (and 
    (equal? (eval x) (eval `(let ((% #t)) ,(expr-compare x y))))
    (equal? (eval y) (eval `(let ((% #f)) ,(expr-compare x y))))))

;; Two Scheme expressions to be used in testing expr-compare's correctness
(define test-expr-x '(lambda (a b) ((if (> a b) (list a (quote (a b))) (lambda (c) (+ c d))))))
(define test-expr-y '(λ (b a) ((if (> a b) (list b (quote (b a))) (lambda (c d) (+ c d))))))