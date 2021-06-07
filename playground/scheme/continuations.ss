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

(define (print-seq l)
  (let ((k.n (call/cc (lambda (k) (cons k 0)))))
    (let ((k (car k.n)) (n (cdr k.n)))
      (cond 
        ((= n l) n)
        (#t (println n) (k (cons k (+ n 1))))))))

(define thread-list '())
(define (make-thread p)
  (set! thread-list (append thread-list (list p))))

(define (start)
  (let ((p (car thread-list)))
    (set! thread-list (cdr thread-list))
    (p)))

(define (pause)
  (call/cc
    (lambda (k)
      (make-thread (lambda () (k #t)))
      (start))))
