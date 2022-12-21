(define-syntax and
  (syntax-rules ()
    ((_) #t)
    ((_ x) x)
    ((_ x1 x2 ...) 
      (if x1 (and x2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((_) #f)
    ((_ x) x)
    ((_ x1 x2 ...)
      (let ((v x1))
        (if v v (or x2 ...))))))

(define-syntax let
  (syntax-rules ()
    ((_ ((name val) ...) body1 body2 ...)
      ((lambda (name ...) body1 body2 ...) val ...))))

(define-syntax let*
  (syntax-rules ()
    ((_ ((n1 v1)) body1 body2 ...)
      (let ((n1 v1)) body1 body2 ...))
    ((_ ((n1 v1) (n2 v2) ...) body1 body2 ...)
      (let ((n1 v1))
        (let* ((n2 v2) ...) body1 body2 ...)))))

(define-syntax when
  (syntax-rules ()
    ((_ test expr1 expr2 ...)
      (if test
        (begin expr1 expr2 ...)
        #f))))

(define-syntax unless
  (syntax-rules ()
    ((_ test expr1 expr2 ...)
      (when (not test) expr1 expr2 ...))))