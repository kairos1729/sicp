(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else 
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((= x1 x2) (union-set (cdr s1) s2))
                 ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
                 ((< x2 x1) (cons x2 (union-set s1 (cdr s2)))))))))