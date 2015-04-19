(define (last-pair l)
  (if (or (null? l) (null? (cdr l)))
      l
      (last-pair (cdr l))))

(define (reverse l)
  (define (iter dest src)
    (if (null? src)
        dest
        (iter (cons (car src) dest) (cdr src))))
  (iter nil l))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 25 50 5 1 10))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (blah x . y)
  (display x)
  (newline)
  (display y)
  (newline)
  (display (cdr y))
  (newline)
  (display (remainder (car y) 2)))

(define (same-parity x . y)
  (define (same-parity-iter listy)
    (if (null? listy)
        nil
        (if (= (remainder x 2) (remainder (car listy) 2))
          (cons (car listy) (same-parity-iter (cdr listy)))
          (same-parity-iter (cdr listy)))))
  (cons x (same-parity-iter y)))





