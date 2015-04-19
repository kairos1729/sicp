(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) 
          (fold-right op initial (cdr sequence)))))

(define accumulate fold-right)

(define (filter pred l)
  (cond ((null? l) nil)
        ((pred (car l)) (cons (car l) (filter pred (cdr l))))
        (else (filter pred (cdr l)))))


(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (+ from 1) to))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) 
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum?
               (unique-pairs n))))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (make-triple-sum triple)
  (list (car triple) (cadr triple) (caddr triple) (triple-sum triple)))

(define (triple-sum triple)
  (accumulate + 0 triple))

(define (triple-sum-equals? triple s)
  (= s (triple-sum triple)))

(define (triples-summing-to n s)
  (map make-triple-sum 
       (filter (lambda (triple) (triple-sum-equals? triple s))
               (unique-triples n))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (append (list new-row) rest-of-queens ))

(define (safe-diags? positions)
  (define (safe-diags-iter? dist row rest-of-rows)
    (if (null? rest-of-rows)
        true
        (let ((illegal-row1 (+ (car rest-of-rows) dist))
              (illegal-row2 (- (car rest-of-rows) dist)))
          (if (or (= illegal-row1 row) (= illegal-row2 row))
              false
              (safe-diags-iter? (+ dist 1) row (cdr rest-of-rows))))))
  (safe-diags-iter? 1 (car positions) (cdr positions)))

(define (safe-rows? positions)
  (accumulate 
   (lambda (x a) (and a (not (= (car positions) x))))
   true
   (cdr positions)))

(define (safe? k positions)
  (and (safe-diags? positions) (safe-rows? positions)))

(define (queens-bad board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))






