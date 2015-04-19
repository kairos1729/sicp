(define (square x)
  (* x x))

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list items)
  (map square items))

(define (my-for-each f items)
  (cond ((not (null? items))
         (f (car items))
         (my-for-each f (cdr items)))))