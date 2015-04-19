(define (square x)
  (* x x))

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

;(define (map p sequence)
;  (accumulate 
;   (lambda (x y) (cons (p x) y))
;   nil
;   sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1))
                   t)))

(define some-tree (list 1 2 (list 3 (list 4 5) 6) 7))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define my-vector1 (list 1 2 3 4))

(define my-vector2 (list 7 6 5 4))

(define my-matrix1 (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define my-matrix2 (list (list 1 2 3) (list 4 5 6) (list 6 7 8) (list 8 9 10)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (mi) (dot-product mi v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
        (map (lambda (mi) (matrix-*-vector cols mi)) m)))

(define (reverse-r sequence)
  (fold-right (lambda (x a) (append a (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left (lambda (a x) (cons x a)) nil sequence))
