(define (reverse-iter l)
  (define (iter dest src)
    (if (null? src)
        dest
        (iter (cons (car src) dest) (cdr src))))
  (iter nil l))

(define (deep-reverse-iter l)
  (define (iter dest src)
    (cond ((null? src) dest)
          ((pair? (car src))
           (iter (cons (deep-reverse (car src)) dest) (cdr src)))
          (else 
           (iter (cons (car src) dest) (cdr src)))))
  (iter nil l))

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
  (cond ((null? l) nil)
         ((not (pair? (car l))) 
          (append (deep-reverse (cdr l)) (list (car l))))
         (else 
          (append (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (fringe (car tree)) 
               (fringe (cdr tree))))))

;(define (make-mobile left right)
;  (list left right))
;
;(define (make-branch length structure)
;  (list length structure))
;
;(define (left-branch mobile)
;  (car mobile))
;
;(define (right-branch mobile)
;  (cadr mobile))
;
;(define (branch-length branch)
;  (car branch))
;
;(define (branch-structure branch)
;  (cadr branch))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (is-weight? structure)
  (not (pair? structure)))

(define (total-weight structure)
  (if (is-weight? structure) 
      structure
      (+ 
       (total-weight (branch-structure (left-branch structure))) 
       (total-weight (branch-structure (right-branch structure))))))

(define my-mobile 
  (make-mobile 
   (make-branch 2 (make-mobile
                   (make-branch 3 3)
                   (make-branch 4 2.25))) 
   (make-branch 1 10.5)))

(define (branch-torque branch)
  (* (branch-length branch) 
     (total-weight (branch-structure branch))))

(define (structure-balanced? structure)
  (=
   (branch-torque (left-branch structure))
   (branch-torque (right-branch structure))))

(define (balanced? structure)
  (if (is-weight? structure)
      true
      (and
       (structure-balanced? structure)
       (balanced? (branch-structure (left-branch structure)))
       (balanced? (branch-structure (right-branch structure))))))




