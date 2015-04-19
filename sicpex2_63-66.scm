(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define fig216a 
  (make-tree 7 
             (make-tree 3 
                        (make-tree 1 '() '()) 
                        (make-tree 5 '() '())) 
             (make-tree 9 
                        '() 
                        (make-tree 11 '() '()))))

(define fig216b
  (make-tree 3 
             (make-tree 1 '() '()) 
             (make-tree 7 
                        (make-tree 5 '() '()) 
                        (make-tree 9 
                                   '() 
                                   (make-tree 11 '() '())))))

(define fig216c 
  (make-tree 5 
             (make-tree 3 
                        (make-tree 1 '() '()) 
                        '()) 
             (make-tree 9 
                        (make-tree 7 '() '()) 
                        (make-tree 11 '() '()))))

(define (union-ordered-list s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else 
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((= x1 x2) (union-ordered-list (cdr s1) s2))
                 ((< x1 x2) (cons x1 (union-ordered-list (cdr s1) s2)))
                 ((< x2 x1) (cons x2 (union-ordered-list s1 (cdr s2)))))))))

(define (intersection-ordered-list s1 s2)
  (if (or (null? s1) (null? s2))
      '()
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((= x1 x2) 
               (cons x1
                     (intersection-ordered-list (cdr s1)
                                                (cdr s2))))
              ((< x1 x2)
               (intersection-ordered-list (cdr s1) s2))
              ((< x2 x1)
               (intersection-ordered-list s1 (cdr s2)))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (balanced-set-binary-op ordered-list-op)
  (lambda (s1 s2) 
    (list->tree 
     (ordered-list-op 
      (tree->list-2 s1)
      (tree->list-2 s2)))))

(define (union-set s1 s2)
  ((balanced-set-binary-op union-ordered-list) s1 s2))

(define (intersection-set s1 s2)
  ((balanced-set-binary-op intersection-ordered-list) s1 s2))

(define (key tree)
  (entry tree))

(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key set-of-records)) (entry set-of-records))
        ((< given-key (key set-of-records)) 
         (lookup-tree given-key (left-branch set-of-records)))
        ((> given-key (key set-of-records)) 
         (lookup-tree given-key (right-branch set-of-records)))))
