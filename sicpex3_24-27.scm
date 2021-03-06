(define (make-assoc-list same-key?)
  (let ((assoc-array '()))
    (define (assoc key)
      (define (assoc-list key records)
	(cond ((null? records) false)
	      ((same-key? key (caar records)) (car records))
	      (else (assoc-list key (cdr records)))))
      (assoc-list key assoc-array))
    (define (insert! key value)
      (let ((record (assoc key)))
	(if record
	    (set-cdr! record value)
	    (set! assoc-array (cons (cons key value) assoc-array)))))
    (define (get key)
      (let ((record (assoc key)))
	(if record (cdr record) false)))
    (define (dispatch m)
      (cond ((eq? m 'get) get)
	    ((eq? m 'insert) insert!)
	    (else (error "ASSOC-LIST unknown operation " m))))
    dispatch))

; We should use a balanced binary tree really, otherwise lookups are too slow in the 
; worst case.
; However, to get a log n balancing operation we need to do complicated stuff 
; (red-black trees, B-trees), so don't bother.
(define (make-assoc-tree same-key? less?)
  (define (entry tree) (car tree))
  (define (left-branch tree) (cadr tree))
  (define (right-branch tree) (caddr tree))
  (define (make-tree entry left right)
    (list entry left right))
  ; entry will be a pair of key and value
  (define (make-entry key value)
    (cons key value))
  (define (key-entry e) (car e))
  (define (value-entry e) (cdr e))
  (define (set-value-entry! e v) (set-cdr! e v))
  (define (key tree)
    (car (entry tree)))
  (define (value tree)
    (cdr (entry tree)))
  (define (lookup-tree given-key tree)
;    (display "lookup: ")
;    (display given-key)
;    (display tree)
;    (newline)
    (cond ((null? tree) false)
	  ((less? given-key (key tree)) 
	   (lookup-tree given-key (left-branch tree)))
	  ((less? (key tree) given-key) 
	   (lookup-tree given-key (right-branch tree)))
	  (else (entry tree))))
  (define (insert-into-tree! new-entry tree)
;    (display "insert-into-tree!: ")
;    (display tree)
;    (display " new-entry: ")
;    (display new-entry)
;    (newline)
    (cond ((null? tree) (make-tree new-entry '() '()))
	  ((less? (key-entry new-entry) (key tree)) 
	   (make-tree (entry tree) 
		      (insert-into-tree! new-entry (left-branch tree))
		      (right-branch tree)))
	  ((less? (key tree) (key-entry new-entry)) 
	   (make-tree (entry tree) 
		      (left-branch tree)
		      (insert-into-tree! new-entry (right-branch tree))))
	  (else (error "cannot insert duplicate elements:" new-entry))))
  (let ((mytree '()))
    (define (insert! key value)
;      (display "tree assoc insert: ")
;      (display key)
;      (display value)
;      (newline)
      (set! mytree (insert-into-tree! (make-entry key value) mytree))
;      (display mytree)
;      (newline))
      )
    (define (get key)
      (let ((found-entry (lookup-tree key mytree)))
	(if found-entry (value-entry found-entry) false)))
    (define (dispatch m)
      (cond ((eq? m 'get) get)
	    ((eq? m 'insert) insert!)
	    (else (error "ASSOC-TREE unknown operation " m))))
    dispatch))


(define (get-from-assoc a key) ((a 'get) key))

(define (insert-into-assoc! a key value) ((a 'insert) key value))

(define (make-value-and-assoc value assoc)
  (let ((value-assoc (cons value assoc)))
    (define (get-v)
      (car value-assoc))
    (define (get-a)
      (cdr value-assoc))
    (define (set-v! new-value)
      (set-car! value-assoc new-value))
    (define (dispatch m)
      (cond ((eq? m 'get-value) get-v)
	    ((eq? m 'get-assoc) get-a)
	    ((eq? m 'set-value) set-v!)
	    (else (error "VALUE-ASSOC unknown operation " m))))
    dispatch))

(define (get-value va) ((va 'get-value)))
(define (get-assoc va) ((va 'get-assoc)))
(define (set-value! va value) ((va 'set-value) value))

(define (make-table same-key? less?)
  (let ((table (make-assoc-tree same-key? less?)))
;  (let ((table (make-assoc-list same-key?)))
    (define (lookup keys)
      (define (lookup-iter keys-left assoc)
	(let ((key (car keys-left))
	      (next-keys (cdr keys-left)))
	  (let ((record (get-from-assoc assoc key)))
	    (cond ((not record) false)
		  ((null? next-keys) (get-value record))
		  (else (lookup-iter next-keys (get-assoc record)))))))
      (lookup-iter keys table))
    (define (insert! keys value)
      (define (insert-iter! keys-left value assoc)
	(let ((key (car keys-left))
	      (next-keys (cdr keys-left)))
	  (let ((r (get-from-assoc assoc key)))
	    (let ((record 
		   (if r 
		       r 
		       (let ((new-record 
			      (make-value-and-assoc 
			       false 
			       (make-assoc-tree same-key? less?))))
;			       (make-assoc-list same-key?))))
			 (insert-into-assoc! assoc key new-record)
			 new-record))))
	    (if (null? next-keys)
		  (set-value! record value)
		  (insert-iter! next-keys value (get-assoc record)))))))
      (insert-iter! keys value table))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert) insert!)
	    (else (error "TABLE unknown operation " m))))
    dispatch))

(define (lookup-table table keys) ((table 'lookup) keys))

(define (insert-table! table keys value) ((table 'insert) keys value))

(define (make-default-table)
  (make-table equal? <))

(define (insert-many table n)
  (define (insert-balanced lo hi)
    (let ((mid (floor (/ (+ lo hi) 2))))
;      (display "insert-balanced: ")
;      (display lo)
;      (display " ")
;      (display hi)
;      (display " ")
;      (display mid)
;      (newline)
      (cond ((< lo hi)
	     (insert-table! table (list mid) (* mid mid))
	     (cond ((= (remainder mid 100) 0) (display mid) (newline)))
	     (insert-balanced lo mid)
	     (insert-balanced (+ 1 mid) hi)))))
  (insert-balanced 0 (+ n 1)))

