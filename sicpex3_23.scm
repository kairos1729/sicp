; Make a doubly linked list nodes first - does this avoid cycles?
(define (make-node prev value next)
  (cons prev (cons value next)))

(define (get-prev x)
  (car x))

(define (get-next x)
  (cddr x))

(define (get-value x)
  (cadr x))

(define (set-prev! x p)
  (set-car! x p))

(define (set-next! x n)
  (set-cdr! (cdr x) n))

(define (set-value! x v)
  (set-car! (cdr x) v))


Nope.  Actually don't do any more on this.  I've understood the main issue, that's to make delete-rear O(1) you need back pointers to be stored.  We could keep a separate list of back pointers, and keep a pair pointing to the front and back of that which would avoid cycles, or we could do a procedural implementation with a doubly linked list.  This would contain cycles but it avoids the interpreter printing it by wrapping it in a procedure/lambda.  However, the question seems to imply it wanted pairs.  All the answers i've seen on the web use doubly-linked-lists and don't prevent the interpreter trying to print a cyclic list (yes, they have print functions, but just referring to the deque variable makes ther interpreter loop).

Anyway, I think I've pulled a lot of understanding from this question and actually implemeting something isn't going to teach me much more.


