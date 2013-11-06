(define (avl-node data left right)
  (list data left right))

(define (avl-node-data a)
  (car a))

(define (avl-node-left a)
  (list-ref a 1))

(define (avl-node-right a)
  (list-ref a 2))

(define (_height root)
  (if (null? root)
      0
      (cond ((and (null? (avl-node-left root)) (null? (avl-node-right root)))
             1)
            ((null? (avl-node-left root))
             (+ 1 (_height (avl-node-right root))))
            ((null? (avl-node-right root))
             (+ 1 (_height (avl-node-left root))))
            (else 
             (+ 1 (max (_height (avl-node-left root)) (_height (avl-node-right root))))))))
  
(define (bfs f tree)
  (define (inner f tree queue)
    (if (not (null? queue))
        (let ((node (car queue)))
          (f node)
          (inner
           f
           tree
           (append (cdr queue)
                   (if (null?(avl-node-left node))
                       '()
                       (list (avl-node-left node)))
                   (if (null?(avl-node-right node))
                       '()
                       (list (avl-node-right node))))))))
  (inner f tree (list tree)))

(define (print-tree tree) (bfs (lambda(x) (display (avl-node-data x))) tree))
             
(define (rotate-right root pivot)
  (list 
   (avl-node-data pivot)
   (avl-node-left pivot) 
   (list (avl-node-data root) 
         (avl-node-right pivot)
         (avl-node-right root))))

(define (rotate-left root pivot)
  (list 
   (avl-node-data pivot)
   (list (avl-node-data root)
         (avl-node-left root)
         (avl-node-left pivot))
   (avl-node-right pivot)))

(define tree '(A (B (1 () ()) (2 () ())) (3 () ())))
      
(define (left-left tree)
  (rotate-right tree (avl-node-left tree)))

(define (right-right tree)
  (rotate-left tree (avl-node-right tree)))

(define (left-right tree)
  (rotate-right tree (rotate-left (avl-node-left tree) (avl-node-right (avl-node-left tree)))))

(define (right-left tree)
  (rotate-left tree (rotate-right (avl-node-right tree) (avl-node-left (avl-node-right tree)))))

(define test1 '(5 (3 (2 (D () ()) (C () ())) (B () ())) (A () ())))
(define test2 '(3 (A () ()) (5 (B () ()) (7 (C () ()) (D () ())))))
(define test3 '(5 (3 (B () ()) (4 (C () ()) (D () ()))) (A () ())))
(define test4 '(3 (A () ()) (5 (4 (D () ()) (C () ())) (B () ()))))


(define (balance-factor tree)
  (- (_height (avl-node-left tree)) (_height (avl-node-right tree))))

(define (balance tree)
  (let ((bf (balance-factor tree)))
;   (display bf)
    (cond ((and (= bf 2) (>= (balance-factor (avl-node-left tree)) 0))
           (begin 
           (left-left tree)))
          ((= bf 2)
           (begin 
           (left-right tree)))
          ((and (= bf -2) (<= (balance-factor (avl-node-right tree)) 0))
           (begin 
           (right-right tree)))
          ((= bf -2)
           (begin
           (right-left tree)))
          (else
           tree))))
          
(define asdf '(3 (2 () ()) (4 () (5 () ()))))

(define (insert elm tree)
  (if (null? tree)
      (avl-node elm '() '())
  (cond ((< (avl-node-data tree) elm)
         (balance (avl-node (avl-node-data tree) (avl-node-left tree) (insert elm (avl-node-right tree)))))
        ((> (avl-node-data tree) elm)
         (balance (avl-node (avl-node-data tree (insert elm (avl-node-left tree)) (avl-node-right tree)))))
         (else
          tree))))