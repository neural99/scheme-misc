(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (cycle? x)
  (define (inner i)
    (cond ((null? i) #f)
          ((eq? i x) #t)
          (else (inner (cdr i)))))
  (inner (cdr x)))
          

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (count-pairs x visited)
  (cond ((not (pair? x)) 0)
        ((memq x visited) 0)
        (else (+ 1
                 (count-pairs (car x) (append visited (list x)))
                 (count-pairs (cdr x) (append visited (list x)))))))
