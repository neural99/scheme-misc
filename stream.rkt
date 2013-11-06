(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define the-empty-stream '())

(define (empty-stream? s)
  (null? s))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval i n)
  (if (> i n)
      the-empty-stream
      (cons-stream i (stream-enumerate-interval (+ i 1) n))))

(define (stream-map proc . argstreams)
  (if (empty-stream? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each f s)
  (if (empty-stream? s)
      'done
      (begin (f (stream-car s))
             (stream-for-each f (stream-cdr s)))))

(define (stream-filter p? s)
  (cond ((empty-stream? s)
         the-empty-stream)
        ((p? (stream-car s))
         (cons-stream (stream-car s) (stream-filter p? (stream-cdr s))))
        (else 
         (stream-filter p? (stream-cdr s)))))

(define (stream-replace p? x s)
  (stream-map (lambda (i)
                (if (p? i)
                    x
                    i))
              s))

(define (display-stream s)
  (define (display-line x)
    (display x)
    (newline))
  (stream-for-each display-line s))

(define (stream-replace-divisible n x s)
  (stream-replace (lambda (i)
                    (and (number? i) (= (remainder i n) 0)))
                  x
                  s))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (stream-for-each-n f n s)
  (if (= n 0)
      'done
      (begin
        (f (stream-car s))
        (stream-for-each-n f (- n 1) (stream-cdr s)))))

(define (display-stream-n n s)
  (define (display-line x)
    (display x)
    (newline))
  (stream-for-each-n display-line n s))

(define (integers-starting-from n)
  (cons-stream
   n
   (integers-starting-from (+ n 1))))

(define (sieve stream)
  (cons-stream 
   (stream-car stream)
   (sieve
    (stream-filter (lambda (x)
                     (not (= (remainder x (stream-car stream)) 0)))
                   (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define integers (integers-starting-from 1))

(define (stream-fill x)
  (stream-map (lambda (y) x) integers))

(define (add-streams s1 s2)
    (stream-map + s1 s2))

(define (mul-streams s1 s2)
    (stream-map * s1 s2))

(define (partial-sums s)
    (cons-stream (stream-car s)
                 (add-streams 
                  (stream-fill (stream-car s))
                  (partial-sums (stream-cdr s)))))

(define (scale-stream s n)
  (stream-map (lambda (x) (* n x)) s))

(define (stream-accumulate-n n op init s)
  (if (or (empty-stream? s) (= n 0))
      init
      (op (stream-car s) (stream-accumulate-n (- n 1) op init (stream-cdr s)))))

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else 
                  (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car 
                               (cons-stream s2car 
                                            (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))))

(define Hamming (cons-stream 1 (merge (scale-stream Hamming 5) (merge (scale-stream Hamming 3) (scale-stream Hamming 2)))))

(define (integrate-series power-series) (mul-streams (stream-map (lambda(x)(/ 1 x)) integers) power-series))

(define exp-series (cons-stream 1 (integrate-series exp-series)))

(define (neg-stream s)
  (stream-map (lambda(x) (- x)) s))

(define cosine-series
  (cons-stream 1 (neg-stream (integrate-series sinus-series))))

(define sinus-series 
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1)
      (stream-car s2))
   (add-streams
    (scale-stream (stream-cdr s2) (stream-car s1))
    (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series S)
  (define X (cons-stream 1 
                         (neg-stream
                          (mul-series (stream-cdr S) X))))
  X)

(define (invert-series S)
  (let ((a0 (stream-car S)))
    (if (= a0 0)
        (error "invert-series")
        (scale-stream
         (invert-unit-series (scale-stream S (/ 1 a0)))
         (/ 1 a0)))))

(define (div-series S1 S2)
  (mul-series S1 (invert-series S2)))

(define (sub-streams s1 s2)
    (stream-map - s1 s2))

(define (abs-stream s)
    (stream-map abs s))

(define (stream-limit S tolerance) 
    (if (< (stream-car (abs-stream (sub-streams S (stream-cdr S)))) tolerance)
        (stream-car (stream-cdr S)) ; cadr
        (stream-limit (stream-cdr S) tolerance)))

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2 (partial-sums (ln2-summands 1.0)))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define mpi (partial-sums (scale-stream (pi-summands 1.0) 4)))

(define (square x)
  (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                         (+ s0 (* -2 s1) s2)))
                (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelerate-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (stream-append s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s)(stream-car t))
   (interleave 
    (stream-map (lambda(x)(list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted 
    (stream-map (lambda(x)(list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
   weight)))

(define (triples s t u)
  (cons-stream
   (list (stream-car s)(stream-car t)(stream-car u))
   (interleave
    (stream-map (lambda(x)(append (list (stream-car s)) x)) (pairs (stream-cdr t)(stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t)(stream-cdr u)))))

(define (consecutive s w)
    (let ((a0 (stream-car s))
          (a1 (stream-car (stream-cdr s))))
      (if (= (w a0) (w a1))
          (cons-stream (list a0 a1) (consecutive (stream-cdr (stream-cdr s)) w))
          (consecutive (stream-cdr s) w))))

(define (ramanuja)
  (define (w x)
    (+ (expt (car x) 3) (expt (cadr x) 3)))
  (consecutive (weighted-pairs integers integers w) w))

(define ramanujan-numbers (stream-map (lambda (x) (append (list ((lambda (y) (+ (expt (car y) 3) (expt (cadr y) 3))) (car x))) x)) (ramanuja)))

(define (cc3 s w)
    (let ((a0 (stream-car s))
          (a1 (stream-car (stream-cdr s)))
          (a2 (stream-car (stream-cdr (stream-cdr s)))))
      (if (and (= (w a0) (w a1)) (= (w a1) (w a2)))
          (cons-stream
           (list a0 a1 a2)
           (cc3 (stream-cdr (stream-cdr (stream-cdr s))) w))
          (cc3 (stream-cdr s) w))))

(define (three-ways)
  (define (w x)
    (+ (square (car x) (square (cadr x)))))
  (stream-map (lambda (x) (append (list ((lambda (y) (+ (square (car y)) (square (cadr y)))) (car x))) x)) (cc3 (weighted-pairs integers integers w) w)))

(define (integral integrand init dt)
  (define int
    (cons-stream init
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)