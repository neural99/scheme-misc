(define (square x)
  (* x x))

(define (monte-carlo nr f)
    (define (iter r p)
      (cond ((= r 0) (/ p nr))
            ((f) (iter (- r 1) (+ p 1)))
            (else (iter (- r 1) p))))
    (iter nr 0))

(define (cesaro-test)
    (= (gcd (random 4294967087) (random 4294967087)) 1))

(define (estimate-pi n)
    (sqrt (/ 6 (monte-carlo n cesaro-test))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random)))))

(define (random-point x1 x2 y1 y2)
  (cons (random-in-range x1 x2)
        (random-in-range y2 y1)))

(define (make-p-tester x1 x2 y1 y2 p)
  (lambda ()
    (let ((point (random-point x1 x2 y1 y2)))
      (p (car point) (cdr point)))))

(define (unit-circle x y)
  (<= (+ (square x) (square y)) 1.0))

(define (estimate-integral x1 x2 y1 y2 p n)
  (monte-carlo n (make-p-tester x1 x2 y1 y2 p)))