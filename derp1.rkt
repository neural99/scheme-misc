
(define (fixed-point f first-guess)
  (define tolerance 0.000000001)
  (define (good-enough? g1 g2)
    (< (abs (- g1 g2)) tolerance))
  (define (try guess)
    (let ((next-guess (f guess)))
      (if (good-enough? guess next-guess)
          next-guess
          (try next-guess))))
  (try first-guess))

(define dx 0.0001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  ((lambda (cube square)
    (lambda (x)
      (+ (cube x) (* a (square x)) (* b x) c)))
   (lambda (x)
     (* x x x))
   (lambda (x)
     (* x x))))

(define (inc x)
  (+ x 1))

(define (double f)
  (lambda (x)
    (f (f x))))

(define (mcompose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (if (= n 1) 
      f
      (compose f (repeat f (- n 1)))))

(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define (average-damp f)
  (lambda (x)
    (/ (+ (f x) x) 2)))

(define (fixed-point-of-transform f transform first-guess)
  (fixed-point (transform f) first-guess))

(define (5-root x)
  (fixed-point-of-transform 
   (lambda (y) (/ x (expt y 4)))
   (repeat average-damp 2)
   1.0))

(define (nth-root n x)
    (let ((damps (floor (/ (log n) (log 2)))))
      (fixed-point-of-transform
       (lambda (y) (/ x (expt y (- n 1))))
       (repeat average-damp damps)
       1.0)))
   