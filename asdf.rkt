(define (square x)
  (* x x))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n)
         (fast-expt-iter (square b) (/ n 2) a))
        (else 
         (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n)
         (square (fast-expt b (/ n 2)))))
        (else (* b (fast-expt b (- n 1)))))

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
         (let ((x (expmod a (/ n 2) m)))
           (let ((r (remainder (square x) m)))
             (if (and (= r 1) (not (= x 1)) (not (= x (- m 1))))
                 0
                 r))))
        (else (remainder (* a (expmod a (- n 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 2)))))

(define (fast-prime n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else #f)))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x)
  (fast-expt x 3))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
  
(define (simpson-integral f a b n)
  (define (y k h)
    (f (+ a (* k h))))
  (define (term k h)
    (* (y k h)
       (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))))
  (let ((h (/ (- b a) n)))
    (* (/ h 3) 
       (sum 
        (lambda (x) (term x h)) 
         0
         (lambda (x) (+ x 1))
         n))))

(define (product factor a next b)
  (if (> a b) 
      1
      (* (factor a) (product factor (next a) next b))))

(define (product-i factor a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (factor a)))))
  (iter a 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-i combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner (term a) res))))
  (iter a null-value))

(define (filter-accumulate combiner null-value term a next b pred?)
  (cond ((> a b) null-value)
        ((pred? a)
         (combiner (term a) (filter-accumulate combiner null-value term (next a) next b pred?)))
        (else 
         (filter-accumulate combiner null-value term (next a) next b pred?))))
      
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))