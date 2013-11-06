(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (inverse x) (apply-generic 'inverse x))

(define (type-tag o)
  (car o))

(define (contents o)
  (cdr o))

(define (attach-tag tag x)
  (cons tag x))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table 
                      (cons (list key-1 
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknow operation - table"))))
    dispatch))

(define operation-table (make-table))

(define (put op type item)
  ((operation-table 'insert-proc!) op type item))

(define (get op type)
  ((operation-table 'lookup-proc) op type))

(define (type-level x)
  (get 'level (type-tag x)))

  (define (raise-all lst level)
    (cond ((null? lst) '())
          ((= (type-level (car lst)) level) 
           (cons (car lst) (raise-all (cdr lst) level)))
          (else
           (raise-all
            (cons (raise (car lst)) (cdr lst))
            level))))
  
(define (apply-generic op . args)
  (define (raise-all lst level)
    (cond ((null? lst) '())
          ((= (type-level (car lst)) level) 
           (cons (car lst) (raise-all (cdr lst) level)))
          (else
           (raise-all
            (cons (raise (car lst)) (cdr lst))
            level))))

          
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let* ((lowest-level (apply min (map type-level args)))
                 (nargs (raise-all args lowest-level))
                 (proc2 (get op (map type-tag nargs))))
              (if proc2
                  (apply proc2 (map contents nargs))
                  (if (= lowest-level 0)
                      (error "apply-generic")
                      (apply apply-generic (append (list op) (raise-all nargs (- lowest-level 1)))))))))))



(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (add x y)
    (make-normal (+ x y)))
  (define (sub x y)
    (make-normal (- x y)))
  (define (mul x y)
    (make-normal (* x y)))
  (define (div x y)
    (make-normal (/ x y)))
  (define (make-normal n)
    (tag n))
  
  (put 'add '(scheme-number scheme-number) add)
  (put 'sub '(scheme-number scheme-numberl) sub)
  (put 'mul '(scheme-number scheme-number) mul)
  (put 'div '(scheme-number scheme-number) div)
  
  (put 'msquare '(scheme-number)
       (lambda (x) (tag (square x))))
  (put 'msqrt '(scheme-number)
       (lambda (x) (tag (sqrt x))))
  (put 'msin '(scheme-number)
       (lambda (x) (tag (sin x))))
  (put 'mcos '(scheme-number)
       (lambda (x) (tag (cos x))))
  (put 'matan '(scheme-number scheme-number)
       (lambda (x y)
         (tag (atan x y))))
  (put 'mgcd '(scheme-number scheme-number)
       (lambda (n d) (tag (gcd n d))))
  
  (put '=zero? '(scheme-number) (lambda (n) (= n 0)))
  
  (put 'negate '(scheme-number) (lambda (n) (make-normal (- n))))
  
  (put 'make 'scheme-number make-normal)
  (put 'raise '(scheme-number) 
       (lambda (n)
         (make-rational n 1)))
  (put 'level 'scheme-number 2)
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (mgcd n d)
  (apply-generic 'mgcd n d))

(define (square x)
  (* x x))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  
  (put '=zero? '(rational) (lambda (r) (= (numer r) 0)))
  
  (put 'negate '(rational) (lambda (r) (tag (make-rat (- (numer r)) (denom r)))))
  
  (put 'make 'rational 
       (lambda (n d) (tag (make-rat n d))))
  (put 'level 'rational 1)
  (put 'raise '(rational) (lambda (x) (make-from-real-imag (tag x) (make-scheme-number 0))))
  
  (put 'inverse '(rational) (lambda (x)
                              (tag (make-rat (denom x) (numer x)))))
  'done)



(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  (define (add-complex z1 z2)
    (make-from-real-imag (add (mreal-part z1) (mreal-part z2))
                         (add (mimag-part z1) (mimag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (mreal-part z1) (mreal-part z2))
                         (sub (mimag-part z1) (mimag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (mmagnitude z1) (mmagnitude z2))
                       (add (mangle z1) (mangle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (mmagnitude z1) (mmagnitude z2))
                       (sub (mangle z1) (mangle z2))))
  
  (define (tag x) (attach-tag 'complex x))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  
  (put 'conjugate '(complex) (lambda (z) (tag (make-from-real-imag (mreal-part z) (sub (make-scheme-number 0) (mimag-part z))))))
  
  (put 'mreal-part '(complex) (lambda (z) (apply-generic 'mreal-part z)))
  (put 'mimag-part '(complex) (lambda (z) (apply-generic 'mimag-part z)))
  (put 'mangle '(complex) (lambda (z) (tag (apply-generic 'mangle z))))
  (put 'mmagnitude '(complex) (lambda (z) (tag (apply-generic 'mmagnitude z))))
  
  (put '=zero? '(complex) (lambda (z) (and (=zero? (mreal-part z)) (=zero? (mimag-part z)))))
  
  (put 'negate '(complex) (lambda (z) (tag (make-from-real-imag (negate (mreal-part z)) (negate (mimag-part z))))))
  
  (put 'make-from-real-imag 'complex 
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex 
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'level 'complex 0)
  'done)

(define (msquare x)
  (apply-generic 'msquare x))
(define (msqrt x)
  (apply-generic 'msqrt x))
(define (msin x)
  (apply-generic 'msin x))
(define (mcos x)
  (apply-generic 'mcos x))
(define (conjugate z)
  (apply-generic 'conjugate z))

(define (install-rectangular-package)
  (define (mreal-part z) (car z))
  (define (mimag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mmagnitude z)
    (msqrt (add (msquare (mreal-part z)) (msquare (mimag-part z)))))
  (define (mangle z)
    (atan (imag-part z) (mreal-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  (define (tag x) (attach-tag 'rectangular x))
  (put 'mreal-part '(rectangular) mreal-part)
  (put 'mimag-part '(rectangular) mimag-part)
  (put 'mmagnitude '(rectangular) mmagnitude)
  (put 'mangle '(rectangular) mangle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (mmagnitude z) (car z))
  (define (mangle z) (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (mreal-part z)
    (mul (mmagnitude z) (mcos (mangle z))))
  (define (mimag-part z)
    (mul (mmagnitude z) (msin (mangle z))))
  (define (make-from-real-imag x y)
    (cons (msqrt (add (msquare x) (msquare y)))
          (matan y x)))
  
  (define (tag x) (attach-tag 'polar x))
  
  (put 'mreal-part '(polar) mreal-part)
  (put 'mimag-part '(polar) mimag-part)
  (put 'mmagnitude '(polar) mmagnitude)
  (put 'mangle '(polar) mangle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (mreal-part z)
  (apply-generic 'mreal-part z))
(define (mimag-part z)
  (apply-generic 'mimag-part z))
(define (mmagnitude z)
  (apply-generic 'mmagnitude z))
(define (mangle z)
  (apply-generic 'mangle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (raise x)
  (apply-generic 'raise x))

(define (=zero? x)
  (apply-generic '=zero? x))

(define (negate x)
  (apply-generic 'negate x))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2) (eq? v1 v2))
  (define (variable? s) (symbol? s))
  
   (define (adjoin-term term term-list)
    (if (= 0 (coeff term))
        term-list
        (cons term term-list)))
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else 
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (+ (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2))))))))) 

  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term 
           (make-term (+ (order t1) (order t2))
                      (* (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist)(the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (/ (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result (div-terms (sub-terms L1 (mul-terms (list (make-term new-o new-c)) L2)) L2)))
                  (list
                   (adjoin-term (make-term new-o new-c)
                                (car rest-of-result))
                   (cadr rest-of-result))))))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  
  (define (negate-terms term-list)
    (map (lambda (term)
           (make-term (order term) (- (coeff term))))
         term-list))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) 
                              (term-list p2)))
        (error "add-poly")))
  
  (define (mul-poly p1 p2) 
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "mul-poly")))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) 
                              (negate-terms (term-list p2))))
        (error "sub-poly")))
  
  (define (div-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (let ((res (div-terms (term-list p1)
                                  (term-list p2))))
              (list 
               (tag (make-poly (variable p1)
                          (car res)))
               (tag (make-poly (variable p1)
                          (cadr res)))))
        (error "div-poly")))
  
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "gcd-poly")))
  
  (define (tag p) (attach-tag 'poly p))
  (put 'add '(poly poly) (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(poly poly) (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(poly poly) (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(poly poly) (lambda (p1 p2) (div-poly p1 p2)))
  (put 'mgcd '(poly poly) (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'poly
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put '=zero? '(poly) (lambda (p) (empty-termlist? (term-list p))))
  (put 'negate '(poly) (lambda (p) (tag (make-poly (variable p) (negate-terms (term-list p))))))
  'done)

(define (make-poly var terms)
  ((get 'make 'poly) var terms))
  
(define (mgcd a b)
  (apply-generic 'mgcd a b))
 
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)