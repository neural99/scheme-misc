#lang racket

(define (n->dec n bin)
  (define (inner i l res)
    (if (null? l)
        res
        (inner (+ i 1) (cdr l) (+ res (* (car l) (expt n i))))))
  (inner 0 (reverse bin) 0))

(define (dec->n n dec)
  (define (inner i res)
    (if (= i 0)
        res
        (let* ((q (quotient i n))
               (r (remainder i n)))
          (inner q (cons r res)))))
  (if (= dec 0)
      '(0)
      (inner dec '())))

(define alpha '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\_))

(define (num->char x)
  (list-ref alpha x))

(define (num->string n)
  (list->string (map num->char n)))

(define (fill-n n len)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (cons n res))))
  (iter len '()))

(define (extend list n len)
  (if (= (length list) len)
      list
      (extend (cons n list) n len)))

(define (brute len)
  (let ((alpha-len (length alpha))) 
    (define (inner n m)
      (if (= n m) 
          '()
          (begin
            (display (num->string (extend (dec->n alpha-len n) 0 len)))
            (newline)
            (inner (+ n 1) m))))
    (inner 0 (expt alpha-len len))))

(define (main len)
  (define (inner i)
    (if (> i len)
        '()
        (begin
         (brute i)
         (inner (+ 1 i)))))
  (inner 1))

(main 4)