(require racket)
(require srfi/1)

(define (fill-n n len)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (cons n res))))
  (iter len '()))

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

(define (binary->base64 lst)
  (define (inner l res)
    (if (null? l)
        res
        (let ((a (take l 3)))
          (inner (drop l 3) (append res (dec->n 64 (n->dec 256 a)))))))
  
  (map 
   (lambda (x)
     (cadr (assv x table)))
   (inner 
    (append lst (if (= 0 (remainder (length lst) 3))
                    '()
                    (fill-n 0 (- 3 (remainder (length lst) 3)))))
    '())))

(define table '((0 #\A)(1 #\B)(2 #\C)(3 #\D)(4 #\E)(5 #\F)(6 #\G)(7 #\H)(8 #\I)(9 #\J)(10 #\K)(11 #\L)(12 #\M)(13 #\N)(14 #\O)(15 #\P)(16 #\Q)(17 #\R)(18 #\S)(19 #\T)(20 #\U)(21 #\V)(22 #\W)(23 #\X)(24 #\Y)(25 #\Z)
                       (26 #\a)(27 #\b)(28 #\c)(29 #\d)(30 #\e)(31 #\f)(32 #\g)(33 #\h)(34 #\i)(35 #\j)(36 #\k)(37 #\l)(38 #\m)(39 #\n)(40 #\o)(41 #\p)(42 #\q)(43 #\r)(44 #\s)(45 #\t)(46 #\u)(47 #\v)(48 #\w)(49 #\x)(50 #\y)(51 #\z)
                       (52 #\0)(53 #\1)(54 #\2)(55 #\3)(56 #\4)(57 #\5)(58 #\6)(59 #\7)(60 #\8)(61 #\9)
                       (62 #\+)(63 #\/)))


(define (base64->binary str)
  (let ((a (map (lambda (c)
                    (car (find (lambda (x) (equal? (char->integer (cadr x)) c)) table)))
                  (map (lambda (x) (char->integer x)) (filter (lambda (c) (not (equal? c #\newline))) (string->list str))))))
    (let ((len (length a)))
      (take 
       (dec->n 256 (n->dec 64 (append a (if (= (remainder len 4) 0)
                                         '()
                                         (fill-n 0 (- 4 (remainder len 4)))))))
       (floor (* len 3/4))))))
                                   
                                        

(define (string->base64 str)
  (binary->base64
   (map 
    (lambda (x)
      (char->integer x))
    (string->list str))))

(define (read-chunk-until-eof n in proc)
  (define (iter i res)
    (let ((c (read-byte in)))
      (if (or (= i 0) (eof-object? c))
          res
          (iter (- i 1) (append res (list c))))))
  (define (iter2 res)
    (if (eof-object? (read-byte in))
        res
        (iter2 (append ))(proc (iter n '()))

(define (encode file)
  (define in (open-input-file file))
  
  (write (list->string (binary->base64 (read-chunk 512 in)))))
