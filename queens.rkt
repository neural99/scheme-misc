(define (integer->string int)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (quotient i 10) (cons (remainder i 10) res))))
  (list->string (map (lambda (i) (integer->char (+ i 48))) (iter int '()))))

(define (enumerate from to)
  (define (iter res i)
    (if (> i to)
        res
        (iter (append res (list i)) (+ 1 i))))
  (iter '() from))

(define (foreach f seqs)
  (if (null? seqs)
      #t
      (begin (f (car seqs))
             (foreach f (cdr seqs)))))

(define (accumulate f init seqs)
  (if (null? seqs)
      init
      (f (car seqs) (accumulate f init (cdr seqs)))))

(define (flatmap f seqs)
  (accumulate 
   append
   '()
   (map f seqs)))

(define (index n lst)
  (define (iter i r)
    (cond ((null? r) #f)
          ((equal? (car r) n) i)
          (else (iter (+ i 1) (cdr r)))))
  (iter 0 lst))

(define (print-board k board)
  (foreach (lambda (row)
         (letrec ((n (index row board))
                  (iter (lambda (i n) (cond ((= i k) (newline)) ((= i n) (begin (display "#") (iter (+ i 1) n))) (else (begin (display "+") (iter (+ i 1) n)))))))
           (iter 0 n)))
       (enumerate 0 (- k 1))))
          
(define (count n lst)
  (accumulate + 0 (map (lambda (x) (if (equal? x n) 1 0)) lst)))

(define (rotate-left k board)
  (map (lambda (i) (- k (index i board) 1)) (enumerate 0 (- k 1))))

(define (rotate-right k board)
  (reverse (map (lambda (i) (index i board)) (enumerate 0 (- k 1)))))   

(define (safe? k positions)
  (let ((new (car positions))
        (old (cdr positions)))
    (and (equal? #f (index new old)) ;; same row
         (not (accumulate ;; diagonals
               (lambda (p q)
                 (or p q))
               #f
               (map (lambda (row col)
                      (= (abs (- new row))
                         col))
                    old
                    (enumerate 1 (- k 1))))))))
         
(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap 
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate 0 (- board-size 1))))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(require graphics/graphics)
(open-graphics)
(define v (open-viewport "a" 500 500))

(define (draw-rect x y w h k)
  (let ((dx (/ w k))
        (dy (/ h k)))
    ((draw-solid-rectangle v) (make-posn (+ 1 (* dx x)) (+ 1 (* dy y))) (- dx 1) (- dy 1) "red")))

(define (draw-grid w h k)
  (let ((dx (/ w k))
        (dy (/ h k))
        (draw (lambda (dx dy x0 y0 x1 y1)
                (map (lambda (i)
                       ((draw-line v) (make-posn (+ x0 (* i dx)) (+ y0 (* i dy))) (make-posn (+ x1 (* i dx)) (+ y1 (* i dy))) "black"))
                     (enumerate 0 k)))))
    (draw dx 0 0 0 0 h)
    (draw 0 dy 0 0 w 0)))

(define (draw-board w h k board)
    (foreach (lambda (row)
               (let ((n (index row board)))
                 (draw-rect n row w h k)))
       (enumerate 0 (- k 1))))

(define (draw w h k)
  (let ((boards (queens k)))
    (map (lambda (board i)
           ((clear-viewport v))
           (draw-grid w h k)
           (draw-board w h k board)
           ((save-pixmap v) (string-append "C:\\Users\\Gustaf\\Pictures\\queens\\" (integer->string k) "-" (integer->string i) ".bmp") 'bmp))
            boards
            (enumerate 1 (length boards)))))
  
(draw 499 499 10)