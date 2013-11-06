(require graphics/graphics)
(open-graphics)
(define v (open-pixmap "a" 5000 5000))

(define (fixed-point f first-guess iterations)
  (define (iter i guess)
    (if (= i 0)
        guess
        (iter (- i 1) (f guess))))
  (iter iterations first-guess))

(define (mandel? c) (> 2 (magnitude (fixed-point (lambda(z)(+ (* z z) c)) 0+0i 60))))

(define (aaa width height proc)
  (let ((dx (/ 4.0 width))
        (dy (/ 4.0 height)))
    (define (iter y res)
      (define (row x res)
        (if (= x width)
            res
            (row (+ x 1) (append res (list (list x y (proc (+ -2.0 (* dx x)) (+ -2.0 (* dy y)))))))))
      (if (= y height)
          res
          (iter (+ y 1) (append res (list (row 0.0 '()))))))
    (iter 0 '())))

(define (flatten lst)
  (cond ((null? lst)
         '())
         ((pair? (car lst))
         (append (flatten (car lst)) (flatten (cdr lst))))
        (else (cons (car lst) (flatten (cdr lst))))))

(define (get-points w h)
  (filter (lambda(x)x) (flatten (map (lambda (x) (map (lambda (y) (if (caddr y) (list->vector (list (car y) (cadr y))) #f)) x)) (aaa w h (lambda(x y) (mandel? (make-rectangular x y))))))))

(define (vecs->posns lst)
  (map 
   (lambda (x)
     (make-posn (vector-ref x 0) (vector-ref x 1)))
   lst))

(define (draw-posns lst)
  (map
   (lambda (x) 
     ((draw-pixel v) x))
   lst)
  'done)

(draw-posns (vecs->posns (get-points 5000 5000)))
((save-pixmap v) "C:\\Users\\Gustaf\\Pictures\\mandel.png" 'png)
