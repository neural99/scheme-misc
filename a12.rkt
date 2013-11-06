(require srfi/1)

(define (dot-product v1 v2)
  (fold + 0 (map * v1 v2)))

(define (matrix-transpose m)
  (map car m)