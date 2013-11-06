#lang scheme/base

(define (force exp)
  (exp))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-null? stream)
  (null? stream))

(define the-empty-stream '())

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (lambda ()
              (stream-map proc (stream-cdr s))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream stream)
  (stream-for-each (lambda (x) (display-line x)) stream))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons 
         low
         (lambda ()
           (stream-enumerate-interval (+ 1 low) high)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons
          (stream-car stream)
          (lambda () 
            (stream-filter pred (stream-cdr stream)))))
         (else (stream-filter pred (stream-cdr stream)))))

(define (prime? x)
  (if (= x 1) 
      #t
      (let ((q (ceiling (sqrt x))))
        (define (inner n)
          (cond ((= q n) #t)
                ((= (remainder x n) 0) #f)
                (else (inner (+ 1 n)))))
        (inner 2))))

(define (print-interactive stream)
  (if (stream-null? stream)
      (begin (display "End") 
             (newline))
      (begin
        (display (stream-car stream))
        (newline)
        (if (equal? (read) 'n)
            (print-interactive (stream-cdr stream))
            #f))))

(define (stream-append stream1 stream2)
  (if (stream-null? stream1)
      stream2
      (cons
       (stream-car stream1)
       (lambda ()
         (stream-append (stream-cdr stream1) stream2)))))

(define (singleton-stream value)
  (cons value
        (lambda ()
          '())))

(define (stream-repeat obj n)
  (cons obj
        (lambda ()
          (define (inner x)
            (if (= x n) 
                '()
                (cons 
                 obj
                 (lambda ()
                   (inner (+ 1 x))))))
          (inner 0))))

(define (flatten-stream stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((null? (stream-car stream))
         (flatten-stream
          (stream-cdr stream)))
        ((list? (stream-car stream))
         (cons (car (stream-car stream))
               (lambda ()
                 (flatten-stream
                  (cons
                   (cdr (stream-car stream))
                   (lambda ()
                     (stream-cdr stream)))))))
        (else
         (cons (stream-car stream)
               (lambda ()
                 (flatten-stream (stream-cdr stream)))))))


(define (stream-flatmap proc stream)
  (flatten-stream (stream-map proc stream)))

(define *db* '())

(define (assert! a)
  (set! *db* (append *db* (list a))))

(assert! '(person daniel))
(assert! '(person kicki))
(assert! '(person sussman))
(assert! '(person asdf))
(assert! '(idol daniel sussman))
(assert! '(idol kicki asdf))

(define (list->stream lst)
  (if (null? lst)
      the-empty-stream
      (cons 
       (car lst)
       (lambda ()
         (list->stream (cdr lst))))))

(define (var? symbol)
  (and (symbol? symbol) (equal? (string-ref (symbol->string symbol) 0) #\?)))

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (pattern-match pattern datum frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pattern datum) frame)
        ((var? pattern) (extend-if-consistent pattern datum frame))
        ((and (pair? pattern) (pair? datum))
         (pattern-match (cdr pattern)
                        (cdr datum)
                        (pattern-match (car pattern)
                                       (car datum)
                                       frame)))
        (else 'failed)))

(define (fetch-assertions pattern frame)
   (list->stream *db*))

(define (check-an-assertion assertion pattern frame)
  (let ((match-result (pattern-match pattern assertion frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (find-assertions query-pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum query-pattern frame))
                  (fetch-assertions query-pattern frame)))
