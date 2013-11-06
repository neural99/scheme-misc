;; stream stuff

(define the-empty-stream '())

(define (stream-null? s)
  (null? s))

(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))


(define (display-stream s)
  (define (display-line x)
    (display x)
    (newline))
  (stream-for-each display-line s))

(define (stream-for-each f s)
  (if (stream-null? s)
      'done
      (begin (f (stream-car s))
             (stream-for-each f (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))



;; serious stuff


(define (minstantiate exp frame unbound-var-handle)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handle exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


(define (pattern-matcher pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-matcher (cdr pat)
                          (cdr dat)
                          (pattern-matcher (car pat)
                                           (car dat)
                                           frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-matcher (binding-value var frame) dat frame)
        (extend var dat frame))))

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result 
         (pattern-matcher query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (qeval query frame-stream)
  (cond ((eq? (car query) 'and)
         (conjoin (cdr query) frame-stream))
        ((eq? (car query) 'or)
         (disjoin (cdr query) frame-stream))
        ((eq? (car query) 'not)
         (negate (cdr query) frame-stream))
        ((eq? (car query) 'lisp-value)
         (lisp-value (cdr query) frame-stream))
        (else
         (simple-query query frame-stream))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input prompt)
  (display prompt)
  (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((eq? q 'q) '())
          ((assertion-to-be-added? q)
             (add-rule-or-assertion! (add-assertion-body q))
             (query-driver-loop))
           
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (minstantiate
                 q
                 frame
                 (lambda (v f)
                   (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

;; compound queries
;; and
(define (conjoin conjucts frame-stream)
  (if (null? conjucts)
      frame-stream
      (conjoin (cdr conjuncts)
               (qeval (car conjucts) frame-stream))))
;; or
(define (disjoin disjuncts frame-stream)
  (if (null? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (car disjuncts) frame-stream)
       (delay (disjoin (cdr disjuncts) frame-stream)))))

;; filters
;; not
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? 
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         (singleton-stream frame)
         the-emtpy-stream))
   frame-stream))

;; lisp-value
(define (lisp-value call frame-stream)
  (stream-flatmap 
   (lambda (frame)
     (if (execute
          (minstantiate 
            call
            frame
            (lambda (v f)
              (error "Unknow pat var -- LISP VALUE"))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (car exp) user-initial-enivonment)
         (cdr exp)))

(define (assertion-to-be-added? exp)
  (eq? 'assert! (car exp)))

(define (add-assertion-body exp)
  (cadr exp))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-matcher query-pattern
                          (conslusion clean-rule)
                          query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-matcher p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-matcher (cdr p1)
                        (cdr p2)
                        (unify-matcher (car p1)
                                       (car p2)
                                       frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-matcher (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-matcher var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               #t
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     #f))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))

(define (rule? exp)
  (and (pair? exp) (eq? 'rule (car exp))))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (conclusion rule)
  (cadr rule))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list
         '?
         (string->symbol
          (substring chars 1 (string-length chars))))
        symbol)))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ rule-counter 1))
  rule-counter)

(define (make-new-variable var id)
  (cons '? (cons id (cdr var))))

(define (var? exp)
  (and (pair? exp) (eq? (car exp) '?)))

(define (constant-symbol? exp)
  (symbol? exp))

(define (contract-question-mark var)
  (string->symbol
   (string-append 
    "?"
    (if (number? (cadr var))
        (string-append (symbol->string (caddr var))
                       "-"
                       (number->string (cadr var)))
        (symbol->string (cadr var))))))

(define (make-binding var val)
  (cons var val))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame var frame)
  (assoc var frame))

(define (extend var val frame)
  (cons (make-binding var val) frame))





(define THE-ASSERTIONS the-empty-stream)
(define THE-RULES the-empty-stream)

(define (fetch-assertions pattern frame)
  THE-ASSERTIONS)

(define (fetch-rules pattern frame)
  THE-RULES)

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))))

(define (add-rule! rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))))

