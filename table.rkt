(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (search same-key? key list)
      (define (inner lst)
        (cond ((null? lst)
               #f)
              ((same-key? (caar lst) key)
               (car lst))
              (else (inner (cdr lst)))))
      (inner list))
    (define (lookup key-list table)
      (cond ((or (not (pair? table)) (null? (cdr key-list)))
             (search same-key? (car key-list) table))
            (else
             (loopup (cdr key-list)
                     (lookup (car key-list) table)))))
    (define (insert! key-1 key-2 value)
      (let ((subtable (search same-key? key-1 (cdr local-table))))
        (if subtable
            (let ((record (search same-key? key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable (cons (cons key-2 value)
                                           (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 
                                  (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "table"))))
    dispatch))

    (define (search same-key? key list)
      (define (inner lst)
        (cond ((null? lst)
               #f)
              ((same-key? (caar lst) key)
               (car lst))
              (else (inner (cdr lst)))))
      (inner list))


    (define (same-key? key1 key2)
      (= key1 key2))
    
    (define (lookup key-list t)
      (cond ((null? key-list)
             #f)
            ((not (pair? key-list))
             (search same-key? key-list t))
            ((= (length key-list) 1)
             (search same-key? (car key-list) t))
            (else
             (let ((subtable (lookup (car key-list) t)))
               (if subtable
                   (lookup (cdr key-list)
                           (cdr subtable))
                   #f)))))
    
    (define (insert-inner! key value t)
      (if (null? key)
          (set-cdr! t (cons value
                            (cdr t)))
          (let ((rec (search same-key? key (cdr t))))
            (if rec
                (set-cdr! rec value)
                (set-cdr! t (cons (cons key value)
                                  (cdr t)))))))
    
    (define (insert! key-list value t)
      (cond ((null? key-list)
             #f)
            ((not (pair? key-list))
             (insert-inner key-list value t))
            ((= (length key-list) 1)
             (insert-inner! (car key-list) value t))
            (else 
             (let ((subtable (lookup (car key-list) (cdr t))))
               (if subtable
                   (insert! (cdr key-list)
                            value
                            subtable)
                   (let ((new (list (car key-list))))
                     (insert-inner! '() new t)
                     (insert! (cdr key-list) value new)))))))