(define (node-index node)
  (list-ref node 0))

(define (node-left node)
  (list-ref node 1))

(define (node-right node)
  (list-ref node 2))

(define (display-node node)
  (display (node-index node)))

(define (bfs root proc)
  (define (bfs-inner queue)
    (if (null? queue)
        'done
        (let ((node (car queue)))
          (proc node)
          (bfs-inner (append (cdr queue)
                             (if (null? (node-left node))
                                 '()
                                 (list (node-left node)))
                             (if (null? (node-right node))
                                 '()
                                 (list (node-right node))))))))
  (bfs-inner (list root)))