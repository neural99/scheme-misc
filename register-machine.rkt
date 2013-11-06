(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else 
             (error "Unknown request -- make register"))))
    dispatch))

(define (get-contents reg)
  (reg 'get))

(define (set-contents! reg val)
  ((reg 'set) val))

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- make stack")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else
             (error "Unknown request -- make-stack"))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (reg-name)
                ((machine 'allocate-register) reg-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table 
           (list (list 'pc pc) (list 'flag flag))))
      
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiple defined registers -- allocated-register-name")
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register -- lookup-register"))))
      
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- make-new-machine"))))
      
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register register-name machine)
  ((machine 'get-register) register-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register register-name machine)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register register-name machine) value)
  'done)

(define (assemble controller-text machine)
  (extract-labels controller-text 
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text recieve)
  (if (null? text)
      (recieve '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (recieve insts
                                       (cons (make-label-entry next-inst insts)
                                             labels))
                              (recieve (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register 'pc machine))
        (flag (get-register 'flag machine))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each 
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine pc flag stack ops)))
     insts)))

(define (instruction-text inst)
  (car inst))

(define (make-instruction text)
  (cons text '()))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- lookup-label"))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (display inst)(newline)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instuction type -- make-execution-procedure"))))

(define (make-assign inst machine labels ops pc)
  (let ((target (get-register (assign-reg-name inst) machine))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine labels ops)
               (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name inst)
  (cadr inst))

(define (assign-value-exp exp)
  (cddr exp))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels ops flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc (make-operation-exp condition machine labels ops)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- make-test"))))

(define (test-condition exp)
  (cdr exp))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- make-branch"))))

(define (branch-dest inst)
  (cadr inst))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- make-goto")))))

(define (goto-dest inst)
  (cadr inst))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name inst)
  (cadr inst))

(define (make-perfrom inst machine labels ops pc)
  (let ((action (perfom-action inst)))
    (if (operation-exp? action)
        (let ((action-proc (make-operation-exp action machine labels ops)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- make-perform"))))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register (register-exp-reg exp) machine)))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- make-primitive-exp"))))

(define (register-exp? exp) (and (list? exp) (eq? (car exp) 'reg)))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (and (list? exp) (eq? (car exp) 'const)))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (and (list? exp) (eq? (car exp) 'label)))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels ops)
  (let ((op (lookup-prim (operation-exp-op exp) ops))
        (aprocs (map (lambda (e) (make-primitive-exp e machine labels)) (operation-exp-operands exp))))
    (lambda () 
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (eq? (caar exp) 'op)))

(define (operation-exp-op exp)
  (cadr (car exp)))

(define (operation-exp-operands exp)
  (cdr exp))

(define (lookup-prim symbol ops)
  (let ((val (assoc symbol ops)))
    (if val
        (cadr val)
        (errror "Unknown operation -- lookup-prim"))))