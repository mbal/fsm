(define-syntax create-fsm2
  (syntax-rules ()
    ((create-fsm name (state transitions ...) ...)
     (define name
       (begin
         (define state
           (process-state2 state transitions ...)) ...)))))

(define (loop s n)
  (cond ((null? s) #f)
        ((equal? (car s) n) #t)
        (else (loop (cdr s) n))))

(define-syntax check
  (syntax-rules ()
    ((check states trans)
     (if (not (foldl ands #t
                     (map (lambda (x) (loop states x)) trans)))
       (syntax-error "anoanoanoa")
       #t))))

(define (ands a b)
  (and a b))

(define-syntax process-state2
  (syntax-rules (-> end)
    ((_ end)
     (lambda (in)
       (cond ((null? in) #t)
             (else #f))))
    ((_ (states ...) (input -> next callback ...) ...)
     (lambda (in)
       (check '(states ...) '(next ...))
       (cond ((null? in) #f)
             ((equal? (car in) 'input) 
              (callback in) ...
              (next (cdr in))) ...
             (else #f))))))
