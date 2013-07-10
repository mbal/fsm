(define (filter predicate list)
  (define (helper list acc)
    (cond ((null? list) acc)
          ((predicate (car list)) (helper (cdr list) (cons (car list) acc)))
          (else (helper (cdr list) acc))))
  (helper list '()))

(define-syntax create-fsm2
  (syntax-rules ()
    ((create-fsm name (state transitions ...) ...)
     (define name
       (letrec 
         ((state 
            (process-state2 transitions ...)) ...)
         (get-first '(state ...)))))))

(define (check-not-outgoing-transitions states transitions)
  (syntax-error transitions))

(define (get-first states) (car states))

(define (check states transitions)
  (define (not-in? list what)
    (cond ((null? list) what)
          ((equal? (car list) what) #f)
          (else (not-in? (cdr list) what))))

  (let ((offending-transitions 
          (filter (lambda (x) x)
                  (map (lambda (x) (not-in? states x)) transitions))))
    (if (not (equal? offending-transitions '()))
      (syntax-error "The following transitions go out of this fsm" 
                    offending-transitions)
      #t)))

(define-syntax process-state2
  (syntax-rules (-> end)
    ((_ end)
     (lambda (in)
       (cond ((null? in) #t)
             (else #f))))
    ((_ (input -> next callback ...) ...)
     (begin 
       (lambda (in)
         (cond ((null? in) #f)
               ((equal? (car in) 'input) 
                (callback in) ...
                (next (cdr in))) ...
               (else #f)))))))
