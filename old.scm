(define input '(a a a a a a))
(define fsm 
  '((a ((a a) (b b)))
    (b ((b b) (a b) (c end)))
    (end)))

(define (filter fn list)
  (cond ((null? list) '())
        ((fn (car list)) (cons (car list) (filter fn (cdr list))))
        (else (filter fn (cdr list)))))

(define (init fsm) 'a)

(define (run-fsm fsm input)
  (define (loop current-state in)
    (if (null? in)
      current-state
      (loop (next-state fsm current-state (car in)) (cdr in))))
  (loop 'a input))

(define (next-state fsm state input)
  (define (loop fn)
    (cond ((null? fn) '())
          ((equal? (caar fn) input) (cadar fn))
          (else (loop (cdr fn)))))
  (let ((transition-function
          (filter (lambda (x) (equal? (car x) state)) fsm)))
    (loop (cadar transition-function))))
