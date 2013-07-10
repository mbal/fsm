;; this first implementation is very basic, not really efficient and 
;; plain ugly in the definition of the automata. 
;; With a macro we can easily improve this latter point.
;; Instead, on the efficiency side, we could employ something like CPS
;; to go from a state to the next one.
;;
;; this version is a bit better, but there are too many parenthesis
;; (create-fsm fsm
;;             (A ((a -> A) (b -> B)))
;;             (B ((b -> B) (a -> A) (c -> C)))
;;             (C end))
;;
;; this is better. It's probably a bit harder to parse, but it's cleaner.
;; (create-fsm fsm
;;             (A (a -> A) (b -> B))
;;             (B (b -> B) (a -> A) (c -> C))
;;             (C end))
;;
;; In this example fsm:
;; 1. the first state is A.
;; 2. C is the accepting state.
;;
;; note that the accepting state can't have transition. There can be
;; more than one halting state. The first state of the automata is the first
;; listed (in this case, A)
;;
;; Now, we should check how's the generated assembler for a non-trivial state
;; machine. 
;; The tail calls can be easily transformed into a GOTO, so the result should
;; be quite performant
;;
;; We can add a callback to every state; this function will be called whenever
;; the fsm enters the state associated to the callback
;;
;; TODO : check if the next state is a fsm state and not a generic function.

(define-syntax create-fsm
  (syntax-rules ()
    ((create-fsm name (state transitions ...) ...)
     (define name
       (begin
         (define state
           (process-state transitions ...)) ...
         (get-first state ...))))))

(define-syntax get-first
  (syntax-rules ()
    ((_ state state2 ...) state)))

(define-syntax process-state
  (syntax-rules (-> end)
                ((_ end)
                 (lambda (in)
                   (cond ((null? in) #t)
                         (else #f))))
                ((_ (input -> next callback ...) ...)
                 (lambda (in)
                   (cond ((null? in) #f)
                         ((equal? (car in) 'input) 
                          (callback in) ...
                          (next (cdr in))) ...
                         (else #f))))))

(define fsm `((A1 (a -> B1 (lambda (x) (display x))) (c -> C (lambda (x) x)))
  (B1 (b -> B1 
         (lambda (y) (display "i just received b, I was in B"))
         (lambda (y) (newline) (display "well, off I go")))
      (a -> A1))
  (C end)))

(create-fsm nome 
    (A1 (a -> B1 (lambda (x) (display x))) (c -> C (lambda (x) x)))
    (B1 (b -> B1 
           (lambda (y) (display "i just received b, I was in B"))
           (lambda (y) (newline) (display "well, off I go")))
        (a -> A1))
    (C end))

