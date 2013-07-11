(module test (run-all)
  (import r5rs)
  (import new)
  (import chicken)

  (define (test-1)
    (create-fsm name
                (A (a -> B) (b -> B))
                (B (a -> A) (b -> B) (C -> C))
                ; note: should disambiguate between input and transition
                (C end))
    (assert (name '(a C))) ; check is case sensitive
    (assert (not (name '(a c))))
    (assert (name '(a a b b b C)))
    (assert (not (name '(a a a)))) ; returns #f, not in final state
    (assert (not (name '(a C C))))) ; the fsm returns #t if the string ended

  (define (test-2)
    (create-fsm name ; check is false, so it doesn't raise errors
                (A (a -> #t) (b -> B)) 
                (B end))
    (assert (name '(b)))) ; if we don't enter the wrong state, everything is OK

  (define (run-all)
    (test-1)
    (test-2)))
