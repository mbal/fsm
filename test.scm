(module test (run-all)
  (import r5rs)
  (import new)
  (import chicken)

  ;; this test checks that the most basic form works: transitions,
  ;; case-sensitivity, accepting states, symbols not in the set
  ;; of supported symbols
  (define (test-1)
    (create-fsm name
                (A (a -> B) (b -> B))
                (B (a -> A) (b -> B) (C -> C))
                ; note: should disambiguate between input and transition
                (C end))
    (assert (name '(a C))) ; check is case sensitive
    (assert (not (name '(a c)))) ; c is not in the alphabet of the fsm
    (assert (not (name '(a d))))
    (assert (not (name '(d a C))))
    (assert (name '(a a b b b C)))
    (assert (not (name '(a a a)))) ; returns #f, not in final state
    (assert (not (name '(a C C))))) ; the fsm returns #t if the string ended

  ;; this test checks that if the validity check isn't enabled, we don't
  ;; encounter any problem as long as we don't enter the wrong state 
  ;; (in this case, #t). If the check is enabled (just add #t to
  ;; create-fsm as first argument), this file will fail to compile
  (define (test-2)
    (create-fsm name ; check is false, so it doesn't raise errors
                (A (a -> #t) (b -> B)) 
                (B end))
    (assert (name '(b)))) ; if we don't enter the wrong state, everything is OK
    
  ;; tests that mixing transitions and final states works as expected from
  ;; theory (namely, that the string is accepted only when the string 
  ;; is read completely and the fsm is in accepting state)
  (define (test-3)
    (create-fsm even
                (A (0 -> B) (1 -> A))
                (B end (1 -> A) (0 -> B)))
    (assert (even '(0 0 0 1 0)))
    (assert (not (even '(0 1))))
    (assert (not (even '(0 1 r))))
    (assert (not (even '(0 r))))
    (assert (even '(0)))
    (assert (not (even '()))))

  ;; tests that the callbacks are working as expected.
  (define (test-4)
    (let ((c 0))
      (create-fsm even
                  (A (0 -> B) (1 -> A))
                  (B end (0 -> B) (1 -> A (lambda (in) (set! c (+ c 1))))))
      (even '(0 0 0 1 0))
      (assert (= c 1))
      (set! c 0)
      (even '(0 1 0 1 0 1 0 1 0))
      (assert (= c 4))))

  (define (run-all)
    (test-1)
    (test-2)
    (test-3)
    (test-4)))
