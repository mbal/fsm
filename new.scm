(module new (create-fsm)
  (import r5rs)

  (define-syntax create-fsm
    ;; like Petrovsky suggests, we can use a string to disambiguate among
    ;; patterns, since patterns are matched using (equal? x y)
    (syntax-rules ()
      ((create-fsm name (state transitions ...) ...) 
       (create-fsm "grouped"  ; so we can actually match the second pattern, 
                   ; and not this again
                   #f        ; don't check
                   name 
                   (state ...) ;; the list of all states in the fsm
                   ((state transitions ...) ...)))
      ((create-fsm check? name (state transitions ...) ...)
       (create-fsm "grouped" 
                   check? 
                   name 
                   (state ...) 
                   ((state transitions ...) ...)))
      ((create-fsm "grouped" check? name states ((state transitions ...) ...))
       ;; this pattern matches when called recursively from the previous one
       ;; we use another definition so we can group the states without
       ;; using let (since (state ...) instead of states down there it's
       ;; forbidden due to too many ....
       (define name
         (letrec 
           ((state (prepare-expansion 
                     state ;; current state (needed only for error reporting)
                     check?  ;; check the transitions
                     states  ;; list of all the states
                     transitions ...)) ...)
           (get-first state ...))))))

  ;; returns the first state that has been declared. It will be used as
  ;; starting state.
  (define-syntax get-first
    (syntax-rules ()
      ((get-first state states ...) state)))

  (define-syntax prepare-expansion
    (syntax-rules (-> end)
      ((_ statename check states end)
       (lambda (in)
         (cond ((null? in) #t)
               (else #f))))
      ((_ statename #t states (input -> next callback ...) ...)
       ;; here we are going to perform a check. We need a "common lisp" macro
       ;; because we need lower level control on the generated code.
       ;; However, we use an ir-macro since we care about hygene.
       (checked-expander 
         #f  ;; the state isn't final
         statename 
         states 
         (input next callback ...) ...))
      ((_ statename #t states end (input -> next callback ...) ...)
       (checked-expander #t statename states (input next callback ...) ...))
      ((_ statename #f states end (input -> next callback ...) ...)
       (unchecked-expander
         statename 
         #t  ;; the state is final
         (input next callback ...) ...))
      ((_ statename #f states (input -> next callback ...) ...)
       (unchecked-expander statename #f (input next callback ...) ...))
      ((_ otherwise ...)
       (syntax-error 
         "No pattern matched in expansion. 
          Possible reason: `end' must go before any transitions!"))))

  ;;; I'm quite satisfied with this macro.
  (define-syntax checked-expander
    (ir-macro-transformer
      (lambda (data ren cmp)
        (let* ((is-final-state? (cadr data))
               (statename (caddr data)) ; the first element is the macro's name
               (states (cadddr data)) ; the args after that are the actual params
               (transitions (cddddr data))
               (offending-transitions
                 (get-outward-transitions states (map cadr transitions))))
          (if (equal? offending-transitions '())
            `(lambda (in)
               (cond ((null? in) ,(if is-final-state? #t #f))
                     ,@(map
                         (lambda (x)
                           `((equal? (car in) ',(car x))
                             ,@(if (>= (length x) 3)
                                 (map (lambda (y) `(,y in)) (cddr x))
                                 '())
                             (,(cadr x) (cdr in))))
                         transitions)
                     (else #f)))
            (syntax-error statename
                          "Transitions go out of the FSM"
                          offending-transitions))))))

  (define-syntax unchecked-expander 
    (syntax-rules ()
      ((_ statename is-final-state? (input next callback ...) ...)
       (lambda (in)
         (cond ((null? in) is-final-state?)
               ((equal? (car in) 'input)
                (callback in) ...
                (next (cdr in))) ...
               (else #f))))))

  (begin-for-syntax
    (define (filter predicate list)
      (define (helper list acc)
        (cond ((null? list) acc)
              ((predicate (car list)) (helper (cdr list) (cons (car list) acc)))
              (else (helper (cdr list) acc))))
      (helper list '()))

    ;; returns all the transitions that are outgoing (i.e. refers to a state
    ;; which is not declared in the FSM)
    ;; (get-outward-transitions (a -> B)) if B is not a FSM state, returns B.
    ;; if a FSM does have one or more outgoing transitions, it's invalid
    ;; and it's rejected at compile-time (if the check is enabled).

    (define (get-outward-transitions states transitions)
      ;; this function returns false when the item is in the list, 
      ;; returns the item if it's present.
      (define (not-in? list what)
        (cond ((null? list) what)
              ((equal? (car list) what) #f)
              (else (not-in? (cdr list) what))))
      (filter (lambda (x) x) 
              (map (lambda (x) (not-in? states x)) transitions))))
  )
