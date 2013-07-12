FSM
========================================

A chicken library to implement finite state machines. 

Example
--------------------

A simple FSM to check whether a binary number is even.

    (create-fsm even?
                (N (0 -> Y) (1 -> N))
                (Y end (0 -> Y) (1 -> N)))
    
    (even? '(0 1 0)) ==> #t
    (even? '(1 1)) ==>   #f

As you can see, we defined a state N, with two outgoing arrows, the first transition is enabled when the input is 0, and goes to the Y state, the second is enabled on 1, and loops back to N.
Y is defined in the same way, but it's marked as final state, with the `end` keyword: the only difference with N is here: when the FSM has read the whole string and is in a state marked with `end`, it outputs #t. If the automata reads the whole string and isn't in a final state, it outputs #f.

You can also add callbacks to states. For example:
    
    (N (0 -> Y) (1 -> N (lambda (in) (display "going to Y state") (newline))))

A transition can have 0 or more callbacks: they are called sequentially, in the same order. A callback is a unary function, with the input string as argument.

Features
--------------------

1. optional __compile time__ checks. If enabled, the library checks for transitions that are not defined or that are outside of the FSM. Such as:

        ;; note the #t as first argument, if omitted, no check is performed.
        (create-fsm #t fsm (A (b -> map))) ==> compile time error, `map' isn't a state of this FSM
        (create-fsm #t fsm (A (b -> C)) (B end)) ==> compile time error, state `C' isn't defined.

This controls are completely performed at compile time, so the runtime performance will not be affected.

2. Callbacks. Every transition can have 0 or more function that will be called sequentially *before* the transition (like a Mealy machine).

3. Can be used with every data type that can be compared with `equal?`

4. Efficient implementations with tail calls.


Tests
--------------------

Tests are written in pure chicken without external libraries. Just import the library test (file test.scm) and call `(run-all)`.

License
--------------------
MIT

