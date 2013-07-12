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

A transition can have 0 or more callbacks: they are called sequentially, in the same order. A callback is a unary function, with the remaining input string as argument.

A FSM can also be checked at compile time for the validity (i.e. all the transitions are to another FSM state).

Tests
--------------------

Tests are written in pure chicken without external libraries. Just import the library test (file test.scm) and call `(run-all)`.

License
--------------------
MIT

