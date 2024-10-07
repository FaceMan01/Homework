sum(X, Y) :-
    Z is X + Y,
    write(Z), nl.

minus(X, Y) :-
    T is X - Y,
    write(T), nl.

multiply(X, Y) :-
    T is X * Y,
    write(T), nl.

divide(X, Y) :-
    T is X / Y,
    write(T), nl.