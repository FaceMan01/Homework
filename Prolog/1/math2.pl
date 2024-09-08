:- use_module(library(clpr)).

calculate_expression(X, Y, Result) :-
    Term1 is 8^X - Y^2,
    Term2 is cos(pi/3),
    Term3 is sin(pi/6),
    Result is (Term1 * Term2 * (1/2)) / Term3.

demo :-
    X = 2,
    Y = 3,
    calculate_expression(X, Y, Result),
    format('X=~w, Y=~w, Ответ: ~w', [X, Y, Result]).