:- use_module(library(clpr)).

calculate_expression(X, Y, Result) :-
    LN4 = log(4),
    LG20 = log10(20),
    Term1 is exp(LN4 + LG20),
    Term2 is X^3,
    Term3 is 5 * Y,
    Result is Term1 + Term2 - Term3.

demo :-
    X = 2,
    Y = 3,
    calculate_expression(X, Y, Result),
    format('X=~w, Y=~w, Ответ: ~w', [X, Y, Result]).