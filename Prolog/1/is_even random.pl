:- use_module(library(random)).

is_even(X) :- 
    X mod 2 =:= 0.

check_even :-
    random_between(1, 100, X),
    write('Случайное число: '), write(X), nl,
    (   is_even(X) ->
        write(X), write(' - чётное');
        write(X), write(' - нечётное.')
    ).