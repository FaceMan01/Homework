:- use_module(library(random)).

is_even(X) :- 
    X mod 2 =:= 0.

check_even :-
    random_between(1, 100, X),
    write('��������� �����: '), write(X), nl,
    (   is_even(X) ->
        write(X), write(' - ������');
        write(X), write(' - ��������.')
    ).