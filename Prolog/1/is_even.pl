is_even(X) :- 
    X mod 2 =:= 0.

check_even :-
    write(' ������� �����: '),
    read(X),
    (   is_even(X) ->
        write(X), write(' - ������');
        write(X), write(' - ��������.')
    ).