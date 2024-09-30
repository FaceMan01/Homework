distinct(List) :-
    sort(List, Sorted),
    length(List, Length),
    length(Sorted, Length).

% A
family(me).
family(mom).
family(dad).

left(me, dad).
left(mom, me).

range_a(X, Y, Z) :-
    left(X, Y),
    left(Y, Z).

print_a() :-
    family(X), 
    family(Y), 
    family(Z), 

    distinct([X, Y, Z]),
    range_a(X, Y, Z),
    write(X), write(' '),
    write(Y),write(' '),
    write(Z).


% �
num(x).
num(y).
num(z).
num(t).

less(x, y).
less(x, t).
less(z, y).
less(t, y).
less(x, z).
less(z, t).

range_b(X, Y, Z, T) :-
    less(X, Y),
    less(Y, Z),
    less(Z, T).

print_b :-
    num(X),
    num(Y),
    num(Z),
    num(T),

    distinct([X, Y, Z, T]),
    range_b(X, Y, Z, T),
    write(X), write(' < '),
    write(Y),write(' < '),
    write(Z),write(' < '),
    write(T).

% �
tree(���).
tree(�����).
tree(������).
tree(������).
tree(����).
tree(����).

less(������, �����).
less(����, ������).
less(������, ������).
less(����, ����).
less(�����, ���).

range_c(�1, �2, �3, �4, �5, �6) :-
    less(�1, �2),
    less(�2, �3),
    less(�3, �4),
    less(�4, �5),
    less(�5, �6).

print_c :-
    tree(�1),
    tree(�2),
    tree(�3),
    tree(�4),
    tree(�5),
    tree(�6),

    distinct([�1, �2, �3, �4, �5, �6]),
    range_c(�1, �2, �3, �4, �5, �6),
    write("����� ������� - "), write(�6),
    write("����� ��������� - "), write(�1).
