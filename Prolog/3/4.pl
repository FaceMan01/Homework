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


% Б
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

% В
tree(ель).
tree(сосна).
tree(тополь).
tree(береза).
tree(липа).
tree(клен).

less(тополь, сосна).
less(липа, береза).
less(береза, тополь).
less(клен, липа).
less(сосна, ель).

range_c(Д1, Д2, Д3, Д4, Д5, Д6) :-
    less(Д1, Д2),
    less(Д2, Д3),
    less(Д3, Д4),
    less(Д4, Д5),
    less(Д5, Д6).

print_c :-
    tree(Д1),
    tree(Д2),
    tree(Д3),
    tree(Д4),
    tree(Д5),
    tree(Д6),

    distinct([Д1, Д2, Д3, Д4, Д5, Д6]),
    range_c(Д1, Д2, Д3, Д4, Д5, Д6),
    write("Самое большое - "), write(Д6),
    write("Самое маленькое - "), write(Д1).
