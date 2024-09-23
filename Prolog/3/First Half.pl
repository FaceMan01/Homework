% Рекурсивный вывод численных значений от 1 до N
print_numbers(N) :-
    N < 8,
    write(N), write('  '),
    N1 is N + 1,
    print_numbers(N1).
% Рекурсивный вывод численных значений от N до 1
print_numbers_dc(N) :-
    N > 0,
    write(N), write('  '),
    N1 is N - 1,
    print_numbers_dc(N1).

% Рекурсивный поиск НОД
nod(X, X, X) :- !.
nod(X, Y, NOD) :-
    X < Y, !,
    Z is Y - X,
    nod(X, Z, NOD).
nod(X, Y, NOD) :-
    X > Y,
    Z is X - Y,
    nod(Z, Y, NOD).

% Рекурсивный поиск суммы чисел от 1 до N
sum_series(0, 0).
sum_series(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum_series(N1, Sum1),
    Sum is Sum1 + N.

% Рекурсивный поиск суммы нечётных чисел от 1 до N
sum_odd_series(0, 0).
sum_odd_series(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum_odd_series(N1, Sum1),
    (
        N mod 2 =:= 1 -> 
        Sum is Sum1 + N;
        Sum is Sum1
    ).

% Рекурсивный вывод численных значений от N1 до N2
for(N1, N2, N1) :- 
    N1 =< N2.
for(N1, N2, X) :- 
    N1 < N2, 
    N11 is N1 + 1, 
    for(N11, N2, X). 

% Определяем правила для построения пути между пунктами
way(1, 2).
way(1, 3).
way(2, 4).
way(2, 5).
way(3, 6).
way(6, 7).
way(6, 9).
way(7, 8).
way(9, 8).
way(9, 10).
way(10, 5).

path(X, X, [X]).

% Рекурсивный случай: путь из X в Y через промежуточные точки
path(X, Y, [X|Path]) :-
    way(X, Z),
    path(Z, Y, Path).

% Проверка достижимости точки Y из точки X
reachable(X, Y) :-
    path(X, Y, _).