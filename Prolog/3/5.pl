% Базовый случай: число однозначное
find_min_max(N, N, N) :-
    N < 10.

% Рекурсивный случай: число многозначное
find_min_max(N, Min, Max) :-
    N >= 10,
    N1 is N mod 10,
    N2 is N // 10,
    find_min_max(N2, Min1, Max1),
    (
        N1 < Min1 -> Min = N1, Max = Max1 ;
        N1 > Max1 -> Min = Min1, Max = N1 ;
        N1 =< Max1, N1 >= Min1 -> Min = Min1, Max = Max1
    ).