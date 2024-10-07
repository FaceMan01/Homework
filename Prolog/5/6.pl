sublist(List, N, M, Sublist) :-
    skip_elements(List, N, Rest),
    take_elements(Rest, M, Sublist).

skip_elements(List, 1, List).    
skip_elements([_|Tail], N, Rest) :-
    N > 1,
    N1 is N - 1,
    skip_elements(Tail, N1, Rest).

take_elements(_, 0, []).          
take_elements([H|T], M, [H|Rest]) :-
    M > 0,
    M1 is M - 1,
    take_elements(T, M1, Rest).
