% Определение списка
list([2, 3, 4, 5, 6, 10, 8, 9, 7, 1, 0, 44, 33, 22, 11, 66, 55, 44, 33]).

print_list :-
    list(L),
    write(L), nl.

% Подсчет количества элементов списка
count_elements([], 0).
count_elements([_|T], Count) :-
    count_elements(T, Count1),
    Count is Count1 + 1.

% Получение N-го элемента списка
nth_element(1, [H|_], H).
nth_element(N, [_|T], Element) :-
    N > 1,
    N1 is N - 1,
    nth_element(N1, T, Element).

count :-
    list(L),
    count_elements(L, Count),
    write(Count), nl.

nth(N, Element) :-
    list(L),
    nth_element(N, L, Element).
