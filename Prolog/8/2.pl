distinct(List) :-
    sort(List, Sorted),
    length(List, Length),
    length(Sorted, Length).

:- dynamic student/3.

student('Дмитрий', 'Иванов', 401).
student('Ислам', 'Хасанов', 401).
student('Олег', 'Иванов', 401).

same_surname :-
    findall(X, student(_, X, _), List),
    (
        writeln(List),
        distinct(List)
    ->  write('Нет студентов с одинаковой фамилией.'), nl
    ;   write('Есть студенты с одинаковой фамилией.'), nl
    ).