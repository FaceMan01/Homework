distinct(List) :-
    sort(List, Sorted),
    length(List, Length),
    length(Sorted, Length).

:- dynamic student/3.

student('�������', '������', 401).
student('�����', '�������', 401).
student('����', '������', 401).

same_surname :-
    findall(X, student(_, X, _), List),
    (
        writeln(List),
        distinct(List)
    ->  write('��� ��������� � ���������� ��������.'), nl
    ;   write('���� �������� � ���������� ��������.'), nl
    ).