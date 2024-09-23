% ���������� �����
runner(yura).
runner(grisha).
runner(tolya).

% ���������� ��������� �����
place(1).
place(2).
place(3).

% ������� ������
solution(YuraPlace, GrishaPlace, TolyaPlace) :-
    % ������ ����� �������� �����
    place(YuraPlace),
    place(GrishaPlace),
    place(TolyaPlace),
    
    % ��� ����� ��������
    YuraPlace \= GrishaPlace,
    YuraPlace \= TolyaPlace,
    GrishaPlace \= TolyaPlace,
    
    % ������� ������
    GrishaPlace \= 2,
    GrishaPlace \= 3,
    TolyaPlace \= 3.

% ����� ����������
print_result :-
    solution(YuraPlace, GrishaPlace, TolyaPlace),
    write('��� ����� '), write(YuraPlace), write(' �����.'), nl,
    write('����� ����� '), write(GrishaPlace), write(' �����.'), nl,
    write('���� ����� '), write(TolyaPlace), write(' �����.'), nl.