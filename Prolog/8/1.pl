:- dynamic football_player/6.

football_player('john', 'Denver Broncos', 11, 190, 90, 5).
football_player('jane', 'Denver Broncos', 1, 196, 100, 5).
football_player('bob', 'Pittsburgh Steelers', 23, 188, 80, 2).




% ����
menu :-
    write('���������� ���� ������:'), nl,
    write('1. �������� ������ ������'), nl,
    write('2. ������� ������'), nl,
    write('3. ������� ���������� �� ������'), nl,
    write('4. ������� ������ ���� �������'), nl,
    write('5. �����'), nl,
    write('�������� ����� ����: '), read(Option), nl,
    m(Option).

% ���������� ���������� ������ ����
m(1) :-
    write('������� ��� ������: '), read(Name),
    write('������� �������� �������: '), read(Team),
    write('������� ����� ������: '), read(Number),
    write('������� ���� ������: '), read(Height),
    write('������� ��� ������: '), read(Weight),
    write('������� ���������� ������� � NFL: '), read(Seasons),
    asserta(football_player(Name, Team, Number, Height, Weight, Seasons)), nl,
    write('����� �������� � ���� ������.'), nl, menu.

m(2) :-
    write('������� ��� ������ ��� ��������: '), read(Name),
    retract(football_player(Name, _, _, _, _, _)), nl,
    write('����� ������ �� ���� ������.'), nl, menu.

m(3) :-
    write('������� ��� ������ ��� ������ ����������: '), read(Name),
    (   football_player(Name, Team, Number, Height, Weight, Seasons)
    ->  write('���: '), write(Name), nl,
        write('�������: '), write(Team), nl,
        write('�����: '), write(Number), nl,
        write('����: '), write(Height), nl,
        write('���: '), write(Weight), nl,
        write('���������� ������� � NFL: '), write(Seasons), nl
    ;   write('����� �� ������.'), nl
    ),
    menu.

m(4) :-
    write('������ ���� �������:'), nl,
    list_all_players,
    menu.

m(5) :-
    write('������ � ���������� ���������.'), nl.

m(_) :-
    write('�������� ����� ����, ���������� ��� ���.'), nl, menu.

% ����� ������ ���� �������
list_all_players :-
    findall(Player, football_player(Player, _, _, _, _, _), Players),
    (   Players = []
    ->  write('���� ������ �����.'), nl
    ;   write('�����'), write('  |  '), write('�������'), write('  |  '), write('�����'), write('  |  '), write('����'), write('  |  '), write('���'), write('  |  '), write('������ � NFL'), nl,
        print_players(Players)
    ),
    nl.

print_players([]).
print_players([Player|Rest]) :-
    football_player(Player, Team, Number, Height, Weight, Seasons),
    write(Player), write('  |  '), write(Team), write('  |  '), write(Number), write('  |  '), write(Height), write('  |  '), write(Weight), write('  |  '), write(Seasons), nl,
    print_players(Rest).