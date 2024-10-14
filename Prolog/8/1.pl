:- dynamic football_player/6.

football_player('john', 'Denver Broncos', 11, 190, 90, 5).
football_player('jane', 'Denver Broncos', 1, 196, 100, 5).
football_player('bob', 'Pittsburgh Steelers', 23, 188, 80, 2).




% Меню
menu :-
    write('Футбольная база данных:'), nl,
    write('1. Добавить нового игрока'), nl,
    write('2. Удалить игрока'), nl,
    write('3. Вывести информацию об игроке'), nl,
    write('4. Вывести список всех игроков'), nl,
    write('5. Выход'), nl,
    write('Выберите пункт меню: '), read(Option), nl,
    m(Option).

% Выполнение выбранного пункта меню
m(1) :-
    write('Введите имя игрока: '), read(Name),
    write('Введите название команды: '), read(Team),
    write('Введите номер игрока: '), read(Number),
    write('Введите рост игрока: '), read(Height),
    write('Введите вес игрока: '), read(Weight),
    write('Введите количество сезонов в NFL: '), read(Seasons),
    asserta(football_player(Name, Team, Number, Height, Weight, Seasons)), nl,
    write('Игрок добавлен в базу данных.'), nl, menu.

m(2) :-
    write('Введите имя игрока для удаления: '), read(Name),
    retract(football_player(Name, _, _, _, _, _)), nl,
    write('Игрок удален из базы данных.'), nl, menu.

m(3) :-
    write('Введите имя игрока для вывода информации: '), read(Name),
    (   football_player(Name, Team, Number, Height, Weight, Seasons)
    ->  write('Имя: '), write(Name), nl,
        write('Команда: '), write(Team), nl,
        write('Номер: '), write(Number), nl,
        write('Рост: '), write(Height), nl,
        write('Вес: '), write(Weight), nl,
        write('Количество сезонов в NFL: '), write(Seasons), nl
    ;   write('Игрок не найден.'), nl
    ),
    menu.

m(4) :-
    write('Список всех игроков:'), nl,
    list_all_players,
    menu.

m(5) :-
    write('Работа с программой завершена.'), nl.

m(_) :-
    write('Неверный пункт меню, попробуйте еще раз.'), nl, menu.

% Вывод списка всех игроков
list_all_players :-
    findall(Player, football_player(Player, _, _, _, _, _), Players),
    (   Players = []
    ->  write('База данных пуста.'), nl
    ;   write('Игрок'), write('  |  '), write('Команда'), write('  |  '), write('Номер'), write('  |  '), write('Рост'), write('  |  '), write('Вес'), write('  |  '), write('Сезоны в NFL'), nl,
        print_players(Players)
    ),
    nl.

print_players([]).
print_players([Player|Rest]) :-
    football_player(Player, Team, Number, Height, Weight, Seasons),
    write(Player), write('  |  '), write(Team), write('  |  '), write(Number), write('  |  '), write(Height), write('  |  '), write(Weight), write('  |  '), write(Seasons), nl,
    print_players(Rest).