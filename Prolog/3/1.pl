% Определяем факты
runner(yura).
runner(grisha).
runner(tolya).

% Определяем возможные места
place(1).
place(2).
place(3).

% Решение задачи
solution(YuraPlace, GrishaPlace, TolyaPlace) :-
    % Каждый бегун занимает место
    place(YuraPlace),
    place(GrishaPlace),
    place(TolyaPlace),
    
    % Все места различны
    YuraPlace \= GrishaPlace,
    YuraPlace \= TolyaPlace,
    GrishaPlace \= TolyaPlace,
    
    % Условия задачи
    GrishaPlace \= 2,
    GrishaPlace \= 3,
    TolyaPlace \= 3.

% Вывод результата
print_result :-
    solution(YuraPlace, GrishaPlace, TolyaPlace),
    write('Юра занял '), write(YuraPlace), write(' место.'), nl,
    write('Гриша занял '), write(GrishaPlace), write(' место.'), nl,
    write('Толя занял '), write(TolyaPlace), write(' место.'), nl.