% Предикат для суммирования чисел в файле
sum_numbers_in_file :-
    % Указываем имя файла
    atom_string(Filename, 'numbers.txt'),

    % Проверяем, существует ли файл
    (   exists_file(Filename)
    ->  % Если файл существует, открываем его для чтения
        open(Filename, read, Stream),
        % Читаем и обрабатываем содержимое файла
        sum_lines(Stream, 0, Sum),
        % Закрываем файл
        close(Stream),
        % Выводим результат
        format('Sum: ~w', [Sum]), nl
    ;   % Если файл не существует, выводим сообщение об ошибке
        format('Error: File not exist: ~w', [Filename]), nl
    ).

% Предикат для чтения содержимого файла
sum_lines(Stream, Acc, Sum) :-
    % Читаем строку из файла
    read_line_to_string(Stream, Line),
    % Проверяем, не достигнут ли конец файла
    (   Line \== end_of_file
    ->  % Если не конец файла, пытаемся преобразовать строку в число
        (   number_string(Number, Line)
        ->  % Если строка представляет число, добавляем его к аккумулятору
            NewAcc is Acc + Number,
            % Рекурсивно вызываем предикат для чтения следующей строки
            sum_lines(Stream, NewAcc, Sum)
        ;   % Если строка не представляет число, пропускаем её
            sum_lines(Stream, Acc, Sum)
        )
    ;   % Если достигнут конец файла, возвращаем аккумулятор как сумму
        Sum = Acc
    ).