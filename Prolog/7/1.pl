% Предикат для записи строки в файл
write_to_file :-
    % Считываем строку с клавиатуры
    write('Write line: '),
    read_line_to_codes(user_input, Input),

    % Указываем имя файла
    atom_string(Filename, 'output.txt'),

    % Открываем файл для записи
    open(Filename, write, Stream),

    % Записываем строку в файл
    format(Stream, '~s', [Input]),

    % Закрываем файл
    close(Stream),

    % Выводим сообщение об успешной записи
    write('Done.').

% Предикат для добавления строки в конец файла
append_to_file :-
    % Считываем строку с клавиатуры
    write('Write line: '),
    read_line_to_codes(user_input, Input),

    % Указываем имя файла
    atom_string(Filename, 'output.txt'),

    % Открываем файл для добавления
    open(Filename, append, Stream),

    % Записываем строку в файл
    format(Stream, '~s', [Input]),

    % Закрываем файл
    close(Stream),

    % Выводим сообщение об успешной записи
    write('Success Append.').




