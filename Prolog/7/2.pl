% Предикат для чтения и обработки данных из файла
read_and_process_file :-
    % Указываем имя файла
    atom_string(Filename, 'input.txt'),

    % Проверяем, существует ли файл
    (   exists_file(Filename)
    ->  % Если файл существует, открываем его для чтения
        open(Filename, read, Stream),
        % Читаем и обрабатываем содержимое файла
        read_file_contents(Stream),
        % Закрываем файл
        close(Stream)
    ;   % Если файл не существует, выводим сообщение об ошибке
        format('Error: File not exist: ~w', [Filename]), nl
    ).

% Предикат для чтения содержимого файла
read_file_contents(Stream) :-
    % Читаем строку из файла
    read_line_to_string(Stream, Line),
    % Проверяем, не достигнут ли конец файла
    (   Line \== end_of_file
    ->  % Если не конец файла, обрабатываем строку
        process_line(Line),
        % Рекурсивно вызываем предикат для чтения следующей строки
        read_file_contents(Stream)
    ;   % Если достигнут конец файла, завершаем чтение
        true
    ).

% Предикат для обработки одной строки
process_line(Line) :-
    % Последующая обработка текста 
    % В данном случае просто вывод текста на экран
    format('Input Text:~n~w', [Line]).