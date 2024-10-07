read_list(List) :-
    read_items([], List).

read_items(Acc, List) :-
    write('¬ведите элемент (или пустую строку дл€ завершени€): '), 
    read_line_to_string(user_input, Input),
    (
        Input = "" -> 
        List = Acc    
    ;
        append(Acc, [Input], NewAcc),
        read_items(NewAcc, List) 
    ).
