split_list(List, Left, Right) :-
    length(List, N),           
    Half is N // 2,            
    length(Left, Half),        
    append(Left, Right, List). 
