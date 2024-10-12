min_element(tree(Root, nil, nil), Root).
min_element(tree(Root, Left, nil), Min) :-
    min_element(Left, LeftMin),
    min(Root, LeftMin, Min).
min_element(tree(Root, nil, Right), Min) :-
    min_element(Right, RightMin),
    min(Root, RightMin, Min).
min_element(tree(Root, Left, Right), Min) :-
    min_element(Left, LeftMin),
    min_element(Right, RightMin),
    min(Root, LeftMin, TempMin),
    min(TempMin, RightMin, Min).

max_element(tree(Root, nil, nil), Root).
max_element(tree(Root, Left, nil), Max) :-
    max_element(Left, LeftMax),
    max(Root, LeftMax, Max).
max_element(tree(Root, nil, Right), Max) :-
    max_element(Right, RightMax),
    max(Root, RightMax, Max).
max_element(tree(Root, Left, Right), Max) :-
    max_element(Left, LeftMax),
    max_element(Right, RightMax),
    max(Root, LeftMax, TempMax),
    max(TempMax, RightMax, Max).

min_max_element(Tree) :-
    min_element(Tree, Min),
    max_element(Tree, Max),
    write('Min: '), write(Min), nl,
    write('Max: '), write(Max), nl.

min(A, B, A) :- A =< B.
min(A, B, B) :- A > B.

max(A, B, A) :- A >= B.
max(A, B, B) :- A < B.

let_tree(tree(5, tree(3, tree(10, tree(20, nil, nil), nil), nil), tree(7, nil, tree(-10, nil, tree(0, nil, nil))))).