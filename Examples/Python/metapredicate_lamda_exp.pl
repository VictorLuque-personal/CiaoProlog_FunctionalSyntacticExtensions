:- module(_,_,[fsyntaxplus,hiord]).

:- fun_eval hiord(true).

:- meta_predicate reduce(pred(3), ?, ?).
reduce(Func, List) := ~reduce_(List1, Acc, Func) :-
    List = [Acc|List1].

:- meta_predicate reduce_(?, ?, pred(3), ?).
reduce_([], Acc, _) := Acc.
reduce_([X|Xs], Acc, F) := ~reduce_(Xs, ~F(Acc,X), F).

% For the function in the example:

evaluationPY_red(L) := ~reduce({
    ''(X,Y) := X >= Y ? X
             | Y
    },L).

% X = ~evaluationPY_red([921, 23, 555, 66, 12]).