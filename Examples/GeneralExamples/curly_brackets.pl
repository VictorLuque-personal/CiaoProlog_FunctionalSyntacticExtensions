:- module(_,_,[fsyntaxplus,hiord]).

:- fun_eval arith(true).
%:- fun_eval hiord(true).

:- fun_eval test1/1.
test1(N) := ~Aux(N) :-
    Aux = { ''(N) := N < 2 ? 1 | ~Aux(N-1) }.

:- fun_eval test2/1.
test2(L) := { let X <- ~get_last(L), return(X) }.

:- fun_eval get_last/1.
get_last(L) := L = [X] ? X
    | L = [_|Rest] ? get_last(Rest).

test_scope1(X,Y) := { let Aux <- { ''(X) := X + Y }, return(~Aux(X)) }.
% ?- Result = ~test_scope1(1,2). (Result = 3)
test_scope2(X,Y) := { let Aux <- { ''(Y) := X + Y }, return(~Aux(X)) }.
% ?- Result = ~test_scope2(1,2). (Result = 2)

test_letrec(L) := { let Length <- {
    ''([]) := 0.
    ''([_|Rest]) := ~Length(Rest) + 1.
    },
    return(~Length(L))}.

test_var_match(X) := { let Var <- X + 1, let Increment <- {''(X) := X + Var}, return(~Increment(Var)) }.

test_nested_let(X) := { let XplusONE <- X + 1, let XplusTWO <- XplusONE + 1, return(XplusTWO + 1) }.
