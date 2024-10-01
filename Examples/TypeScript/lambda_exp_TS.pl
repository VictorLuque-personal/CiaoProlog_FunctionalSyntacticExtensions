:- module(_,_,[functional,hiord]).

:- fun_eval hiord(true).

%% const applyOp = (x: number, y: number, operation: (a: number, b: number) => number) => {
%%    return operation(x, y);
%% };

%% var applyOp = function (x, y, operation) {
%%    return operation(x, y);
%% };

% console.log(applyOp(10, 20, function (a, b) { return a + b; })

%% :- pred foldl(+P, ?Xs, ?V0, ?V)
%%    : mprd(P, ''(X,Y,Z):(num(X),num(Y),num(Z))) * list(term,Xs) * term(V0) * term(V)

%% :- pred ApplyOp(+A, +B, +F, ?R)
%%    : num(A) * num(B) * mprd(F, ''(X,Y,Z):(num(X),num(Y),num(Z))) * num(R).

evaluationTS := ~ApplyOp(10,20,{''(A,B) := A + B}) :-
    ApplyOp = {
        ''(A,B,Op) := ~Op(A,B)
        }.