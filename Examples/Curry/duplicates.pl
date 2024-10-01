:- module(_,_,[fsyntaxplus, lazy]).

:- fun_eval arith(true).

app([],Ys,Ys).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

app3(Xs,Ys,Zs,Ts) :- app(Xs,Ys,Rs), app(Rs,Zs,Ts).

dup(Xs,Z) :- app3(_,[Z|_],[Z|_],Xs).

% dup([1,2,2,1],Z).

% Curry's functional transformation:
%% app [] ys = ys
%% app (x:xs) ys = x : app xs ys
%% app3 xs ys zs = app (app xs ys) zs
%% dup xs | xs =:= app3 _ (z:_) (z:_)
%%          = z

app_func([],Ys) := Ys.
app_func([X|Xs],Ys) := [X|~app_func(Xs,Ys)].

:- lazy fun_eval app3_func/3.

app3_func(Xs,Ys,Zs) := ~app_func(~app_func(Xs,Ys),Zs).

%:- lazy fun_eval dup_func/1.

dup_func(Xs) := Z :-
    Xs = ~app3_func(_,[Z|_],[Z|_]).