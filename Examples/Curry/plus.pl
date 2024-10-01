:- module(_,_,[fsyntaxplus, lazy]).

:- fun_eval arith(true).

nat(0).
nat(s(X)) :- nat(X).

plus_prolog(0,Y,Y).
plus_prolog(s(X),Y,s(Z)) :- plus_prolog(X,Y,Z).

% Curry's functional transformation:
%% plus O y = y
%% plus (S x) y | z =:= plus x y = S z
% or the same writen with "where" clause systactic suggar:
%% plus O y = y
%% plus (S x) y = S z where z = plus x y

plus1(0,Y) := Y.
plus1(s(X),Y) := s(Z) :- Z = ~plus1(X,Y).

% The above Curry definition can be even more functional-style
% transformed, as 'z' is used only once:
%% plus O y = y
%% plus (S x) y = S (plus x y)

:- lazy fun_eval plus2/2.

plus2(0,Y) := Y.
plus2(s(X),Y) := s(~plus2(X,Y)).

% we can try a non-strict evaluation of plus

:- lazy fun_eval isPos/1.

isPos(0) := false.
isPos(s(_X)) := true.

% Use the next query to force evaluation of both functions:
% X = ~isPos(~plus2(s(s(s(s(s(0))))),s(s(0)))), X = true -> fail.
