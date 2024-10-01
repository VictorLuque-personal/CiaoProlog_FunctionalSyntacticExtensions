:- module(_,_,[fsyntaxplus,hiord]).

:- fun_eval hiord(true).

%% def replicate (n : Nat) (a : α) : List α :=
%%   loop n []
%% where
%%   loop : Nat → List α → List α
%%     | 0,   as => as
%%     | n+1, as => loop n (a::as)

:- fun_eval replicate/2.
replicate(N,X) := ~Loop(N,[]) :- % where
                    Loop = {
                      ''(N,L) := N = 0 ? L
                               | ~Loop(N-1,[X|L])
                      }.

% X = ~replicate(3,a).
% X = [a,a,a]

% As an alternative to functional pattern matching, we can take advantage
% of clause matching of logic programming that is more elegant:

:- fun_eval replicate_clauses/2.
replicate_clauses(N,X) := ~Loop(N,[]) :- % where
                    Loop = { %-[N] ->
                      ''(0,L) := L.
                      ''(N,L) := ~Loop(N-1,[X|L]) :- N > 0. % to avoid possible infinite computations
                      }.

% X = ~replicate_clauses(3,a).
% X = [a,a,a]