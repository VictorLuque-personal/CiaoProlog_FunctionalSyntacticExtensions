:- module(_,_,[fsyntaxplus,hiord]).

:- fun_eval arith(true).
:- fun_eval hiord(true).

% This is the Fibonacci function with accumulator written in Lean4:
%% def fibFast (n : Nat) : Nat :=
%%   let rec loop : Nat → Nat × Nat
%%     | 0   => (0, 1)
%%     | n+1 => let p := loop n; (p.2, p.1 + p.2)
%%   (loop n).2

% We have a cut "!" in the translation of pattern matching, but we
% simulate it by having several clauses that is the equivalent in logic
% programming. Besides, there is the arithmetic instantiation problem
% and it forces us to write the inductive step with "the previous
% one" and not stating it as "the next one".

% With the {} notation in Ciao could be:
:- fun_eval fibFast/1.
fibFast(N) := ~arg(2,~Loop(N)) :-
                Loop = { -[N] -> % to be able to use N again as a free variable
                      ''(0) := ^(0-1).
                      ''(N) := { let P <- ~Loop(N-1), return(^(~arg(2,P)-(~arg(1,P) + ~arg(2,P)))) }.
                    }.

% X = ~fibFast(10).
% X = 89.