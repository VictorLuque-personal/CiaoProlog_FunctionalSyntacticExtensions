:- module(_,_,[fsyntaxplus,hiord]).

:- fun_eval hiord(true).

%% def twice (f : Nat -> Nat) (x : Nat) : Nat :=
%%   f (f x)

%% #eval twice (fun x => x + 1) 3

% result is 5

twice(F,X) := ~F(~F(X)).

evaluation(N) := ~twice({''(X) := X + 1}, N).

% X = ~evaluation(3).

% There must be something to put hiord(true) in the top level so that
% users can test things directly.

% Moreover, in Lean there is a syntactic suggar for lamda expresions:
% The same could be written as:

% #eval twice (· + 1) 3