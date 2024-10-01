:- module(_,_,[fsyntaxplus,hiord]).

:- fun_eval arith(true).
:- fun_eval hiord(true).

% In Lean4 they have their own let notation separated with ";"
%% def twice_double (x : Nat) : Nat :=
%%   let y := x + x; y * y

twice_double(X) := Y = X+X ? Y*Y. % This could be a translation for a let

% for the let notation we can use {} too, because ? has a cut !

twice_double_let(X) := { let Y<-X+X, return(Y*Y) }.

% lets can be combined (nested):

%% #check let y := 2 + 2; let z := y + y; z * z   -- Nat
%% #eval  let y := 2 + 2; let z := y + y; z * z   -- 64

nested_let(X) := { let Y <- X + X, let Z <- Y + Y, return(Z * Z) }.

% X = ~nested_let(2).
% X = 64