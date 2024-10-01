:-module(_,_,[fsyntaxplus,lazy]).

:- fun_eval arith(true).

nat(0).
nat(s(X)) :- nat(X).

dec_eager(s(X)) := X.

const_eager(X,_) := X.

% Try the next query:
% X = ~const_eager(0,~dec_eager(0)), nat(X).
% it fails because of the strict nature of Ciao, so, the
% "call-by-value" or eager evaluation of the function
% const_eager, forces the evaluation of dec_eager and it
% fails (it is not defined for value "0")

% But in constrast if we try it non-strictly:

:- lazy fun_eval dec/1.

dec(s(X)) := X.

:- lazy fun_eval const/2.

const(X,_) := X.

% X = ~const(0,~dec(0)), nat(X).

% This succeeds and unifies X with 0. It is dealying the
% evaluation of "~dec(0)" until needed and it is never needed
% due to the definition of const/2 that gives the result
% ignoring the value of the second argument (that would fail).
