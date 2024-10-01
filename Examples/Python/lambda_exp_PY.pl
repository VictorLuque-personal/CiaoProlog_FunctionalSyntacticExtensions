:- module(_,_,[functional,hiord]).

:- use_module(library(hiordlib)).

:- fun_eval hiord(true).

%% a = [1, 2, 3, 4, 5, 6, 7, 8]
%% b = list(filter(lambda x: (x%2 == 0) , a))

%% # However, the same can be achieved with list comprehension
%% c = [x for x in a if x%2 == 0]
%% print(b)
%% print(c)

evaluationPY(L) := ~filter({''(X) :- X mod 2 =:= 0}, L). % filter is in

% X = ~evaluationPY([1,2,3,4,5,6,7,8]).