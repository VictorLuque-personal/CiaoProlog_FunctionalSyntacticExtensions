:-module(_,_,[fsyntaxplus, hiord, assertions, basicmodes, nativeprops]).

:- use_module(library(hiordlib)). % this must be commented when using the hiord assertions
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(engine(io_basic)).
:- use_module(library(arithpreds)). % TODO: include this with arith(true)
:- use_module(library(lazy/lazy_lib)).
:- use_module(library(sort)).

:-fun_eval hiord(true).

part1(L) := ~Part1(0,0,L) :-
    Part1 = {
        ''(M,C,L) := L = [] ? M
                   | L = [[]|Xs] ? ~Part1(~max(M,C),0,Xs)
                   | L = [X|Xs] ? ~Part1(M,C+X,Xs)
    }.

part2(Lines) := { let Snacks <- ~qsort(~Part2([],0,Lines)),
                  return(~foldl('+',~take(3,Snacks),0)) }
    :- Part2 = {
        ''(M,C,L) := L = [] ? M
                   | L = [[]|Xs] ? ~Part2([C|M],0,Xs)
                   | L = [X|Xs] ? ~Part2(M,C+X,Xs)
    }.

main :- open('input.txt', read, Str),
    L = ~get_lines_converted(Str),
    R1 = ~part1(L),
    write('The result for part 1 is: '),
    write(R1),
    nl,
    R2 = ~part2(L),
    write('The result for part 2 is: '),
    write(R2),
    close(Str).

get_lines_converted(Str) := at_end_of_stream(Str) ? []
    | number_codes(N,~get_line(Str)) ? [N|~get_lines_converted(Str)]
    | [""|~get_lines_converted(Str)].

%%%%%%%%%%%%% AUXILIARY PREDICATES %%%%%%%%%%%%%

max(X,Y) := X >= Y ? X
    | Y.

qsort([]) := [].
qsort([X | L]) := ~conc(~qsort(L1), [X | ~qsort(L2)]) :-
    partition('<'(X), L, L1, L2).

conc([], L) := L.
conc([H | T], K) := [H | ~conc(T, K)].

%% :- pred foldl(+P, ?Xs, ?V0, ?V)
%%    :: mprd(P, ''(X,Y,Z):(num(X),num(Y),num(Z))), list(term,Xs), term(V0) * term(V)
%%    # "Reduces (fold) @var{Xs} from the left applying @var{P} and using
%%       @var{V0}-@var{V} as accumulator.".

%% foldl(P, Xs, V0, V) :-
%%     foldl1(Xs, P, V0, V).

%% :- meta_predicate foldl1(?, pred(3), ?, ?).
%% :- pred foldl1(?, +, ?, ?) : list(term) * prd(''/3:(num,num,num)) * term * term.
%% foldl1([], _P, V, V).
%% foldl1([X|Xs], P, V0, V) :-
%%     P(X, V0, V1),
%%     foldl1(Xs, P, V1, V).

%% :- meta_predicate partition(pred(1), ?, ?, ?).
%% :- pred partition(+P, +Xs, ?Ys, ?Zs) ::
%%    prd(''/1:(num)) * list * list * list
%%    # "@var{Ys} contains all elements @var{X} of @var{Xs} such that
%%       @tt{P(X)} holds, and @var{Zs} all that does not (preserving the
%%       order)".
%%
%% partition(P, Xs, Ys, Zs) :-
%%     partition_(Xs, P, Ys, Zs).

%% :- pred partition(?List, +OrderRelation, ?Smallers, ?GreaterOrEquals) : list(List), mprd(OrderRelation, ''(X):(num(X))), list(Smallers), list(GreaterOrEquals).
%% partition_([], _, [], []).
%% partition_([X|Xs], P, Ys, Zs) :-
%%     ( P(X) ->
%%         Ys = [X|Ys0],
%%         Zs = Zs0
%%     ; Ys = Ys0,
%%       Zs = [X|Zs0]
%%     ),
%%     partition_(Xs, P, Ys0, Zs0).
