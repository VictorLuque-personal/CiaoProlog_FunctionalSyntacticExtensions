:- module(_,_,[functional, fsyntaxplus, hiord, assertions, nativeprops, basicmodes]).

:- use_module(library(hiordlib)).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(engine(io_basic)).
:- use_module(library(iso_char)).
:- use_module(library(lists)).
:- use_module(library(arithpreds)). % TODO: include this with arith(true)

:- fun_eval hiord(true).

% in prolog we can make "tuples" by writting terms unevaluated,
% tipically, it is used the "-" predicate.

gameScore(Hand) :=
    Hand = ^('A'-'Y') ? 6
    | Hand = ^('A'-'X') ? 3
    | Hand = ^('A'-_) ? 0
    | Hand = ^('B'-'Z') ? 6
    | Hand = ^('B'-'Y') ? 3
    | Hand = ^('B'-_) ? 0
    | Hand = ^('C'-'X') ? 6
    | Hand = ^('C'-'Z') ? 3
    | Hand = ^('C'-_) ? 0.
    % In logic programming is not necessary to cover all cases

handScore(Hand) :=
    Hand = ^(_-'X') ? 1
    | Hand = ^(_-'Y') ? 2
    | Hand = ^(_-'Z') ? 3.
    % Again, no need to cover all cases

pickResponse(Hand) :=
    Hand = ^('A'-'X') ? 'Z'
    | Hand = ^('A'-'Y') ? 'X'
    | Hand = ^('A'-_) ? 'Y'
    | Hand = ^('B'-'X') ? 'X'
    | Hand = ^('B'-'Y') ? 'Y'
    | Hand = ^('B'-_) ? 'Z'
    | Hand = ^('C'-'X') ? 'Y'
    | Hand = ^('C'-'Y') ? 'Z'
    | Hand = ^('C'-_) ? 'X'.

% both parts have been translated using the ":-" as a where condition
% instead of using the let notation with return() (taht could be an
% alternative).

part1(Hands) := ~foldl('+',~maplist(ProcessHand,Hands),0) :-
    ProcessHand = {
        ''(Hand) := ~handScore(Hand) + ~gameScore(Hand)
    }.

part2(Hands) := ~foldl('+',~maplist(ProcessHand,Hands),0) :-
    ProcessHand = {
        ''(Hand) := ~handScore(NewHand) + ~gameScore(NewHand) :-
            NewHand = ^(~arg(1,Hand)-(~pickResponse(Hand)))
    }.

main :- open('input.txt', read, Str),
    L = ~get_lines_converted(Str),
    R1 = ~part1(L),
    write('The result for part 1 is: '),
    write(R1),
    R2 = ~part2(L),
    nl,
    write('The result for part 2 is: '),
    write(R2),
    close(Str).

get_lines_converted(Str) := at_end_of_stream(Str) ? []
    | [~convert_line(~get_line(Str))|~get_lines_converted(Str)].

convert_line(Line) := ^(X-Y) :-
    char_code(X,~nth(1,Line)), char_code(Y,~nth(3,Line)).

%% hiord assertions %%

%% :- pred foldl(+P, ?Xs, ?V0, ?V)
%%    : mprd(P, ''(X,Y,Z):(num(X),num(Y),num(Z))) * list(term,Xs) * term(V0) * term(V)
%%    # "Reduces (fold) @var{Xs} from the left applying @var{P} and using
%%       @var{V0}-@var{V} as accumulator.".

%% foldl(P, Xs, V0, V) :-
%%     foldl1(Xs, P, V0, V).

%% :- meta_predicate foldl1(?, pred(3), ?, ?).
%% :- pred foldl1(?, +, ?, ?) : list(term), prd(''/3:(num,num,num)), term, term.
%% foldl1([], _P, V, V).
%% foldl1([X|Xs], P, V0, V) :-
%%     P(X, V0, V1),
%%     foldl1(Xs, P, V1, V).

%% :- pred maplist(+P, +Xs, ?Ys) ::
%%     (mprd(P, ''(X,Y):(nonvar(X),num(Y))), list(nonvar,Xs), list(num,Ys) % can I put ''(X):(-(char,char))?
%%    # "Like @pred{maplist/2} but applied to successive tuples
%%       from @var{Xs}, @var{Ys}.".

%% maplist(P, Xs, Ys) :-
%%     maplist2(Xs, P, Ys).

%% :- meta_predicate maplist2(?, pred(2), ?).
%% :- pred maplist2(?, +, ?) : list(nonvar) * prd(''/2:(nonvar,num)) * list(num).
%% maplist2([], _, []).
%% maplist2([X|Xs], P, [Y|Ys]) :-
%%     P(X, Y),
%%     maplist2(Xs, P, Ys).