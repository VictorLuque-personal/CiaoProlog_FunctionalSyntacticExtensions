:-module(_,_,[fsyntaxplus,hiord, assertions, basicmodes, nativeprops]).

:- use_module(library(hiordlib)).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(engine(io_basic)).
:- use_module(library(lists)).
:- use_module(library(arithpreds)).
:- use_module(engine(basic_props)).

% WE HAVE NO PIPELI
%% let part_1 strings =
%%   List.map ~f:(String.filter ~f:Char.is_digit) strings
%%   |> List.map ~f:(fun s -> String.of_char s.[0] ^ String.of_char s.[String.length s - 1])
%%   |> List.map ~f:Int.of_string
%%   |> List.reduce_exn ~f:( + )
%% ;;

part1(Lines) := ~foldl('+',~maplist({''(X) := N :- ~number_codes(N) = X},~maplist({''(X) := [~nth(1,X),~nth(~length(X),X)]},~maplist(filter(is_digit),Lines))),0).

is_digit(48).
is_digit(49).
is_digit(50).
is_digit(51).
is_digit(52).
is_digit(53).
is_digit(54).
is_digit(55).
is_digit(56).
is_digit(57).

part2(Lines) := ~foldl('+',~maplist({''(S) := (10 * ~hd(Digits)) + ~last(Digits) :- Digits = ~digits_in_string(S)},Lines),0).

digits := [^("one"-1),
           ^("two"-2),
           ^("three"-3),
           ^("four"-4),
           ^("five"-5),
           ^("six"-6),
           ^("seven"-7),
           ^("eight"-8),
           ^("nine"-9)].

digits_in_string(S) := ~F(S) :-
    F = {
        ''(Chars) :=
            Chars = [] ? []
            | Chars = [C|Rest], is_digit(C), number_codes(Digit,[C]) ? [Digit| ~F(Rest)]
            | Chars = [_|Rest], ^(_-Digit) = ~find({''(^(Word-_)) :- is_prefix(Chars,Word)},~digits) ? [Digit| ~F(Rest)]
            | Chars = [_|Rest] ? ~F(Rest)
        }.

main :- open('input.txt', read, Str),
    Lines = ~get_lines_to_string_list(Str),
    R1 = ~part1(Lines),
    write(R1),
    nl,
    R2 = ~part2(Lines),
    write(R2),
    close(Str).

get_lines_to_string_list(Str) := at_end_of_stream(Str) ? []
    | [~get_line(Str)|~get_lines_to_string_list(Str)].

%%%%%%%%%%%%% AUXILIARY PREDICATES %%%%%%%%%%%%%%

% The idea comes from 'List.is_prefix' of OCaml but without the 'equal' argument because
% we don't have types
% https://ocaml.janestreet.com/ocaml-core/v0.12/doc/core_kernel/_docdir/CHANGES.md
is_prefix(L,[]) :- list(L).
is_prefix([X|ListRest],[X|PrefixRest]) :- is_prefix(ListRest,PrefixRest).

hd([X|_]) := X.

% 'List.find' of OCaml:
% https://v2.ocaml.org/api/List.html#VALfind

:- meta_predicate find(pred(1), +, ?).
%% :- pred find(+P, +Xs, ?X)
%%     :: cgoal, list(term) * term
%%    # "@var{X} is the first element of @var{Xs} such that
%%      @tt{P(X)} holds".

find(Goal, List, Result) :-
    find_(List,Goal,Result).

:- meta_predicate find_(+, pred(1), ?).
find_([X|_], P, X) :- P(X).
find_([_|Xs], P, R) :-
    find_(Xs, P, R).

%% VERSION WITH HIGHER-ORDER ASSERTIONS %%

%% :- pred find(+P, +Xs, ?X)
%%     : mprd(P, ''(Elem):(nonvar(Elem))), list(term,Xs), term(X)
%%    # "@var{X} is the first element of @var{Xs} such that
%%      @tt{P(X)} holds".

%% find(Goal, List, Result) :-
%%     find_(List,Goal,Result).

%% :- meta_predicate find_(+, pred(1), ?).
%% :- pred find_(+,+,?) : list(term) * prd(''/1:nonvar) * term.
%% find_([X|_], P, X) :- P(X).
%% find_([_|Xs], P, R) :-
%%     find_(Xs, P, R).

%% :- pred filter(+P, +Xs, ?Ys)
%%     : (mprd(P, ''(A):int(A)), list(Xs), list(Ys)) + is_det
%%    # "@var{Ys} contains all elements @var{X} of @var{Xs} such that
%%      @tt{P(X)} holds (preserving the order)".

%% filter(Goal, List, Included) :-
%%     filter_(List, Goal, Included).

%% :- meta_predicate filter_(?, pred(1), ?).
%% :- pred filter_(?,+,?) : list * prd(''/1:int) * list.
%% filter_([], _, []).
%% filter_([X|Xs], P, Ys) :-
%%     ( P(X) ->
%%         Ys = [X|Ys0]
%%     ; Ys = Ys0
%%     ),
%%     filter_(Xs, P, Ys0).
