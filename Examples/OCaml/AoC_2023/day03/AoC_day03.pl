:-module(_,_,[fsyntaxplus, hiord]).

:- use_module(library(hiordlib)).
:- use_module(engine(stream_basic)).
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(engine(io_basic)).
:- use_module(library(iso_char)).
:- use_module(library(lists)).
:- use_module(library(arithpreds)).

:-fun_eval arith(true).

% not having foldi, I put the index inside the accumulator in the usual fold left

part1(Input,Line_length) := { let Symbol_adjacent <- {
        ''(I) :- exists({
            ''(Idx) :- Idx >= 1,
                Idx < ~length(Input) * Line_length,
                Char = ~nth(Idx,Input),
                Char \= '.',
                Char \= '\n',
                not_is_digit(Char)
        }, [I - 1,
            I + 1,
            I - Line_length,
            I + Line_length,
            I - Line_length - 1,
            I + Line_length - 1,
            I - Line_length + 1,
            I + Line_length + 1
           ])
    },
    return(~nth(4,~foldl({
        ''(C,[Idx,Acc,Adjacent,Sum]) :=
            is_digit(C), number_chars(Digit,[C]) ?
            [Idx+1, (10 * Acc) + Digit, ((Adjacent = adjacent; Symbol_adjacent(Idx)) ? adjacent | not_adjacent), Sum]
        | [Idx+1, 0, not_adjacent, ((Adjacent = adjacent) ? Sum + Acc | Sum) ]
        }, Input, [1,0,not_adjacent,0])))
}.

% if statement blocks cannot be return values.

not_is_digit(Char) :- X = ~char_code(Char), (X < ~char_code('0'); X > ~char_code('9')).

is_digit('0').
is_digit('1').
is_digit('2').
is_digit('3').
is_digit('4').
is_digit('5').
is_digit('6').
is_digit('7').
is_digit('8').
is_digit('9').

part2(Input,Line_length) := ~arg(2,
        ~foldl({
            ''(C,^(Idx-Total)) :=
                C = '*' ? {
                    let Part_numbers <- ~maplist({ ''(S) := N :- number_chars(N, S) }, ~filter(non_empty_list, ~Expand_around(Idx))),
                    return(
                        (
                        Part_numbers = [X,Y] ? ^((Idx+1)-(Total + (X * Y)))
                        | ^((Idx+1)-Total)
                        )
                    )
                }
                | ^((Idx+1)-Total)
            }, Input, ^(1-0)))
    :- Is_digit = {
        ''(Idx) :- Idx > 0, Idx =< ~length(Input), is_digit(~nth(Idx,Input))
    },
    Expand = {
        ''(Idx, Delta) :=
            Is_digit(Idx) ? [~nth(Idx,Input) | ~Expand(Idx+Delta, Delta) ]
            | []
        },
    Expand_left = {
        ''(Idx) := ~reverse(~Expand(Idx, -1))
        },
    Expand_right = {
        ''(Idx) := ~Expand(Idx, 1)
        },
    Expand_around = {
        ''(Idx) := { let Left <- ~Expand_left(Idx-1),
                     let Right <- ~Expand_right(Idx+1),
                     let Upper_left <- ~Expand_left(Idx - Line_length - 1),
                     let Upper_right <- ~Expand_right(Idx - Line_length + 1),
                     let Lower_left <- ~Expand_left(Idx + Line_length - 1),
                     let Lower_right <- ~Expand_right(Idx + Line_length + 1),
                     let Above <- (
                         Is_digit(Idx - Line_length) ? ~list_concat([Upper_left,[~nth(Idx-Line_length,Input)],Upper_right])
                         | [Upper_left,Upper_right]
                         ),
                     let Below <- (
                         Is_digit(Idx + Line_length) ? ~list_concat([Lower_left,[~nth(Idx+Line_length,Input)],Lower_right])
                         | [Lower_left,Lower_right]
                         ),
                     return(~list_concat([[Left, Right], Above, Below]))
                   }
        }.

main :- open('input.txt', read, Str),
    Input = ~get_lines_to_string_list(Str),
    Line_length = ~index(Input,'\n') + 1,
    R1 = ~part1(Input,Line_length),
    write(R1),
    nl,
    R2 = ~part2(Input,Line_length),
    write(R2),
    nl,
    close(Str).

get_lines_to_string_list(Str) := at_end_of_stream(Str) ? []
    | [~get_char(Str)|~get_lines_to_string_list(Str)].

%%%%%%%%%%% AUXILIARY PREDICATES %%%%%%%%%%%

:- meta_predicate exists(pred(1), +).
%% :- pred exists(+P, +Xs)
%%     :: cgoal, list
%%    # "Cheks if it exists at least one element @var{X} of @var{Xs}
%%       such that @tt{P(X)} holds".

exists(Goal, List) :-
    exists_(List,Goal).

:- meta_predicate exists(pred(1), +, ?).
exists_([X|_], P) :- P(X).
exists_([_|Xs], P) :-
    exists_(Xs, P).

index([Char|_],Char,0).
index([_|Chars],Char,NewIndex) :- index(Chars,Char,Index), NewIndex is Index + 1.