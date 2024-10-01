:- module(_1,_2,[fsyntaxplus,hiord,fsyntax]).

test1(N,_1) :-
    'PAEnv'(-[N,_2],'PA'(-[N,_2],''(N,_2),('arithmetic:<'(N,2)->'term_basic:='(_2,1);'arithmetic:is'(_3,N-1),'hiord_rt:call'(Aux,''(_3,_2)))))=Aux,
    call(Aux,''(N,_1)).

test2(L,_1) :-
    '\006\block_expr'('test2/2/1/$meta/1'(L),_2),
    _1=_2.

'test2/2/1/$meta/1'(L) :-
    get_last(L,_3),
    '\006\letvar'(X,_3),
    '\006\return'(X).

get_last(L,_1) :-
    L=[X],
    !,
    _1=X.
get_last(L,_1) :-
    L=[_2|Rest],
    !,
    get_last(Rest,_1).

test_scope1(X,Y,_1) :-
    '\006\block_expr'('test_scope1/3/1/$meta/2'(X,Y),_2),
    _1=_2.

'test_scope1/3/1/$meta/2'(X,Y) :-
    'PAEnv'(-[X,_4],'PA'(-[X,_4],''(X,_4),'arithmetic:is'(_4,X+Y)))=_3,
    '\006\letvar'(Aux,_3),
    call(Aux,''(X,_5)),
    '\006\return'(_5).

test_scope2(X,Y,_1) :-
    '\006\block_expr'('test_scope2/3/1/$meta/3'(X,Y),_2),
    _1=_2.

'test_scope2/3/1/$meta/3'(X,Y) :-
    'PAEnv'(-[Y,_4],'PA'(-[Y,_4],''(Y,_4),'arithmetic:is'(_4,X+Y)))=_3,
    '\006\letvar'(Aux,_3),
    call(Aux,''(X,_5)),
    '\006\return'(_5).

test_letrec(L,_1) :-
    '\006\block_expr'('test_letrec/2/1/$meta/4'(L),_2),
    _1=_2.

'test_letrec/2/1/$meta/4'(L) :-
    'PAEnv'(-[_4,_5],'PA'(-[_4,_5],''(_4,_5),('term_basic:='(''(_4,_5),''([],0)),true;'term_basic:='(''(_4,_5),''([_7|_8],_6)),'hiord_rt:call'(Length,''(_8,_9)),'arithmetic:is'(_6,_9+1))))=_3,
    '\006\letvar'(Length,_3),
    call(Length,''(L,_10)),
    '\006\return'(_10).

test_var_match(X,_1) :-
    '\006\block_expr'('test_var_match/2/1/$meta/5'(X),_2),
    _1=_2.

'test_var_match/2/1/$meta/5'(X) :-
    _3 is X+1,
    '\006\letvar'(Var,_3),
    'PAEnv'(-[X,_5],'PA'(-[X,_5],''(X,_5),'arithmetic:is'(_5,X+Var)))=_4,
    '\006\letvar'(Increment,_4),
    call(Increment,''(Var,_6)),
    '\006\return'(_6).

test_nested_let(X,_1) :-
    '\006\block_expr'('test_nested_let/2/1/$meta/6'(X),_2),
    _1=_2.

'test_nested_let/2/1/$meta/6'(X) :-
    _3 is X+1,
    '\006\letvar'(XplusONE,_3),
    _4 is XplusONE+1,
    '\006\letvar'(XplusTWO,_4),
    _5 is XplusTWO+1,
    '\006\return'(_5).


