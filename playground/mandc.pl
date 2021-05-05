move([M,C,B_old], '1M 1C', [MO,CO,B_new]) :- 
    MO is 3-M+1,
    CO is 3-C+1,
    change(B_old, B_new).

move([M,C,B_old], '1M', [MO,CO,B_new]) :- 
    MO is 3-M+1,
    CO is 3-C,
    change(B_old, B_new).

move([M,C,B_old], '2M', [MO,CO,B_new]) :- 
    MO is 3-M+2,
    CO is 3-C,
    change(B_old, B_new).

move([M,C,B_old], '1C', [MO,CO,B_new]) :- 
    MO is 3-M,
    CO is 3-C+1,
    change(B_old, B_new).

move([M,C,B_old], '2C', [MO,CO,B_new]) :- 
    MO is 3-M,
    CO is 3-C+2,
    change(B_old, B_new).

change(e,w).
change(w,e).

legal([M,C,_]) :- 
    MO is 3-M,
    CO is 3-C,
    (M = 0 ; M >= C),
    (MO = 0; MO >= CO),
    M >= 0,
    C >= 0,
    M =< 3,
    C =< 3.

solution([3,3,w],[]).
solution(State,[NextMove|Rest]) :-
    move(State,NextMove,NextState),
    legal(NextState),
    solution(NextState,Rest).

output([]) :- nl.
output([Move|Rest]) :-
    write(Move), nl,
    output(Rest).

solve(X,L) :- length(X,L), solution([3,3,e],X), output(X).