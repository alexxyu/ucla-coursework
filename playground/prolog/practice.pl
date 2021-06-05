mappend([],L,L).
mappend([A|Rest],L,[A|LM]) :- mappend(Rest,L,LM).

mreverse([],[]).
mreverse([A|Rest],L) :- mreverse(Rest,TL), append(TL,[A],L).

revhalf([],[]).
revhalf(L,LR) :- 
    evens(L,LE,1), 
    evens(L,LO,0), 
    mreverse(LE, LER), 
    alternate(LO, LER, LR),
    !.

evens([],[],_).
evens([A|Rest],[A|LE],M) :-
    0 is (M mod 2),
    N is M + 1,
    evens(Rest,LE,N),
    !.
evens([_|Rest],LE,M) :-
    1 is (M mod 2),
    N is M + 1,
    evens(Rest,LE,N),
    !.

alternate(L,[],L).
alternate([],L,L).
alternate([A|LL],[B|LM],[A,B|LN]) :- alternate(LL,LM,LN),!.

subbag(L,[]) :- 
    length(L,N),
    N is 0.
subbag([A|LA],[B|LB]) :-
    A is B,
    subbag(LA,LB),
    !.
subbag(A,[_|LB]) :-
    subbag(A,LB),
    !.

adjdups([],[]).
adjdups([A],[A]).
adjdups([A,B|Rest],[A,B|LM]) :- 
    A \= B, 
    adjdups([B|Rest],[B|LM]), 
    !.
adjdups([A,A|Rest],[A|LM]) :- adjdups([A|Rest],[A|LM]), !.