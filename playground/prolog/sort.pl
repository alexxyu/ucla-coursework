mysort(L,P) :- permute(L,P), sorted(P).

sorted([]).
sorted([_]).
sorted([X,Y|L]) :- X =< Y, sorted([Y|L]).

permute([],[]).
permute([X|L],PX) :- permute(L,P), myappend(P1,P2,P), myappend(P1,[X|P2],PX).

myappend([],L,L).
myappend([X|L],M,[X|LM]) :- myappend(L,M,LM).