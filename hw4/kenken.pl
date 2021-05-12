/* 
    1. KenKen using finite domain solver
    Performance on the 4x4 kenken_testcase_2: 1 ms
    | ?- statistics, kenken_testcase_2(N,C), kenken(N,C,T), statistics, fail.
*/
kenken(N, C, T) :- 
    length(T, N),
    maplist(row_length(N), T),
    maplist(row_domain(N), T),
    maplist(fd_all_different, T),
    transpose(T, TT),
    maplist(fd_all_different, TT),
    all_constraints(C, T),
    maplist(fd_labeling, T).

row_domain(N, L) :- fd_domain(L, 1, N).
row_length(N, L) :- length(L, N).

/* 
    2. Plain KenKen
    Performance on the 4x4 kenken_testcase_2: 2608 ms
    | ?- statistics, kenken_testcase_2(N,C), plain_kenken(N,C,T), statistics, fail.
*/
plain_kenken(N, C, T) :-
    plain_grid_sat(N, T),
    all_constraints(C, T).

plain_grid_sat(N, G) :-
    length(G, N),
    rows_sat(N, G),
    transpose(G, GT),
    rows_sat(N, GT).

rows_sat(_, []).
rows_sat(N, [A|R]) :-
    unordered_range(N, A),
    rows_sat(N, R).

all_unique([]).
all_unique([A | Rest]) :-
    \+ member(A, Rest),
    all_unique(Rest).

unordered_range(N, L) :-
    length(L, N),
    maplist(between(1, N), L),
    all_unique(L).

/* Check list of constraints */
all_constraints([], _).
all_constraints([C|RestOfConstraints], T) :-
    constraint(C, T),
    all_constraints(RestOfConstraints, T).

constraint(+(0, []), _).
constraint(+(S, [[I|J]|RestOfSquares]), L) :- 
    square(I, J, L, V),
    constraint(+(T, RestOfSquares), L),
    S #= V+T,
    !.

constraint(*(1, []), _).
constraint(*(P, [[I|J]|RestOfSquares]), L) :- 
    square(I, J, L, V),
    constraint(*(T, RestOfSquares), L),
    P #= V*T,
    !.

constraint(-(D, [I1|J1], [I2|J2]), L) :- 
    square(I1, J1, L, V1),
    square(I2, J2, L, V2),
    (D #= V1 - V2 ; D #= V2 - V1).

constraint(/(Q, [I1|J1], [I2|J2]), L) :- 
    square(I1, J1, L, V1),
    square(I2, J2, L, V2),
    (Q #= V1 / V2 ; Q #= V2 / V1).

/* Utility predicates */
square(I, J, L, V) :- 
    nth1(I, L, M), 
    nth1(J, M, V).

transpose([[]|_], []).
transpose(G, [R|Rest]) :-
    first_col_as_row(G, R, GT),
    transpose(GT, Rest).

first_col_as_row([], [], []).
first_col_as_row([[H|T]|Rest], [H|HT], [T|TT]) :- 
    first_col_as_row(Rest, HT, TT).

/*
    Test cases
    Example usage: fd_set_vector_max(255), kenken_testcase_1(N,C), kenken(N,C,T). 
*/
kenken_testcase_1(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

kenken_testcase_2(
  4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ]
).

kenken_testcase_3(
  3,
  [
   +(4, [[1|1], [1|2]]),
   *(2, [[1|2], [2|1], [3|1]])
  ]
).