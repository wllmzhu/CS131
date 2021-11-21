%Using First|Rest terminology in this assignment to not
%confuse with T for the matrix

%get number at square [R|C]
get_num(T, [R|C], Num) :-
    nth(R, T, Row),
    nth(C, Row, Num).

%add
add_constraint(_, [], 0).
add_constraint(T, [First|Rest], S) :-
    get_num(T, First, Num),
    add_constraint(T, Rest, Rest_S),
    S #= Num + Rest_S.

%sub
sub_constraint(T, J, K, D) :-
    get_num(T, J, Num1),
    get_num(T, K, Num2),
    (D #= Num1 - Num2; D #= Num2 - Num1).

%mul
mul_constraint(_, [], 1).
mul_constraint(T, [First|Rest], P) :-
    get_num(T, First, Num),
    mul_constraint(T, Rest, Rest_P),
    P #= Num * Rest_P.

%div
div_constraint(T, J, K, Q) :-
    get_num(T, J, Num1),
    get_num(T, K, Num2),
    (Q #= Num1 / Num2; Q #= Num2 / Num1).

%each type of constraints
constraint_satisfied(T, +(S, L)) :-
    add_constraint(T, L, S).
constraint_satisfied(T, -(D, J, K)) :-
    sub_constraint(T, J, K, D).
constraint_satisfied(T, *(P, L)) :-
    mul_constraint(T, L, P).
constraint_satisfied(T, /(Q, J, K)) :-
    div_constraint(T, J, K, Q).

%all constraints satisfied
all_constraints_satisfied(_, []).
all_constraints_satisfied(T, [First|Rest]) :-
    constraint_satisfied(T, First),
    all_constraints_satisfied(T, Rest).

%all rows satisfied
%each row: all unique, all within range N
row_satisfied(Row, N) :-
    fd_domain(Row, 1, N),
    fd_all_different(Row).
all_rows_satisfied([], _).
all_rows_satisfied([First|Rest], N) :-
    row_satisfied(First, N),
    all_rows_satisfied(Rest, N).

%matrix of right size:
% right number of rows,
right_size(T, N) :-
    length(T, N),
    all_rows_right_size(T, N).
all_rows_right_size([], _).
all_rows_right_size([First|Rest], N) :-
    length(First, N),
    all_rows_right_size(Rest, N).

%transpose matrix, from TA GitHub
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).


kenken(N, C, T) :-
    right_size(T, N),
    all_rows_satisfied(T, N),
    transpose(T, T_Trans),
    all_rows_satisfied(T_Trans, N),
    all_constraints_satisfied(T, C),
    maplist(fd_labeling, T).

%================ plain kenken ================

%add
add_constraint_P(_, [], 0).
add_constraint_P(T, [First|Rest], S) :-
    get_num(T, First, Num),
    add_constraint_P(T, Rest, Rest_S),
    S is Num + Rest_S.

%sub
sub_constraint_P(T, J, K, D) :-
    get_num(T, J, Num1),
    get_num(T, K, Num2),
    (D is Num1 - Num2; D is Num2 - Num1).

%mul
mul_constraint_P(_, [], 1).
mul_constraint_P(T, [First|Rest], P) :-
    get_num(T, First, Num),
    mul_constraint_P(T, Rest, Rest_P),
    P is Num * Rest_P.

%div
div_constraint_P(T, J, K, Q) :-
    get_num(T, J, Num1),
    get_num(T, K, Num2),
    (Q is Num1 / Num2; Q is Num2 / Num1).

%each type of constraints
constraint_satisfied_P(T, +(S, L)) :-
    add_constraint_P(T, L, S).
constraint_satisfied_P(T, -(D, J, K)) :-
    sub_constraint_P(T, J, K, D).
constraint_satisfied_P(T, *(P, L)) :-
    mul_constraint_P(T, L, P).
constraint_satisfied_P(T, /(Q, J, K)) :-
    div_constraint_P(T, J, K, Q).

%all constraints satisfied
all_constraints_satisfied_P(_, []).
all_constraints_satisfied_P(T, [First|Rest]) :-
    constraint_satisfied_P(T, First),
    all_constraints_satisfied_P(T, Rest).

%all rows satisfied
%each row: all unique, all within range N
row_satisfied_P(Row, N) :-
    elements_between(Row, 1, N),
    all_unique(Row).
all_rows_satisfied_P([], _).
all_rows_satisfied_P([First|Rest], N) :-
    row_satisfied_P(First, N),
    all_rows_satisfied_P(Rest, N).

%number in list all different, from TA slides
all_unique([ ]).
all_unique([H|T]) :-
    member(H, T), !, fail.
all_unique([_|T]) :- 
    all_unique(T).
elements_between(List, Min, Max) :-
    maplist(between(Min, Max), List).


plain_kenken(N, C, T) :-
    right_size(T, N),
    all_rows_satisfied_P(T, N),
    transpose(T, T_Trans),
    all_rows_satisfied_P(T_Trans, N),
    all_constraints_satisfied_P(T, C).

%=============== misc ================

kenken_testcase(
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

%================== report ======================
%plain_kenken: 
%Times              since start      since last
%
%   user   time     122.172 sec       2.782 sec
%   system time       0.209 sec       0.005 sec
%   cpu    time     122.381 sec       2.787 sec
%   real   time    1561.628 sec       2.789 sec

%kenken:
%Times              since start      since last
%
%   user   time     122.175 sec       0.000 sec
%   system time       0.214 sec       0.000 sec
%   cpu    time     122.389 sec       0.000 sec
%   real   time    1616.660 sec       0.000 sec

%with the script
%
%statistics,
%kenken(
%  4,
%  [
%   +(6, [[1|1], [1|2], [2|1]]),
%   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
%   -(1, [3|1], [3|2]),
%   -(1, [4|1], [4|2]),
%   +(8, [[3|3], [4|3], [4|4]]),
%   *(2, [[3|4]])
%  ],
%  T
%), write(T), nl, fail. 
%
%statistics.

%================ no-op ====================
%Constraints would just like in the original but
%without the operations
%A new term called Ops should be passed in,
%which is a list of a list of two elements like
%this: [+, [[1|2],[1|3]]], where the first
%element, a ground term, is the operation, and
%the second element is a list of squares it is
%associated to in the cage.

%The solver then will try to unify Ops and output
%the correct ops with each cage.

% EXAMPLE
% noop_kenken_testcase(
%  2,
%  [
%   [3, [[1|1], [2|1]]],
%   [1, [1|2], [1|3]]]
%  ]
%).
% 
% THEN CALL
% noop_kenken_testcase(N,C), noop_kenken(N,C,T,Ops).
%
% would return
% T = [[1,2],[3,4]]
% Ops = [[+,[[1|1], [2|1]]], [-,[1|2], [1|3]]]]

