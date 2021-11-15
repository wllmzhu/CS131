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



