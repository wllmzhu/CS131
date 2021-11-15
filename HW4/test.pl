%get number at square [R|C]
get_ele(T, [R|C], Num) :-
    nth(R, T, Row),
    nth(C, Row, Num).


add_constraint(_, [], 0).
add_constraint(T, [First|Rest], S) :-
    get_ele(T, First, Num),
    add_constraint(T, Rest, Rest_S),
    S #= Num + Rest_S.

mul_constraint(_, [], 1).
mul_constraint(T, [First|Rest], P) :-
    get_ele(T, First, Num),
    add_constraint(T, Rest, Rest_P),
    P #= Num * Rest_P.


sub_constraint(T, J, K, D):-get_ele(T,J,Ele1), get_ele(T,K,Ele2),(D #=  Ele1 - Ele2; D #=  Ele2 - Ele1).

div_constraint(T, J, K, Q):-get_ele(T,J,Ele1), get_ele(T,K,Ele2),(Q #=  Ele1 / Ele2; Q #=  Ele2 / Ele1).



check_rule(T,+(S, L)):-add_constraint(T,L,S).
check_rule(T,*(P, L)):-mul_constraint(T,L,P).
check_rule(T,-(D, J, K)):-sub_constraint(T,J,K,D).
check_rule(T,/(Q, J, K)):-div_constraint(T,J,K,Q).

check_row_len([], _).
check_row_len([Head|Tail], N):-length(Head, N),check_row_len(Tail, N).

check_len(T,N):-length(T,N),check_row_len(T,N).

check_uniq([]).
check_uniq([Head|Tail]):-fd_all_different(Head),check_uniq(Tail).

check_rules(_, []).
check_rules(T, [Head|Tail]):- check_rule(T, Head), check_rules(T ,Tail).

test_emp([[]]).
test_emp([[]|Tail]):- test_emp(Tail).

change_fir([], [], []).
change_fir([[Row_head|Row_tail]|Tail_rows], [Row_head|Trans_Hs], [Row_tail|Row]):- change_fir(Tail_rows, Trans_Hs, Row).

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

check_domain(N, L) :- fd_domain(L, 1, N).

kenken(N,C,T):-
        check_len(T,N),
        check_uniq(T),
        transpose(T, Trans_T),
        check_uniq(Trans_T),
        maplist(check_domain(N), T),
        check_rules(T, C),
        maplist(fd_labeling, T).

make_test(0, []).
make_test(N, [N|Tail]):-N > 0, N_next is N - 1, make_test(N_next, Tail).

m_check(List, N):-make_test(N, Test_List), permutation(Test_List, List).

m_check_list([], 0, _).
m_check_list([Head|Tail], Nth, N):-Nth > 0, Next_N is Nth-1, m_check(Head,N),m_check_list(Tail, Next_N, N).


plain_kenken(N,C,T):- m_check_list(T, N, N), trans(T, Trans_T), m_check_list(Trans_T, N, N), check_rules(T, C).

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