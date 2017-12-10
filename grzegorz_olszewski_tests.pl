
:- module(grzegorz_olszewski_tests, [tests/5]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

tests(double_same_literal, validity, [p,p], 500, solution([(p,t)])).
tests(literal_and_its_negation, validity, [p,~p], 500, count(0)).
tests(excluding_literal_from_alternative, validity, [p, ~p v r], 500, solution([(p,t),(r,t)])).
tests(alternatives_literals_and_negated_literals, validity, [p v r, ~p v ~r], 500, count(2)).
tests(literals_and_alternative_of_their_negations, validity, [p, r, ~p v ~r], 500, count(0)).
tests(test1, validity, [p, q, p v r, q v s, r v s], 500, count(3)).
tests(test2, validity, [p, p v q v r, ~q v ~r ], 500, count(3)).
tests(literal_or_negated_literal, validity, [~p v r], 500, count(3)).
tests(dont_care_value, validity, [p v q, p , ~q, ~q v r], 500, solution([(p,t),(q,f),(r,f)])).
tests(triple_alternative,validity, [p v q v r], 500, count(7)).
tests(simple, validity, [p], 500, solution([(p,t)])).
tests(negation, validity, [~p], 500, solution([(p,f)])).
tests(empty, validity, [], 500, count(0)).
tests(one_unsolvable, validity, [p, ~p], 500, count(0)).
tests(two_unsolvable, validity, [p v q, ~p v q, ~q v p, ~q v ~p], 2500, count(0)).
tests(four_unsolvable, validity, [p v q, ~q v ~r, s v r, s v q, ~s v ~p, r v p], 3500, count(0)).
tests(one_solution, validity, [p, p v ~q, ~q], 1000, solution([(p,t),(q,f)])).
tests(two_solutions, validity, [p, p v ~q, p v q], 1000, solution([(p,t),(q,f)])).
tests(multiple_solutions, validity, [p v q, p v ~r, ~p v ~r], 3000, solution([(p,f),(q,t),(r,f)])).
tests(unsolvable_with_empty, validity, [p, [], q v p, q], 1000, count(0)).
tests(repeatable, validity, [p, p, p, p v p, p, p], 1500, solution([(p,t)])).
tests(many_odd, validity, [p v q, n v m, z v o, z], 2000, solution([(p,t), (q,f),(n,t),(m,f),(z,t),(o,f)])).
tests(quick_result, performance, [p v r v h, ~p, ~r, ~h], 1000, count(0)).
tests(medium_result, performance, [p v r, ~p v ~r, ~r v ~h, ~h v ~p, h v p, h v r, r v h], 5000, count(0)).
tests(slow_result, performance, [p v r, p v h, ~h v ~p, ~z v ~p, ~z v ~h, g v ~z, g v ~h, p v g, g v ~p, r v z, r v h, g v ~r], 10000, solution([(p,t),(g,t),(h,f),(z,f),(r,t)])).
%validity tests:
tests(emptyClause, validity, [], 500, count(0)).

tests(conjunction, validity, [p, q], 500, count(1)).

tests(contradiction, validity, [p,~p], 500, count(0)).

tests(tautology, validity, [p v ~p], 500, count(2)).

tests(withEmptyClauseVal, validity, [p v r ,s v t v p ,q v r ,t ,q ,q v p ,[] ,e], 500, count(0)).

tests(clauseWithEmptyClause, validity, [[]], 500, count(0)).

tests(contradiction2, validity, [a v b, ~b v ~c, d v c, d v b,~a v ~d, c v a], 700, count(0)).

tests(test1, validity, [p v p], 500, count(1)).

tests(test2, validity,[p v q v r, ~r v ~q v ~p, ~q v r, ~r v p],500, solution([(r,t),(p,t),(q,f)])).

tests(test3, validity, [p, r, s, t, u, w, y, z], 500, count(1)).

tests(test4, validity, [p v q v s, p v u], 500, solution([(p,t),(q,t),(s,t),(u,t)])).

tests(test5, validity, [a v p v q] , 500, count(7)).

tests(test6,  validity, [a v p v q, u v r v s, d v l v b],800, count(343)).

tests(test7, validity, [p v q v ~r v s v ~t v ~e, p, a v ~s, s v e], 800, solution([(p,t), (q,f), (r,f), (s,f), (t,f),(e,t), (a,t)])).

tests(test8, validity, [p v r v ~s, g v r, ~q v a, u v ~r], 500, solution([(p,f),(r,f),(s,f),(g,t),(q,f),(a,t),(u,t)])).

tests(test9, validity, [p v ~a, ~a v r v s, ~p, ~r v ~s, a v s,a v r],500, count(0)).

tests(test10, validity, [p v ~a v r v s v u, p v a v ~r v u], 500, solution([(p,f),(a,t),(r,f),(s,t),(u,f)])).

%performance tests:

tests(withEmptyClausePerf, performance, [p v r ,s v t v p ,q v r ,t ,q ,q v p ,[],e], 500, count(0)).

tests(conjunctionOfLiterals, performance, [p ,q ,r ,s ,t ,u, w, y, i, l, z], 700, solution([(p,t),(q,t),(r,t),(s,t),(t,t),(u,t),(w,t), (y,t), (i,t), (l,t), (z,t)])).

tests(test11, performance, [p v r v t v f v ~p v e ,r ], 500, solution([(p,f),(r,t),(t,f),(f,f),(e,t)])).

tests(test12, performance, [p v q v ~r v s v ~t v ~e, p, a v ~s, s v e], 800, solution([(p,t), (q,f), (r,f), (s,f), (t,f),(e,t), (a,t)])).

tests(test13, performance, [p v ~a, r v q, u v a, r v ~d, ~s v ~k], 500, count(60)).

tests(test14, performance, [p v a v s, u v e v i v k v q, f v r], 500, count(651)).

tests(test15, performance,[p, s v q, a v ~r, t, t v s v u v a, ~p v ~r, s, ~r], 500, count(8)).

tests(test16, performance, [p, d, f v s, q, r, s v d v f, f, g, h v r, q v a, ~p, g], 500, count(0)).

tests(test17, performance, [p v a v r, f v s v d, ~p, ~f v ~z, ~a, f v a, ~r v ~a, q v ~f], 500, count(4)).

tests(test18, performance, [~p v ~r v r v s v ~s v u v t v q],500, solution([(p,t),(r,t),(s,f),(u,f),(t,f),(q,t)])).


tests(t1,performance,[p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v r],500, solution([(p,t),(r,t)])).
tests(one_literal, validity, [p], 1000, solution([(p, t)])).
tests(negated_literal, validity, [~p], 1000, solution([(p, f)])).

tests(two_same, validity, [p, p], 1000, solution([(p, t)])).
tests(two_same_c, validity, [p, p], 1000, count(1)).

tests(two_negations, validity, [~p, ~p], 1000, solution([(p, f)])).
tests(two_negations_c, validity, [~p, ~p], 1000, count(1)).

%% Basic with alternatives

tests(excluded_middle, validity, [~p v p], 1000, solution([(p, t)])).

tests(doubled, validity, [p v q, ~p v ~q], 1000, solution( [(p, t), (q, f)])).
tests(doubled_c, validity, [p v q, ~p v ~q], 1000, count(2)).

%% Three-variables



tests(basic2_c, validity, [p v q v r, ~p v ~q v ~r], 1000, count(6)).

tests(basic3, validity, [p v q v r, ~r v ~q v ~p, ~q v r, ~r], 3000,
              solution( [(p, t), (r, f), (q, f)])).

%% Unsolvable tests

tests(unsolvable_basic, validity, [p, ~p], 1500, count(0)).
tests(unsolvable_alternative, validity, [p v q, ~p, ~q], 1500, count(0)).
tests(unsolvable_complex, validity, [p v q v ~r, ~p, ~q v p, r], 1500, count(0)).

%% Multiple variables

tests(fourVars, validity, [p v q v r v w, ~p, ~r, q], 3000,
                solution( [(w, t), (p, f), (r, f), (q, t)])).
tests(fourVars_c, validity, [p v q v r v w, ~p, ~r, q], 3000, count(2)).

tests(fiveVars, validity, [p v q v r v w, ~p, ~r, q, p v ~q v o, w v r], 4000,
                solution( [(p, f),  (r, f),  (q, t),  (o, t),  (w, t)])).

%% Chaining

tests(chaining, validity,
      [p, ~p v q, ~q v r, ~r v w, ~w v o, ~o v k], 5000,
      solution([(p, t),  (q, t),  (r, t),  (w, t),  (o, t),  (k, t)])).
tests(chaining_c, validity,
      [p, ~p v q, ~q v r, ~r v w, ~w v o, ~o v k], 5000, count(1)).
