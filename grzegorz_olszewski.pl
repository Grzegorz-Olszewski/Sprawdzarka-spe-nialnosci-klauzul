%:- module(grzegorz_olszewski, [solve/2]).
:- op(200, fx, ~).
:- op(500, xfy, v).

is_clause([]) :- !.
is_clause(C) :- is_non_empty_clause(C).

is_non_empty_clause(L) :- is_literal(L), !.
is_non_empty_clause(L v C) :-
  is_literal(L), is_non_empty_clause(C).

is_literal(X) :- is_variable(X), !.
is_literal(~X) :- is_variable(X).

is_variable([]) :- !, fail.
is_variable(X) :- atom(X).

clause_to_list(K, T):-
	clause_to_list(K, T, []).

clause_to_list(K,[K|T],T):-
	is_literal(K).

clause_to_list(L v C, F, K):-
	is_literal(L),
	is_non_empty_clause(C),
	clause_to_list(C, V, []),
	append(V,[L|K], F).

clause_to_list([],K,K).

is_empty_list([]).
clauses_to_list_of_lists(K, T):-
	clauses_to_list_of_lists(K, T, []).

clauses_to_list_of_lists([H|T],S,K):-
	clause_to_list(H,Z),
	sort(Z,V),
	clauses_to_list_of_lists(T,S,[V|K]).

clauses_to_list_of_lists([],K, K).

get_size(L,(L,Size)):-
	length(L,Size).

list_of_clauses_to_list_of_pairs(ListClauses, ListPairs):-
	maplist(get_size,ListClauses,ListPairs).

get_pairs(K,T):-
	clauses_to_list_of_lists(K,S),
	list_of_clauses_to_list_of_pairs(S,T).	

sort_list_by_length(Pairs, SortedPairs):-
	sort(2, @=<, Pairs, SortedPairs).

get_sorted_pairs(Clauses, SortedPairs):-
	get_pairs(Clauses, Pairs),
	sort_list_by_length(Pairs,SortedPairs).

	
vars(~X,X).
vars(X,X):-
	atom(X).

get_vars([],[]).
get_vars([H|T], Vars):-
	maplist(vars,H,Vars1), get_vars(T,Vars2), append(Vars1,Vars2,Vars3),sort(Vars3,Vars).

get_element([([H|_],_)|_],H).

make_a_pair(X,t,(X,t)).
make_a_pair(X,f,(X,f)).

find_and_move(H,Y1,V1,Y2,[Pair|V1]):-
	atom(H),
	select(H,Y1,Y2),
	make_a_pair(H,t,Pair).

find_and_move(H,Y1,V1,Y2,[Pair|V1]):-
	opposite(H,X),
	atom(X),
	select(X,Y1,Y2),
	make_a_pair(X,f,Pair).

get_pair_with_x(H,(H,x)).
merge([],V,V).
merge(V,Y,Z):-
	is_not_empty_list(V),
	maplist(get_pair_with_x,V,Z1),append(Z1,Y,Z).
opposite(~X,X).
opposite(X,~X):-
	atom(X).

check(X,T,T2):-
	check(X,T,[],T2).

check(_,[],T2,T2).

check(X,[(T1,_)|T2],T3,T4):-
	member(X,T1),
	check(X,T2,T3,T4).

check(X,[(T1,L)|T2],T3,T5):-
	opposite(X,H),
	member(H,T1),
	select(H,T1,T4),
	L2 is L-1,
	check(X,T2,[(T4,L2)|T3],T5).

check(X,[(T1,L)|T2],T3,T4):-
	not(member(X,T1)),
	opposite(X,H),
	not(member(H,T1)),
	not(is_empty_list(T1)),
	check(X,T2,[(T1,L)|T3],T4).
is_not_empty_list(X) :-
        var(X), !,
        fail.
is_not_empty_list([_|T]) :-
        is_list(T).
give_value(X,X):-
	atom(X).
give_value(~X,~X).
give_value(X,H):-
	opposite(X,H).


solve(T,Solution):-
	%is_not_empty_list(T),
	solve(T,[],[],Solution).

solve([H|T],[],[],Solution):-
	not(is_not_empty_list(H)),
	clauses_to_list_of_lists([H|T],ListOfClauses),
	get_vars(ListOfClauses,Variables),
	get_sorted_pairs([H|T],SortedPairs),
	get_element(SortedPairs,Element),	
	give_value(Element,ElementWithValue),
	find_and_move(ElementWithValue,Variables,[],Variables2,Values),
	check(ElementWithValue,SortedPairs,SortedPairs2),
	solve(SortedPairs2,Variables2,Values,Solution).

solve(SortedPairs,Variables,Values,Solution):-
	get_element(SortedPairs,Element),
	give_value(Element,ElementWithValue),
	find_and_move(ElementWithValue,Variables,Values,Variables2,Values2),
	check(ElementWithValue,SortedPairs,SortedPairs2),
	solve(SortedPairs2,Variables2,Values2,Solution).

solve([],Variables,Values,Solution):-
	merge(Variables,Values,Solution).

