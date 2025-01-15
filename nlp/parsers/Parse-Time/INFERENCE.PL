%  :- ensure_loaded('inf-top-.pl').

 % :- ensure_loaded('inf-bott.pl').

 :- ensure_loaded('inf-earley.pl').

 %:- ensure_loaded('inf2-earley.pl').

%:- ensure_loaded('inf-ccg.pl').

% :- ensure_loaded('inf-cyk.pl').

%:- ensure_loaded('inf-kay-td.pl').

%:- ensure_loaded('inf-kay-bu.pl').

%:- ensure_loaded('inf-kay-tdbu.pl').

initial_items(Items) :-
	all_solutions(Item, initial_item(Item), Items).

