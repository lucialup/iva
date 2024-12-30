time :-
        statistics(walltime, [Start,_]),
        test,nl,nl,
        statistics(walltime, [End,_]),
        Time is End - Start,
        format('all solutions in ~3d seconds.~n', [Time])
                              ,told.
timef :-
        statistics(walltime, [Start,_]),
        testf,nl,nl,
        statistics(walltime, [End,_]),
        Time is End - Start,
        format('all solutions in ~3d seconds.~n', [Time])
                              ,told.

parse(Value) :-
	read_input,
	init_chart,
	init_agenda(Agenda),
	exhaust(Agenda),
	final_item(Goal, Value),
	item_in_chart(Goal),
	format("~n   arb.synt.=  ~p",[Value]),fail.
parse(_).

init_agenda(Agenda) :- 
	initial_items(Axioms),  
	empty_agenda(Empty),
	add_items_to_agenda(Axioms, Empty, Agenda).

exhaust(Empty) :-
	is_empty_agenda(Empty),!.

exhaust(Agenda0) :-
	pop_agenda(Agenda0, Index, Agenda1),
	add_item_to_chart(Index),
	add_consequences_to_agenda(Index, Agenda1, Agenda),
	exhaust(Agenda).

add_consequences_to_agenda(Index, Agenda0, Agenda) :-
	all_solutions(Consequence, 
		consequence(Index, Consequence),
		Consequences),
	add_items_to_agenda(Consequences, Agenda0, Agenda).

consequence(Index, Consequent) :-
	index_to_item(Index, Trigger),
	matching_rule(Trigger, RuleName, Others, Consequent, SideConds),
	items_in_chart(Others, Index),
	hold(SideConds),
	notify_consequence(RuleName, Trigger, Others, SideConds, Consequent).

items_in_chart([], _Index).
items_in_chart([Antecedent|Antecedents], Index) :-
	item_in_chart(Antecedent, Index),
	items_in_chart(Antecedents, Index).

hold([]).
hold([Cond|Conds]) :-
	call(Cond),
	hold(Conds).

matching_rule(Trigger, RuleName, Others, Consequent, SideConds) :-
	inference(RuleName, Antecedent, Consequent, SideConds),
	split(Trigger, Antecedent, Others).
/*
test(X):-parse(X).
testf(X):-assert(trasare),parse(X),nl,nl,listing(word),listing(stored),listing(key_index),retract(trasare).
*/
nc:- unix(shell(nc)).               %SICStus
