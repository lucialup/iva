:- dynamic trasare/0.

notify_consequence(RuleName, Trigger, Others,
		SideConds, Consequent) :-
	(trasare ->
		format("~p:~n pivot: ~p ~n",
			[RuleName, Trigger]),
	format(" others: ~p ~n", [Others]),
		format(" constrains: ~p ~n", [SideConds]),
		format(" conseqvent: ~p ~n", [Consequent])
		; true).

notify_agenda_addition(Item) :-
	(trasare
		-> (format('Adding to agenda: <-> ~p ~n', [Item]))
		;true ).    % (print('.'),ttyflush)).

notify_chart_addition(Index) :-
	index_to_item(Index, Item),

item_to_key(Item, Key),
	(trasare
		-> (format('Adding to chart: <~p> ~p ~n',
				[Key, Item]))
		;true).   % (print(':'), ttyflush)).