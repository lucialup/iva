init_chart :-
	retractall(stored(_,_)),
	retractall(key_index(_,_)).

item_in_chart(Item, RefIndex) :-
	item_stored(Item, ItemIndex),
	(ItemIndex =< RefIndex
		-> true
		; !, fail).
/*
item_in_chart(Item,Index) :-
	item_stored(Item, _).*/

item_in_chart(Item) :-
	item_stored(Item, _).

add_item_to_chart(Index) :-
	notify_chart_addition(Index).
