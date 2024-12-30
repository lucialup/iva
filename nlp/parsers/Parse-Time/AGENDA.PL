is_empty_agenda(queue(Front, Back)) :-
	Front >= Back.

empty_agenda(queue(0,0)).

pop_agenda(queue(Front, Back),
		Front, queue(NewFront, Back)) :-
	Front < Back, 
	NewFront is Front + 1.

add_item_to_agenda(Item, queue(Front, Back),
	queue(Front, NewBack)) :-
	(\+ subsumed_item(Item)
	-> (assertz(stored(Back, Item)),
	   item_to_key(Item, Key),
	   assert(key_index(Key, Back)),
	   NewBack is Back + 1,
	   notify_agenda_addition(Item));
	NewBack = Back).

add_items_to_agenda([], Agenda, Agenda).
add_items_to_agenda([Item|Items], Agenda0, Agenda) :-
	add_item_to_agenda(Item, Agenda0, Agenda1),
	add_items_to_agenda(Items, Agenda1, Agenda).


index_to_item(Index, Item) :-
	stored(Index, Item).


	