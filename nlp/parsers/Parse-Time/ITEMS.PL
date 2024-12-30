
:- dynamic stored/2.

:- dynamic key_index/2.


item_stored(Item, Index) :-
	item_to_key(Item, Key),
	key_index(Key, Index),
	stored(Index, Item).

similar_item(Item, SimilarItem) :-
	item_to_key(Item, Key),
	key_index(Key, IndexofSimilar),
	stored(IndexofSimilar, SimilarItem).

subsumed_item(Item) :-
	similar_item(Item, OtherItem), 
	subsumes_(OtherItem, Item).

:- ensure_loaded('chart').
:- ensure_loaded('agenda').
	