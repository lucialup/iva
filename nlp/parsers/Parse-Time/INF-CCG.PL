initial_item(item(Cat, I, J1, [Cat, Word])) :-
	word(J1, Word),
	lex(Word, Cat),
	I is J1 - 1.

final_item(item(Start, 0, Length, D), D) :-
	startsymbol(Start),
	sentencelength(Length).

item_to_key(item(_,_,_,_),i).

inference(forward-application,
	[item(X + Y, I, J, D1), item(Y, J, K, D2)],
	item(X, I, K, [X,D1, D2]),
	[]).

inference(backword-application, 
	[item(Y, I, J, D1), item(X - Y, J, K, D2)],
	item(X, I, K, [X,D1, D2]),
	[]).

inference(forward-composition1,
	[item(X + Y, I, J, D1), item(Y + Z, J, K, D2)],
	item(X + Z, I, K, [X+Z,D1, D2]),
	[]).

infernce(forward-composition2,
	[item(X + Y, I, J, D1), item(Y - Z, J, K, D2)],
	item(X - Z, I, K, [X-Z,D1, D2]),
	[]).

inference(backword-composition1,
	[item(Y + Z, I, J, D1), item(X - Y, J, K, D2)],
	item(X + Z, I, K, [X+Z,D1, D2]),
	[]).

inference(backword-composition2,
	[item(Y - Z, I, J, D1), item(X - Y, J, K, D2)],
	item(X - Z, I, K, [X-Z,D1, D2]),
	[]).

test:- tell('rez_ded_ccg.doc'),parse(X).


test:- tell('rez_ded_ccg.doc'),parse(X).
testf:-tell('rez_ded_ccg-trasare.doc'),assert(trasare),parse(X),nl,nl,listing(word),listing(stored),listing(key_index),retract(trasare).
