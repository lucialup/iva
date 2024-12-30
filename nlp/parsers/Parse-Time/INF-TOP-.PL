%:- op(1200, xfx, --->).

initial_item(item([Start], 0, Start)) :-
	startsymbol(Start).

final_item(item([], Length, Value), Value) :-
	sentencelength(Length).

item_to_key(item([First|_], I, _Value), Index) :-
	First =..[Firstcat|_],
	hash_term(a(I, Firstcat), Index).

item_to_key(item([], I, _Value), Index) :-
	hash_term(a(I, none), Index).

inference( scanner,
	[ item([B|Beta], I, Value) ],
	item(Beta, I1, Value),
	[I1 is I + 1,
	word(I1, Bterm),
	lex(Bterm, B)]).

inference( predictor,
	[ item([A|Alpha], I, Value) ],
	item(BetaAlpha, I, Value),
	[(A--->Beta), 
	append(Beta, Alpha, BetaAlpha)]).

test:- tell('rez_ded_top_down.doc'),parse(X).
testf:-tell('rez_ded_top_down-trasare.doc'),assert(trasare),parse(X),nl,nl,listing(word),listing(stored),listing(key_index),retract(trasare).
