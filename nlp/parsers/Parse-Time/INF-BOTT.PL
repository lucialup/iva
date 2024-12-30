%:- op(1200, xfx,--->).

initial_item(item([],0)).

final_item(item([Value], Length), Value) :-
	sentencelength(Length),
	startsymbol(Value).

item_to_key(item([First|_], I), Index) :-
	First =..[Firstcat|_],
	hash_term(a(I, Firstcat), Index).

item_to_key(item([],I), Index) :-
	hash_term(a(I, none), Index).

inference(reduce,
	[item(BetaAlpha, I)],
	item([A|Alpha], I),
	[append(BetaR,Alpha,BetaAlpha),
         reverse(BetaR,Beta),
         (A ---> Beta)]).

inference( shift,
	[ item(Beta, I)],
	item([B|Beta], I1),
	[I1 is I + 1,
	word(I1, Bterm),
	lex(Bterm, B)]).

test:- tell('rez_ded_bott_up.doc'),parse(X).
testf:-tell('rez_ded_bott_up-trasare.doc'),assert(trasare),parse(X),nl,nl,listing(word),listing(stored),listing(key_index),retract(trasare).



