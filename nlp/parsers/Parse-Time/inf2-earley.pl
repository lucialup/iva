
%:- op(1200, xfx, --->).

% :- ensure_loaded(library(strings)).

initial_item(item('<start>',  [Start], 0, 0)) :- 
	startsymbol(Start).

/*final_item(item('<start>', [Start], [], 0, Length), Start) :-
	startsymbol(Start), 
	sentencelength(Length).*/

final_item(item(Start, [],  0, Length), Start) :-
	startsymbol(Start), 
	sentencelength(Length).

item_to_key(item(_A,  [B|_Beta], _I, J), Hash) :-
	B =.. [Bcat|_],
	hash_term(a(J, Bcat), Hash).

item_to_key(item(A,  [], I, _J), Hash) :-
	A =.. [Acat|_],
	hash_term(p(I, Acat), Hash).

inference(scanner, 
	[item(A,  [B|Beta], I, J )],
	item(A,  Beta, I, J1),
	[J1 is J + 1, 
	word(J1, Bterm),
	lex(Bterm, B)]).

inference(predictor,
	[item(_A,  [B|_Beta], _I, J)],
	item(B,  Gamma, J, J),
	[(B ---> Gamma)]).

inference(completor,
	[item(A,  [B|Beta], I, J),
	item(B , [], J, K) ],
	item(A,  Beta, I, K),
	[]).

test:- tell('rez_ded_earley2.doc'),parse(X).
testf:-tell('rez_ded_earley2-trasare.doc'),assert(trasare),parse(X),nl,nl,listing(word),listing(stored),listing(key_index),retract(trasare).
