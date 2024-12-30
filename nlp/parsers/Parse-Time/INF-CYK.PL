%:- op(1200, xfx, --->).

% :- ensure_loaded(library(strings)).

initial_item( item(A,I,I1) ):-
		word(I1, Aterm),
		lex(Aterm, A),
		I is I1-1.			   

final_item(item(Start,   0, Length), Start) :-
	startsymbol(Start), 
	sentencelength(Length).
/*
item_to_key(item(A,_I,J), Hash) :-
	A =.. [Acat|_],
	hash_term(a(J, Acat), Hash).
*/
 
item_to_key(item(A,   I, J), Hash) :-
%        nonvar(A),
%	A =.. [Acat|_],
	hash_term(p(I, J), Hash).


inference(cyk2,
	[item(B,  I, J),
	item(C ,  J, K) ],
	item(A,   I, K),
	[nonvar(B),nonvar(C),(A ---> [B,C])]).


inference(cyk1,
	[item(B,  I, J) ],
	item(A,   I, J),
	[(A ---> [B])]).

test:- tell('rez_ded_cyk.doc'),parse(X).
testf:-tell('rez_ded_cyk-tasare.doc'),assert(trasare),parse(X),nl,nl,listing(word),listing(stored),listing(key_index),retract(trasare).
