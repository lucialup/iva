
:- dynamic sentencelength/1.

:- dynamic word/2.

read_input :-
	read_in(S),
	format("~n  prop=    ~p~n",[S]),
	encode_sentence(S).

encode_sentence(Sentence) :- 
	retractall(word(_,_)),
	retractall(sentencelength(_)),
	encode_words(Sentence, 0, Length),
	assert(sentencelength(Length)).

encode_words(['.'], Length, Length) :- !.
encode_words([Word|Words], Length0, Length) :-
	Length1 is Length0 + 1,
	assert(word(Length1, Word)),
	encode_words(Words, Length1, Length).

