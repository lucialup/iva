subsumes_(General,Specific) :-
	subsumes(General, Specific).        %SICStus

all_solutions(Var, Goal, Solutions) :-
	findall(Var, Goal, Solutions).       %SICStus

split(Term, [Term|Rest], Rest).
split(Term, [First|Rest0], [First|Rest]) :-
	split(Term, Rest0, Rest).

hash_term(Item, Hash) :-
	term_hash(Item, 2, 10, Hash).         %SICStus

%read_in(S) :- read(S).

read_in(S) :- exe(S),nl.      
