lex(john,np).
lex(bananas,np).
lex(likes,(s - np) + np).
lex(really,((s - np) + (s - np))).

startsymbol(s).

exe([john,really,likes,bananas,.]).
