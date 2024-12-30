
nc(nc(N)) ---> [n(N)].
nc(nc(N1,N2)) ---> [nc(N1),nc(N2)].

lex(skin,n(n(skin))).
lex(desease,n(n(desease))).
lex(treatment,n(n(treatment))).
lex(elephant,n(n(elephant))).

startsymbol(nc(_)).



exe([elephant,skin,desease,treatment,.]).
exe([skin,desease,treatment,.]).

