:- dynamic arc/5.

inits(ut).

r(ut, [s]).
r(ut, [s, coord, s]).
r(s, [np2, vp2]).
r(s, [advp, np2, vp2]).
r(advp, [pp]).
r(np2, [np1]).
r(np2, [np1, coord, np1]).
r(np1, [det, n1]).
r(np1, [det, a2, pp]).
r(np1, [num, np1]).
r(np1, [n1, a2]).
r(np1, [a2, n1]).
r(np1, [n1]).
r(np1, [pn]).
r(np1, [pro]).
r(np1, [relpro]).
r(np1, [pp]).

r(n1, [n0]).
r(n1, [n0, n0]).
r(n1, [n0, pp]).
r(n1, [n0, rc]).
r(n0, [noun]).
r(n0, [p_nehot]).

r(a2, [a1]).
r(a2, [a1, coord, a1]).
r(a1, [a0]).
r(a1, [det, a1]).
r(a0, [adj]).
r(a0, [ppos]).

r(vp2, [vp1]).
r(vp2, [vp1, coord, vp1]).
r(vp1, [pro, v1]).
r(vp1, [pro, v1, adv]).
r(vp1, [v1]).
r(vp1, [v1, adv]).

r(v1, [iv]).
r(v1, [iv, pps]).
r(v1, [tv, np2]).
r(v1, [tv, pp]).
r(v1, [tv, adv, np2]).
r(v1, [tv, sc]).
r(v1, [iv, adv]).

r(pps, [pp]).
r(pps, [pp, pps]).
r(pp, [prep, np2]).
r(pp, [pentru, inf_clause]).
r(inf_clause, [a, tv, np2]).
r(sc, [sa, v1]).
r(rc, [relpro, vp2]).

% Lexicon
w(coord, si).
w(coord, iar).
w(coord, sa).

w(prep, in).
w(prep, pe).
w(prep, de).
w(prep, la).
w(prep, din).

w(det, o).
w(det, cel).
w(det, cele).
w(det, mai).
w(det, fiecare).

w(noun, ana).
w(noun, saptamana).
w(noun, biblioteca).
w(noun, orasului).
w(noun, carti).
w(noun, bibliotecarul).
w(noun, romane).
w(noun, poezii).
w(noun, stiinta).
w(noun, sfarsitul).
w(noun, zilei).

w(p_nehot, tot).

w(tv, merge).
w(tv, imprumuta).
w(tv, recomanda).
w(tv, alege).
w(tv, atrag).
w(tv, incepe).
w(tv, citeasca).

w(iv, merge).
w(iv, citeste).
w(iv, citeasca).
w(iv, intoarce).

w(relpro, care).
w(relpro, ce).

w(pn, ana).
w(pro, ii).
w(pro, le).
w(pro, se).
w(pro, o).

w(ppos, meu).
w(ppos, mea).

w(adj, noi).
w(adj, mult).
w(adj, atragatoare).

w(adv, entuziasmata).
w(adv, acasa).
w(adv, adesea).
w(adv, mai).
w(adv, cel_mai_mult).

w(num, un).
w(num, o).



test(S):-
    V0 is 1,
    inits(Sym),
    parse(V0,Vn,S),
    dofor(arc(V0,Vn,Sym,[],As),mwrite(As)),
    nl,
    retract2.

parse(V0,Vn,S):-
    init_chart(V0,Vn,S),
    inits(Sym).

init_chart(V,V,[]).
init_chart(V0,Vn,[W|Rw]):-
    V1 is V0+1,
    dofor(w(N,W),add_arc(V0,V1,N,[],[W,N])),
    init_chart(V1,Vn,Rw).

add_arc(V1, V2, Ln, RRn, Ap):- 
    arc(V1, V2, Ln, RRn, Ap),!.  
add_arc(V1, V2, Ln, [], Aln):-
    asserta(arc(V1, V2, Ln, [], Aln)),
    dofor(r(NLn,[Ln|RRn]),
        add_arc(V1,V1,NLn,[Ln|RRn],[NLn])),
    dofor(arc(V0, V1, NLn, [Ln|RRn], ANLn),
        add_arc(V0, V2, NLn, RRn, [Aln|ANLn])).
add_arc(V1, V2, Ln, [FRn|RRn], Aln):-
    asserta(arc(V1, V2, Ln, [FRn|RRn], Aln)),
    dofor(arc(V2, V3, FRn, [], AFRn),
        add_arc(V1, V3, Ln, RRn, [AFRn|Aln])).

dofor(X, Y):- X, do(Y), fail.
dofor(X, Y).
do(Y):- Y, !.

mwrite(Arb):- invers(Arb, Ia), write(Ia), nl.

invers([], []):- !.
invers(A, A):- atomic(A).
invers([H|T], Ia):- invers(H, Ih), invers(T, It), append(It, [Ih], Ia).

append([], L, L).
append([H|T], L, [H|S]):- append(T, L, S).

retract2:-
    retract(arc(A,B,C,D,E)), 
    X = arc(A,B,C,D,E), 
    write(X),
    nl,
    retract2.
retract2.

% Test
test1 :- test(['in', 'fiecare', 'saptamana', 'ana', 'merge', 'la', 'biblioteca', 'orasului', 
               'pentru', 'a', 'imprumuta', 'carti', 'noi']).

test2 :- test(['bibliotecarul', 'ii', 'recomanda', 'adesea', 'romane', 'poezii', 'si', 
               'carti', 'de', 'stiinta']).

test3 :- test(['ana', 'le', 'alege', 'pe', 'cele', 'atragatoare', 'din', 'biblioteca']).

test4 :- test(['la', 'sfarsitul', 'zilei', 'se', 'intoarce', 'acasa', 'si', 'incepe', 
               'sa', 'citeasca', 'entuziasmata']).

test_all:-
    tell('chart-bu-time.doc'),
    test1,
    test2,
    test3,
    test4,
    told.

test_time:-
    statistics(walltime, [Start,_]),
    test_all,
    statistics(walltime, [End,_]),
    Time is End - Start,
    nl,
    format('All solutions in ~3d seconds.~n', [Time]).