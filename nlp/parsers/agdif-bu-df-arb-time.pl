inits(ut).

r(ut, [s]).
r(ut, [ut, coord, s]).

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
r(pp, ['pentru', inf_clause]).

r(inf_clause, ['a', tv, np2]).
r(sc, ['sa', v1]).
r(rc, [relpro, vp2]).

% Lexicon
w(coord, 'si').
w(coord, 'iar').
w(coord, 'sa').

w(prep, 'in').
w(prep, 'pe').
w(prep, 'de').
w(prep, 'la').
w(prep, 'din').
w(prep, 'pentru').

w(det, 'o').
w(det, 'cel').
w(det, 'cele').
w(det, 'mai').
w(det, 'fiecare').

w(noun, 'ana').
w(noun, 'saptamana').
w(noun, 'biblioteca').
w(noun, 'orasului').
w(noun, 'carti').
w(noun, 'bibliotecarul').
w(noun, 'romane').
w(noun, 'poezii').
w(noun, 'stiinta').
w(noun, 'sfarsitul').
w(noun, 'zilei').

w(p_nehot, 'tot').

w(tv, 'merge').
w(tv, 'imprumuta').
w(tv, 'recomanda').
w(tv, 'alege').
w(tv, 'atrag').
w(tv, 'incepe').
w(tv, 'citeasca').

w(iv, 'merge').
w(iv, 'citeste').
w(iv, 'citeasca').
w(iv, 'intoarce').

w(relpro, 'care').
w(relpro, 'ce').

w(pn, 'ana').
w(pro, 'ii').
w(pro, 'le').
w(pro, 'se').
w(pro, 'o').

w(ppos, 'meu').
w(ppos, 'mea').

w(adj, 'noi').
w(adj, 'mult').
w(adj, 'atragatoare').

w(adv, 'entuziasmata').
w(adv, 'acasa').
w(adv, 'adesea').
w(adv, 'mai').
w(adv, 'cel_mai_mult').

w(num, 'un').
w(num, 'o').

w('a', 'a').
w('sa', 'sa').
w('pentru', 'pentru').

% utils
member(X,[X|_]):-!.
member(X,[_|Y]):-member(X,Y).

append([H|X],Y,[H|Z]):-append(X,Y,Z).
append([],X,X).

findall(X,Y,Z):-setof(X,Y,Z),!.
findall(X,Y,[]).

write_chart([]).
write_chart([H|T]):-write_arc(H),nl,write_chart(T).

write_arc(arc(V0,V1,Ln,Rn,As)):-
    write_dif(V0,V1),write(Ln),write('->.'),write(Rn),write('  As='),mwrite(As).

write_dif(L,L):-write(' = ').
write_dif([H|T],L):-write(H),write('_'),write_dif(T,L).

mwrite(Arb):- invers(Arb, Ia), write(Ia), nl.

invers([], []):- !.
invers(A, A):- atomic(A).
invers([H|T], Ia):- invers(H, Ih), invers(T, It), append(It, [Ih], Ia).

% parser
init_agenda([W|RW],V0,Agenda):-
    V1 is V0+1,
    findall(arc([W|RW],RW,Ln,[],[W,Ln]),w(Ln,W),AgW),
    init_agenda(RW,V1,AgRW),
    append(AgW,AgRW,Agenda).
init_agenda([],_,[]).

expand_arc([],Chart,Chart).
expand_arc([Arc|Rarc],Chart,NewChart):-
    member(Arc,Chart),!,
    expand_arc(Rarc,Chart,NewChart).
expand_arc([Arc|Rarc],Chart,NewChart):-
    Pchart=[Arc|Chart],
    newarce(Arc,Pchart,NewArce),
    add_arce(NewArce,Rarc,NewAgenda),
    expand_arc(NewAgenda,Pchart,NewChart).

add_arce([],Arce,Arce).
add_arce([Arc|Rarc],Arce,NewArce):-
    add_1arc(Arc,Arce,Parce),
    add_arce(Rarc,Parce,NewArce).

add_1arc(Arc,Arce,Arce):-member(Arc,Arce),!.
add_1arc(Arc,Arce,[Arc|Arce]).

newarce(arc(V1,V2,Ln,[],ALn),Chart,Newarce):-
    findall(arc(V1,V1,NLn,[Ln|RRn],[NLn]),r(NLn,[Ln|RRn]),Arce1),
    findall(arc(V0,V2,NLn,RRn,[ALn|ANLn]),member(arc(V0,V1,NLn,[Ln|RRn],ANLn),Chart),Arce2),
    add_arce(Arce1,Arce2,Newarce).
newarce(arc(V1,V2,Ln,[FRn|RRn],ALn),Chart,Newarce):-
    findall(arc(V1,V3,Ln,RRn,[AFRn|ALn]),member(arc(V2,V3,FRn,[],AFRn),Chart),Newarce).

% Test
test(S):-
    init_agenda(S,0,Agenda),
    %write_chart(Agenda),nl,
    expand_arc(Agenda,[],Chart),
    %write_chart(Chart),
    inits(Sym),
    member(arc(S,[],Sym,[],As),Chart),
    nl,write('Prop='),write(S),
    nl,write('Arb='),mwrite(As),nl.

test1 :- test(['in', 'fiecare', 'saptamana', 'ana', 'merge', 'la', 'biblioteca', 'orasului', 
               'pentru', 'a', 'imprumuta', 'carti', 'noi']).

test2 :- test(['bibliotecarul', 'ii', 'recomanda', 'adesea', 'romane', 'poezii', 'si', 
               'carti', 'de', 'stiinta']).

test3 :- test(['ana', 'le', 'alege', 'pe', 'cele', 'atragatoare', 'din', 'biblioteca']).

test4 :- test(['la', 'sfarsitul', 'zilei', 'se', 'intoarce', 'acasa', 'si', 'incepe', 
               'sa', 'citeasca', 'entuziasmata']).

test_all :-
    tell('agdif-bu-df-arb-time.doc'),
    test1,
    test2,
    test3,
    test4.
    %told.

test_time:-
    statistics(walltime, [Start,_]),
    test_all,
    statistics(walltime, [End,_]),
    Time is End - Start,
    format('All solutions in ~3d seconds.~n', [Time]),told.