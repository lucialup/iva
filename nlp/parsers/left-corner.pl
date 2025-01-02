leaf(noun)-->[biblioteca].
leaf(noun)-->[bibliotecarul].
leaf(noun)-->[orasului].
leaf(noun)-->[carti].
leaf(noun)-->[romane].
leaf(noun)-->[poezii].
leaf(noun)-->[stiinta].
leaf(noun)-->[sfarsitul].
leaf(noun)-->[zilei].
leaf(noun)-->[saptamana].
leaf(noun)-->[ana].

leaf(p_nehot)-->[tot].

leaf(tv)-->[merge].
leaf(tv)-->[imprumuta].
leaf(tv)-->[recomanda].
leaf(tv)-->[alege].
leaf(tv)-->[incepe].
leaf(tv)-->[citeasca].

leaf(iv)-->[merge].
leaf(iv)-->[citeste].
leaf(iv)-->[intoarce].
leaf(iv)-->[citeasca].

leaf(adv)-->[adesea].
leaf(adv)-->[acasa].
leaf(adv)-->[entuziasmata].
leaf(adv)-->[mai].
leaf(adv)-->[cel].
leaf(adv)-->[mult].

leaf(pro)-->[ii].
leaf(pro)-->[le].
leaf(pro)-->[se].
leaf(pro)-->[o].

leaf(coord)-->[si].
leaf(coord)-->[iar].
leaf(coord)-->[','].

leaf(prep)-->[in].
leaf(prep)-->[pe].
leaf(prep)-->[de].
leaf(prep)-->[la].
leaf(prep)-->[din].
leaf(prep)-->[pentru].

leaf(det)-->[o].
leaf(det)-->[cel].
leaf(det)-->[cele].
leaf(det)-->[mai].
leaf(det)-->[fiecare].
leaf(det)-->[sa].

leaf(pn)-->[ana].

leaf(adj)-->[noi].
leaf(adj)-->[mult].
leaf(adj)-->[atragatoare].

leaf(a)-->[a].



lcd(s,ut):-!.
lcd(np2,s):-!.
lcd(np1,np2):-!.
lcd(det,np1):-!.
lcd(n1,np1):-!.
lcd(a2,np1):-!.
lcd(pp,np1):-!.
lcd(n0,n1):-!.
lcd(noun,n0):-!.
lcd(p_nehot,n0):-!.
lcd(vp1,vp2):-!.
lcd(v1,vp1):-!.
lcd(pro,vp1):-!.
lcd(pro,np1):-!.
lcd(pn,np1):-!.
lcd(iv,v1):-!.
lcd(tv,v1):-!.
lcd(prep,pp):-!.
lcd(adv,advp):-!.
lcd(advp,s):-!.
lcd(pp,advp):-!.
lcd(pp,s):-!.
lcd(a0,a1):-!.
lcd(a1,a2):-!.
lcd(adj,a0):-!.
lcd(pp,ut):-!.
lcd(np2,ut):-!.
lcd(inf_clause,ut):-!.

lc(X,X):-!.
lc(X,Y):-lcd(X,Y),!.
lc(X,Y):-lcd(X,Z),lc(Z,Y).


parse(Nterm,As,[W0|W1],Wn):-
    leaf(Pterm,[W0|W1],W1),
    lc(Pterm,Nterm),
    Ap=..[Pterm,W0],
    P=..[Pterm,Nterm,Ap,As,W1,Wn],
    call(P).

coord(coord,A,A)-->[].
prep(prep,A,A)-->[].
det(det,A,A)-->[].
noun(noun,A,A)-->[].
p_nehot(p_nehot,A,A)-->[].
tv(tv,A,A)-->[].
iv(iv,A,A)-->[].
pro(pro,A,A)-->[].
pn(pn,A,A)-->[].
adj(adj,A,A)-->[].
adv(adv,A,A)-->[].
a(a,A,A)-->[].




ut(ut,A,A)-->[].
s(Nt,S,As)-->{lc(ut,Nt)},ut(Nt,ut(S),As).
s(Nt,S1,As)-->{lc(ut,Nt)},parse(coord,C),parse(s,S2),ut(Nt,ut(S1,C,S2),As).

s(s,A,A)-->[].
np2(Nt,NP,As)-->{lc(s,Nt)},parse(vp2,VP),s(Nt,s(NP,VP),As).
pp(Nt,PP,As)-->{lc(s,Nt)},parse(np2,NP),parse(vp2,VP),s(Nt,s(PP,NP,VP),As).

np2(np2,A,A)-->[].
np1(Nt,NP,As)-->{lc(np2,Nt)},np2(Nt,np2(NP),As).
np1(Nt,NP1,As)-->{lc(np2,Nt)},parse(coord,C),parse(np1,NP2),np2(Nt,np2(NP1,C,NP2),As).

np1(np1,A,A)-->[].
det(Nt,Det,As)-->{lc(np1,Nt)},parse(n1,Noun),np1(Nt,np1(Det,Noun),As).
det(Nt,Det,As)-->{lc(np1,Nt)},parse(a2,Adj),parse(pp,PP),np1(Nt,np1(Det,Adj,PP),As).
n1(Nt,Noun,As)-->{lc(np1,Nt)},np1(Nt,np1(Noun),As).
n1(Nt,Noun,As)-->{lc(np1,Nt)},parse(a2,Adj),np1(Nt,np1(Noun,Adj),As).
a2(Nt,Adj,As)-->{lc(np1,Nt)},parse(n1,Noun),np1(Nt,np1(Adj,Noun),As).
pn(Nt,NP,As)-->{lc(np1,Nt)},np1(Nt,np1(NP),As).
pro(Nt,NP,As)-->{lc(np1,Nt)},np1(Nt,np1(NP),As).
pp(Nt,NP,As)-->{lc(np1,Nt)},np1(Nt,np1(NP),As).

n1(n1,A,A)-->[].
n0(Nt,N,As)-->{lc(n1,Nt)},n1(Nt,n1(N),As).
n0(Nt,N1,As)-->{lc(n1,Nt)},parse(n0,N2),n1(Nt,n1(N1,N2),As).
n0(Nt,N,As)-->{lc(n1,Nt)},parse(pp,PP),n1(Nt,n1(N,PP),As).

n0(n0,A,A)-->[].
noun(Nt,N,As)-->{lc(n0,Nt)},n0(Nt,n0(N),As).
p_nehot(Nt,P,As)-->{lc(n0,Nt)},n0(Nt,n0(P),As).

a2(a2,A,A)-->[].
a1(Nt,A,As)-->{lc(a2,Nt)},a2(Nt,a2(A),As).
a1(Nt,A1,As)-->{lc(a2,Nt)},parse(coord,C),parse(a1,A2),a2(Nt,a2(A1,C,A2),As).

a1(a1,A,A)-->[].
a0(Nt,Adj,As)-->{lc(a1,Nt)},a1(Nt,a1(Adj),As).
det(Nt,D,As)-->{lc(a1,Nt)},parse(a1,Adj),a1(Nt,a1(D,Adj),As).

a0(a0,A,A)-->[].
adj(Nt,Adj,As)-->{lc(a0,Nt)},a0(Nt,a0(Adj),As).

vp2(vp2,A,A)-->[].
vp1(Nt,VP,As)-->{lc(vp2,Nt)},vp2(Nt,vp2(VP),As).
vp1(Nt,VP1,As)-->{lc(vp2,Nt)},parse(coord,C),parse(vp1,VP2),vp2(Nt,vp2(VP1,C,VP2),As).

vp1(vp1,A,A)-->[].
v1(Nt,V,As)-->{lc(vp1,Nt)},vp1(Nt,vp1(V),As).
v1(Nt,V,As)-->{lc(vp1,Nt)},parse(adv,Adv),vp1(Nt,vp1(V,Adv),As).
pro(Nt,P,As)-->{lc(vp1,Nt)},parse(v1,V),vp1(Nt,vp1(P,V),As).
pro(Nt,P,As)-->{lc(vp1,Nt)},parse(v1,V),parse(adv,Adv),vp1(Nt,vp1(P,V,Adv),As).

v1(v1,A,A)-->[].
iv(Nt,V,As)-->{lc(v1,Nt)},v1(Nt,v1(V),As).
iv(Nt,V,As)-->{lc(v1,Nt)},parse(pp,PP),v1(Nt,v1(V,PP),As).
iv(Nt,V,As)-->{lc(v1,Nt)},parse(adv,Adv),v1(Nt,v1(V,Adv),As).
tv(Nt,V,As)-->{lc(v1,Nt)},parse(np2,NP),v1(Nt,v1(V,NP),As).
tv(Nt,V,As)-->{lc(v1,Nt)},parse(adv,Adv),parse(np2,NP),v1(Nt,v1(V,Adv,NP),As).
tv(Nt,V,As)-->{lc(v1,Nt)},parse(pp,PP),v1(Nt,v1(V,PP),As).
tv(Nt,TV,As)-->{lc(v1,Nt)},parse(det,SC),parse(v1,V),v1(Nt,v1(TV,sc(SC,V)),As).

pp(pp,A,A)-->[].
prep(Nt,Prep,As)-->{lc(pp,Nt)},parse(np2,NP),pp(Nt,pp(Prep,NP),As).

/* pentru a + infinitiv */
pp(pp,A,A)-->[].
prep(Nt,Prep,As)-->{lc(pp,Nt)},parse(np2,NP),pp(Nt,pp(Prep,NP),As).
prep(Nt,pentru,As)-->{lc(pp,Nt)},[a],parse(tv,V),parse(np2,NP),
    pp(Nt,pp(pentru,inf_clause(a,V,NP)),As).

advp(advp,A,A)-->[].
adv(Nt,Adv,As)-->{lc(advp,Nt)},advp(Nt,advp(Adv),As).
pp(Nt,PP,As)-->{lc(advp,Nt)},advp(Nt,advp(PP),As).



/* Test */
%test1(A):-parse(ut,A,[in,fiecare,saptamana,ana,merge,la,biblioteca,orasului,pentru,a,imprumuta,carti,noi],[]).
test1(A):-parse(ut,A,[in,fiecare,saptamana,ana,merge,la,biblioteca,pentru,carti,noi],[]).
test2(A):-parse(ut,A,[bibliotecarul,ii,recomanda,adesea,romane,poezii,si,carti,de,stiinta],[]).
test3(A):-parse(ut,A,[ana,le,alege,pe,cele,atragatoare,din,biblioteca],[]).
test4(A):-parse(ut,A,[la,sfarsitul,zilei,se,intoarce,acasa,si,incepe,sa,citeasca,entuziasmata],[]).

exe([in,fiecare,saptamana,ana,merge,la,biblioteca,orasului,pentru,a,imprumuta,carti,noi]).
exe([bibliotecarul,ii,recomanda,adesea,romane,poezii,si,carti,de,stiinta]).
exe([ana,le,alege,pe,cele,atragatoare,din,biblioteca]).
exe([la,sfarsitul,zilei,se,intoarce,acasa,si,incepe,sa,citeasca,entuziasmata]).

test_run(E,A):-exe(E), parse(ut,A,E,[]).

test_all:-
    tell('lc-time.doc'),
    findall((E,A), test_run(E,A), L),
    write_l(L).

write_l([]):-!.
write_l([(E,A)|T]):-
    write('Propozitie: '),write(E), nl,
    write('Arb: '), write(A),nl,nl,
    write_l(T).

test_time:-
    statistics(walltime, [Start,_]),
    test_all,nl,
    statistics(walltime, [End,_]),
    Time is End - Start,
    format('all solutions in ~8d seconds.~n', [Time]),
    told.