ut(ut(S)) ---> [s(S)].
ut(ut(S1, C, S2)) ---> [s(S1), coord(C), s(S2)].

s(s(NP, VP)) ---> [np2(NP), vp2(VP)].
s(s(AdvP, NP, VP)) ---> [advp(AdvP), np2(NP), vp2(VP)].

advp(advp(PP)) ---> [pp(PP)].

np2(np2(NP)) ---> [np1(NP)].
np2(np2(NP1, C, NP2)) ---> [np1(NP1), coord(C), np1(NP2)].

np1(np1(Det, Noun)) ---> [det(Det), n1(Noun)].
np1(np1(Det, Adj, PP)) ---> [det(Det), a2(Adj), pp(PP)].
np1(np1(Num, NP)) ---> [num(Num), np1(NP)].
np1(np1(Noun, Adj)) ---> [n1(Noun), a2(Adj)].
np1(np1(Adj, Noun)) ---> [a2(Adj), n1(Noun)].
np1(np1(Noun)) ---> [n1(Noun)].
np1(np1(NP)) ---> [pn(NP)].
np1(np1(NP)) ---> [pro(NP)].
np1(np1(NP)) ---> [relpro(NP)].
np1(np1(NP)) ---> [pp(NP)].

n1(n1(N)) ---> [n0(N)].
n1(n1(N1, N2)) ---> [n0(N1), n0(N2)].
n1(n1(N, PP)) ---> [n0(N), pp(PP)].
n1(n1(N, RC)) ---> [n0(N), rc(RC)].
n0(n0(N)) ---> [noun(N)].
n0(n0(P)) ---> [p_nehot(P)].

a2(a2(A)) ---> [a1(A)].
a2(a2(A1, C, A2)) ---> [a1(A1), coord(C), a1(A2)].

a1(a1(Adj)) ---> [a0(Adj)].
a1(a1(D, Adj)) ---> [det(D), a1(Adj)].

a0(a0(Adj)) ---> [adj(Adj)].
a0(a0(Adj)) ---> [ppos(Adj)].

vp2(vp2(VP)) ---> [vp1(VP)].
vp2(vp2(VP1, C, VP2)) ---> [vp1(VP1), coord(C), vp1(VP2)].

vp1(vp1(P, V)) ---> [pro(P), v1(V)].
vp1(vp1(P, V, Adv)) ---> [pro(P), v1(V), adv(Adv)].

vp1(vp1(V)) ---> [v1(V)].
vp1(vp1(V, Adv)) ---> [v1(V), adv(Adv)].

v1(v1(IV)) ---> [iv(IV)].
v1(v1(IV, PPS)) ---> [iv(IV), pps(PPS)].
v1(v1(TV, NP)) ---> [tv(TV), np2(NP)].
v1(v1(TV, PP)) ---> [tv(TV), pp(PP)].
v1(v1(TV, Adv, NP)) ---> [tv(TV), adv(Adv), np2(NP)].
v1(v1(TV, sc(SC))) ---> [tv(TV), subj(SC)].
v1(v1(IV, Adv)) ---> [iv(IV), adv(Adv)].

pps(pps(PP)) ---> [pp(PP)].
pps(pps(PP, More)) ---> [pp(PP), pps(More)].

pp(pp(Prep, NP)) ---> [prep(Prep), np2(NP)].
pp(pp(pentru, Inf)) ---> [[pentru], inf_clause(Inf)].

inf_clause(inf_clause(a, V, NP)) ---> [[a], tv(V), np2(NP)].

subj(sc(sa, V1)) ---> [[sa], v1(V1)].

rc(rc(RP, VP)) ---> [relpro(RP), vp2(VP)].

lex(si, coord(coord(si))).
lex(iar, coord(coord(iar))).
lex(sa, coord(coord(sa))).

lex(in, prep(prep(in))).
lex(pe, prep(prep(pe))).
lex(de, prep(prep(de))).
lex(la, prep(prep(la))).
lex(din, prep(prep(din))).

lex(o, det(det(o))).
lex(cel, det(det(cel))).
lex(cele, det(det(cele))).
lex(mai, det(det(mai))).
lex(fiecare, det(det(fiecare))).

lex(ana, noun(noun(ana))).
lex(saptamana, noun(noun(saptamana))).
lex(biblioteca, noun(noun(biblioteca))).
lex(orasului, noun(noun(orasului))).
lex(carti, noun(noun(carti))).
lex(bibliotecarul, noun(noun(bibliotecarul))).
lex(romane, noun(noun(romane))).
lex(poezii, noun(noun(poezii))).
lex(stiinta, noun(noun(stiinta))).
lex(sfarsitul, noun(noun(sfarsitul))).
lex(zilei, noun(noun(zilei))).

lex(tot, p_nehot(p_nehot(tot))).

lex(merge, tv(tv(merge))).
lex(imprumuta, tv(tv(imprumuta))).
lex(recomanda, tv(tv(recomanda))).
lex(alege, tv(tv(alege))).
lex(atrag, tv(tv(atrag))).
lex(incepe, tv(tv(incepe))).
lex(citeasca, tv(tv(citeasca))).

lex(merge, iv(iv(merge))).
lex(citeste, iv(iv(citeste))).
lex(citeasca, iv(iv(citeasca))).
lex(intoarce, iv(iv(intoarce))).

lex(care, relpro(relpro(care))).
lex(ce, relpro(relpro(ce))).

lex(ana, pn(pn(ana))).

lex(ii, pro(pro(ii))).
lex(le, pro(pro(le))).
lex(se, pro(pro(se))).
lex(o, pro(pro(o))).

lex(meu, ppos(ppos(meu))).
lex(mea, ppos(ppos(mea))).

lex(noi, adj(adj(noi))).
lex(mult, adj(adj(mult))).
lex(atragatoare, adj(adj(atragatoare))).

lex(entuziasmata, adv(adv(entuziasmata))).
lex(acasa, adv(adv(acasa))).
lex(adesea, adv(adv(adesea))).
lex(mai, adv(adv(mai))).

lex(un, num(num(un))).
lex(o, num(num(o))).

startsymbol(ut(_)).

exe([in,fiecare,saptamana,ana,merge,la,biblioteca,orasului,.]).
exe([bibliotecarul,ii,recomanda,adesea,romane,poezii,si,carti,de,stiinta,.]).
exe([ana,le,alege,pe,cele,atragatoare,din,biblioteca,.]).
exe([la,sfarsitul,zilei,se,intoarce,acasa,sa,citeasca,entuziasmata,.]).
