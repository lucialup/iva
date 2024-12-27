/*****************************************************
   MAIN SENTENCE RULES
*****************************************************/

% Main sentence rules
ut(ut(S))            --> s(S).
ut(ut(S1, C, S2))    --> s(S1), coord(C), s(S2).

% A sentence can be NP + VP or an adverbial PP + NP + VP
s(s(NP, VP))         --> np2(NP), vp2(VP).
s(s(AdvP, NP, VP))   --> advp(AdvP), np2(NP), vp2(VP).

% An adverbial phrase is just a PP
advp(advp(PP))       --> pp(PP).

/*****************************************************
   NOUN PHRASE RULES
*****************************************************/

np2(np2(NP))         --> np1(NP).
np2(np2(NP1, C, NP2))--> np1(NP1), coord(C), np1(NP2).

np1(np1(Det, Noun))       --> det(Det), n1(Noun).
np1(np1(Det, Adj, PP))    --> det(Det), a2(Adj), pp(PP).
np1(np1(Num, NP))         --> num(Num), np1(NP).
np1(np1(Noun, Adj))       --> n1(Noun), a2(Adj).
np1(np1(Adj, Noun))       --> a2(Adj), n1(Noun).
np1(np1(Noun))            --> n1(Noun).
np1(np1(NP))              --> pn(NP).
np1(np1(NP))              --> pro(NP).
np1(np1(NP))              --> relpro(NP).
np1(np1(NP))              --> pp(NP).

n1(n1(N))                 --> n0(N).
n1(n1(N1, N2))            --> n0(N1), n0(N2).    % e.g. "biblioteca orasului", "romane poezii"
n1(n1(N, PP))             --> n0(N), pp(PP).     % e.g. "biblioteca orasului pentru studenti"
n1(n1(N, RC))             --> n0(N), rc(RC).     % e.g. "carti care imi plac"

n0(n0(N))                 --> noun(N).
n0(n0(P))                 --> p_nehot(P).

/*****************************************************
   ADJECTIVE RULES
*****************************************************/

a2(a2(A))                --> a1(A).
a2(a2(A1, C, A2))        --> a1(A1), coord(C), a1(A2).

a1(a1(Adj))              --> a0(Adj).
a1(a1(D, Adj))           --> det(D), a1(Adj).

a0(a0(Adj))              --> adj(Adj).
a0(a0(Adj))              --> ppos(Adj).

/*****************************************************
   VERB PHRASE RULES
*****************************************************/

vp2(vp2(VP))             --> vp1(VP).
vp2(vp2(VP1, C, VP2))    --> vp1(VP1), coord(C), vp1(VP2).

% Allow "pro + v1" or "pro + v1 + adv"
vp1(vp1(P, V))           --> pro(P), v1(V).
vp1(vp1(P, V, Adv))      --> pro(P), v1(V), adv(Adv).

% A simple VP can be just a v1, or v1 + adv
vp1(vp1(V))              --> v1(V).
vp1(vp1(V, Adv))         --> v1(V), adv(Adv).

% Possible verb combinations
v1(v1(IV))               --> iv(IV).
v1(v1(IV, PPS))          --> iv(IV), pps(PPS).
v1(v1(TV, NP))           --> tv(TV), np2(NP).
v1(v1(TV, PP))           --> tv(TV), pp(PP).
v1(v1(TV, Adv, NP))      --> tv(TV), adv(Adv), np2(NP).
v1(v1(TV, sc(SC)))       --> tv(TV), subcj(SC).  % "incepe sa citeasca (entuziasmata)"
v1(v1(IV, Adv))          --> iv(IV), adv(Adv).   % Intransitive verb + adverb

pps(pps(PP))             --> pp(PP).
pps(pps(PP, More))       --> pp(PP), pps(More).

pp(pp(Prep, NP))         --> prep(Prep), np2(NP).
pp(pp(pentru, Inf))      --> [pentru], inf_clause(Inf).

inf_clause(inf_clause(a, V, NP)) --> [a], tv(V), np2(NP).

% Subjunction "sa" => parse a subordinate v1
subcj(sc(sa, V1))        --> [sa], v1(V1).

% Relative clause
rc(rc(RP, VP))           --> relpro(RP), vp2(VP).

/*****************************************************
   LEXICON
*****************************************************/

coord(coord(si))         --> [si].
coord(coord(iar))        --> [iar].
coord(coord(sa))         --> [sa].

prep(prep(in))           --> [in].
prep(prep(pe))           --> [pe].
prep(prep(de))           --> [de].
prep(prep(la))           --> [la].
prep(prep(din))          --> [din].

det(det(o))              --> [o].
det(det(cel))            --> [cel].
det(det(cele))           --> [cele].
det(det(mai))            --> [mai].
det(det(fiecare))        --> [fiecare].

noun(noun(ana))          --> [ana].
noun(noun(saptamana))    --> [saptamana].
noun(noun(biblioteca))   --> [biblioteca].
noun(noun(orasului))     --> [orasului].
noun(noun(carti))        --> [carti].
noun(noun(bibliotecarul))--> [bibliotecarul].
noun(noun(romane))       --> [romane].
noun(noun(poezii))       --> [poezii].
noun(noun(stiinta))      --> [stiinta].
noun(noun(sfarsitul))    --> [sfarsitul].
noun(noun(zilei))        --> [zilei].

p_nehot(p_nehot(tot))    --> [tot].

tv(tv(merge))            --> [merge].
tv(tv(imprumuta))        --> [imprumuta].
tv(tv(recomanda))        --> [recomanda].
tv(tv(alege))            --> [alege].
tv(tv(atrag))            --> [atrag].
tv(tv(incepe))           --> [incepe].
tv(tv(citeasca))         --> [citeasca].

iv(iv(merge))            --> [merge].
iv(iv(citeste))          --> [citeste].
iv(iv(citeasca))         --> [citeasca].
iv(iv(intoarce))         --> [intoarce].

relpro(relpro(care))     --> [care].
relpro(relpro(ce))       --> [ce].

pn(pn(ana))              --> [ana].
pro(pro(ii))             --> [ii].
pro(pro(le))             --> [le].
pro(pro(se))             --> [se].
pro(pro(o))              --> [o].

ppos(ppos(meu))          --> [meu].
ppos(ppos(mea))          --> [mea].

adj(adj(noi))            --> [noi].
adj(adj(mult))           --> [mult].
adj(adj(atragatoare))    --> [atragatoare].

adv(adv(entuziasmata))   --> [entuziasmata].
adv(adv(acasa))          --> [acasa].
adv(adv(adesea))         --> [adesea].
adv(adv(mai))            --> [mai].
adv(adv(cel_mai_mult))   --> [cel, mai, mult].

num(num(un))             --> [un].
num(num(o))              --> [o].

/*****************************************************
   TESTING
*****************************************************/

exe(['in', 'fiecare', 'saptamana', 'ana', 'merge', 'la', 'biblioteca', 'orasului', 'pentru', 'a', 'imprumuta', 'carti', 'noi']).
exe(['bibliotecarul', 'ii', 'recomanda', 'adesea', 'romane', 'poezii', 'si', 'carti', 'de', 'stiinta']).
exe(['ana', 'le', 'alege', 'pe', 'cele', 'atragatoare', 'din', 'biblioteca']).
exe(['la', 'sfarsitul', 'zilei', 'se', 'intoarce', 'acasa', 'si', 'incepe', 'sa', 'citeasca', 'entuziasmata']).

test_run(E, P, A) :-
    exe(E),
    ut(A, E, []),
    ut(A, P, []).

test_sentence(E) :-
    (   ut(Tree, E, [])
    ->  write('Parse tree:'), nl,
        write(Tree), nl
    ;   write('No parse found for: '), write(E), nl, fail
    ).

test_all_sentences :-
    test_sentence(['in', 'fiecare', 'saptamana', 'ana', 'merge', 'la', 'biblioteca', 'orasului', 'pentru', 'a', 'imprumuta', 'carti', 'noi']),
    test_sentence(['bibliotecarul', 'ii', 'recomanda', 'adesea', 'romane', 'poezii', 'si', 'carti', 'de', 'stiinta']),
    test_sentence(['ana', 'le', 'alege', 'pe', 'cele', 'atragatoare', 'din', 'biblioteca']),
    test_sentence(['la', 'sfarsitul', 'zilei', 'se', 'intoarce', 'acasa', 'si', 'incepe', 'sa', 'citeasca', 'entuziasmata']).
