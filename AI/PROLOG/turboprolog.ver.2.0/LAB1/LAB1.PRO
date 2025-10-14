domains
    person = symbol

predicates
    /* base facts */
    male(person).
    female(person).
    father(person, person).
    mother(person, person).
    married(person, person).

    /* derived relations */
    spouse(person, person).
    parent(person, person).
    child(person, person).
    sibling(person, person).
    brother(person, person).
    sister(person, person).
    grandparent(person, person).
    grandfather(person, person).
    grandmother(person, person).
    greatgrandparent(person, person).
    uncle(person, person).
    aunt(person, person).
    cousin(person, person).
    zolovka(person, person).      /* sister of the husband for the wife */

clauses
    /* === Logical rules === */

    /* spouses are symmetric */
    spouse(X, Y) if married(X, Y).
    spouse(X, Y) if married(Y, X).

    /* parent/child */
    parent(P, C) if father(P, C).
    parent(P, C) if mother(P, C).
    child(C, P)  if parent(P, C).

    /* siblings / brother / sister */
    sibling(X, Y) if parent(P, X), parent(P, Y), X <> Y.
    brother(X, Y) if sibling(X, Y), male(X).
    sister(X, Y)  if sibling(X, Y), female(X).

    /* grandparents and great-grandparents */
    grandparent(G, C)       if parent(G, P), parent(P, C).
    grandfather(G, C)       if grandparent(G, C), male(G).
    grandmother(G, C)       if grandparent(G, C), female(G).
    greatgrandparent(GG, C) if parent(GG, G), grandparent(G, C).

    /* uncles/aunts by blood and by marriage */
    uncle(U, N) if brother(U, P), parent(P, N).
    uncle(U, N) if married(U, W), sister(W, P), parent(P, N).

    aunt(A, N)  if sister(A, P), parent(P, N).
    aunt(A, N)  if married(A, H), brother(H, P), parent(P, N).

    /* cousins: parents are siblings */
    cousin(C1, C2) if parent(P1, C1), parent(P2, C2), sibling(P1, P2), C1 <> C2.

    /* zolovka(X,Y): X is sister of Y's husband (sister of the husband for the wife) */
    zolovka(X, Y) if female(X), married(Y, M), sister(X, M).
    zolovka(X, Y) if female(X), married(M, Y), sister(X, M).

    /* === Facts: male === */
    male(alexandr_kolomiets).
    male(roman_kolomiets).
    male(dmitry_kolomiets).
    male(andrey_kolomiets).
    male(denis_kolomiets).
    male(victor_kolomiets).
    male(pavel_kolomiets).
    male(petr_kolomiets).
    male(sergei_kolomiets).
    male(yuri_kolomiets).
    male(alexey_kolomiets).
    male(kirill_kolomiets).

    /* === Facts: female === */
    female(anya_kolomiets).
    female(katya_kolomiets).
    female(liza_kolomiets).
    female(ksyusha_kolomiets).
    female(olga_kolomiets).
    female(elena_kolomiets).
    female(irina_kolomiets).
    female(dasha_kolomiets).
    female(anna_kolomiets).
    female(marina_kolomiets).
    female(nina_kolomiets).
    female(zoya_kolomiets).
    female(tatiana_kolomiets).
    female(vera_kolomiets).

    /* === Facts: married === */
    married(roman_kolomiets, ksyusha_kolomiets).
    married(dmitry_kolomiets, olga_kolomiets).
    married(andrey_kolomiets, elena_kolomiets).
    married(victor_kolomiets, tatiana_kolomiets).
    married(pavel_kolomiets, irina_kolomiets).
    married(petr_kolomiets, anna_kolomiets).
    married(sergei_kolomiets, marina_kolomiets).
    married(yuri_kolomiets, nina_kolomiets).
    married(alexey_kolomiets, zoya_kolomiets).

    /* === Facts: father === */
    father(roman_kolomiets, alexandr_kolomiets).
    father(roman_kolomiets, anya_kolomiets).
    father(roman_kolomiets, katya_kolomiets).
    father(roman_kolomiets, liza_kolomiets).

    father(dmitry_kolomiets, roman_kolomiets).
    father(dmitry_kolomiets, elena_kolomiets).
    father(dmitry_kolomiets, vera_kolomiets).

    father(andrey_kolomiets, denis_kolomiets).

    father(victor_kolomiets, ksyusha_kolomiets).
    father(victor_kolomiets, pavel_kolomiets).

    father(pavel_kolomiets, dasha_kolomiets).
    father(pavel_kolomiets, kirill_kolomiets).

    father(petr_kolomiets, dmitry_kolomiets).
    father(sergei_kolomiets, olga_kolomiets).
    father(yuri_kolomiets, victor_kolomiets).
    father(alexey_kolomiets, tatiana_kolomiets).

    /* === Facts: mother === */
    mother(ksyusha_kolomiets, alexandr_kolomiets).
    mother(ksyusha_kolomiets, anya_kolomiets).
    mother(ksyusha_kolomiets, katya_kolomiets).
    mother(ksyusha_kolomiets, liza_kolomiets).

    mother(olga_kolomiets, roman_kolomiets).
    mother(olga_kolomiets, elena_kolomiets).
    mother(olga_kolomiets, vera_kolomiets).

    mother(elena_kolomiets, denis_kolomiets).

    mother(tatiana_kolomiets, ksyusha_kolomiets).
    mother(tatiana_kolomiets, pavel_kolomiets).

    mother(irina_kolomiets, dasha_kolomiets).
    mother(irina_kolomiets, kirill_kolomiets).

    mother(anna_kolomiets, dmitry_kolomiets).
    mother(marina_kolomiets, olga_kolomiets).
    mother(nina_kolomiets, victor_kolomiets).
    mother(zoya_kolomiets, tatiana_kolomiets).