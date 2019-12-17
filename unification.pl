%
%%
%%% primitives d affichage

%
% set_echo active l affichage

set_echo :-
    assert(echo_on).

%
% clr_echo desactive l affichage

clr_echo :-
    retractall(echo_on).

%
% echo affiche T si l affichage est active

echo(T) :-
    set_echo,
    !,
    write(T).

echo(_).

%
% echo_trace affiche la trace d execution

echo_trace(R, E, P) :-
    echo('System : '),
    echo(P), nl,
	echo(R),
	echo(' : '),
	echo(E), nl.

%
% trace_unif unifie le programme en affichant une trace de l execution

trace_unif(P, S) :-
    set_echo,
    unifie(P, S).

%
% unif unifie le programme en affichant aucune trace d execution

unif(P, S) :-
    clr_echo,
    unifie(P, S).

%
% trace_unif appelle l unification avec ou non la trace (affichage des differentes etapes)

trace_unif(P, S, o) :-
    trace_unif(P, S).

trace_unif(P, S, _) :-
    unif(P, S).

%
% unifie demande si oui ou non l utilisateur veut une trace de l execution et unifie le programme

unifie(P) :-
    echo('Choix de la stratégie : '), nl,
    echo('(1) choix_premier'), nl,
    echo('(2) choix_pondéré'), nl,
    read(InputStrategie),
    (InputStrategie == 1 -> S = choix_premier;
    InputStrategie == 2 -> S = choix_pondere;
    S = premier),
    echo('Stratégie choisie : '),
    echo(S), nl,
    echo('Afficher la trace d\'execution ? (o/n)'), nl,
    read(InputAffichage), nl,
    trace_unif(P, S, InputAffichage).

%
%%
%%% operateur

%
% definition de l operateur ?= qui prend deux arguments de la forme x ?= y avec une priorite de 20
:- op(20, xfy, ?=).

%
%%
%%% primitives utilitaires

%
% pop supprime le premier element d une liste

pop([_|Q], Q).

pop([], []).

%
% reverse retourne une liste

reverse([],Z,Z).

reverse([H|T],Z,Acc) :-
    reverse(T,Z,[H|Acc]).

%
% remove supprime un element d une liste

remove(_, [], []).

remove(E, [H|Q], S) :-
    E == H,
    remove(E, Q, S).

remove(E, [H|Q], S) :-
    E \== H,
    append([H], V, S),
    remove(E, Q, V),
    !.

%
% assemble sert a fusionner deux listes en mettant chacun de leur element sous l operateur ?=

assemble([], [], []).

assemble([H1|Q1], [H2|Q2], L) :-
    append([H1 ?= H2], T, L),
    assemble(Q1, Q2, T).

%
%%
%%% primitives de logique

%
% occur_check vérifie si la variable S est présente dans T

occur_check(S, T) :-
    var(S),
    S \== T,
    contains_var(S, T).

%
%%
%%% primitives d application des regles de Martelli Montanari

%
% regle verifie ici si rename s applique a S ?= T

regle(S ?= T, rename) :-
    var(S),
    var(T).

%
% regle verifie ici si simplify s applique a S ?= T

regle(S ?= T, simplify) :-
    var(S),
    atom(T).

%
% regle verifie ici si expand s applique a S ?= T

regle(S ?= T, expand) :-
    var(S),
    compound(T),
    \+(occur_check(S, T)).

%
% regle verifie ici si check s applique a S ?= T

regle(S ?= T, check) :-
    var(S),
    occur_check(S, T).

%
% regle verifie ici si orient s applique a S ?= T

regle(T ?= S, orient) :-
    var(S),
    nonvar(T).

%
% regle verifie ici si decompose s applique a S ?= T

regle(S ?= T, decompose) :-
    nonvar(S),
    nonvar(T),
    functor(S, SName, SArity),
    functor(T, TName, TArity),
	SName == TName,
	SArity =:= TArity.

%
% regle verifie ici si clash s applique a S ?= T

regle(S ?= T, clash) :-
    nonvar(S);
    nonvar(T),
	functor(S, SName, SArity),
	functor(T, TName, TArity),
    (SName \== TName;
    SArity =\= TArity),
    !.

%
% reduction du programme P par la regle rename sur l equation S ?= T

reduit(rename, S ?= T, P, Q) :-
    remove(S ?= T, P, Q),
    S = T.

%
% reduction du programme P par la regle simplify sur l equation S ?= T

reduit(simplify, S ?= T, P, Q) :-
    remove(S ?= T, P, Q),
    S = T.

%
% reduction du programme P par la regle expand sur l equation S ?= T

reduit(expand, S ?= T, P, Q) :-
    remove(S ?= T, P, Q),
    S = T.

%
% reduction du programme P quelconque par la regle check sur une equation quelconque

reduit(check, _, _, []) :-
    false.

%
% reduction du programme P par la regle orient sur l equation S ?= T

reduit(orient, T ?= S, P, Q) :-
    remove(T ?= S, P, Q2),
    append([S ?= T], Q2, Q).

%
% reduction du programme P par la regle decompose sur l equation S ?= T

reduit(decompose, S ?= T, P, Q) :-
    S =.. SList,
    T =.. TList,
	pop(SList, SPopped),
	pop(TList, TPopped),
    assemble(SPopped, TPopped, L),
    remove(S ?= T, P, V),
    append(L, V, Q).

%
% reduction du programme P quelconque par la regle clash sur une equation quelconque

reduit(clash, _, _, []) :-
    false.

%
%%
%%% primitives de strategies

%
% unifie applique les regles de Martelli Montanari sur le programme P en utilisant une strategie

unifie([], _).

unifie(P, S) :-
    choix(S, P, E, R),
    echo_trace(R, E, P),
    reduit(R, E, P, Q),
    unifie(Q, S),
    !.

%
% choix differentie les differents choix de strategie

choix(choix_premier, P, E, R) :-
    choix_premier(P, E, R).

choix(choix_pondere, P, E, R) :-
    choix_pondere(P, E, R).

%
% liste des choix ponderee

poids([check, rename, simplify, orient, decompose, expand, clash]).

%
% appliquer trouve une equation sur laquelle appliquer une regle et l appliquer

appliquer(_, [], _, _) :-
    false.

appliquer(X, [_|Q], E, R) :-
    appliquer(X, Q, E, R).

appliquer([R|_], [E|_], E, R) :-
    regle(E, R).

%
% choisir_regle trouve une regle a appliquer

choisir_regle(X, P, E, R) :-
    appliquer(X, P, E, R).

choisir_regle([_|Q], P, E, R) :-
    choisir_regle(Q, P, E, R).

%
% choix_premier applique une regle a la premiere equation

choix_premier([E|_], E, R) :-
    regle(E, R),
    !.

%
% choix_pondere applique une regle a une equation par rapport au poids de chaque regle

choix_pondere(P, E, R) :-
    poids(X),
    choisir_regle(X, P, E, R),
    !.
