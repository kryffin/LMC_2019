%
%%
%%% primitives d affichage

set_echo :-
	assert(echo_on).

echo(T) :-
	echo_on,
	!,
	write(T).

echo(_).

%
% definition de l operateur ?= qui prend deux arguments de la forme x ?= y avec une priorité de 20

:- op(20, xfy, ?=).

%
%%
%%% utilitaires

%
% reverse retourne une liste
%%% [H|T] : liste à retourner
%%% Z : liste retournée
%%% Acc : liste intermédiaire

reverse([],Z,Z).

reverse([H|T],Z,Acc) :-
    reverse(T,Z,[H|Acc]).

%
% suppr supprime un element E de la liste Q
%%% E : element à supprimer
%%% [H|Q] : liste d entrée
%%% L : liste de sortie

suppr(E, [], L) :-
    reverse(L, Z, []),
    write(L).

suppr(E, [H|Q], L) :-
    E \= H,
    suppr(E, Q, [H|L]),
    !.

suppr(E, [H|Q], L) :-
    E == H,
    suppr(E, Q, L).

%
%%
%%% règles logiques

%
% regle(E, R) qui indique si R est appliquable sur E
%%% E : equation sur laquelle appliquer la règle
%%% R : règle à appliquer

regle(E, R) :-
	arg(1, E, ArgGauche),
	arg(2, E, ArgDroite),
	call(R, ArgGauche, ArgDroite).

%
% occur check d une variable X sur un terme T
%%% X : variable à rechercher
%%% T : terme dans lequel rechercher

occur_check(V, T) :-
    V == T,
    !.

occur_check(V, T) :-
    nonvar(T),
	functor(T, F, A),
	A > 0,
	arg(N, T, X),
	occur_check(V, X),
	!.

%
%%
%%% règles Martelli-Montanari

%
% rename vérifie si S est renommable par T (ssi T est une variable)
%%% S : vecteur gauche
%%% T : vecteur droit

rename(S, T) :-
    var(T).

%
% rename qui remplace les t variables par x
%%% E : element de remplacement

rename(E) :-
    arg(1, E, X),
    agr(2, E, T),
    X = T.

%
% simplify vérifie si S est renommable par T (ssi T est une constante)
%%% S : vecteur gauche
%%% T : vecteur droit

simplify(S, T) :-
    functor(T, T, 0).

%
% simplify qui remplace les t constantes par x
%%% E : element de remplacement

simplify(E) :-
    arg(1, E, X),
    arg(2, E, T),
    X = T.

%
% expand vérifie si S est remplacable par T
%%%
%%%

expand(S, T) :-
    nonvar(T),
    functor(T, A, N),
    N > 0,
    \+check(S, T),
    !.

%
% expand qui remplace les t composés par x
%%% E : Equation de remplacement

expand(E) :-
    arg(1, E, X),
    arg(2, E, T),
    X = T.

%
% check appelle un occur_check sur S et T
%%% S : vecteur gauche
%%% T : vecteur droite

check(S, T) :-
    occur_check(S, T).

%
% reoriente une equation t ?= x en x ?= t si t est une variable
%%% E : equation a reorienter

orient(S, T) :-
    var(T),
    write(T ?= S).

%
% application de la regle de décomposition
%%% S : vecteur de gauche
%%% T : vecteur de droite
%%% Q : résultat (par unification)
%%% N : indice

decompose(S, T, Q, 1) :- write(Q), !.

decompose(S, T, Q, N) :-
	succ(M, N),
	arg(M, S, Si),
	arg(M, T, Ti),
	L = Q,
	Q = [?=(Si, Ti)|L],
	write(Q),
	decompose(S, T, Q, M).

decompose(S, T, Q) :-
    Sliste =.. S,
    Tliste =.. T,
    depile(Q, V),
    unif_liste().


depile([_|Q], Q).
depile([], []).


%
% test d un clash
%%% ArgGauche : vecteur de gauche
%%% ArgDroite : vecteur de droite

clash(ArgGauche, ArgDroite) :-
	functor(ArgGauche, F, AriteGauche),
	functor(ArgDroite, F, AriteDroite),
	AriteGauche == AriteDroite.


reduit(decompose, E, P, Q) :-
    arg(1, E, Eg),
    arg(2, E, Ed),
    arg(1, P, Pp),
    functor(P, A, N),
    succ(N, M),
    decompose(Eg, Ed, Q, M).

unifie([H|P]) :-
    Q = [],
    reduit(decompose, H, [H|P], Q).
