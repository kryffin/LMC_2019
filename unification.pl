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

*
**
*** utilitaires

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

occur_check(X, X) :-
	!.

occur_check(V, T) :-
	functor(T, F , A),
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
%%% [H|Q] : liste sur laquelle remplacer
%%% L : liste du résultat

rename(S, [], L) :- write(L).

rename(E, [H|Q], L) :-
    arg(1, E, ArgUn),
    arg(2, E, ArgDeux),
    arg(1, H, S),
    arg(2, H, T),
    T == ArgUn,
    rename(E, Q, [?=(S, ArgDeux)|L]).

rename(E, [H|Q], L) :-
    arg(1, E, ArgUn),
    arg(2, E, ArgDeux),
    arg(1, H, S),
    arg(2, H, T),
    T \= ArgUn,
    rename(E, Q, [?=(S, T)|L]),
    !.

%
% simplify vérifie si S est renommable par T (ssi T est une constante)
%%% S : vecteur gauche
%%% T : vecteur droit

simplify(S, T) :-
    functor(T, T, 0).

%
% simplify qui remplace les t constantes par x
%%% E : element de remplacement
%%% [H|Q] : liste sur laquelle remplacer
%%% L : liste du résultat

simplify(S, [], L) :- write(L).

simplify(E, [H|Q], L) :-
    arg(1, E, ArgUn),
    arg(2, E, ArgDeux),
    arg(1, H, S),
    arg(2, H, T),
    T == ArgUn,
    rename(E, Q, [?=(S, ArgDeux)|L]).

simplify(E, [H|Q], L) :-
    arg(1, E, ArgUn),
    arg(2, E, ArgDeux),
    arg(1, H, S),
    arg(2, H, T),
    T \= ArgUn,
    rename(E, Q, [?=(S, T)|L]),
    !.

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

decompose(S, T, Q, 1) :- !.

decompose(S, T, Q, N) :-
	succ(M, N),
	arg(M, S, Si),
	arg(M, T, Ti),
	L = [?=(Si, Ti)|Q],
	decompose(S, T, L, M).

%
% test d un clash
%%% ArgGauche : vecteur de gauche
%%% ArgDroite : vecteur de droite

clash(ArgGauche, ArgDroite) :-
	functor(ArgGauche, F, AriteGauche),
	functor(ArgDroite, F, AriteDroite),
	AriteGauche == AriteDroite.
