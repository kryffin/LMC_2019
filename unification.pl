%
% primitives d affichage

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
% test d un clash
%%% ArgGauche : vecteur de gauche
%%% ArgDroite : vecteur de droite

clash(ArgGauche, ArgDroite) :-
	functor(ArgGauche, F, AriteGauche),
	functor(ArgDroite, F, AriteDroite),
	AriteGauche == AriteDroite.

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
% reoriente une equation t ?= x en x ?= t si t est une variable
%%% E : equation a reorienter

orient(S, T) :-
    atom(T),
    write(T ?= S).

%
% check appelle un occur_check sur S et T
%%% S : vecteur gauche
%%% T : vecteur droite

check(S, T) :-
    occur_check(S, T).

%
% simplify
%%% WIP