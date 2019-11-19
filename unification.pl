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
% definition de l operateur ?=

:- op(20, xfy, ?=).

%
% regle(E, R) qui indique si R est appliquable sur E

regle(E, R) :-
	arg(1, E, ArgGauche),
	arg(2, E, ArgDroite),
	call(R, ArgGauche, ArgDroite).

%
% occur check d une variable X sur un terme T

occur_check(X, X) :-
	!.

occur_check(V, T) :-
	functor(T, F , A),
	A > 0,
	arg(N, T, X),
	occur_check(V, X),
	!.

%
% test et decomposition sur deux arguments (de la forme f(s, s, s), f(t, t, t))
%%% WIP

decompose(ArgGauche, ArgDroite) :-
	functor(ArgGauche, F, AriteGauche),
	functor(ArgDroite, F, AriteDroite),
	AriteGauche == AriteDroite,
	arg(ElemI, ArgGauche, Si),
	arg(ElemI, ArgDroite, Ti).
	%ajouter[?=(Si, Ti)] a Q
