:- module(juego,[cantidad_casilleros/2,
                 estado_inicial/1,
                 posicion_pelota/2,
                 mover/3,
                 prefijo_movimiento/2,
                 gol/2,
                 turno/2]).


%% cantidad_casilleros(?Ancho,?Alto)
%
%  Ancho es la cantidad de casilleros que hay de ancho en el tablero y Alto es
% la cantidad que hay de alto (sin contar los arcos). Ambos números son pares.
% Notar que la cantidad de vértices es uno más que la cantidad de casilleros.
% Este predicado está dado, no hay que modificarlo.

cantidad_casilleros(8,10).


%% estado_inicial(?E)
%
% E es el estado inicial del juego, de tamaño AltoxAncho
% unificado por cantidad_casilleros(Ancho,Alto).
% La pelota comienza en la posición p(0,0), que es la posición central
% del tablero.
% El turno inicial es del jugador 1, que es el que patea hacia arriba.

estado_inicial(e). % TODO


%% posicion_pelota(+E,?P)
%
% P es la posición de la pelota para el estado E.

posicion_pelota(_,p(0,0)). % TODO


%% mover(+E,?LP,?E2)
%
% E2 el estado resultante de hacer un movimiento con la pelota,
% a través de las posiciones de la lista LP en el estado E
% y de cambiar el turno.

mover(E,_,E). % TODO


%% gol(+E,?NJugador)
%
% La pelota está en situación de gol a favor del jugador NJugador
% para el estado E.

gol(_,_):-
    fail. % TODO


%% turno(+E,?NJugador)
%
% NJugador es el jugador que tiene que mover en el siguiente turno
%para el estado E.

turno(_,1). % TODO

%% prefijo_movimiento(+E,+LP)
%
% LP es una lista no vacía de posiciones que constituyen
% el prefijo de un movimiento para el estado E,
% sin llegar a formar un movimiento.

prefijo_movimiento(_,_). % TODO
