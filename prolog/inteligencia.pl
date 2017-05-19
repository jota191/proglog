:- module(inteligencia,[nombre/1,
                        hacer_jugada/3]).

:- use_module(juego).

%% nombre(?Nombre)
%
% Nombre es el nombre de la inteligencia,
% que deberá ser "Grupo XX", siendo XX el número de grupo.

nombre("Grupo 04").


%% niveles_minimax(?MaxNiveles)
%
% MaxNiveles es la cantidad máxima de niveles de Minimax de la inteligencia.

niveles_minimax(1). % TODO


%% hacer_jugada(+E,?LP,?E2)
%
% E2 es el estado resultante de mover según la lista de posiciones LP
% de un movimiento que la inteligencia elige jugar para el estado E5.
% El turno de jugar en el estado E es el de la inteligencia,
% mientras que en E2 es del otro jugador.

%hacer_jugada(E,LP,E2):-
%    mover(E,LP,E2). % TODO

hacer_jugada(E,[p(0,1)],E2):-
    mover(E,[p(0,1)],E2). % TODO
