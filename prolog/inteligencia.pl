:- module(inteligencia,[nombre/1,
                        hacer_jugada/3,
                       minimax/5]).

:- use_module(juego).

%% nombre(?Nombre)
%
% Nombre es el nombre de la inteligencia,
% que deberá ser "Grupo XX", siendo XX el número de grupo.

nombre("Grupo 04").


%% niveles_minimax(?MaxNiveles)
%
% MaxNiveles es la cantidad máxima de niveles de Minimax de la inteligencia.

niveles_minimax(2).


%% hacer_jugada(+E,?LP,?E2)
%
% E2 es el estado resultante de mover según la lista de posiciones LP
% de un movimiento que la inteligencia elige jugar para el estado E.
% El turno de jugar en el estado E es el de la inteligencia,
% mientras que en E2 es del otro jugador.

hacer_jugada(E,LP,E2):- niveles_minimax(N),
                        minimax(E,N,true,LP,_),
                        mover(E,LP,E2).

% LP es la lista de movimientos hasta ahora
% E es el estado previo, y E2 el resultante



%% max(+Listamvs, -Mov, -V)
%
% Listamovs es una lista de functores mv(Mov,V) en donde Mov es un movimiento
% (movimiento = lista de posiciones)
% y V el valor del movimiento (listamovs viene a representar la sintetizacion
% de todos los hijos de un nodo de minimax).
% de la entrada mv(Mov,V) con Mayor V, se retornan Mov y V

max([mv(Mov,V)],Mov,V).
max([mv(Mov,V)|T],BM,Max) :-
    max(T,Bm2,Max2),
    (Max2>V -> (BM = Bm2,Max = Max2);(BM=Mov,Max = V)).


% analogo a Max, retorna el menor V de la lista con su correspondiente Mov
min([mv(Mov,V)],Mov,V).
min([mv(Mov,V)|T],BM,Min) :-
    min(T,Bm2,Min2),
    (Min2<V -> (BM = Bm2,Min = Min2);(BM=Mov,Min = V)).




%% minimax (+E     : Estado,
%           +Prof  : Int,
%           +Is_Maximizing : Bool,
%           BestMov : movimiento (lista de posiciones)),
%           Value   : Int
%
% BestMov es el movimiento optimo segun el algoritmo de minimax,
% y tiene un valor V según la función de evaluación.
% Is_Maximizing indica si el nodo es min o max,
% E es el estado en el nodo donde se llama a la función
% Prof es la profundidad
% Jugador indica hacia donde "patea" la IA
% (todavia no se usa porque esta hardcodeado hacia abajo)

minimax(E,Prof,Is_Maximizing,BestMov,Value,Jugador) :-
    Prof > 0,
    NewProf is Prof-1,
    findall(mv(Mov,0),mover(E,Mov,_),MovsPosibles),
    length(MovsPosibles,Len),print(Len),
    MovsFunc =.. [movs|MovsPosibles],
    recursive(E,MovsFunc,Len,NewProf,Is_Maximizing),
    MovsFunc =.. [_|Movs],
    (Is_Maximizing = true -> max(Movs,BestMov,Value)
     ;min(Movs,BestMov,Value)),!.

% Si la profundidad es 0 (llegue al ultimo nivel),
% (se evalua la funcion)
minimax(E,0,_Is_Maximizing,_,Value) :-
    posicion_pelota(E,p(_X,Y)),
    juego:cantidad_casilleros(H,_V),
    Value is H-Y.

recursive(E,MovsF,Len,Prof,Is_Maximizing) :-
    between(1,Len,I),
    arg(I,MovsF,mv(Mov,_)),
    mover(E,Mov,E2),
    (Is_Maximizing = true -> NotIM = false; NotIM = false),
    minimax(E2,Prof,NotIM,_,Value),
    nb_setarg(I,MovsF,mv(Mov,Value)),
    fail.

recursive(_,_,_,_,_).



