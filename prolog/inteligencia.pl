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

niveles_minimax(2).


%% hacer_jugada(+E,?LP,?E2)
%
% E2 es el estado resultante de mover según la lista de posiciones LP
% de un movimiento que la inteligencia elige jugar para el estado E.
% El turno de jugar en el estado E es el de la inteligencia,
% mientras que en E2 es del otro jugador.

hacer_jugada(E,LP,E2):- niveles_minimax(N),
                        turno(E,J),
                        minimax(E,N,true,LP,_,J,alpha(-1),beta(1000)),
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
min([mv(Mov,V)],Mov,V):- !.

min([mv(_Mov,0)|T],BM,Min) :-
    min(T,BM,Min),!.

min([mv(Mov,V)|T],BM,Min) :-
    V > 0,
    min(T,Bm2,Min2),
    Min2 \= 0,
    ((Min2<V)-> (BM = Bm2,Min = Min2);(BM=Mov,Min = V)),!.

min([mv(Mov,V)|T],BM,Min) :-
    V>0,
    min(T,_Bm2,Min_2),
    Min_2 = 0,
    BM=Mov,Min = V,!.



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


minimax(E,Prof,Is_Maximizing,BestMov,Value,Jugador,Alpha,Beta) :-

    % caso profundidad no nula (recrsivo)
    Prof > 0,
    NewProf is Prof-1,

    % encuentra todos los movimientos posibles a partir del estado E,
    % se guardan en un "diccionario" con valor 0
    % (podria usarse la estructura de diccionario de swipl, prefiero dejarlo
    % estandar por ahora (capaz si terminamos antes reimplementamos y
    % testeamos qué es más rápido))
    findall(mv(Mov,-500),mover(E,Mov,_),MovsPosibles),

    % en lugar de una lista, usamos una representacion functorial
    % para acceder en o(1) a cada indice (la otra opcion es pasar acumulador en
    % la llamada recursiva)
    MovsFunc =.. [movs|MovsPosibles],

    length(MovsPosibles,Len),

    % llamada recursiva, se llama minimax para cada entrada de MovsFunc
%    duplicate_term(Alpha,AlphaC),
 %   duplicate_term(Beta,BetaC),
    recursive(E,MovsFunc,Len,NewProf,Is_Maximizing,Jugador,Alpha,Beta),

    % despues se elige el maximo o el minimo de la lista, dependiendo del nodo
    % TODO: va a ser mas eficiente implementar directamente max y min
    % sobre la coleccion representada como functores y no pasar de uno a otro y
    % despues al primero
    MovsFunc =.. [_|Movs],
    (Is_Maximizing = true -> max(Movs,BestMov,Value)
     ;min(Movs,BestMov,Value),!).


minimax(E,0,_Is_Maximizing,_,Value,Jugador,_Alpha,_Beta) :-
    %     ↑
    % caso profundidad 0 (llegué al ultimo nivel),
    % se evalua la funcion de las hojas
    % TODO: usar el parametro Jugador, ahora esta pateando pa' abajo siempre,
    % mi idea es que el parametro venga fijo, como contexto,
    % (se puede usar variable global tambien sino)
    % se fija en hacer_jugada y se pasa siempre el mismo, aca en las hojas
    % segun el valor se decide si se quiere hacer gol arriba o abajo...
    distancia_arco(E,Jugador,Value).

% esta funcion evalua la distancia solo vertical, capaz hay que ponderar la
% horizontal tambien (para calcular distancia en pasos o cosas asi),
% o ver como se puede mejorar


distancia_arco(E,Jugador,Value) :-
    juego:cantidad_casilleros(_Ancho,Alto),
    posicion_pelota(E,p(_X,Y)),
    MitadAlto is Alto // 2 + 1,
    (Jugador = 1 -> % patea para abajo
         Value is MitadAlto + Y;
    %patea para arriba
    %(Jugador = 2 -> % patea para abajo
         Value is MitadAlto - Y).

% recursive (+E : Estado,
%            +MovsF : Functor con parametros mv(Mov,Int),
%            +Len : Int ,
%            +Prof : Int,
%            +Is_Maximizing : Bool,
%            +Jugador : Int)
% se hace la llamada recursiva del minimax, para cada movimiento posible, se
% hace el movimiento y se llama a minimax en ese escenario

recursive(E,MovsF,Len,Prof,Is_Maximizing,Jugador,Alpha,Beta) :-
    % loop-fail, para cada movimiento
    between(1,Len,I),
    arg(I,MovsF,mv(Mov,_)),

    Alpha = alpha(A),
    Beta  = beta(B),

    % hago el movimiento
    mover(E,Mov,E2),
    (Is_Maximizing = true ->
         duplicate_term(Alpha,AlphaC),
         duplicate_term(Beta,BetaC),
         minimax(E2,Prof,false,_,Value,Jugador,AlphaC,BetaC),
         NewA  = max(A,Value),
         nb_setarg(1,Alpha,NewA),
         (B =< A -> Poda = /**/true; Poda = false)

      %Not Is_Maximizing
     ;   duplicate_term(Alpha,AlphaC),
         duplicate_term(Beta,BetaC),
         minimax(E2,Prof,true,_,Value,Jugador,AlphaC,BetaC),
         NewB  = min(B,Value),
         nb_setarg(1,Beta,NewB),
         (B =< A -> Poda = /**/true; Poda = false)
    ),

    % llamo a minimax con ¬Is_Maximizing,
    % Prof reducido en 1 respecto a la llamada anterior (se hace en minimax,
    % antes de llamar a recursive para hacer la resta una vez sola)


    % el valor que retorna el minimax se setea en la entrada correspondiente
    % del diccionario (habia un 0 como placeholder)
    nb_setarg(I,MovsF,mv(Mov,Value)),
    arg(I,MovsF,mv(Mov,Value)),
    %writeln(Mov),
    (Poda = true -> true;fail).

recursive(_,_,_,_,_,_,_,_).%end del loop




prueba(X) :-
    between(1,X,I),
    writeln(I),
    (I=10 -> true,!; fail).

prueba(_).

