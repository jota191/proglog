:- module(juego,[cantidad_casilleros/2,
                 estado_inicial/1,
                 posicion_pelota/2,
                 mover/3,
                 prefijo_movimiento/2,
                 gol/2,
                 turno/2]).

:- use_module(lab1).
% probablemente tengamos que usar los predicados de matrices del lab1,
% por ahora lo importé y estoy usando el mio en mi repo (sin pushearlo,
% despues vemos cual usamos y eso -el que anda más rápido?-)


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
%
% La pelota se representa (aca) con coordenadas del tablero,
% despues hacia afuera las mandamos con el sistema que la interfaz requiere
% TODO: la posicion inicial (5,5) esta hardcodeada, hay que calcular el centro
% en función de las dimensiones del tablero
%% representamos a los vértices con un booleano que indica si fue visitado
% y una lista de las direcciones a las que se puede ir desde el mismo

estado_inicial(estado(Tablero,pelota(5,5),turno(jugador1))) :-
    cantidad_casilleros(K,L),
    M is L+3,  % modifico las dimensiones porque cuento vertices, y porque
    N is K+1,  % hay una fila mas atrasde cada arco
    inicial_medio(Medio),
    matriz_f(M,N,Medio,Tablero),
    inicializar_bandas(Tablero). %corte elimina choicepoints


%% inicializar_bandas(+Tablero)
% -predicado con efecto, iteración loopfail-
% Setea el estado de las bandas del tablero, oeste y este
% TODO : parametrizar segun el tamaño del tablero
inicializar_bandas(Tablero) :-
    inicial_oeste(Oeste),
    inicial_este(Este),
    between(3,11,F),
    nuevo_valor_celda_f(F,1,Tablero,Oeste),
    nuevo_valor_celda_f(F,9,Tablero,Este),
    fail.

inicializar_bandas(_).


%% inicial_medio(?Vertice)
% predicado que define las casillas iniciales del medio del tablero
inicial_medio(vertice(false,[n,ne,e,se,s,sw,w,nw])).

%% inicial_{oeste|este}(?Vertice)
% predicado que define las casillas iniciales del {oeste|este} del tablero
inicial_oeste(vertice(true,[n,ne,e,se,s])).
inicial_este(vertice(true,[n,s,sw,w,nw])).


%% posicion_pelota(+E,?P)
%
% P es la posición de la pelota para el estado E.

posicion_pelota(estado(_,pelota(M,N),_),p(X,Y)) :-
    X is M-5,
    Y is N-5.
    % TODO : esto no es parametrico con la dimension del tablero


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
% para el estado E.

turno(_,1). % TODO


%% prefijo_movimiento(+E,+LP)
%
% LP es una lista no vacía de posiciones que constituyen
% el prefijo de un movimiento para el estado E,
% sin llegar a formar un movimiento.
% (Se usa para validar las jugadas de un jugador humano)

prefijo_movimiento(_,_). % TODO






%% para testear
print_Estado(estado(Board,_,_)) :-
    between(1,13,F),
    nl,
    between(1,9,C),
    valor_celda_f(F,C,Board,V),
    print_cell(V),
    fail.
print_Estado(_).

print_cell(vertice(false,[])) :- write('_').
print_cell(vertice(false,[n,ne,e,se,s,sw,w,nw])) :-
    write('┼'). %unicode 253C
print_cell(vertice(false,[n,ne,e,se,s])) :-
    write('├').


%% unit test suite
:- begin_tests(juego).

test(posicion_pelota_inicial) :-
    estado_inicial(E),
    posicion_pelota(E,p(0,0)).

:- end_tests(juego).
