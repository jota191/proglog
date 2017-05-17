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
% TODO: la posicion inicial (7,5) esta hardcodeada, hay que calcular el centro
% en función de las dimensiones del tablero
% TODO: en general no es parametrico respecto al tamaño del tablero
% representamos a los vértices con un booleano que indica si fue visitado
% y una lista de las direcciones a las que se puede ir desde el mismo

estado_inicial(estado(Tablero,pelota(7,5),turno(jugador1))) :-
    cantidad_casilleros(K,L),
    M is L+3,  % modifico las dimensiones porque cuento vertices, y porque
    N is K+1,  % hay una fila mas atrasde cada arco
    inicial_medio(Medio),
    matriz_f(M,N,Medio,Tablero),
    inicializar_bandas(Tablero),
    %esquinas
    nuevo_valor_celda_f(2,1,Tablero,vertice(true,[se])),
    nuevo_valor_celda_f(2,9,Tablero,vertice(true,[sw])),
    nuevo_valor_celda_f(12,1,Tablero,vertice(true,[ne])),
    nuevo_valor_celda_f(12,9,Tablero,vertice(true,[nw])),
    %borde norte
    nuevo_valor_celda_f(2,2,Tablero,vertice(true,[se,s,sw])),
    nuevo_valor_celda_f(2,3,Tablero,vertice(true,[se,s,sw])),
    nuevo_valor_celda_f(2,7,Tablero,vertice(true,[se,s,sw])),
    nuevo_valor_celda_f(2,8,Tablero,vertice(true,[se,s,sw])),
    nuevo_valor_celda_f(1,5,Tablero,vertice(true,[se,s,sw])),%arco

    %borde sur
    nuevo_valor_celda_f(12,2,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(12,3,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(12,7,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(12,8,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(13,5,Tablero,vertice(true,[n,ne,nw])),%arco

    %dentro del arco (igual a esquinas)
    nuevo_valor_celda_f(1,4,Tablero,vertice(true,[se])),
    nuevo_valor_celda_f(1,6,Tablero,vertice(true,[sw])),
    nuevo_valor_celda_f(13,4,Tablero,vertice(true,[ne])),
    nuevo_valor_celda_f(13,6,Tablero,vertice(true,[nw])),

    %palos
    nuevo_valor_celda_f(2,4,Tablero,vertice(true,[ne,e,se,s,sw])),
    nuevo_valor_celda_f(2,6,Tablero,vertice(true,[se,s,sw,w,nw])),
    nuevo_valor_celda_f(12,4,Tablero,vertice(true,[n,ne,e,se,nw])),
    nuevo_valor_celda_f(12,6,Tablero,vertice(true,[n,ne,sw,w,nw])),
    %afuera
    nuevo_valor_celda_f(1,1,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(1,2,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(1,3,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(1,7,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(1,8,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(1,9,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(13,1,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(13,2,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(13,3,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(13,7,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(13,8,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(13,9,Tablero,vertice(true,[])),!.

    %% inicializar_bandas(+Tablero)
% Setea el estado de las bandas del tablero, oeste y este
% TODO : parametrizar segun el tamaño del tablero

inicializar_bandas(Tablero) :-
    inicializar_bandas(Tablero,3).

inicializar_bandas(_Tablero,12). %paro
inicializar_bandas(Tablero,F) :-
    inicial_oeste(Oeste),
    inicial_este(Este),
    nuevo_valor_celda_f(F,1,Tablero,Oeste),
    nuevo_valor_celda_f(F,9,Tablero,Este),
    NewF is F+1,
    inicializar_bandas(Tablero,NewF).

/* en esto me confundí feo y lo dejo como referencia,
que los predicados eficientes del lab1 tengan efectos no implica que no se haga
backtracking sobre ellos (este predicado asi como esta, no cambia el Tablero)

inicializar_bandas(Tablero) :-
    inicial_oeste(Oeste),
    inicial_este(Este),
    between(3,11,F),
    nuevo_valor_celda_f(F,1,Tablero,Oeste),
    nuevo_valor_celda_f(F,9,Tablero,Este),
    fail.

inicializar_bandas(_).
*/

%% inicial_medio(?Vertice)
% predicado que define las casillas iniciales del medio del tablero
inicial_medio(vertice(false,[n,ne,e,se,s,sw,w,nw])).

%% inicial_{oeste|este}(?Vertice)
% predicado que define las casillas iniciales del {oeste|este} del tablero
inicial_oeste(vertice(true,[ne,e,se])).
inicial_este(vertice(true,[sw,w,nw])).


%% posicion_pelota(+E,?P)
%
% P es la posición de la pelota para el estado E.

posicion_pelota(estado(_,pelota(M,N),_),p(X,Y)) :-
    X is M-7,
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

gol(estado(_Tablero,pelota(M,N),_),jugador1):-
    cantidad_casilleros(C,_F),
    N2 is div(C,2) + 1,%pelota en columna central
    M2 is 1, % en fila 1
    abs(N2-N) =< 1,
    abs(M2-M) =< 1,!.

gol(estado(_Tablero,pelota(M,N),_),jugador2):-
    cantidad_casilleros(C,F),
    N2 is div(C,2) + 1,%pelota en columna central
    M2 is F+3, % en la ultima fila
    abs(N2-N) =< 1,
    abs(M2-M) =< 1.



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


%% para testear estado inicial
% (estado_inicial(E),juego:print_Estado(E)) imprime un dibujito con sentido
% (esto fué de hecho bastante útil y encontré bugs)

print_Estado(estado(Board,_,_)) :-
    between(1,13,F),
    nl,
    between(1,9,C),
    valor_celda_f(F,C,Board,V),
    print_cell(V),
    fail.
print_Estado(_).

%vertices afuera, que solo existen en las filas de los arcos
print_cell(vertice(_,[])) :- write(' '),!.
%vertices internos
print_cell(vertice(false,[n,ne,e,se,s,sw,w,nw])) :-
    write('┼'),!. %unicode 253C
%vertices bandas
print_cell(vertice(_,[ne,e,se])) :-
    write('┠'),!.
print_cell(vertice(_,[sw,w,nw])) :-
    write('┨'),!.
%vertices fondo
print_cell(vertice(_,[se,s,sw])) :-
    write('┯'),!.
print_cell(vertice(_,[n,ne,nw])) :-
    write('┷'),!.
%vertices esquinas
print_cell(vertice(_,[se])) :-
    write('┏'),!.
print_cell(vertice(_,[sw])) :-
    write('┓'),!.
print_cell(vertice(_,[ne])) :-
    write('┗'),!.
print_cell(vertice(_,[nw])) :-
    write('┛'),!.
%vertices palos
print_cell(vertice(_,[n,ne,e,se,nw])) :-
    write('╅'),!.
print_cell(vertice(_,[n,ne,sw,w,nw])) :-
    write('╆'),!.
print_cell(vertice(_,[ne,e,se,s,sw])) :-
    write('╃'),!.
print_cell(vertice(_,[se,s,sw,w,nw])) :-
    write('╄'),!.

print_cell(_) :- write('X').%no deberían aparecer X en el tablero inicial


/*con un tablero inicial deberia imprimirse:
   ┏┯┓   
┏┯┯╃┼╄┯┯┓
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┠┼┼┼┼┼┼┼┨
┗┷┷╅┼╆┷┷┛
   ┗┷┛   
*/

%% unit test suite
:- begin_tests(juego).

test(posicion_pelota_inicial) :-
    estado_inicial(E),
    posicion_pelota(E,p(0,0)).

:- end_tests(juego).
