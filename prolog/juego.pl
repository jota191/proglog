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

estado_inicial(estado(Tablero,pelota(7,5),turno(1))) :-
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

    %borde sur
    nuevo_valor_celda_f(12,2,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(12,3,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(12,7,Tablero,vertice(true,[n,ne,nw])),
    nuevo_valor_celda_f(12,8,Tablero,vertice(true,[n,ne,nw])),

    %dentro del arco
    nuevo_valor_celda_f(1,4,Tablero,vertice(false,[se])),
    nuevo_valor_celda_f(1,6,Tablero,vertice(false,[sw])),
    nuevo_valor_celda_f(13,4,Tablero,vertice(false,[ne])),
    nuevo_valor_celda_f(13,6,Tablero,vertice(false,[nw])),

    nuevo_valor_celda_f(13,5,Tablero,vertice(false,[n,ne,nw])),%arco
    nuevo_valor_celda_f(1,5,Tablero,vertice(false,[se,s,sw])),%arco

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
    nuevo_valor_celda_f(13,9,Tablero,vertice(true,[])),
    nuevo_valor_celda_f(7,5,Tablero,vertice(true,[n,ne,e,se,s,sw,w,nw])),!.

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
% predicado que define los vértices iniciales del medio del tablero
inicial_medio(vertice(false,[n,ne,e,se,s,sw,w,nw])).

%% inicial_{oeste|este}(?Vertice)
% predicado que define los vértices iniciales del {oeste|este} del tablero
inicial_oeste(vertice(true,[ne,e,se])).
inicial_este(vertice(true,[sw,w,nw])).


%% posicion_pelota(+E,?P)
%
% P es la posición de la pelota para el estado E.

posicion_pelota(estado(_,pelota(M,N),_),p(X,Y)) :-
    traducir_coordenadas(interna(M,N),interfaz(X,Y)).



%% traducir_coordenadas/2 (?Interna,?Interfaz)
%
% traduce de coordenadas internas ((1,1) es el vértice
% de arriba a la izquierda)
% a coordenadas usadas por la interfaz web ((0,0) es
% el vértice central del tablero)
% funciona con cualquiera de los dos instanciado

traducir_coordenadas(interna(F,C),interfaz(X,Y)) :-
    (ground(F) -> Y is 7-F,
                  X is C-5);
    (F is 7-Y, C is X+5).
    %%TODO: parametrizar con dimension del tablero


%% mover(+E,?LP,?E2)
%
% E2 el estado resultante de hacer un movimiento con la pelota,
% a través de las posiciones de la lista LP en el estado E
% y de cambiar el turno.

%mover(E,_,E). % TODO

%una instancia particular para testear, consiste en, a partir del estado inicial,
% mover hacia el norte

%mover(Einput,[p(0,1)],Eoutput) :-
%    Einput = estado(Tablero,pelota(7,5),turno(1)),
%    nuevo_valor_celda_f(7,5,Tablero,vertice(true,[ne,e,se,s,sw,w,nw])),
%    nuevo_valor_celda_f(6,5,Tablero,vertice(true,[n,ne,e,se,sw,w,nw])),
%    Eoutput = estado(Tablero,pelota(6,5),turno(2)).


mover(E,L,EOutput) :-
    snoc(Prefix,p(X,Y),L),
    prefijo_movimiento2(E,Prefix),
    mover_pelota(E,_D),
    posicion_pelota(E,p(X,Y)),
    traducir_coordenadas(interna(F,C),interfaz(X,Y)),
    arg(1,E,Tablero),
    valor_celda_f(F,C,Tablero,vertice(false,Dirs)),
    nuevo_valor_celda_f(F,C,Tablero,vertice(true,Dirs)),
    (turno(E,1) -> setarg(3,E,turno(2));setarg(3,E,turno(1))),
    EOutput = E.

% el ultimo movimiento del prefijo se inserta al final de la lista,
% implemento este snoc para salir del paso pero es mejorable
% en cuanto a la performance i se usan predicados extralogicos

% snoc/2(+L,?X,?L2) 
snoc([],X,[X]).
snoc([H|T],X,[H|L2]) :- snoc(T,X,L2).



%% gol(+E,?NJugador)
%
% La pelota está en situación de gol a favor del jugador NJugador
% para el estado E.

gol(estado(_Tablero,pelota(F,C),_),1):-
    cantidad_casilleros(NCols,NFilas),
    Central is div(NCols,2) + 1,%pelota en columna central
    F is 1, % en fila 1
    abs(Central-C) =< 1,!.

gol(estado(_Tablero,pelota(F,C),_),1):-
    cantidad_casilleros(NCols,NFilas),
    Central is div(NCols,2) + 1,%pelota en columna central
    F is NFilas+3,
    abs(Central-C) =< 1,!.


%% turno(+E,?NJugador)
%
% NJugador es el jugador que tiene que mover en el siguiente turno
% para el estado E.

turno(estado(_,_,turno(J)),J).


%% eliminar_direccion(+LDirIn,+Dir,?LDirOut)
%
% elimina la direccion D de la lista de direcciones LDirIn
% se implementa trivialmente con sin_elem del lab1, prefiero usar este wrapper
% por si despues optimizamos el conjunto de direcciones como algo que no sea
% una lista o así

eliminar_direccion(LDirIn,Dir,LDirOut) :-
    sin_elem(LDirIn,Dir,LDirOut),!.

%% prefijo_movimiento(+E,+LP)
%
% LP es una lista no vacía de posiciones que constituyen
% el prefijo de un movimiento para el estado E,
% sin llegar a formar un movimiento.
% (Se usa para validar las jugadas de un jugador humano)

% La diferencia entre un prefijo y un movimiento, es que en el prefijo
% todos los pasos se dan sobre casillas ya. (De hecho un movimiento toca
% exactamente UNA casilla sin visitar, la última)

%prefijo_movimiento(_,_). % TODO
prefijo_movimiento2(E,[]).

prefijo_movimiento2(E,L) :-
    snoc(Prefijo,p(X,Y),L),
    prefijo_movimiento2(E,Prefijo),
    mover_pelota(E,_D),
    posicion_pelota(E,p(X,Y)),
    traducir_coordenadas(interna(F,C),interfaz(X,Y)),
    arg(1,E,Tablero),
    valor_celda_f(F,C,Tablero,vertice(true,_)).
    %F /= 1. %%si estoy en el arco, no es prefijo

prefijo_movimiento(E,L) :-
    prefijo_movimiento2(E,L),
    L \= [].



%% para testear estado inicial
% (estado_inicial(E),juego:print_Estado(E)) imprime un dibujito con sentido
% (esto fué de hecho bastante útil y encontré bugs)




%% mover_pelota(+E,Dir)
% --predicado impuro (efecto sobre E)
% realiza un movimiento de la pelota en el estado E, en la direccion Dir
%


mover_pelota(E,n) :-
    E =.. [estado,Tablero,pelota(F,C),turno(J)],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,n,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    FNew is F-1,
    valor_celda_f(FNew,C,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,s,NewDirs2),
    nuevo_valor_celda_f(FNew,C,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(FNew,C)).

mover_pelota(E,s) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,s,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    FNew is F+1,
    valor_celda_f(FNew,C,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,n,NewDirs2),
    nuevo_valor_celda_f(FNew,C,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(FNew,C)).

mover_pelota(E,e) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,e,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    CNew is C+1,
    valor_celda_f(F,CNew,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,w,NewDirs2),
    nuevo_valor_celda_f(F,CNew,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(F,CNew)).

mover_pelota(E,w) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,w,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    CNew is C-1,
    valor_celda_f(F,CNew,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,e,NewDirs2),
    nuevo_valor_celda_f(F,CNew,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(F,CNew)).

mover_pelota(E,ne) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,ne,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    CNew is C+1,
    FNew is F-1,
    valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,sw,NewDirs2),
    nuevo_valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(FNew,CNew)).

mover_pelota(E,sw) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,sw,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    CNew is C-1,
    FNew is F+1,
    valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,ne,NewDirs2),
    nuevo_valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(FNew,CNew)).

mover_pelota(E,nw) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,nw,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    CNew is C-1,
    FNew is F-1,
    valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,se,NewDirs2),
    nuevo_valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(FNew,CNew)).

mover_pelota(E,se) :-
    E =.. [estado,Tablero,pelota(F,C),_J],
    valor_celda_f(F,C,Tablero,vertice(Vis,Dirs)),
    eliminar_direccion(Dirs,se,NewDirs),
    nuevo_valor_celda_f(F,C,Tablero,vertice(Vis,NewDirs)),
    CNew is C+1,
    FNew is F+1,
    valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,Dirs2)),
    eliminar_direccion(Dirs2,nw,NewDirs2),
    nuevo_valor_celda_f(FNew,CNew,Tablero,vertice(Vis2,NewDirs2)),
    setarg(2,E,pelota(FNew,CNew)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TESTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
print_cell(vertice(_,[n,ne,e,se,s,sw,w,nw])) :-
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


print_Visitas(estado(Board,_,_)) :-
    between(1,13,F),
    nl,
    between(1,9,C),
    valor_celda_f(F,C,Board,V),
    print_vis(V),
    fail.
print_Visitas(_).

%vertices afuera, que solo existen en las filas de los arcos
print_vis(vertice(false,_)) :- write(' '),!.
print_vis(vertice(true,_)) :- write('X'),!.


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
