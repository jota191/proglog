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

%% tablero_offset(HalfWidth,HalfHeight) :-
%%    cantidad_casilleros(Width, Height),
%%    HalfWidth is Width / 2,
%%    HalfHeight is Height / 2.

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
% TODO: en general no es parametrico respecto al tamaño del tablero
% representamos a los vértices con un booleano que indica si fue visitado
% y una lista de las direcciones a las que se puede ir desde el mismo

estado_inicial(estado(Tablero,pelota(PelotaX,PelotaY),turno(1))) :-
    cantidad_casilleros(K,L),
    M is L+3,  % modifico las dimensiones porque cuento vertices, y porque
    N is K+1,  % hay una fila mas atras de cada linea de meta
    PelotaX is div(M,2) + 1,
    PelotaY is div(N,2) + 1,
    inicial_medio(Medio),
    %% FilasMatriz is M + 1,
    %% ColumnasMatriz is N + 1,
    generar_f(Medio,M,N,Tablero),
    % se inicializa a cada vertice como si
    % tuviera todos los movimientos disponibles, y despues
    % se cambian aquellos que esten mas limitados
    inicializar_bandas(Tablero,N,M),
    % se inicializan celdas de extremos este y oeste
    %esquinas

    nuevo_valor_celda_f(2,1,Tablero,vertice(true,dirs(f,f,f,se,f,f,f,f))),
    nuevo_valor_celda_f(2,N,Tablero,vertice(true,dirs(f,f,f,f,f,sw,f,f))),
    FilaEsquinaInferior is M - 1,
    nuevo_valor_celda_f(FilaEsquinaInferior,1,Tablero,
                        vertice(true,dirs(f,ne,f,f,f,f,f,f))),
    nuevo_valor_celda_f(FilaEsquinaInferior,N,Tablero,
                        vertice(true,dirs(f,f,f,f,f,f,f,nw))),
    % Lineas de meta.
    inicializar_lineas_meta(Tablero,N,M),
    % Inicializacion arco y palos.
    inicializar_arcos_y_palos(Tablero,N,M),
    % Lineas externas.
    inicializar_lineas_externas(Tablero,N,M),
    nuevo_valor_celda_f(PelotaX,PelotaY,Tablero,
                        vertice(true,dirs(n,ne,e,se,s,sw,w,nw))),!.

% inicializar_arcos_y_palos(+Tablero, +CantidadColumnas)
% Se inicializan los vértices correspondientes a los arcos y palos del tablero.
inicializar_arcos_y_palos(Tablero, CantidadColumnas, CantidadFilas) :-
    % vertices arcos
    C is div(CantidadColumnas, 2),
    nuevo_valor_celda_f(1,C,Tablero,
                        vertice(false,dirs(f,f,f,se,f,f,f,f))),%oeste
    nuevo_valor_celda_f(CantidadFilas,C,Tablero,
                        vertice(false,dirs(f,ne,f,f,f,f,f,f))), %oeste
    C1 is C+1,
    nuevo_valor_celda_f(1,C1,Tablero,
                        vertice(false,dirs(f,f,f,se,s,sw,f,f))), %norte
    nuevo_valor_celda_f(CantidadFilas,C1,Tablero,
                        vertice(false,dirs(n,ne,f,f,f,f,f,nw))), %norte
    C2 is C1+1,
    nuevo_valor_celda_f(1,C2,Tablero,
                        vertice(false,dirs(f,f,f,f,f,sw,f,f))), %este
    nuevo_valor_celda_f(CantidadFilas,C2,Tablero,
                        vertice(false,dirs(f,f,f,f,f,f,f,nw))), %este
    %palos
    F is CantidadFilas - 1,
    nuevo_valor_celda_f(2,C,Tablero,
                        vertice(true,dirs(f,ne,e,se,s,sw,f,f))), %oeste
    nuevo_valor_celda_f(2,C2,Tablero,
                        vertice(true,dirs(f,f,f,se,s,sw,w,nw))), %este
    nuevo_valor_celda_f(F,C,Tablero,
                        vertice(true,dirs(n,ne,e,se,f,f,f,nw))),%oeste
    nuevo_valor_celda_f(F,C2,Tablero,
                        vertice(true,dirs(n,ne,f,f,f,sw,w,nw))). %este

% inicializar_lineas_externas(+Tablero,+CantidadColumnas,+CantidadFilas)
% Setea el estado de las lineas externas al tablero.
inicializar_lineas_externas(Tablero,CantidadColumnas,CantidadFilas) :-
    %% UltimaColumna is CantidadColumnas - 1, % ajusto columnas para
    % no incluir ultima columna (no linea de meta)
    % que no son esquina pero tampoco linea de meta
    inicializar_linea_externa(Tablero,1,1,CantidadColumnas),
    inicializar_linea_externa(Tablero,CantidadFilas,1,CantidadColumnas).

% inicializar_linea_externa(+Tablero,+Fila,+ColumnaActual,+CantidadColumnas)
% Inicializa la línea externa de Tablero dada por Fila y ColumnaActual,
% tomando como limite para iterar al valor CantidadColumnas.
inicializar_linea_externa(Tablero,Fila,CantidadColumnas,CantidadColumnas) :-
    actualizar_celda_linea_externa(Tablero, CantidadColumnas,
                                   CantidadColumnas, Fila).
inicializar_linea_externa(Tablero,Fila,ColumnaActual,CantidadColumnas) :-
    actualizar_celda_linea_externa(Tablero, ColumnaActual,
                                   CantidadColumnas, Fila),
    NuevaColumnaActual is ColumnaActual + 1,
    (NuevaColumnaActual =< CantidadColumnas) ->
        inicializar_linea_externa(Tablero,
                                  Fila,
                                  NuevaColumnaActual,
                                  CantidadColumnas)
        ; true.

% actualizar_celda_linea_externa(+Tablero, +ColumnaActual,
%                                +CantidadColumnas, +Fila)
% Actualiza una celda de una linea externa de Tablero,
% dada por Fila y ColumnaActual, para que tome el valor que corresponda.
% Usa CantidadColumnas para verificar que la columna no corresponda
% a alguna de un arco.
actualizar_celda_linea_externa(Tablero, ColumnaActual,CantidadColumnas,Fila) :-
    columna_fuera_arco(ColumnaActual,CantidadColumnas) ->
        (inicial_borde_externo(Vertice),
         nuevo_valor_celda_f(Fila,ColumnaActual,Tablero,Vertice))
        ; true.

% inicializar_lineas_meta(+Tablero,+CantidadColumnas,+CantidadFilas)
% Setea el estado de las bandas de Tablero, oeste y este.
inicializar_lineas_meta(Tablero,CantidadColumnas,CantidadFilas) :-
    inicializar_linea_meta(Tablero,2,2,CantidadColumnas),
    UltimaLineaMeta is CantidadFilas - 1,
    inicializar_linea_meta(Tablero,UltimaLineaMeta,2,CantidadColumnas).

% inicializar_linea_meta(+Tablero,+Fila,+ColumnaActual,+CantidadColumnas)
% Recorre la fila indicada por Fila inicializando los vertices
% de acuerdo a inicial_borde_norte,
% a excepcion de los palos del arco o vertices internos al arco.
inicializar_linea_meta(_,_,CantidadColumnas,CantidadColumnas).
inicializar_linea_meta(Tablero,Fila,ColumnaActual,CantidadColumnas) :-
    actualizar_celda_linea_meta(Tablero, ColumnaActual, CantidadColumnas, Fila),
    NuevaColumnaActual is ColumnaActual + 1,
    inicializar_linea_meta(Tablero,Fila,NuevaColumnaActual,CantidadColumnas).

% actualizar_celda_linea_meta(+Tablero, +ColumnaActual, +CantidadColumnas, +Fila)
% Actualiza una celda de la linea de meta en Tablero, dada por Fila y ColumnaActual, para que tome el valor que corresponda. 
% Usa CantidadColumnas para verificar que la columna no corresponda a alguna de un arco.
actualizar_celda_linea_meta(Tablero, ColumnaActual, CantidadColumnas, Fila) :-
    columna_fuera_arco(ColumnaActual,CantidadColumnas) ->
    ((Fila =:= 2 -> inicial_borde_norte(Vertice)
         ; inicial_borde_sur(Vertice)),
           nuevo_valor_celda_f(Fila,ColumnaActual,Tablero,Vertice))
    ; true.

% columna_fuera_arco(+Columna,+CantidadColumnas) 
% La columna dada en Columna no es interna a un arco ni coincide con las de sus palos.
% Se usa CantidadColumnas para verificar que la columna sea interna al tablero.
columna_fuera_arco(Columna,CantidadColumnas) :-
    ColumnaPaloIzq is div(CantidadColumnas,2),
    Columna > 0,
    Columna < ColumnaPaloIzq, !.
columna_fuera_arco(Columna,CantidadColumnas) :-
    ColumnaPaloDer is div(CantidadColumnas,2) + 2,
    Columna =< CantidadColumnas,
    Columna > ColumnaPaloDer.


% inicializar_bandas(+Tablero,+CantidadColumnas,+CantidadFilas)
% Setea el estado de las bandas de Tablero, oeste y este.
% Se utilizan CantidadColumnas y CantidadFilas para limitar la inicialización.
inicializar_bandas(Tablero,CantidadColumnas,CantidadFilas) :-
    % Se ajustan las filas para que no se tome la linea de meta
    MaximaFila is CantidadFilas - 1,
    inicializar_bandas(Tablero,3,MaximaFila,CantidadColumnas).
    % se arranca por la fila 3 (debajo del arco)

% inicializar_bandas(+Tablero,+FilaActual,+MaximaFila,+MaximaColumna)
% Setea el estado de las bandas de Tablero, oeste y este en la fila FilaActual.
% Se utilizan MaximaFila y MaximaColumna para limitar la inicialización.
inicializar_bandas(_,MaximaFila,MaximaFila,_). % Condición de parada.
inicializar_bandas(Tablero,FilaActual,MaximaFila,MaximaColumna) :-
    inicial_oeste(Oeste),
    inicial_este(Este),
    nuevo_valor_celda_f(FilaActual,1,Tablero,Oeste),
    nuevo_valor_celda_f(FilaActual,MaximaColumna,Tablero,Este),
    NuevaFilaActual is FilaActual+1,
    inicializar_bandas(Tablero,NuevaFilaActual,MaximaFila,MaximaColumna).

% inicial_medio(?Vertice)
% Predicado que define los vértices iniciales del medio del tablero.
inicial_medio(vertice(false,dirs(n,ne,e,se,s,sw,w,nw))).

% inicial_{oeste|este|borde_norte|borde_sur|borde_externo}(?Vertice)
% Predicado que define los vértices iniciales del
% {oeste|este|borde_norte|borde_sur|borde_externo} del tablero.
inicial_oeste(vertice(true,dirs(f,ne,e,se,f,f,f,f))).
inicial_este(vertice(true,dirs(f,f,f,f,f,sw,w,nw))).
inicial_borde_norte(vertice(true,dirs(f,f,f,se,s,sw,f,f))).
inicial_borde_sur(vertice(true,dirs(n,ne,f,f,f,f,f,nw))).
inicial_borde_externo(vertice(true, dirs(f,f,f,f,f,f,f,f))).

% posicion_pelota(+E,?P)
% P es la posición de la pelota para el estado E.
posicion_pelota(estado(_,pelota(M,N),_),p(X,Y)) :-
    traducir_coordenadas(interna(M,N),interfaz(X,Y)).

% traducir_coordenadas (?Interna,?Interfaz)
% Traduce de coordenadas internas ((1,1) es el vértice
% de arriba a la izquierda)
% a coordenadas usadas por la interfaz web ((0,0) es
% el vértice central del tablero).
% Funciona con cualquiera de los dos instanciado.
traducir_coordenadas(Interna,Interfaz) :-
    cantidad_casilleros(Ancho,Alto),
    OffsetColumna is div(Ancho,2) + 1,
    OffsetFila is div(Alto,2) + 2,
    traducir_coordenadas(Interna,Interfaz,OffsetFila,OffsetColumna).

traducir_coordenadas(interna(F,C),interfaz(X,Y),OffsetFila,OffsetColumna) :-
    (ground(F) -> Y is OffsetFila-F,
                  X is C-OffsetColumna);
    (F is OffsetFila-Y, C is X+OffsetColumna).

% gol(+E,?NJugador)
% La pelota está en situación de gol a favor del jugador NJugador
% para el estado E.
gol(estado(_Tablero,pelota(F,C),_),1):-
    cantidad_casilleros(NCols,_NFilas),
    Central is div(NCols,2) + 1,%pelota en columna central
    F is 1, % en fila 1
    abs(Central-C) =< 1,!.

gol(estado(_Tablero,pelota(F,C),_),2):-
    cantidad_casilleros(NCols,NFilas),
    Central is div(NCols,2) + 1,%pelota en columna central
    F is NFilas+3,
    abs(Central-C) =< 1,!.

% turno(+E,?NJugador)
% NJugador es el jugador que tiene que mover en el siguiente turno
% para el estado E.
turno(estado(_,_,turno(J)),J).

%
% eliminar_direccion(+LDirIn,+Dir,?LDirOut)
% Elimina la direccion D de la lista de direcciones LDirIn
% se implementa trivialmente con sin_elem del lab1, prefiero usar este wrapper
% por si despues optimizamos el conjunto de direcciones como algo que no sea
% una lista o así.
% s̶e̶ i̶m̶p̶l̶e̶m̶e̶n̶t̶a̶ t̶r̶i̶v̶i̶a̶l̶m̶e̶n̶t̶e̶ c̶o̶n̶ s̶i̶n̶_̶e̶l̶e̶m̶ d̶e̶l̶ l̶a̶b̶1̶,̶ p̶r̶e̶f̶i̶e̶r̶o̶ u̶s̶a̶r̶
% e̶s̶t̶e̶ w̶r̶a̶p̶p̶e̶r̶ p̶o̶r̶ s̶i̶ d̶e̶s̶p̶u̶e̶s̶ o̶p̶t̶i̶m̶i̶z̶a̶m̶o̶s̶ e̶l̶
% c̶o̶n̶j̶u̶n̶t̶o̶ d̶e̶ d̶i̶r̶e̶c̶c̶i̶o̶n̶e̶s̶ c̶o̶m̶o̶ a̶l̶g̶o̶ q̶u̶e̶ n̶o̶ s̶e̶a̶ u̶n̶a̶ l̶i̶s̶t̶a̶ o̶ a̶s̶í̶.̶

% eliminar_direccion(LDirIn,Dir,LDirOut) :-
%    sin_elem(LDirIn,Dir,LDirOut),!.

% optimizacion, representacion con functores
% el codigo queda mas largo, hay que hacer case analysis pero esto deberia ser
% mas eficiente que manipular las listas (igual no es una mejora radical)
% el parametro de salida ya no es necesario ya que el predicado computa
% un efecto sobre la entrada, se mantiene la interfaz de todas formas
% (analogo a lo que pasa con mover/3 que podria ser mover/2)

eliminar_direccion(DirsIn,n,DirsIn)  :-
    arg(1,DirsIn,n),
    setarg(1,DirsIn,f).
eliminar_direccion(DirsIn,ne,DirsIn) :-
    arg(2,DirsIn,ne),
    setarg(2,DirsIn,f).
eliminar_direccion(DirsIn,e,DirsIn)  :-
    arg(3,DirsIn,e),
    setarg(3,DirsIn,f).
eliminar_direccion(DirsIn,se,DirsIn) :-
    arg(4,DirsIn,se),
    setarg(4,DirsIn,f).
eliminar_direccion(DirsIn,s,DirsIn)  :-
    arg(5,DirsIn,s),
    setarg(5,DirsIn,f).
eliminar_direccion(DirsIn,sw,DirsIn) :-
    arg(6,DirsIn,sw),
    setarg(6,DirsIn,f).
eliminar_direccion(DirsIn,w,DirsIn)  :-
    arg(7,DirsIn,w),
    setarg(7,DirsIn,f).
eliminar_direccion(DirsIn,nw,DirsIn) :-
    arg(8,DirsIn,nw),
    setarg(8,DirsIn,f).





% prefijo_movimiento(+E,+LP)
% LP es una lista no vacía de posiciones que constituyen
% el prefijo de un movimiento para el estado E,
% sin llegar a formar un movimiento.
% (Se usa para validar las jugadas de un jugador humano)
% La diferencia entre un prefijo y un movimiento, es que en el prefijo
% todos los pasos se dan sobre casillas ya visitadas. (De hecho un movimiento toca
% exactamente UNA casilla sin visitar, la última)
prefijo_movimiento(E,L) :-
    L \= [],
    prefijo_movimiento2(E,L).


% prefijo_movimiento2(+E,+LP)
% Análogo a prefijo_movimiento pero admite listas vacías.
% Podría definirse prefijo_movimiento directamente para que no sea verdadero
% en listas vacías pero prefijo_movimiento2 sera util en mover.
% (Tiene de hecho sentido que la lista vacía sea un prefijo,
% sin embargo si no se
% respeta la especificación la interfaz anda mal)
prefijo_movimiento2(_,[]).

prefijo_movimiento2(E,[p(X,Y)|Prefijo]) :-
    mover_pelota(E,_D),
    arg(1,E,Tablero),
    posicion_pelota(E,p(X,Y)),
    % verifico que mover_pelota me haya hecho coincidir a la pelota
    % con la posicion dada por p(X,Y)
    traducir_coordenadas(interna(F,C),interfaz(X,Y)),
    \+(celda_borde(F,C)), % No es una celda de algún borde (que siempre es visitada)
    valor_celda_f(F,C,Tablero,vertice(true,_)), % chequea que la celda este visitada
    % si no fue visitada, entonces esto devuelve false y da la pauta de que
    % el movimiento se puede realizar (último elemento de la lista pasada a prefijo_movimiento)
    prefijo_movimiento2(E,Prefijo).

prefijo_movimiento2(E,[p(X,Y)|Prefijo]) :-
    mover_pelota(E,_D),
    arg(1,E,Tablero),
    posicion_pelota(E,p(X,Y)),
    traducir_coordenadas(interna(F,C),interfaz(X,Y)),
    celda_borde(F,C),
    valor_celda_f(F,C,Tablero,vertice(true,Dirs)),
    largo(Dirs,N), N > 0, % mientras un vertice de borde sea visitable
    prefijo_movimiento2(E,Prefijo).

% celda_borde(+Fila,+Columna)
% La celda dada por (Fila,Columna) es de borde (o sea una esquina, linea de meta, o banda este/oeste)

celda_borde(_,1).
% Columna este.
celda_borde(_,Columna) :- cantidad_casilleros(X,_), Columna is X + 1.
% Segunda fila.
celda_borde(2, Columna) :- cantidad_casilleros(X,_), Y is X + 1, columna_fuera_arco(Columna, Y).
% Penúltima fila.
celda_borde(Fila, Columna) :- cantidad_casilleros(_,Fila), cantidad_casilleros(_,X), Y is X + 1, columna_fuera_arco(Columna, Y).


%% mover(+E,?LP,?E2)
%
% E2 el estado resultante de hacer un movimiento con la pelota,
% a través de las posiciones de la lista LP en el estado E
% y de cambiar el turno.
mover(E,L,EOut) :-
    prefijo_movimiento2(E,Prefijo),
    mover_pelota(E,_D),
    arg(1,E,Tablero),
    posicion_pelota(E,p(X,Y)),
    snoc(Prefijo,p(X,Y),L),
    traducir_coordenadas(interna(F,C),interfaz(X,Y)),
    arg(1,E,Tablero),
    valor_celda_f(F,C,Tablero,vertice(false,Dirs)),
    nuevo_valor_celda_f(F,C,Tablero,vertice(true,Dirs)),
    (turno(E,1) -> setarg(3,E,turno(2));setarg(3,E,turno(1))),
    %duplicate_term(E,EOut).
    EOut = E.

mover(E,L,EOut) :-
    prefijo_movimiento2(E,Prefijo),
    mover_pelota(E,_D),
    arg(1,E,Tablero),
    posicion_pelota(E,p(X,Y)),
    snoc(Prefijo,p(X,Y),L),
    traducir_coordenadas(interna(F,C),interfaz(X,Y)),
    arg(1,E,Tablero),
    celda_borde(F,C),
    valor_celda_f(F,C,Tablero,vertice(true,Dirs)),
    nuevo_valor_celda_f(F,C,Tablero,vertice(true,Dirs)),
    %duplicate_term(E,EOut).
    EOut = E.


% snoc/2(+L,?X,?L2)
snoc([],X,[X]).
snoc([H|T],X,[H|L2]) :- snoc(T,X,L2).



% mover_pelota(+E,Dir)
% --predicado impuro (efecto sobre E)
% realiza un movimiento de la pelota en el estado E, en la direccion Dir,
mover_pelota(E,n) :-
    E =.. [estado,Tablero,pelota(F,C),turno(_J)],
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
print_cell(vertice(_,dirs(n,ne,e,se,s,sw,w,nw))) :-
    write('┼'),!. %unicode 253C
%vertices bandas
print_cell(vertice(_,dirs(f,ne,e,se,f,f,f,f))) :-
    write('┠'),!.
print_cell(vertice(_,dirs(f,f,f,f,f,sw,w,nw))) :-
    write('┨'),!.
%vertices fondo
print_cell(vertice(_,dirs(f,f,f,se,s,sw,f,f))) :-
    write('┯'),!.
print_cell(vertice(_,dirs(n,ne,f,f,f,f,f,nw))) :-
    write('┷'),!.
%vertices esquinas
print_cell(vertice(_,dirs(f,f,f,se,f,f,f,f))) :-
    write('┏'),!.
print_cell(vertice(_,dirs(f,f,f,f,f,sw,f,f))) :-
    write('┓'),!.
print_cell(vertice(_,dirs(f,ne,f,f,f,f,f,f))) :-
    write('┗'),!.
print_cell(vertice(_,dirs(f,f,f,f,f,f,f,nw))) :-
    write('┛'),!.
%vertices palos
print_cell(vertice(_,dirs(n,ne,e,se,f,f,f,nw))) :-
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

% Test para determinar si la posicion inicial de la pelota es correcta.
test(posicion_pelota_inicial) :-
    estado_inicial(E),
    posicion_pelota(E,p(0,0)).

% Test para determinar si el tablero inicializado es correcto (para tamaño 8x10 o 9 columnas y 13 filas).
test(tablero_inicial_correcto) :-
    estado_inicial(X),
    X = estado(matriz(
    row(vertice(true, []), vertice(true, []), vertice(true, []), vertice(false, [se]), vertice(false, [se, s, sw]), vertice(false, [sw]), vertice(true, []), vertice(true, []), vertice(true, [])), 
    row(vertice(true, [se]), vertice(true, [se, s, sw]), vertice(true, [se, s, sw]), vertice(true, [ne, e, se, s, sw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [se, s, sw, w, nw]), vertice(true, [se, s, sw]), vertice(true, [se, s, sw]), vertice(true, [sw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne, e, se]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [sw, w, nw])), 
    row(vertice(true, [ne]), vertice(true, [n, ne, nw]), vertice(true, [n, ne, nw]), vertice(true, [n, ne, e, se, nw]), vertice(false, [n, ne, e, se, s, sw, w, nw]), vertice(true, [n, ne, sw, w, nw]), vertice(true, [n, ne, nw]), vertice(true, [n, ne, nw]), vertice(true, [nw])), 
    row(vertice(true, []), vertice(true, []), vertice(true, []), vertice(false, [ne]), vertice(false, [n, ne, nw]), vertice(false, [nw]), vertice(true, []), vertice(true, []), vertice(true, []))), pelota(7, 5), turno(1)).

% Test para predicado columna_fuera_arco.
test(columna_fuera_arco) :-
    juego:columna_fuera_arco(3,9),
    juego:columna_fuera_arco(2,9),
    juego:columna_fuera_arco(1,9),
    \+(juego:columna_fuera_arco(0,9)),
    \+(juego:columna_fuera_arco(6,9)),
    \+(juego:columna_fuera_arco(5,9)),
    juego:columna_fuera_arco(7,9),
    juego:columna_fuera_arco(8,9),
    juego:columna_fuera_arco(9,9),
    \+(juego:columna_fuera_arco(10,9)),
    \+(juego:columna_fuera_arco(10,9)),
    \+(juego:columna_fuera_arco(10,9)).

:- end_tests(juego).
