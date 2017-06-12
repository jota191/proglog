%
% Author: Juan Pablo Garcia Garland - 4553034
% Programación Logica 2017 - Laboratorio 1
%
% Si bien swi-plolog soporta codigo fuente unicode, para evitar posibles
% problemas se omiten los tildes en la documentacion.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Encabezado de modulo:

:- module(lab1,
[
    largo/2,
    todos_iguales/2,
    concatenacion/3,
    merge_ordenado/3,
    sin_elem/3,
    rotacion/2,
    subsecuencia/2,
    jugada_valida/3,
    matriz/4,
    valor_celda/4,
    nuevo_valor_celda/5,
    adyacente/6,
    matriz_f/4,
    valor_celda_f/4,
    nuevo_valor_celda_f/4,
    adyacente_f/6,
    generar_f/4
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1) Predicados sobre Listas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% largo_/2(+L, ?N)
%
% N es el largo de la lista L
% Se implementa por recursion sobre la lista L

largo_([],0).
largo_([_X|XS],L) :-
    largo(XS,L2),
    L is L2 +1.

%% largo/2(+L, ?N)
%
% N es el largo de la lista L
% implementacion tail recursive, usando acumulador, mejora eficiencia
% (el stack no crece linealmente)

largo(L,N) :-
    largo(L,0,N).

%% largo/3(+L,+P,?N)
%
% N es el largo de la lista L sumado al acumulador P

largo([],N,N).
largo([_X|Xs], PredL, N) :-
    L is PredL + 1,
    largo(Xs,L,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% todos_iguales/2 (?L, ?E)
%
% Todos los elementos de la lista L son iguales a E
% Se implementa por recursion sobre la lista L

todos_iguales([],_).
todos_iguales([X|Xs],X) :-
    todos_iguales(Xs,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% concatenacion/3 (?L1, ?L2, ?L)
%
% La lista L es la concatenacion de L1 con L2
% Se implementa por recursion sobre L1

concatenacion([],Ys,Ys).
concatenacion([X|Xs],Ys,[X|Zs]) :-
    concatenacion(Xs,Ys,Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%reverse/2(?L,?R).
%
% R y L son listas reversas
%función reverse, es un wrapper a reverseAcc
reverse(L,R) :- reverseAcc(L,[],R).


%%reverseAcc/3(+L,+LAcc,?LRev)
% implementacion usual de reverse con acumulador

reverseAcc([],Acc,Acc).
reverseAcc([H|T],Acc,R) :-
    reverseAcc(T,[H|Acc],R).


%mergeAcc/4(+L1,+L2,+Acc,?Merged)
% merge con acumulador
% Se hace recursión sobre las dos listas a la vez, consumiendo de la que
% tenga un valor menor en la cabeza, y pusheandolo en el acumulador.
% Entonces guarda en el acumulador el merge de las listas invertido

mergeAcc([],[],Acc,Acc).
mergeAcc([X|Xs],[],Acc,Zs) :-
    mergeAcc(Xs,[],[X|Acc],Zs).
mergeAcc([],[Y|Ys],Acc,Zs) :-
    mergeAcc([],Ys,[Y|Acc],Zs).
mergeAcc([X|Xs],[Y|Ys],Acc,Zs) :-
    X < Y, % es seguro aca porque las listas estan instanciadas,
           % notese que al no poner <= aca, siempre se encuentra una unica
           % prueba
    mergeAcc(Xs,[Y|Ys],[X|Acc],Zs).

mergeAcc([X|Xs],[Y|Ys],Acc,Zs) :-
    X >= Y,
    mergeAcc([X|Xs],Ys,[Y|Acc],Zs).



%%merge_ordenado/3 (+Xs, +Ys, ?Zs)
%
%   Zs es el resultado de mergear Xs e Ys, se asume que las listas vienen
% ordenadas. Merge es entonces un wrapper.

merge_ordenado(Xs,Ys,Zs) :-
    mergeAcc(Xs,Ys,[],RZs),
    reverse(RZs,Zs). %importa el orden de estos parametros


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% sin_elem/3 (+L, ?E, ?LSinE)
%
% La lista LSinE es la lista L sin una ocurrencia del elemento E
% Notese que el resultado es no determinista, en particular cuando E ocurre
% mas de una vez en la lista L hay multiples valores posibles de LSinE
% y reciprocamente sin_elem([1,1],X,[1]) tiene dos pruebas posibles

sin_elem([X|Xs],X,Xs).
sin_elem([X|Xs],E,[X|Ys]) :-
    sin_elem(Xs,E,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% rotacion/2 (+L, ?R)
%
%   La lista R es una rotacion de la lista L.
% Se hace pattern matching en la primer lista para evitar duplicar el resultado
% de la rotacion identidad. (si solo pedimos
% concatenacion(L1,L2,L) y concatenacion(L2,L1,R) se duplica la pruebacon
% el caso L = R)

rotacion([],[]).
rotacion(L,R) :-
    concatenacion([X|L1],L2,L),
    concatenacion(L2,[X|L1],R).

% nota: Habria que ver si se puede ser mas eficiente aca, lo que es seguro es
% que no podemos ser mas elegantes <3

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% subsecuencia/2 (?L, ?Subsec)
%
% La lista Subsec contiene un subconjunto de los elementos de la lista L,
% conservando el orden. i.e. Subsec es la Lista L quitandole algunos elementos

subsecuencia(_Xs,[]).
subsecuencia([X|Xs],[X|Ys]) :-
    subsecuencia(Xs,Ys).
subsecuencia([_X|Xs],[Y|Ys]) :-
    subsecuencia(Xs,[Y|Ys]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Parte2 - Juego de cartas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%jugada_valida/3(+Mano,+Tapete,?Jugada)
%
% Jugada es una jugada valida para un jugador con mano Mano y Tapete cartas
% en el tapete. Se implementa usando la tecnica de generacion-chequeo

jugada_valida(Mano,Tapete,jugada(DeMano,DeTapete)) :-
    generacion(Mano,Tapete,jugada(DeMano,DeTapete)),
    chequeo(Mano,Tapete,jugada(DeMano,DeTapete)).


%%generacion/3(+Mano, +Tapete, -Jugada)
%
% dada una mano Mano y un tapete Tapete, genera las posibles jugadas
% (elegir una carta de la mano y un conjunto de cartas del tapete)
% Las cartas del tapete se generan en orden (ver predicado inclusion) para
% evitar soluciones duplicadas (y reducir el espacio de busqueda)

generacion(Mano,Tapete,jugada(DeMano,DeTapete)):-
    miembro(Mano,DeMano),
    subsecuencia(Tapete,DeTapete).


%%chequeo/3(+Mano,+Tapete,+Jugada)
%
% Siendo Jugada=jugada(DeMano,DeTapete), DeMano es una carta de Mano,
% y DeTapete una subsecuencia de Tapete.

chequeo(Mano,Tapete,jugada(DeMano,DeTapete)) :-
    miembro(Mano,DeMano),
    subsecuencia(Tapete,DeTapete),
    sumaCartas(DeMano,DeTapete,15).

%% miembro/2(+L, ?C)
%
% C es miembro de la lista L
% Vamos a utilizarlo para cartas C en listas de cartas L,
% Se podria ser mas especifico (esta version funciona para listas con
% contenido cualquiera)

miembro([C|_],C).
miembro([_|Cs],C):-
    miembro(Cs,C).


%% sumaListaCartas/2(+ListaCartas, ?N)
%
% La suma de los valores de la lista ListaCartas es N


sumaListaCartas(L,N) :-
    sumaListaCartas(L,0,N).


%% sumaListaCartas/3(+ListaCartas,+Acc, ?N)
% La suma de los valores de la lista ListaCartas es N-Acc

sumaListaCartas([],Acc,Acc).
sumaListaCartas([carta(C,_Palo)|Cartas],Acc,N) :-
    NewAcc is Acc + C,
    sumaListaCartas(Cartas,NewAcc,N).


%%sumaCartas/3(+CartaMano,+Tapete,?Sum)
%
% Sum es la suma de los valores de las cartas de la lista Tapete, mas el valor
% de la carta CartaMano

sumaCartas(carta(M,_Palo),Tapete,Sum) :-
    sumaListaCartas(Tapete,S),
    Sum is M+S.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 3 Predicados sobre Matrices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parte A

%% matriz/4(+M, +N, +E, ?A)
%
%   A es una matriz de M filas por N columnas implementada como listas de
% listas, conteniendo el valor E en todas las entradas.

matriz(M,N,E,A):-
    vector(N,E,F),
    vector(M,F,A).


%% vector/3(+N, +E, ?V) (Auxiliar)
%
%  V es un vector (implementado simplemente como una lista), de largo N
% que contiene el valor E en todas las entradas. Se construye inductivamente.

vector(0,_E,[]). %el vector de dimension 0 es la lista vacia.
vector(N,E,[E|Es]) :-
    N > 0, %para evitar diverger al buscar segunda prueba TODO:preguntar
    PredN is N-1,
    vector(PredN,E,Es).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parte B


%% valor_celda/4(+I, +J, +A, ?E)
%   E es el valor de la celda de coordenadas (I,J) en la matriz A

valor_celda(I,J,A,E) :-
    valor_celda_vec(I,A,FilaI),
    valor_celda_vec(J,FilaI,E).



%% valor_celda_vec/3(+I, +V, ?E) (Auxiliar)
%
%   E es el valor en la posicion I del vector V.
% Donde 1<I<N, siendo N es el tamanio del vector V

valor_celda_vec(1,[E|_],E).
valor_celda_vec(N,[_|Es],E) :-
    PredN is N - 1,
    valor_celda_vec(PredN,Es,E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parte C

%% nuevo_valor_celda/5(+I, +J, +A1, +E, -A2)
%
%   A2 es una matriz con el valor E en la entrada (I,J), y el resto de las
% entradascon iguales a A1

nuevo_valor_celda(I,J,A1,E,A2) :-
    valor_celda_vec(I,A1,FilaI),
    nuevo_valor_vec(J,FilaI,E,FilaINueva),
    nuevo_valor_vec(I,A1,FilaINueva,A2).



%% nuevo_valor_vec/4(+N, +V1, +E, -V2) (Auxiliar)
%   V2 es el vector V1 con la posicion N cambiada por el valor E
% El objetivo del ejercicio es comparar esta implementacion pura
% con una mas eficiente usando predicados extra logicos, vamos a intentar ser
% eficientes en esta implementacion pura.
%   Necesariamente debemos recorrer la lista hasta la posicion a actualizar
% ya que lo unico que podemos hacer es pattern matching. Luego hay que cambiar
% la cabeza y agregar de nuevo las cabezas anteriores.
% Se implementa como un wrapper utilizando un predicado con doble acumulador

nuevo_valor_vec(N,V1,E,V2) :-
    nuevo_valor_vec(f,[],N,V1,E,V2).


%% nuevo_valor_vec/6(+Cambie,+Acc,+N, +V1, +E, -V2)
%
% Se van consumiendo posiciones de V1 y colocandolas en Acc (que es una lista)
% hasta llegar a la
% posicion N, en donde se cambia la cabeza de la lista que representa el vector
% por E. Luego se restauran las posiciones que estan guardadas en Acc.
% el primer parametro funciona como un booleano que indica si consumir del
% vector V1 (cuando todavia no cambie, vale f), o restaurar del acumulador
% (cuando ya se actualizo la posicion N, donde vale t)
%

nuevo_valor_vec(f,Acc,1,[_|V1],E,V2) :-
    nuevo_valor_vec(t,Acc,1,[E|V1],E,V2).
nuevo_valor_vec(f,Acc,N,[V|V1],E,V2) :-
    PredN is N-1,
    nuevo_valor_vec(f,[V|Acc],PredN,V1,E,V2).
nuevo_valor_vec(t,[V|Acc],_,V1,E,V2) :-
    nuevo_valor_vec(t,Acc,_,[V|V1],E,V2).
nuevo_valor_vec(t,[],_,V1,_,V1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% adyacente/6 (+I,+J,+A,?I2,?J2,?V)
%
% (I2,J2) es una celda adyacente a (I,J) ( a un paso de Rey en el ajedrez)
% con valor V, en la matriz A.
% Observese que el predicado auxiliar adyCoord ignora el tamanio de la matriz A
% Al generar las coordenadas si quedan fuera de la matriz lo que va a fallar es
% valor_celda. Podrian controlarse antes estos bordes considerando las
% dimensiones de la matriz, y se ganaria eficiencia al ahorrarse inferencias en
% las llamadas a valor_celda cuando el predicado va a fallar, sin embargo se
% perderia eficiencia al obtener las dimensiones en los casos que no van a
% ser necesarias (por ejemplo siempre que se llame al predicado con I y J
% instanciados y validos). Habria que ver cual es el uso mas usual
% del predicado (o incluso hacer implementaciones distintas para cada caso).
% Por ahora elijo esta por ser la mas simple (y creo la mas rapida para el uso
% mas usual)

adyacente(I,J,A,I1,J1,V) :-
    adyCoord(I,J,I1,J1),
    valor_celda(I1,J1,A,V).

%% adyCoord/4 (+I, J+, I2?, J2?)
%
% (I,J) e (I2,J2) son coordenadas enteras que cumplen:
%   |I - I2| <= 1,
%   |J - J2| <= 1,
% y (I,J) != (I2,J2)
%
% Se implementa caso a caso para que funcione como generador,
% sin usar between.

adyCoord(I, J, I1, J):-
    I1 is I+1.
adyCoord(I, J, I1, J) :-
    I1 is I-1.
adyCoord(I, J, I, J1) :-
    J1 is J-1.
adyCoord(I, J, I, J1) :-
    J1 is J+1.


adyCoord(I, J, I1, J1) :-
    J1 is J-1,
    I1 is I-1.
adyCoord(I, J, I1, J1) :-
    J1 is J-1,
    I1 is I+1.
adyCoord(I, J, I1, J1) :-
    J1 is J+1,
    I1 is I-1.
adyCoord(I, J, I1, J1) :-
    J1 is J+1,
    I1 is I+1.

% nota: puede implementarse between en prolog puro:
% stackoverflow.com/questions/18337235/can-you-write-between-3-in-pure-prolog
% Mi implementacion si bien es menos elegante por todo el boilerplate,
% es clara y rapida, para este uso que le damos, implementar un generador mas
% complejo es medio un overkill.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% 3 Predicados eficientes sobre Matrices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parte A


%% matriz_f/2 +M +N +E ?A
%   A es una matriz de M filas por N columnas con todas las entradas
% conteniendo el valor E

matriz_f(M,N,E,A) :-
    matriz_f(0,matriz,M,N,E,A).

matriz_f(M,Matrix,M,_N,_E,Matrix).
matriz_f(K,Matrix,M,N,E,A) :-
    K < M,
    row_f(N,E,Row),
    Matrix =.. [F|List],
    MatrixNew =.. [F,Row|List],
    KNew is K+1,
    matriz_f(KNew,MatrixNew,M,N,E,A).



matriz_f_(M,N,E,A) :-
    row_f(N,E,Row),
    vector(M,Row,CList),%creamos el vector como una lista
    A =.. [matrix|CList].
% esta implementacion, si bien es la intuitiva, falla. Todas las filas,
% son la misma en memoria, al usar predicados impuros, con efectos,
% se modifican valores indeseados.


%% row_f/3(+M, +E, ?V) (Auxiliar)
%
%   V es un vector de tamanio M conteniendo el valor E en todas sus entradas
% representado de forma tal que el acceso y modificacion pueden implementarse
% de forma eficiente utilizando predicados extra logicos. Se utiliza un
% functor row/n

row_f(N,E,V) :-
    vector(N,E,VList),%creamos el vector como una lista
    V =.. [row|VList].

%% valor_celda_f/4(+I, +J, +A, ?E)
%
%   E es el contenido de la entrada (I,J) de la matriz A

valor_celda_f(I,J,A,E) :-
    arg(I,A,Row),
    arg(J,Row,E).


%% nuevo_valor_celda_f/4 +I +J +A ?E
%
%   Predicado extra logico. Modifica la coordenada (M,N) de la matriz A
% asignando como nuevo valor a E

nuevo_valor_celda_f(I,J,A,E) :-
    arg(I,A,Row),
    setarg(J,Row,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%adyacente_f/6(+I, +J, +A, ?I1, ?J1, ?V)
%
%   (I2,J2) es una celda adyacente a (I,J) ( a un paso de Rey en el ajedrez)
% con valor V, en la matriz A (representada con functores)
% Ver predicado adyacente/6 para mas info.

adyacente_f(I,J,A,I1,J1,V) :-
    adyCoord(I,J,I1,J1),
    valor_celda_f(I1,J1,A,V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parte 5 - Pruebas de eficiencia
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%generar/6(+M, +N, +K, +L, A, B)
%
%   Se utiliza como predicado auxiliar para generar/3. Se tienen 3 acumuladores
% que son K, L y A. Los enteros M y N son las dimensiones de la matriz a crear,
% que es B.
% Se "simula" un doble bucle for, en donde K y L son las variables de control,
% y M y N los bordes. Se va acumulando en A el resultado (cambiando la
% posicion (K,L) para todas las combinaciones posibles, i.e todas las
% entradas de la matriz).

generar(M,N,K,L,A,B) :-
    L < N,
    LNew is L+1,
    V is K+L,
    nuevo_valor_celda(K,L,A,V,ANew),
    generar(M,N,K,LNew,ANew,B).
generar(M,N,K,N,A,B) :-
    K < M,
    KNew is K+1,
    V is K+N,
    nuevo_valor_celda(K,N,A,V,ANew),
    generar(M,N,KNew,1,ANew,B).
generar(M,N,M,N,A,B) :-
    V is M+N,
    nuevo_valor_celda(M,N,A,V,B).


%%generar/3(+M ,+N,-A)
%
%   A es una matriz de M filas y N columnas en donde en la entrada (I,J)
% contiene el valor I+J.
%   Se parte de una matriz de ceros que se actualiza con el predicado generar/6
% Notese que se podria ser mas eficiente y crear la matriz con los valores
% directamente sin hacer dos recorridas. Se opto por utilizar los predicados
% de la parte 4 como si fuera una interfaz, y no acceder a la implementacion.
% (lo considero conceptualmente mas correcto)

generar(M,N,A) :-
    matriz(M,N,0,Init),
    generar(M,N,1,1,Init,A).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%generar_f/7(+M, +N, +K, +L, A, B)
%
%   Analogo generar/6 con la implementacion eficiente

generar_f(Valor,M,N,K,L,A,B) :-
    L < N,
    LNew is L+1,
    duplicate_term(Valor,V),
    nuevo_valor_celda_f(K,L,A,V),
    generar_f(Valor,M,N,K,LNew,A,B).
generar_f(Valor,M,N,K,N,A,B) :-
    K < M,
    KNew is K+1,
    duplicate_term(Valor,V),
    nuevo_valor_celda_f(K,N,A,V),
    generar_f(Valor,M,N,KNew,1,A,B).
generar_f(Valor,M,N,M,N,A,A) :-
    duplicate_term(Valor,V),
    nuevo_valor_celda_f(M,N,A,V).


%%generar_f/4(Value,+M ,+N,-A)
%
%   Analogo a generar/3, con la implementacion eficiente

generar_f(Valor,M,N,A) :-
    matriz_f(M,N,0,Init),
    generar_f(Valor,M,N,1,1,Init,A).



