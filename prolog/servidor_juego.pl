:- module(servidor_juego,[servidor_juego/0,servidor_juego/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/thread_httpd)).
:- use_module(juego).

:- set_setting(http:cors,[*]).

servidor_juego:-
    http_server(http_dispatch,[]).

servidor_juego(Puerto):-
    http_server(http_dispatch,[port(Puerto)]).

:- http_handler('/cantidad_casilleros',handler_cantidad_casilleros,[]).
:- http_handler('/estado_inicial',handler_estado_inicial,[]).
:- http_handler('/mover',handler_mover,[]).
:- http_handler('/puede_mover',handler_puede_mover,[]).
:- http_handler('/hay_algun_movimiento',handler_hay_algun_movimiento,[]).
:- http_handler('/prefijo_movimiento',handler_prefijo_movimiento,[]).
:- http_handler('/gol',handler_gol,[]).
:- http_handler('/turno',handler_turno,[]).

handler_estado_inicial(_):-
    make,
    cors_enable,
    estado_inicial(E),
    format('Content-type: text/plain~n~n~w',[E]).

handler_cantidad_casilleros(_):-
    make,
    cors_enable,
    cantidad_casilleros(Ancho,Alto),
    format('Content-type: text/plain~n~n~wx~w',[Ancho,Alto]).

handler_mover(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_mover(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string([E,LP],Body),
    format('Content-type: text/plain~n~n'),
    (mover(E,LP,E2) -> write(E2) ; write(false)).

handler_puede_mover(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_puede_mover(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string([E,LP],Body),
    format('Content-type: text/plain~n~n'),
    (mover(E,LP,_) -> write(true) ; write(false)).

handler_hay_algun_movimiento(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_hay_algun_movimiento(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string(E,Body),
    format('Content-type: text/plain~n~n'),
    (mover(E,_,_) -> write(true) ; write(false)).

handler_prefijo_movimiento(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_prefijo_movimiento(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string([E,LP],Body),
    format('Content-type: text/plain~n~n'),
    (prefijo_movimiento(E,LP) -> write(true) ; write(false)).

handler_gol(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_gol(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string(E,Body),
    format('Content-type: text/plain~n~n'),
    (gol(E,NJugador) -> write(NJugador) ; write(false)).

handler_turno(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_turno(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string(E,Body),
    turno(E,Turno),
    format('Content-type: text/plain~n~n~w',[Turno]).
