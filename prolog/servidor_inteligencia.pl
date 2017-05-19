:- module(servidor_inteligencia,[servidor_inteligencia/0,
                                 servidor_inteligencia/1]).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/thread_httpd)).
:- use_module(inteligencia).
:- use_module(juego).

:- set_setting(http:cors,[*]).

servidor_inteligencia:-
    http_server(http_dispatch,[]).

servidor_inteligencia(Puerto):-
    http_server(http_dispatch,[port(Puerto)]).

:- http_handler('/nombre',handler_nombre,[]).
:- http_handler('/hacer_jugada',handler_hacer_jugada,[]).

handler_nombre(_):-
    make,
    cors_enable,
    nombre(Nombre),
    format('Content-type: text/plain~n~n~w',[Nombre]).

handler_hacer_jugada(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_hacer_jugada(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string(E,Body),
    hacer_jugada(E,LP,E2),
    format('Content-type: text/plain~n~n~w~n~w',[LP,E2]).

% Lo siguiente sirve para modificar el estado que maneja la inteligencia,
% de manera independiente a cómo lo hace.

:- http_handler('/estado_inicial',handler_estado_inicial_inteligencia,[]).
:- http_handler('/mover',handler_mover_inteligencia,[]).

handler_estado_inicial_inteligencia(_):-
    make,
    cors_enable,
    estado_inicial(E),
    format('Content-type: text/plain~n~n~w',[E]).

handler_mover_inteligencia(Request):-
    option(method(options),Request),
    !,
    cors_enable(Request,[methods([post])]),
    format('~n').

handler_mover_inteligencia(Request):-
    make,
    cors_enable,
    http_read_data(Request,Body,[to(string)]),
    term_string([E,LP],Body),
    format('Content-type: text/plain~n~n'),
    (mover(E,LP,E2) -> write(E2) ; write(false)).
