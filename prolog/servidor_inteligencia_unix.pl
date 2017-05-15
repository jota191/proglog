:- module(servidor_inteligencia_unix,[servidor_inteligencia/0]).

:- use_module(library(http/http_unix_daemon)).
:- use_module(servidor_inteligencia).

:- initialization http_daemon.
