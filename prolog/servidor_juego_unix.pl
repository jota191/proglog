:- module(servidor_juego_unix,[servidor_juego/0]).

:- use_module(library(http/http_unix_daemon)).
:- use_module(servidor_juego).

:- initialization http_daemon.
