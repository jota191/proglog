:: Tienen que correr en distintos programas Prolog para no interferirse.

SET LOCATION="%ProgramFiles%\swipl"

IF NOT EXIST %LOCATION% (
  SET LOCATION="%ProgramFiles(x86)%\swipl"
)

IF "%1%"=="start" (
    start "servidor_juego" %LOCATION%\bin\swipl -f prolog\servidor_juego.pl -g "servidor_juego(4567)"
    start "servidor_inteligencia1" %LOCATION%\bin\swipl -f prolog\servidor_inteligencia.pl -g "servidor_inteligencia(4568)"
    start "servidor_inteligencia2" %LOCATION%\bin\swipl -f prolog\servidor_inteligencia.pl -g "servidor_inteligencia(4569)"
)
