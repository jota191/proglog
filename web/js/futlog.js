class Futlog {
  constructor(tiposDeJugador) {
    $("body").css("background-color", COLOR_FONDO);

    this.$canvas = $("canvas")[0];
    this.$canvas.width = (ancho + 1) * MULTIPLIER;
    this.$canvas.height = (alto + 3) * MULTIPLIER;

    this.context = this.$canvas.getContext("2d");

    usarCoordenadasJuego(this.$canvas, this.context);

    this.posPelota = [0, 0];

    this.estado = null;
    let estadoFunction = () => this.estado;

    this.jugadores = [];
    for (let i = 0; i < 2; i++) {
      this.jugadores[i] = tiposDeJugador[i] == "humano" ?
          new Humano(i + 1, estadoFunction, this.$canvas, this.context) :
          new Maquina(i + 1, estadoFunction, this.$canvas, this.context, tiposDeJugador[i]);
    }

    this.$etiquetasJugadores = [];
    for (let i = 0; i < 2; i++) {
      let nJugador = i + 1;
      this.$etiquetasJugadores[i] = $("#etiquetaJugador" + nJugador);
      this.$etiquetasJugadores[i].css('color', colorJugador(nJugador));
    }

    this.$status = $("#status");
    this.$status.html("&nbsp;");
  }

  jugador(nJugador) {
    return this.jugadores[nJugador - 1];
  }

  elOtroJugador(nJugador) {
    return this.jugador(3 - nJugador);
  }

  inicializar() {
    return Rx.Observable.zip(
        inicializarJuegoServicio().do(e => this.estado = e),
        this.jugador(1).inicializar(),
        this.jugador(2).inicializar()
    )
        .do(null, error => {
          if (error.status === 0 && error.xhr.readyState == 4) {
            this.$status.text("¡Ups! Hubo un problema al inicializar el juego o una de las inteligencias. Tal vez no están encendidos los servicios :)");
            this.$status.css("color", "red");
            this.$status.css("font-weight", "bold");
          }
          return Rx.Observable.throw(error);
        }).single();
  }

  dibujar() {
    limpiarDibujo(this.context);
    dibujarGrilla(this.context);
    dibujarBordes(this.context);
    for (let i = 1; i < 3; i++) {
      this.jugador(i).dibujarMovimientos();
    }
    dibujarPelota(this.context, this.posPelota);
  }

  loop() {
    this.dibujar();
    turnoServicio(this.estado)
        .flatMap(nJugador => {
          blink(nJugador);
          return this.jugador(nJugador).hacerJugada(this.posPelota)
              .do(posicion => {
                removerBlink();

                this.posPelota = posicion;
                this.dibujar();
              })
              .toArray()
              .do(_ => {
                blink(0);
                mostrarProgresoEnCursor();
              })
              .flatMap(listaPosiciones =>
                  moverServicio(this.estado, listaPosiciones)
                      .do(e => this.estado = e)
                      .map(e => {
                        if (e === null) {
                          return [nJugador, "invalido", null];
                        } else {
                          return e;
                        }
                      })
                      .flatMap(_ =>
                          this.elOtroJugador(nJugador).movioRival(listaPosiciones)
                              .map(_ => [nJugador, "sigue", null])))
              .catch(error => {
                if ("name" in error && error.name == "TimeoutError") {
                  return Rx.Observable.of([nJugador, "timeout", null]);
                } else {
                  return Rx.Observable.throw(error);
                }
              });
        }).flatMap(trio => {
          if (trio[1] == "sigue") {
            return golServicio(this.estado)
                .flatMap(nJugadorGol => {
                if (nJugadorGol) {
                  return Rx.Observable.of([trio[0], "gol", nJugadorGol]);
                } else {
                  return hayAlgunMovimientoServicio(this.estado)
                      .flatMap(hayMovimiento => {
                        if (hayMovimiento) {
                          return Rx.Observable.of(trio);
                        } else {
                          return Rx.Observable.of([trio[0], "sin movimientos", null]);
                        }
                      });
                }
              });
          } else {
            return Rx.Observable.of(trio);
          }
        })
        .subscribe(trio => {
          removerBlink();
          terminarProgresoEnCursor();

          let nJugador = trio[0];
          let accion = trio[1];

          if (accion == "sigue") {
            this.loop();
          } else {
            terminarProgresoEnCursor();

            let texto = "";
            let ganador = 0;

            if (accion == "gol") {
              texto = nJugador == trio[2] ? "¡Gol!" : "Gol en contra.";
              ganador = trio[2];
            } else if (accion == "sin movimientos") {
              texto = "Sin movimientos.";
              ganador = 3 - nJugador;
            } else if (accion == "timeout") {
              texto = "Timeout.";
              ganador = 3 - nJugador;
            } else if (accion == "invalido") {
              texto = "Movimiento inválido.";
              ganador = 3 - nJugador;
            }

            this.$status.text(`${texto} Gana el Jugador ${ganador}: ${this.$etiquetasJugadores[ganador - 1].text()}`);
            this.$status.css("color", colorJugador(ganador));
          }
        }, error => console.error(error));
  }

  jugar() {
    this.inicializar().subscribe(_ => this.loop());
  }
}
