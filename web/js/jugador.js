class Jugador {
  constructor(nJugador, estadoDelJuegoFunction, canvas, context) {
    if (new.target === Jugador) {
      throw new TypeError("Jugador es una clase abstracta");
    }
    this.nJugador = nJugador;
    this.estadoDelJuegoFunction = estadoDelJuegoFunction;
    this.canvas = canvas;
    this.context = context;
    this.movimientos = [];
  }

  get estadoDelJuego() {
    return this.estadoDelJuegoFunction();
  }

  get numeroJugador() {
    return this.nJugador;
  }

  inicializar() {
    return Rx.Observable.of(null);
  }

  hacerJugada(posPelota) {
    return Rx.Observable.of(null);
  }

  movioRival(listaPosiciones) {
    return Rx.Observable.of(null);
  }

  dibujarMovimientos() {
    this.movimientos.forEach(movimiento => dibujarMovimiento(this.context, movimiento[0], movimiento.slice(1), this.nJugador));
  }
}

class Humano extends Jugador {
  constructor(nJugador, estadoDelJuegoFunction, canvas, context) {
    super(nJugador, estadoDelJuegoFunction, canvas, context);
  }

  hacerJugada(posPelota) {
    this.movimientos.push([posPelota]);
    return Rx.Observable.of([false, [], null])
        .expand(trio => {
          return Rx.Observable.fromEvent(this.canvas, "click")
              .first()
              .map(evento => coordenadaEnJuego(this.canvas, [evento.clientX, evento.clientY]))
              .flatMap(posicion => {
                let listaPosiciones = trio[1].concat([posicion]);
                return prefijoMovimientoServicio(this.estadoDelJuego, listaPosiciones)
                    .flatMap(esPrefijo => {
                      if (esPrefijo) {
                        return Rx.Observable.of([false, listaPosiciones, posicion]);
                      } else {
                        return puedeMoverServicio(this.estadoDelJuego, listaPosiciones)
                            .map(puedeMover => {
                              if (puedeMover) {
                                return [true, listaPosiciones, posicion];
                              } else {
                                return [false, listaPosiciones.slice(0, listaPosiciones.length - 1), null];
                              }
                            });
                      }
                    });
              });
        })
        .filter(trio => trio[2])
        .flatMap(trio => {
          if (trio[0]) {
            return Rx.Observable.of([false, [], trio[2]], [true, [], null]);
          } else {
            return Rx.Observable.of(trio);
          }
        })
        .takeWhile(trio => !trio[0])
        .map(trio => trio[2])
        .do(posicion => this.movimientos[this.movimientos.length - 1].push(posicion));
  }
}

class Maquina extends Jugador {
  constructor(nJugador, estadoDelJuegoFunction, canvas, context, urlBase) {
    super(nJugador, estadoDelJuegoFunction, canvas, context);
    this.urlBase = urlBase;
    this.estado = null;
  }

  inicializar() {
    return inicializarJuegoInteligenciaServicio(this.urlBase)
        .do(e => this.estado = e)
        .single();
  }

  hacerJugada(posPelota) {
    this.movimientos.push([posPelota]);
    comenzarTimeout(this.numeroJugador);
    mostrarProgresoEnCursor();
    return hacerJugadaServicio(this.urlBase, this.estado)
        .timeout(TIMEOUT)
        .do(pair => {
          apagarTimeout();
          terminarProgresoEnCursor();

          this.estado = pair[1];
        })
        .map(pair => pair[0])
        .flatMap(listaPosiciones => Rx.Observable.from(listaPosiciones))
        .scan((par, posicion) => [posicion, par[1] + 1], [null, -1])
        .flatMap(par => Rx.Observable.of(par[0]).delay(500 * par[1]))
        .do(posicion => this.movimientos[this.movimientos.length - 1].push(posicion));
  }

  movioRival(listaPosiciones) {
    return moverInteligenciaServicio(this.urlBase, this.estado, listaPosiciones)
        .do(e => this.estado = e)
        .single();
  }
}
