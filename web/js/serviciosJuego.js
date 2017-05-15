const URL_BASE = "http://localhost:4567/";

function cantidadCasillerosServicio() {
  return Rx.Observable.ajax({
    url: URL_BASE + "cantidad_casilleros",
    crossDomain: true,
    responseType: "text",
  })
      .map(call => call.response)
      .map(texto => texto.split("x"))
      .flatMap(tamano => Rx.Observable.from(tamano))
      .map(dimensionStr => parseInt(dimensionStr))
      .toArray()
      .single();
}

function inicializarJuegoServicio() {
  return Rx.Observable.ajax({
    url: URL_BASE + "estado_inicial",
    crossDomain: true,
    responseType: "text",
  }).map(call => call.response).single();
}

function moverServicio(estado, listaPosiciones) {
  return Rx.Observable.ajax({
    url: URL_BASE + "mover",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: "[" + estado + "," + listaPosicionesAStringProlog(listaPosiciones) + "]",
  }).map(call => {
    if (call.response == "false") {
      return null;
    } else {
      return call.response;
    }
  }).single();
}

function puedeMoverServicio(estado, listaPosiciones) {
  return Rx.Observable.ajax({
    url: URL_BASE + "puede_mover",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: "[" + estado + "," + listaPosicionesAStringProlog(listaPosiciones) + "]",
  }).map(call => call.response == "true").single();
}

function hayAlgunMovimientoServicio(estado) {
  return Rx.Observable.ajax({
    url: URL_BASE + "hay_algun_movimiento",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: estado,
  }).map(call => call.response == "true").single();
}

function prefijoMovimientoServicio(estado, listaPosiciones) {
  return Rx.Observable.ajax({
    url: URL_BASE + "prefijo_movimiento",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: "[" + estado + "," + listaPosicionesAStringProlog(listaPosiciones) + "]",
  }).map(call => call.response == "true").single();
}

function golServicio(estado) {
  return Rx.Observable.ajax({
    url: URL_BASE + "gol",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: estado,
  }).map(call => {
    if (call.response == "false") {
      return false;
    } else {
      return parseInt(call.response);
    }
  }).single();
}

function turnoServicio(estado) {
  return Rx.Observable.ajax({
    url: URL_BASE + "turno",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: estado,
  }).map(call => parseInt(call.response)).single();
}
