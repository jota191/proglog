function nombreServicio(urlBase) {
  return Rx.Observable.ajax({
    url: urlBase + "nombre",
    crossDomain: true,
    responseType: "text",
  }).map(call => call.response).single();
}

function hacerJugadaServicio(urlBase, estado) {
  return Rx.Observable.ajax({
    url: urlBase + "hacer_jugada",
    crossDomain: true,
    responseType: "text",
    method: "POST",
    headers: {
      "Content-Type": "application/x-prolog",
    },
    body: estado,
  }).map(call => {
    let output = call.response.split("\n");
    let listaPosiciones = stringPrologAListaPosiciones(output[0]);
    let estado = output[1];
    return [listaPosiciones, estado];
  }).single();
}

function inicializarJuegoInteligenciaServicio(urlBase) {
  return Rx.Observable.ajax({
    url: urlBase + "estado_inicial",
    crossDomain: true,
    responseType: "text",
  }).map(call => call.response).single();
}

function moverInteligenciaServicio(urlBase, estado, listaPosiciones) {
  return Rx.Observable.ajax({
    url: urlBase + "mover",
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
