function posicionAStringProlog(posicion) {
  return "p(" + posicion[0] + "," + posicion[1] + ")";
}

function listaPosicionesAStringProlog(listaPosiciones) {
  let res = "[";
  for (let posicion of listaPosiciones) {
    res += posicionAStringProlog(posicion) + ",";
  }

  if (res.endsWith(",")) {
    res = res.substring(0, res.length - 1);
  }

  return res + "]";
}

function stringPrologAListaPosiciones(listaPosicionesString) {
  listaPosicionesString = listaPosicionesString.substring(1, listaPosicionesString.length - 1);
  let tokens = listaPosicionesString.split("p(");

  let posiciones = [];

  for (let token of tokens) {
    if (token) {
      if (token.endsWith("),")) {
        token = token.substring(0, token.length - 2);
      } else if (token.endsWith(")")) {
        token = token.substring(0, token.length - 1);
      }
      let posicionesStr = token.split(",");
      let posicion = [parseInt(posicionesStr[0]), parseInt(posicionesStr[1])];
      posiciones.push(posicion);
    }
  }

  return posiciones;
}
