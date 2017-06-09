const MULTIPLIER = 40;

let ancho;
let alto;
let min_x;
let max_x;
let min_y;
let max_y;

const PELOTA = new Image();
PELOTA.src = "img/ball.png";

const TAM_PELOTA = MULTIPLIER / 2;

const COLOR_FONDO = "#fafafa";
const COLOR_BORDE = "#212121";
const COLOR_GRILLA = "#bdbdbd";
const COLOR_JUGADOR_1 = "#2196F3";
const COLOR_JUGADOR_2 = "#f44336";

const TIMEOUT = 400000;

function inicializarTamano(nuevoAncho, nuevoAlto) {
  ancho = nuevoAncho;
  alto = nuevoAlto;
  min_x = - ancho / 2;
  max_x = ancho / 2;
  min_y = - alto / 2;
  max_y = alto / 2;
}

function usarCoordenadasJuego(canvas, context) {
  let transX = canvas.width * 0.5;
  let transY = canvas.height * 0.5;
  context.translate(transX, transY);

  context.scale(1, -1);
}

function adentroDelTablero(x, y) {
  return Math.abs(y) < (max_y + 1) * MULTIPLIER || Math.abs(x) <= MULTIPLIER;
}

function colorJugador(nJugador) {
  switch (nJugador) {
    case 1:
      return COLOR_JUGADOR_1;
    case 2:
      return COLOR_JUGADOR_2;
    default:
      return COLOR_BORDE;
  }
}

function limpiarDibujo(context) {
  context.clearRect((min_x - 1) * MULTIPLIER, (min_y - 2) * MULTIPLIER,
      (ancho + 2) * MULTIPLIER, (alto + 4) * MULTIPLIER);
  // Limpio un poco más arriba y abajo así la pelota no queda dibujada pasando los bordes.
}

function dibujarGrilla(context) {
  context.lineWidth = 0.5;
  context.strokeStyle = COLOR_GRILLA;

  for (let x = min_x * MULTIPLIER; x <= max_x * MULTIPLIER; x += MULTIPLIER) {
    context.beginPath();

    let yInicial = (min_y - 1) * MULTIPLIER;
    if (!adentroDelTablero(x, yInicial)) {
      yInicial += MULTIPLIER;
    }
    context.moveTo(x, yInicial);

    let yFinal = (max_y + 1) * MULTIPLIER;
    if (!adentroDelTablero(x, yFinal)) {
      yFinal -= MULTIPLIER;
    }

    for (let y = yInicial + MULTIPLIER; y <= yFinal; y += MULTIPLIER) {
      context.lineTo(x, y);
    }
    context.stroke();
  }

  for (let y = min_y * MULTIPLIER; y <= max_y * MULTIPLIER; y += MULTIPLIER) {
    context.beginPath();
    context.moveTo(min_x * MULTIPLIER, y);
    for (let x = (min_x + 1) * MULTIPLIER; x <= max_x * MULTIPLIER; x += MULTIPLIER) {
      context.lineTo(x, y);
    }
    context.stroke();
  }
}

function dibujarBordeVertical(context, x) {
  context.beginPath();
  context.moveTo(x, min_y * MULTIPLIER);
  for (let y = (min_y + 1) * MULTIPLIER; y <= max_y * MULTIPLIER; y += MULTIPLIER) {
    context.lineTo(x, y);
  }
  context.stroke();
}

function dibujarBordeIzquierdo(context) {
  dibujarBordeVertical(context, min_x * MULTIPLIER);
}

function dibujarBordeDerecho(context) {
  dibujarBordeVertical(context, max_x * MULTIPLIER);
}

function dibujarBordeHorizontal(context, y) {
    context.beginPath();
    context.moveTo(min_x * MULTIPLIER, y);
    for (let x = (min_x + 1) * MULTIPLIER; x <= - MULTIPLIER; x += MULTIPLIER) {
      context.lineTo(x, y);
    }
    context.stroke();

    context.beginPath();
    context.moveTo(MULTIPLIER, y);
    for (let x = 2 * MULTIPLIER; x <= max_x * MULTIPLIER; x += MULTIPLIER) {
      context.lineTo(x, y);
    }
    context.stroke();
}

function dibujarBordeSuperior(context) {
    dibujarBordeHorizontal(context, max_y * MULTIPLIER);
}

function dibujarBordeInferior(context) {
    dibujarBordeHorizontal(context, min_y * MULTIPLIER);
}

function dibujarArcos(context) {
  context.beginPath();
  context.moveTo(- MULTIPLIER, max_y * MULTIPLIER);
  context.lineTo(- MULTIPLIER, (max_y + 1) * MULTIPLIER);
  context.stroke();
  context.beginPath();
  context.moveTo(- MULTIPLIER, (max_y + 1) * MULTIPLIER);
  context.strokeStyle = colorJugador(2);
  context.lineTo(0, (max_y + 1) * MULTIPLIER);
  context.lineTo(MULTIPLIER, (max_y + 1) * MULTIPLIER);
  context.stroke();
  context.beginPath();
  context.moveTo(MULTIPLIER, (max_y + 1) * MULTIPLIER);
  context.strokeStyle = COLOR_BORDE;
  context.lineTo(MULTIPLIER, max_y * MULTIPLIER);
  context.stroke();

  context.beginPath();
  context.moveTo(- MULTIPLIER, min_y * MULTIPLIER);
  context.lineTo(- MULTIPLIER, (min_y - 1) * MULTIPLIER);
  context.stroke();
  context.beginPath();
  context.moveTo(- MULTIPLIER, (min_y - 1) * MULTIPLIER);
  context.strokeStyle = colorJugador(1);
  context.lineTo(0, (min_y - 1) * MULTIPLIER);
  context.lineTo(MULTIPLIER, (min_y - 1) * MULTIPLIER);
  context.stroke();
  context.beginPath();
  context.moveTo(MULTIPLIER, (min_y - 1) * MULTIPLIER);
  context.strokeStyle = COLOR_BORDE;
  context.lineTo(MULTIPLIER, min_y * MULTIPLIER);
  context.stroke();
}

function dibujarBordes(context) {
  context.lineWidth = 1;
  context.strokeStyle = COLOR_BORDE;
  dibujarBordeIzquierdo(context);
  dibujarBordeDerecho(context);
  dibujarBordeSuperior(context);
  dibujarBordeInferior(context);
  dibujarArcos(context);
}

function dibujarMovimiento(context, origen, listaPosiciones, nJugador) {
  context.beginPath();
  context.moveTo(origen[0] * MULTIPLIER, origen[1] * MULTIPLIER);
  context.strokeStyle = colorJugador(nJugador);
  for (let posicion of listaPosiciones) {
    context.lineTo(posicion[0] * MULTIPLIER, posicion[1] * MULTIPLIER);
    context.stroke();
  }
}

function isNegativeZero(x) {
  return x === 0 && (1/x < 0);
}

function roundWithoutNegativeZero(x) {
  let round = Math.round(x);
  return isNegativeZero(round) ? 0 : round;
}

function coordenadaEnJuego(canvas, posicion) {
  let rect = canvas.getBoundingClientRect();

  let transX = canvas.width * 0.5;
  let transY = canvas.height * 0.5;

  return [roundWithoutNegativeZero((posicion[0] - rect.left - transX) / MULTIPLIER),
      roundWithoutNegativeZero(- (posicion[1] - rect.top - transY) / MULTIPLIER)];
}

function dibujarPelota(context, posicion) {
  context.beginPath();
  context.arc(posicion[0] * MULTIPLIER, posicion[1] * MULTIPLIER, TAM_PELOTA / 2, 0, 2 * Math.PI);
  context.fillStyle = COLOR_FONDO;
  context.fill();

  context.drawImage(PELOTA, posicion[0] * MULTIPLIER - TAM_PELOTA / 2,
      posicion[1] * MULTIPLIER - TAM_PELOTA / 2, TAM_PELOTA, TAM_PELOTA);
}

function blink(nJugador) {
  let $element;
  switch (nJugador) {
    case 1:
      $element = $("#etiquetaJugador1");
      break;
    case 2:
      $element = $("#etiquetaJugador2");
      break;
    default:
      $element = $("h1");
  }
  $element.css("animation", "1s blinker linear infinite");
}

function removerBlink() {
  $("#etiquetaJugador1").css("animation", "");
  $("#etiquetaJugador2").css("animation", "");
  $("h1").css("animation", "");
}

function mostrarProgresoEnCursor() {
  $("body").css("cursor", "progress");
}

function terminarProgresoEnCursor() {
  $("body").css("cursor", "default");
}

function comenzarTimeout(nJugador) {
  $("#timeout .progress-bar")
      .css("background-color", colorJugador(nJugador))
      .animate({ width: "100%" }, TIMEOUT, "linear");
}

function apagarTimeout() {
  $("#timeout .progress-bar")
      .stop(true)
      .width("0%");
}
