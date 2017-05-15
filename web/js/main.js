const paramsFunction = query => {
  if (!query) {
    return { };
  }

  return (/^[?#]/.test(query) ? query.slice(1) : query)
    .split('&')
    .reduce((params, param) => {
      let [ key, value ] = param.split('=');
      params[key] = value ? decodeURIComponent(value.replace(/\+/g, ' ')) : '';
      return params;
    }, { });
};

function queryParams() {
  return paramsFunction(window.location.search);
}

function esFirefox() {
  return navigator.userAgent.toLowerCase().indexOf('firefox') > -1;
}

function jugadoresDisponibles() {
  return Rx.Observable.concat(Rx.Observable.of(["humano", "Humano"]),
      Rx.Observable.generate(0, n => n < 2, n => n + 1, n => `http://localhost:${4568 + n}/`)
          .concatMap(url => nombreServicio(url).map(nombre => [url, nombre])));
}

function noDejarJugarContraSiMismo($selectTipoJugador, $selectOtroTipoJugador) {
  $selectOtroTipoJugador.find("option[disabled='disabled']").removeAttr("disabled");
  if ($selectTipoJugador.val() != "humano") {
    if ($selectTipoJugador.val() == $selectOtroTipoJugador.val()) {
      $selectOtroTipoJugador.find("option[selected='selected']").removeAttr("selected");
      $selectOtroTipoJugador.find("option[value='humano']").attr("selected", "selected");
    }
    $selectOtroTipoJugador.find(`option[value="${$selectTipoJugador.val()}"]`).attr("disabled", "disabled");
  }
}

$(document).ready(_ => {
  $("body").css("color", COLOR_BORDE);

  if (esFirefox()) {
    $("select").removeClass("form-control");
  }

  cantidadCasillerosServicio()
      .subscribe(tamano => {
        inicializarTamano(tamano[0], tamano[1]);

        let params = queryParams();
        params.tipoJugador1 = params.tipoJugador1 || "humano";
        params.tipoJugador2 = params.tipoJugador2 || "http://localhost:4568/";

        let $selectTipoJugador1 = $("#tipoJugador1").change(() =>
            noDejarJugarContraSiMismo($selectTipoJugador1, $selectTipoJugador2));
        let $selectTipoJugador2 = $("#tipoJugador2").change(() =>
            noDejarJugarContraSiMismo($selectTipoJugador2, $selectTipoJugador1));

        const NOMBRES = new Set();

        jugadoresDisponibles()
            .subscribe(par => {
              let valor = par[0];
              let nombre = par[1];

              while (NOMBRES.has(nombre)) {
                nombre += " (2)";
              }
              NOMBRES.add(nombre);

              $selectTipoJugador1.append($("<option>", {
                value: valor,
                text: nombre,
              }));
              $selectTipoJugador2.append($("<option>", {
                value: valor,
                text: nombre,
              }));
            }, error => {
              if (error.status === 0 && error.xhr.readyState == 4) {
                let $status = $("#status");

                $status.text("¡Ups! Hubo un problema al obtener los nombres de las inteligencias. Tal vez no están encendidos los servicios :)");
                $status.css("color", "red");
                $status.css("font-weight", "bold");
              }
              console.log(error);
            }, () => {
              let $optionTipoJugador1 =
                  $selectTipoJugador1.find(`option[value="${params.tipoJugador1}"]`).attr("selected", "selected");
              let $optionTipoJugador2 =
                  $selectTipoJugador2.find(`option[value="${params.tipoJugador2}"]`).attr("selected", "selected");

              $("#etiquetaJugador1").text($optionTipoJugador1.text());
              noDejarJugarContraSiMismo($selectTipoJugador1, $selectTipoJugador2);
              $("#etiquetaJugador2").text($optionTipoJugador2.text());
              noDejarJugarContraSiMismo($selectTipoJugador2, $selectTipoJugador1);

              new Futlog([$optionTipoJugador1.val(), $optionTipoJugador2.val()]).jugar();
            });
      }, error => {
        if (error.status === 0 && error.xhr.readyState == 4) {
          let $status = $("#status");

          $status.text("¡Ups! Hubo un problema al obtener el tamaño del tablero. Tal vez no están encendidos los servicios :)");
          $status.css("color", "red");
          $status.css("font-weight", "bold");
        }
        console.log(error);
      });
});
