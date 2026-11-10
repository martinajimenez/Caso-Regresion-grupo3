# ============================================================================
# GRUPO 3 | OLIST | GRAFICOS DESCRIPTIVOS ETAPA 1 - VERSION 2.1
# Solo R base: graphics, grDevices, stats y utils, incluidos con R.
# No modifica datos, etiquetas, resultados numericos ni modelos.
# ============================================================================

crear_graficos_etapa1 <- function(y, resumen_mes, figuras) {
  y <- suppressWarnings(as.numeric(y))
  if (length(y) < 100L || anyNA(y) || any(!is.finite(y)) || any(y <= 0)) {
    stop("La respuesta debe contener solo tiempos positivos y finitos.", call. = FALSE)
  }
  campos <- c("mes_aprobacion", "n", "mediana_dias")
  if (!all(campos %in% names(resumen_mes))) {
    stop("Faltan columnas del resumen mensual.", call. = FALSE)
  }
  if (anyNA(resumen_mes$mediana_dias) ||
      any(!is.finite(as.numeric(resumen_mes$mediana_dias))) ||
      anyDuplicated(resumen_mes$mes_aprobacion) ||
      !identical(as.character(resumen_mes$mes_aprobacion),
                 sort(as.character(resumen_mes$mes_aprobacion))) ||
      sum(resumen_mes$n) != length(y)) {
    stop("Resumen mensual inconsistente con los tiempos observados.", call. = FALSE)
  }

  dir.create(figuras, recursive = TRUE, showWarnings = FALSE)
  tinta <- "#18334D"
  suave <- "#596E81"
  rejilla <- "#E6EBF0"
  celeste <- "#8DBBC7"
  petroleo <- "#187A85"
  naranja <- "#D26A32"
  naranja_claro <- "#FFF2E9"
  blanco <- "#FFFFFF"

  guardar_png <- function(nombre, dibujar) {
    destino <- file.path(figuras, nombre)
    temporal <- tempfile(pattern = "stage1_fig_", tmpdir = figuras,
                         fileext = ".png")
    grDevices::png(temporal, width = 1800, height = 1000, res = 180,
                   bg = blanco)
    dispositivo <- grDevices::dev.cur()
    tryCatch({
      dibujar()
      grDevices::dev.off(which = dispositivo)
    }, error = function(e) {
      abiertos <- grDevices::dev.list()
      if (!is.null(abiertos) && dispositivo %in% abiertos) {
        try(grDevices::dev.off(which = dispositivo), silent = TRUE)
      }
      unlink(temporal)
      stop(paste("No se pudo dibujar", nombre, ":", conditionMessage(e)),
           call. = FALSE)
    })
    if (!file.exists(temporal) || is.na(file.info(temporal)$size) ||
        file.info(temporal)$size < 3500L) {
      unlink(temporal)
      stop(paste("El archivo PNG esta vacio o incompleto:", nombre),
           call. = FALSE)
    }
    if (!file.copy(temporal, destino, overwrite = TRUE)) {
      unlink(temporal)
      stop(paste("No se pudo reemplazar", destino,
                 "Cierra el visualizador si esta bloqueado."), call. = FALSE)
    }
    unlink(temporal)
    invisible(destino)
  }

  # HISTOGRAMA. Se limita solo el encuadre; ninguna observacion es excluida.
  mediana <- stats::median(y)
  p99 <- unname(stats::quantile(y, probs = 0.99, names = FALSE))
  limite <- max(10, ceiling(p99 / 10) * 10)
  ancho_bin <- if (limite <= 60) 2 else 5
  cortes <- seq(0, limite, by = ancho_bin)
  if (tail(cortes, 1) < limite) cortes <- c(cortes, limite)
  visibles <- y[y < limite]
  fuera <- sum(y >= limite)
  histo <- graphics::hist(visibles, breaks = cortes, right = FALSE,
                          include.lowest = TRUE, plot = FALSE)
  max_y <- max(histo$counts) * 1.23
  marcas_y <- pretty(c(0, max(histo$counts)), n = 5)
  marcas_y <- marcas_y[marcas_y >= 0 & marcas_y < max_y]

  guardar_png("01_histograma_tiempo_entrega.png", function() {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    # Los ejes verticales se rotulan HORIZONTALMENTE sobre el panel.
    # Asi evitamos recortes en PNG y en distintos visores/equipos.
    graphics::par(mar = c(6.1, 4.5, 6.0, 1.3), mgp = c(2.1, 0.65, 0),
                  las = 1, family = "sans", bty = "n", xaxs = "i", yaxs = "i")
    graphics::plot(NA_real_, NA_real_, type = "n", xlim = c(0, limite),
                   ylim = c(0, max_y), axes = FALSE, xlab = "", ylab = "")
    graphics::abline(h = marcas_y, col = rejilla, lwd = 1)
    graphics::rect(head(histo$breaks, -1L), 0,
                   tail(histo$breaks, -1L), histo$counts,
                   col = celeste, border = blanco, lwd = 1.0)
    graphics::abline(v = mediana, col = naranja, lwd = 2.9, lty = 2)
    graphics::axis(1, at = seq(0, limite, by = 10),
                   col = "#CED8DF", col.axis = tinta, cex.axis = 1.02,
                   tck = -0.015)
    graphics::axis(2, at = marcas_y,
                   labels = format(marcas_y, big.mark = ".", decimal.mark = ",",
                                   scientific = FALSE, trim = TRUE),
                   col = NA, col.axis = tinta, cex.axis = 0.95, tick = FALSE)
    graphics::mtext("Días desde la aprobación hasta la entrega", side = 1,
                    line = 2.7, cex = 1.12, col = tinta)
    graphics::mtext("Pedidos por intervalo", side = 3, line = 0.55,
                    adj = 0, cex = 0.94, col = suave)
    graphics::mtext("Distribución del tiempo real de entrega", side = 3,
                    line = 3.65, adj = 0, cex = 1.57, font = 2, col = tinta)
    graphics::mtext(paste0("Desarrollo | ",
                   format(length(y), big.mark = ".", decimal.mark = ",", trim = TRUE),
                   " pedidos con entrega observada al corte"),
                   side = 3, line = 2.37, adj = 0, cex = 1.00, col = suave)
    graphics::text(mediana + 0.02 * limite, max_y * 0.925,
                   paste0("Mediana: ", formatC(mediana, format = "f", digits = 2),
                          " días"), col = naranja,
                   adj = c(0, 1), cex = 1.09, font = 2)
    pie <- paste0("Vista hasta ", limite, " días (P99 = ",
                  formatC(p99, format = "f", digits = 1), "). ",
                  format(fuera, big.mark = ".", decimal.mark = ",", trim = TRUE),
                  " pedidos fuera del encuadre; ninguno fue eliminado.")
    graphics::mtext(pie, side = 1, line = 4.75, adj = 0,
                    cex = 0.91, col = suave)
  })

  # TENDENCIA MENSUAL. Junio 2018 no se une por una linea a los demas
  # meses: su mediana esta condicionada por la censura en el corte historico.
  fechas <- as.Date(paste0(as.character(resumen_mes$mes_aprobacion), "-01"))
  if (anyNA(fechas)) stop("Meses de aprobacion no validos.", call. = FALSE)
  n <- length(fechas)
  val <- as.numeric(resumen_mes$mediana_dias)
  rango <- diff(range(val))
  ymin <- min(val) - max(0.85, 0.15 * rango)
  ymax <- max(val) + max(1.25, 0.22 * rango)
  especial <- as.character(tail(resumen_mes$mes_aprobacion, 1)) == "2018-06"
  meses <- c("ene", "feb", "mar", "abr", "may", "jun",
             "jul", "ago", "sep", "oct", "nov", "dic")
  etiquetas <- paste0(meses[as.integer(format(fechas, "%m"))], " ",
                      format(fechas, "%y"))
  marcas_x <- sort(unique(c(1L, seq(4L, n, by = 3L), n)))

  guardar_png("02_mediana_mensual_desarrollo.png", function() {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    graphics::par(mar = c(6.0, 4.5, 6.0, 1.3), mgp = c(2.1, 0.65, 0),
                  las = 1, family = "sans", bty = "n", xaxs = "i", yaxs = "i")
    graphics::plot(NA_real_, NA_real_, type = "n",
                   xlim = c(0.5, n + 0.48), ylim = c(ymin, ymax),
                   axes = FALSE, xlab = "", ylab = "")
    if (especial) {
      graphics::rect(n - 0.48, ymin, n + 0.48, ymax,
                     col = naranja_claro, border = NA)
    }
    marcas_y <- pretty(c(ymin, ymax), n = 6)
    marcas_y <- marcas_y[marcas_y >= ymin & marcas_y <= ymax]
    graphics::abline(h = marcas_y, col = rejilla, lwd = 1)
    hasta <- if (especial) n - 1L else n
    graphics::lines(seq_len(hasta), val[seq_len(hasta)],
                    col = petroleo, lwd = 2.9)
    graphics::points(seq_len(hasta), val[seq_len(hasta)],
                     pch = 21, bg = petroleo, col = petroleo, cex = 1.32)
    if (especial) {
      # La observacion de junio no forma parte de la linea de tendencia.
      graphics::points(n, val[n], pch = 21, cex = 1.65,
                       bg = blanco, col = naranja, lwd = 2.6)
      graphics::text(n - 0.12, val[n] + 0.52,
                     paste0(formatC(val[n], format = "f", digits = 2), "*"),
                     adj = c(1, 0), col = naranja, font = 2, cex = 1.07)
      etiquetas[n] <- paste0(etiquetas[n], "*")
    }
    graphics::axis(1, at = marcas_x, labels = etiquetas[marcas_x],
                   col = "#CED8DF", col.axis = tinta, cex.axis = 0.99,
                   tick = FALSE)
    graphics::axis(2, at = marcas_y, col = NA, col.axis = tinta,
                   cex.axis = 0.98, tick = FALSE)
    graphics::mtext("Mes de aprobación", side = 1,
                    line = 2.7, cex = 1.12, col = tinta)
    graphics::mtext("Mediana de entrega (días)", side = 3,
                    line = 0.55, adj = 0, cex = 0.94, col = suave)
    graphics::mtext("Evolución mensual del tiempo de entrega", side = 3,
                    line = 3.65, adj = 0, cex = 1.57, font = 2, col = tinta)
    graphics::mtext("Desarrollo | Pedidos entregados al corte del 01-07-2018",
                    side = 3, line = 2.37, adj = 0, cex = 1.0, col = suave)
    nota <- if (especial) {
      "* Junio de 2018: cohorte incompleta al corte. Su mediana no es comparable directamente."
    } else {
      "Serie descriptiva de pedidos con entrega conocida al corte histórico."
    }
    graphics::mtext(nota, side = 1, line = 4.75, adj = 0,
                    col = if (especial) naranja else suave, cex = 0.91)
  })

  invisible(file.path(figuras,
                      c("01_histograma_tiempo_entrega.png",
                        "02_mediana_mensual_desarrollo.png")))
}
