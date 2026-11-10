# =============================================================================
# USACH | Estadistica Aplicada | Grupo 3 | Olist
# Etapa 4B v1.4 - GRAFICOS PORTABLES CON R BASE
# Diseno limpio: barras con origen cero y matriz de R2 sin superposiciones.
# SOLO lee CSV ya guardados de Etapa 4, valida su consistencia y genera PNG/PDF.
# NO entrena modelos, NO toca fechas de test ni cambia CSV de resultados.
# Desde la raiz del proyecto:
# source("scripts/04B_regenerar_graficos_datacamp.R", encoding = "UTF-8")
# =============================================================================

local({
  raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  dir_in <- file.path(raiz, "resultados", "04_baseline_y_ols")
  dir_graf <- file.path(raiz, "graficos", "04_baseline_y_ols")
  entradas <- c(
    folds = file.path(dir_in, "01_metricas_por_fold.csv"),
    resumen = file.path(dir_in, "02_resumen_comparativo_principal.csv"),
    predicciones = file.path(dir_in, "03_predicciones_validacion.csv")
  )
  comprobar <- function(cond, descripcion) {
    if (!isTRUE(cond)) stop(descripcion, call. = FALSE)
  }
  comprobar(all(file.exists(entradas)), paste(
    "Faltan los CSV de Etapa 4:",
    paste(entradas[!file.exists(entradas)], collapse = " | ")))
  # Esta version solo usa paquetes incluidos con R (graphics, grDevices,
  # utils y tools). No requiere instalar ggplot2.
  # Guardar huellas de entrada para comprobar que no se modifica nada.
  md5_antes <- unname(tools::md5sum(entradas))
  leer <- function(ruta) utils::read.csv(
    ruta, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA"))
  m <- leer(entradas[["folds"]])
  s <- leer(entradas[["resumen"]])
  p <- utils::read.csv(
    entradas[["predicciones"]], stringsAsFactors = FALSE,
    check.names = FALSE, colClasses = c(order_id = "character"),
    na.strings = c("", "NA"))

  modelos <- c("Baseline_media_train", "A_temporal",
               "B_pedido_geografia", "C_catalogo_exploratoria")
  mes_etiqueta <- c("2017-12" = "Dic 2017", "2018-01" = "Ene 2018",
                    "2018-02" = "Feb 2018", "2018-03" = "Mar 2018")
  columnas_m <- c("fold", "mes_validacion", "modelo", "n_validacion",
                  "rmse", "mae", "r2")
  columnas_s <- c("modelo", "rmse_promedio", "r2_promedio")
  columnas_p <- c("fold", "order_id", "modelo", "dias_reales", "dias_predichos")
  comprobar(all(columnas_m %in% names(m)) &&
              all(columnas_s %in% names(s)) &&
              all(columnas_p %in% names(p)),
            "Los CSV no corresponden a la estructura esperada de Etapa 4.")
  comprobar(nrow(m) == 16L && nrow(s) == 4L &&
              !anyNA(m[, columnas_m]) && !anyNA(s[, columnas_s]) &&
              !anyNA(p[, columnas_p]),
            "Filas incompletas o cantidades inesperadas en los CSV.")
  comprobar(setequal(unique(m$modelo), modelos) &&
              setequal(unique(s$modelo), modelos) &&
              setequal(unique(p$modelo), modelos) &&
              setequal(unique(m$fold), 1:4),
            "Se detectaron modelos o folds diferentes a los acordados.")
  comprobar(!anyDuplicated(paste(m$fold, m$modelo, sep = "|")) &&
              !anyDuplicated(paste(p$fold, p$modelo, p$order_id, sep = "|")),
            "Hay metricas o predicciones duplicadas para modelo/fold/pedido.")
  comprobar(setequal(unique(m$mes_validacion), names(mes_etiqueta)),
            "Los meses no coinciden con diciembre 2017 - marzo 2018.")

  # Revisiones independientes: las mismas observaciones y los mismos valores
  # reales para todos los modelos; las metricas provienen de predicciones.
  for (k in 1:4) {
    referencia <- p[p$fold == k & p$modelo == modelos[1L],
                    c("order_id", "dias_reales"), drop = FALSE]
    comprobar(nrow(referencia) > 0 && !anyDuplicated(referencia$order_id),
              paste("Faltan referencias correctas en fold", k))
    for (modelo in modelos) {
      z <- p[p$fold == k & p$modelo == modelo, , drop = FALSE]
      i <- match(referencia$order_id, z$order_id)
      comprobar(nrow(z) == nrow(referencia) && !anyNA(i) &&
                  !anyDuplicated(z$order_id) &&
                  max(abs(z$dias_reales[i] - referencia$dias_reales)) < 1e-8,
                paste("Los pedidos o las respuestas se mezclaron:", modelo, k))
      e <- z$dias_reales[i] - z$dias_predichos[i]
      sst <- sum((z$dias_reales[i] - mean(z$dias_reales[i]))^2)
      comprobar(is.finite(sst) && sst > 0, "Respuesta sin variacion.")
      calc <- c(rmse = sqrt(mean(e^2)), mae = mean(abs(e)),
                r2 = 1 - sum(e^2) / sst)
      r <- m[m$fold == k & m$modelo == modelo, , drop = FALSE]
      comprobar(nrow(r) == 1L && r$n_validacion == nrow(referencia) &&
                  all(is.finite(calc)) &&
                  max(abs(as.numeric(r[1, names(calc)]) - calc)) < 1e-8,
                paste("La metrica no coincide con las predicciones:", modelo, k))
    }
  }
  for (modelo in modelos) {
    z <- m[m$modelo == modelo, , drop = FALSE]
    r <- s[s$modelo == modelo, , drop = FALSE]
    comprobar(nrow(z) == 4L && nrow(r) == 1L &&
                abs(mean(z$rmse) - r$rmse_promedio) < 1e-8 &&
                abs(mean(z$r2) - r$r2_promedio) < 1e-8,
              paste("El resumen no coincide con los cuatro folds:", modelo))
  }


  # ------------------------------------------------------------
  # Graficos con R base: sin paquetes externos ni reentrenamiento.
  # ------------------------------------------------------------
  ink <- "#17324B"
  muted <- "#53697D"
  teal <- "#0D807B"
  colores <- c(Baseline_media_train = "#A1ABB6",
               A_temporal = "#7E96AC",
               B_pedido_geografia = "#127F79",
               C_catalogo_exploratoria = "#69ABA6")
  textos_cortos <- c(
    Baseline_media_train = "Referencia: media de entrenamiento",
    A_temporal = "OLS A: variables temporales",
    B_pedido_geografia = "OLS B: pedido y geografía",
    C_catalogo_exploratoria = "OLS C: catálogo exploratorio"
  )
  formato <- function(x, decimales = 3L) {
    gsub(".", ",", sprintf(paste0("%.", decimales, "f"), x), fixed = TRUE)
  }
  dir.create(dir_graf, showWarnings = FALSE, recursive = TRUE)

  dibujar_rmse <- function() {
    d <- s[order(s$rmse_promedio), , drop = FALSE]
    y <- rev(seq_len(nrow(d)))
    max_x <- max(14, ceiling(max(d$rmse_promedio) + 2))
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    graphics::par(mar = c(7.4, 18.5, 6.8, 2.5),
                  family = "sans", las = 1, xaxs = "i", yaxs = "i")
    graphics::plot.new()
    graphics::plot.window(xlim = c(0, max_x), ylim = c(0.3, 4.7))
    for (x in seq(0, max_x, by = 2)) {
      graphics::segments(x, 0.45, x, 4.55,
                         col = "#E6EBF0", lwd = 1)
    }
    for (j in seq_len(nrow(d))) {
      model <- d$modelo[j]
      graphics::rect(0, y[j] - 0.25, d$rmse_promedio[j], y[j] + 0.25,
                     border = NA, col = unname(colores[model]))
      graphics::text(-0.25, y[j], labels = unname(textos_cortos[model]),
                     adj = 1, xpd = NA, cex = 1.06, col = ink)
      graphics::text(d$rmse_promedio[j] + 0.15, y[j],
                     labels = paste0(formato(d$rmse_promedio[j]), " días"),
                     adj = 0, cex = 1.06, font = 2, col = ink)
    }
    graphics::axis(1, at = seq(0, max_x, by = 2),
                   labels = seq(0, max_x, by = 2),
                   col.axis = muted, col = "#D6DFE7", cex.axis = 1.1)
    graphics::mtext("RMSE medio de validación (días)", side = 1,
                    line = 3.1, col = ink, cex = 1.15)
    graphics::mtext("Error predictivo por alternativa", side = 3,
                    line = 4.5, adj = 0, cex = 1.65, col = ink, font = 2)
    graphics::mtext("Cuatro folds temporales | Menor es mejor | Eje desde cero",
                    side = 3, line = 2.9, adj = 0, cex = 1.05, col = muted)
    bc <- abs(s$rmse_promedio[s$modelo == "B_pedido_geografia"] -
                s$rmse_promedio[s$modelo == "C_catalogo_exploratoria"])
    graphics::mtext(paste0("OLS B y C difieren ", formato(bc),
                  " días. Comparación descriptiva, sin probar superioridad estadística."),
                    side = 1, line = 5.65, adj = 0, cex = 0.84, col = muted)
  }

  fondo_r2 <- function(valor) {
    if (valor < 0) return("#F5DFDE")
    if (valor < 0.10) return("#E9F5F3")
    if (valor < 0.17) return("#CFEBE6")
    "#A9DDD5"
  }
  dibujar_r2 <- function() {
    op <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(op), add = TRUE)
    graphics::par(mar = c(6.6, 16.2, 7.2, 2.5),
                  family = "sans", xaxs = "i", yaxs = "i")
    graphics::plot.new()
    graphics::plot.window(xlim = c(0.5, 4.5), ylim = c(0.5, 4.5))
    nombres_meses <- c("Dic 2017", "Ene 2018", "Feb 2018", "Mar 2018")
    modelos_orden <- c("B_pedido_geografia", "C_catalogo_exploratoria",
                       "A_temporal", "Baseline_media_train")
    etiquetas_filas <- c("OLS B: pedido y geografía", "OLS C: catálogo",
                         "OLS A: temporal", "Referencia: media train")
    for (j in seq_along(modelos_orden)) {
      yy <- 5 - j
      model <- modelos_orden[j]
      graphics::text(0.40, yy, etiquetas_filas[j], adj = 1,
                     xpd = NA, cex = 1.02, col = ink)
      for (k in 1:4) {
        registro <- m[m$modelo == model & m$fold == k, , drop = FALSE]
        comprobar(nrow(registro) == 1L,
                  paste("Falta combinación modelo/fold", model, k))
        r2 <- registro$r2
        graphics::rect(k - 0.46, yy - 0.37, k + 0.46, yy + 0.37,
                       col = fondo_r2(r2), border = "white", lwd = 3)
        graphics::text(k, yy, formato(r2), cex = 1.40, col = ink, font = 2)
      }
    }
    graphics::axis(3, at = 1:4, labels = nombres_meses, tick = FALSE,
                   cex.axis = 1.2, col.axis = ink, line = -0.55)
    graphics::mtext("R² por modelo y mes de validación", side = 3,
                    line = 4.6, adj = 0, cex = 1.64, col = ink, font = 2)
    graphics::mtext("Misma población de validación por mes | Valores exactos",
                    side = 3, line = 3.10, adj = 0, cex = 1.05, col = muted)
    graphics::mtext("Verde: R² positivo | Rosado: R² negativo",
                    side = 1, line = 2.8, adj = 0, cex = 1.05, col = ink)
    graphics::mtext(paste("Un R² negativo indica que el modelo es peor que",
                          "predecir la media real del mes validado."),
                    side = 1, line = 4.8, adj = 0, cex = 0.90, col = muted)
  }

  # No se comprueba un tamaño arbitrario de 6 KB: un PDF legítimo
  # puede ser más pequeño. Se verifica la cabecera real PNG/PDF.
  verificar_archivo <- function(ruta, extension) {
    comprobar(file.exists(ruta), paste("No se generó:", ruta))
    bytes <- file.info(ruta)$size
    comprobar(!is.na(bytes) && bytes > 100,
              paste("Archivo vacío o incompleto:", basename(ruta),
                    "| bytes:", bytes))
    conexion <- file(ruta, open = "rb")
    on.exit(close(conexion), add = TRUE)
    firma <- readBin(conexion, what = "raw", n = 8L)
    correcta <- if (extension == "png") {
      identical(firma, as.raw(c(0x89, 0x50, 0x4E, 0x47,
                               0x0D, 0x0A, 0x1A, 0x0A)))
    } else {
      length(firma) >= 5L &&
        identical(firma[1:5], charToRaw("%PDF-"))
    }
    comprobar(correcta, paste("Firma de archivo incorrecta:", basename(ruta)))
    cat("  VALIDADO:", basename(ruta), "|", bytes, "bytes\n")
    invisible(TRUE)
  }

  generar <- function(ruta, formato_archivo, dibujar) {
    cat("Generando:", basename(ruta), "\n")
    if (formato_archivo == "png") {
      grDevices::png(filename = ruta, width = 2400, height = 1300,
                     units = "px", res = 180, bg = "white")
    } else {
      grDevices::pdf(file = ruta, width = 13.333, height = 7.222,
                     bg = "white", useDingbats = FALSE)
    }
    tryCatch(dibujar(), finally = grDevices::dev.off())
    verificar_archivo(ruta, formato_archivo)
  }

  archivos_png <- c(
    file.path(dir_graf, "01_rmse_validacion_por_modelo.png"),
    file.path(dir_graf, "02_r2_validacion_por_fold.png"))
  archivos_pdf <- sub("\\.png$", ".pdf", archivos_png)
  generar(archivos_png[1], "png", dibujar_rmse)
  generar(archivos_png[2], "png", dibujar_r2)
  generar(archivos_pdf[1], "pdf", dibujar_rmse)
  generar(archivos_pdf[2], "pdf", dibujar_r2)

  comprobar(identical(unname(tools::md5sum(entradas)), md5_antes),
            "ALERTA: se modificaron los CSV de resultados de Etapa 4.")
  cat("\nETAPA 4B v1.4 COMPLETADA - GRAFICOS CON R BASE\n")
  cat("Comparabilidad: pedidos, fechas, predicciones y metricas verificadas.\n")
  cat("Archivos originales de resultados: SIN CAMBIOS (MD5).\n")
  cat("Se regeneraron 2 PNG y 2 PDF en:", dir_graf, "\n")
  cat("No se entrenaron modelos ni se consultó el test histórico.\n")
})
