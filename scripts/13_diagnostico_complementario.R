# OLIST | ETAPA 13 | Diagnostico descriptivo reproducible de desarrollo
# Desde la raiz: source('scripts/13_diagnostico_complementario.R', encoding='UTF-8')
# R base. Lee predicciones archivadas; no ajusta modelos ni consulta el test.
local({
  raiz <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
  ruta <- function(...) file.path(raiz, ...)
  exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  out <- ruta('resultados', '13_diagnostico_complementario')
  fig <- ruta('graficos', '13_diagnostico_complementario')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig, recursive = TRUE, showWarnings = FALSE)
  cert <- file.path(out, 'CONTROL_ETAPA13_APROBADO.txt')
  if (file.exists(cert)) unlink(cert)
  leer <- function(f) utils::read.csv(ruta('resultados', f), check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c('', 'NA'), fileEncoding = 'UTF-8-BOM')
  guardar <- function(d, f) utils::write.csv(d, file.path(out, f), row.names = FALSE, na = '')
  historicos <- list.files(ruta('resultados'), full.names = TRUE, recursive = TRUE)
  historicos <- historicos[!grepl('/(13_diagnostico_complementario|14_interpretacion_e_incertidumbre)/', historicos)]
  huellas <- tools::md5sum(historicos)
  modelos <- c('Baseline_media_train', 'B_pedido_geografia', 'Ridge_min', 'Lasso_min',
      'ElasticNet_min', 'Splines_B_anidado', 'Arbol_B_anidado',
      'RandomForest_B_anidado', 'Boosting_B_anidado', 'Averaging_Splines_Boosting')
  etiquetas <- c('Baseline', 'OLS B', 'Ridge', 'Lasso', 'Elastic net', 'Splines',
      'Arbol', 'Random Forest', 'Boosting', 'Averaging 50/50*')
  archivos <- c('04_baseline_y_ols/03_predicciones_validacion.csv',
      '05_regularizacion/05_predicciones_validacion_regularizados.csv',
      '06_splines/05_predicciones_splines_por_pedido.csv',
      '07_ensembles/05_predicciones_validacion.csv', '07_ensembles/12_averaging_predicciones.csv')
  cols <- c('fold', 'order_id', 'modelo', 'dias_reales', 'dias_predichos')
  cat('[13:1/5] Comprobando predicciones comunes de las diez alternativas...\n')
  pred <- do.call(rbind, lapply(archivos, function(f) {
    d <- leer(f); exigir(all(cols %in% names(d)), paste('Columnas ausentes:', f)); d[, cols]
  }))
  pred <- pred[pred$modelo %in% modelos, , drop = FALSE]
  exigir(nrow(pred) == 262790L && !anyNA(pred) &&
      !anyDuplicated(pred[c('modelo', 'fold', 'order_id')]) &&
      all(is.finite(pred$dias_reales)) && all(is.finite(pred$dias_predichos)),
      'Predicciones faltantes, duplicadas, no finitas o con conteo inesperado')
  clave <- function(d) paste(d$fold, d$order_id, sep = '|')
  ref <- pred[pred$modelo == modelos[1], , drop = FALSE]
  ids <- leer('03_protocolo_y_folds/04_ids_train_validacion.csv')
  iv <- ids[ids$rol == 'validacion', , drop = FALSE]
  exigir(nrow(iv) == 26279L && setequal(clave(ref), clave(iv)), 'IDs distintos del protocolo')
  for (m in modelos) {
    d <- pred[pred$modelo == m, , drop = FALSE]; ix <- match(clave(d), clave(ref))
    exigir(nrow(d) == 26279L && !anyNA(ix) && setequal(clave(d), clave(ref)) &&
        max(abs(d$dias_reales - ref$dias_reales[ix])) < 1e-10 &&
        identical(sort(unique(as.integer(d$fold))), 1:4), paste('Comparacion no alineada:', m))
  }
  pred$error <- pred$dias_predichos - pred$dias_reales
  # Las tolerancias 3 y 5 son ilustrativas, no exigencias operacionales aprobadas.
  metricas <- function(d) {
    e <- d$error; a <- abs(e); y <- d$dias_reales; p <- d$dias_predichos
    ss <- sum((y - mean(y))^2)
    exigir(nrow(d) > 1L && ss > 0, 'Muestra vacia o respuesta constante')
    variable <- diff(range(p)) > 1e-12
    c(rmse = sqrt(mean(e^2)), mae = mean(a), r2 = 1 - sum(e^2)/ss,
      sesgo = mean(e), mediana_error_absoluto = stats::median(a),
      p90_error_absoluto = unname(stats::quantile(a, .9, type = 7)),
      porcentaje_dentro_3_dias = 100 * mean(a <= 3),
      porcentaje_dentro_5_dias = 100 * mean(a <= 5),
      porcentaje_subestima = 100 * mean(e < 0),
      pearson = if (variable) stats::cor(y, p) else NA_real_,
      spearman = if (variable) stats::cor(y, p, method = 'spearman') else NA_real_)
  }
  media_segura <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  por_fold <- do.call(rbind, lapply(modelos, function(m) do.call(rbind, lapply(1:4, function(k) {
    d <- pred[pred$modelo == m & pred$fold == k, , drop = FALSE]
    data.frame(modelo = m, fold = k, n = nrow(d), as.list(metricas(d)), check.names = FALSE)
  }))))
  numericas <- setdiff(names(por_fold), c('modelo', 'fold', 'n'))
  resumen <- do.call(rbind, lapply(modelos, function(m) {
    d <- por_fold[por_fold$modelo == m, , drop = FALSE]
    v <- vapply(d[, numericas], media_segura, numeric(1))
    names(v) <- paste0(names(v), '_promedio')
    data.frame(modelo = m, numero_folds = nrow(d), n_validacion = sum(d$n),
      as.list(v), sd_rmse_folds = stats::sd(d$rmse), sd_mae_folds = stats::sd(d$mae), check.names = FALSE)
  }))
  original <- leer('10_decision_modelo/08_comparacion_10_alternativas.csv')
  ix <- match(resumen$modelo, original$modelo)
  exigir(!anyNA(ix), 'Faltan modelos en la tabla original')
  for (z in c('rmse_promedio', 'mae_promedio', 'r2_promedio', 'sesgo_promedio'))
    exigir(max(abs(resumen[[z]] - original[[z]][ix])) < 1e-8, paste('Metrica no coincide:', z))
  combinado <- do.call(rbind, lapply(modelos, function(m) {
    d <- pred[pred$modelo == m, , drop = FALSE]
    data.frame(modelo = m, n = nrow(d), as.list(metricas(d)), check.names = FALSE)
  }))
  guardar(por_fold, '01_metricas_por_fold.csv')
  guardar(resumen, '02_promedios_entre_folds.csv')
  guardar(combinado, '03_metricas_combinadas_aparte.csv')

  cat('[13:2/5] Calibracion por grupos de prediccion y duraciones reales...\n')
  calibracion <- do.call(rbind, lapply(modelos, function(m) do.call(rbind, lapply(1:4, function(k) {
    d <- pred[pred$modelo == m & pred$fold == k, , drop = FALSE]
    cortes <- unique(as.numeric(stats::quantile(d$dias_predichos, seq(0, 1, .1), type = 7)))
    grupo <- if (length(cortes) == 1L) rep(1L, nrow(d)) else
      as.integer(cut(d$dias_predichos, cortes, include.lowest = TRUE))
    do.call(rbind, lapply(sort(unique(grupo)), function(g) {
      a <- d[grupo == g, , drop = FALSE]
      data.frame(modelo = m, fold = k, grupo_prediccion = g, n = nrow(a),
        prediccion_minima = min(a$dias_predichos), prediccion_maxima = max(a$dias_predichos),
        media_predicha = mean(a$dias_predichos), media_real = mean(a$dias_reales), sesgo = mean(a$error))
    }))
  }))))
  exigir(sum(calibracion$n) == nrow(pred), 'Los grupos no cubren todas las observaciones')
  guardar(calibracion, '04_calibracion_por_prediccion_y_fold.csv')
  pred$tramo_real <- cut(pred$dias_reales, c(-Inf, 7, 14, 30, Inf), right = FALSE,
      labels = c('Menos de 7', '7 a menos de 14', '14 a menos de 30', '30 o mas'))
  tramos <- do.call(rbind, lapply(modelos, function(m) do.call(rbind, lapply(levels(pred$tramo_real), function(t) {
    d <- pred[pred$modelo == m & pred$tramo_real == t, , drop = FALSE]
    todo <- pred[pred$modelo == m, , drop = FALSE]
    data.frame(modelo = m, tramo_real = t, n = nrow(d), as.list(metricas(d)),
      porcentaje_pedidos = 100 * nrow(d)/nrow(todo),
      porcentaje_error_cuadratico = 100 * sum(d$error^2)/sum(todo$error^2), check.names = FALSE)
  }))))
  guardar(tramos, '05_error_por_duracion_real_retrospectivo.csv')

  # Sensibilidad SOLO de la seleccion interna archivada. No calcula un nuevo
  # error externo de modelos reoptimizados por MAE ni cambia la decision original.
  registros <- list()
  anadir <- function(k, familia, a, regular = FALSE) {
    r <- if (regular) order(a$rmse, -a$lambda, a$alpha_enet) else order(a$rmse, a$config)
    q <- if (regular) order(a$mae, -a$lambda, a$alpha_enet) else order(a$mae, a$config)
    r <- a[r[1], ]; q <- a[q[1], ]
    registros[[length(registros) + 1L]] <<- data.frame(fold = k, familia = familia,
      configuracion_RMSE = r$config, configuracion_MAE = q$config,
      MAE_regla_RMSE = r$mae, MAE_regla_MAE = q$mae, cambia = r$config != q$config,
      mejora_MAE_interna_pct = 100 * (r$mae - q$mae)/r$mae)
  }
  g <- leer('05_regularizacion/02_grilla_resultados_validacion_interna.csv')
  g$familia <- ifelse(grepl('^ENet', g$algoritmo), 'ElasticNet', g$algoritmo)
  for (k in 1:4) for (f in unique(g$familia)) {
    d <- g[g$fold_externo == k & g$familia == f, , drop = FALSE]
    a <- stats::aggregate(cbind(rmse, mae) ~ algoritmo + alpha_enet + lambda, d, mean)
    a$config <- paste(a$algoritmo, a$alpha_enet, format(a$lambda, digits = 17), sep = ';')
    anadir(k, f, a, TRUE)
  }
  for (familia in c('Splines', 'Ensembles')) {
    g <- if (familia == 'Splines') leer('06_splines/02_seleccion_interna_candidatos.csv') else
      leer('07_ensembles/02_validaciones_internas.csv')
    g$familia <- if (familia == 'Splines') 'Splines' else g$modelo
    if (familia == 'Splines') g$config <- g$configuracion
    for (k in 1:4) for (f in unique(g$familia)) {
      d <- g[g$fold_externo == k & g$familia == f, , drop = FALSE]
      anadir(k, f, stats::aggregate(cbind(rmse, mae) ~ config, d, mean))
    }
  }
  sensibilidad <- do.call(rbind, registros)
  exigir(nrow(sensibilidad) == 28L, 'Grillas internas incompletas')
  guardar(sensibilidad, '06_sensibilidad_interna_RMSE_MAE.csv')

  cat('[13:3/5] Resumiendo distribuciones SOLO en entrenamiento del fold 4...\n')
  base <- leer('02_variables_candidatas/01_variables_candidatas_desarrollo.csv')
  y <- leer('01_revision_datos/06_etiquetas_solo_desarrollo.csv')
  tr_ids <- ids$order_id[ids$fold == 4 & ids$rol == 'train']
  tr <- base[match(tr_ids, base$order_id), , drop = FALSE]
  tr$dias_entrega <- y$dias_entrega[match(tr$order_id, y$order_id)]
  exigir(nrow(tr) == 46300L && !anyNA(tr$dias_entrega), 'Train 4 no coincide')
  num <- c('dias_entrega', 'horas_desde_compra', 'n_items', 'n_vendedores', 'precio_total_brl',
      'flete_total_brl', 'peso_total_g', 'volumen_total_cm3')
  distrib <- do.call(rbind, lapply(num, function(z) {
    x <- tr[[z]]; v <- x[!is.na(x)]
    data.frame(variable = z, n = length(v), faltantes = sum(is.na(x)), media = mean(v),
      mediana = stats::median(v), p90 = unname(stats::quantile(v, .9)),
      p99 = unname(stats::quantile(v, .99)), minimo = min(v), maximo = max(v))
  }))
  guardar(distrib, '07_distribuciones_numericas_train_fold4.csv')
  freq <- do.call(rbind, lapply(c('hora_aprobacion', 'dia_semana', 'mes_aprobacion', 'estado_cliente',
      'todos_vendedores_mismo_estado', 'n_items', 'n_vendedores'), function(z) {
    t <- table(tr[[z]], useNA = 'ifany')
    data.frame(variable = z, categoria = names(t), n = as.integer(t), porcentaje = 100 * as.numeric(t)/nrow(tr))
  }))
  guardar(freq, '08_frecuencias_train_fold4.csv')

  cat('[13:4/5] Generando figuras con ejes comparables y signo unico...\n')
  colores <- c('#267389', '#A8672B', '#6F5B96', '#3D8060')
  mes <- c('Dic 2017', 'Ene 2018', 'Feb 2018', 'Mar 2018')
  png <- function(nombre, filas = 2, columnas = 5) {
    grDevices::png(file.path(fig, nombre), width = if (columnas == 5) 3000 else 2600,
      height = 1500, res = 180, bg = 'white')
    par(mfrow = c(filas, columnas), mar = c(4, 4, 3, .8), oma = c(4, 1, 4, 0), family = 'sans')
  }
  pie <- function(titulo, nota) {
    mtext(titulo, outer = TRUE, side = 3, line = 1.5, cex = 1.25, font = 2)
    for (k in 1:4) mtext(mes[k], outer = TRUE, side = 1, line = .1,
      at = c(.17, .39, .61, .83)[k], col = colores[k], cex = .78, font = 2)
    mtext(nota, outer = TRUE, side = 1, line = 1.5, cex = .78)
    grDevices::dev.off()
  }
  pal <- grDevices::adjustcolor(colores, alpha.f = .10)
  lim <- ceiling(max(c(pred$dias_reales, pred$dias_predichos))/20) * 20
  lo <- min(0, floor(min(pred$dias_predichos)/5) * 5)
  for (zoom in c(FALSE, TRUE)) {
    limite <- if (zoom) 60 else lim
    png(if (zoom) '02_real_predicho_zoom_10_modelos.png' else '01_real_predicho_completo_10_modelos.png')
    for (i in seq_along(modelos)) {
      d <- pred[pred$modelo == modelos[i], , drop = FALSE]
      plot(d$dias_reales, d$dias_predichos, pch = 16, cex = .25, col = pal[d$fold],
        xlim = c(lo, limite), ylim = c(lo, limite), xlab = 'Real (dias)', ylab = 'Predicho (dias)',
        main = etiquetas[i], asp = 1)
      abline(a = 0, b = 1, lty = 2, col = '#B57426', lwd = 1.6)
    }
    fuera <- sum(ref$dias_reales > limite | ref$dias_predichos > limite | ref$dias_reales < lo | ref$dias_predichos < lo)
    pie('Olist | real frente a predicho | 26.279 pedidos por alternativa',
      paste(if (zoom) paste('Zoom:', fuera, 'pedidos por modelo fuera de ventana; metricas completas.') else
        'Rango completo. Diagonal: predicho = real.', '* Averaging retrospectivo. Desarrollo, no test nuevo.'))
  }
  png('03_residuos_validacion_10_modelos.png')
  er_lim <- range(c(0, pred$error))
  for (i in seq_along(modelos)) {
    d <- pred[pred$modelo == modelos[i], , drop = FALSE]
    plot(d$dias_predichos, d$error, pch = 16, cex = .25, col = pal[d$fold],
      xlim = range(c(0, pred$dias_predichos)), ylim = er_lim,
      xlab = 'Predicho (dias)', ylab = 'Predicho - real (dias)', main = etiquetas[i])
    abline(h = 0, lty = 2, col = '#B57426', lwd = 1.6)
  }
  pie('Olist | residuos externos de validacion | diez alternativas',
      'Todos los residuos. Negativo = subestimacion. Colores: cuatro folds. * Averaging retrospectivo.')
  png('04_residuos_por_fold_OLS_Splines.png', columnas = 4)
  for (m in c('B_pedido_geografia', 'Splines_B_anidado')) for (k in 1:4) {
    d <- pred[pred$modelo == m & pred$fold == k, , drop = FALSE]
    plot(d$dias_predichos, d$error, pch = 16, cex = .30,
      col = grDevices::adjustcolor(colores[k], alpha.f = .14),
      xlim = range(c(0, pred$dias_predichos)), ylim = er_lim,
      xlab = 'Predicho (dias)', ylab = 'Predicho - real (dias)',
      main = paste(etiquetas[match(m, modelos)], '|', mes[k]))
    abline(h = 0, lty = 2, col = '#B57426', lwd = 1.5)
  }
  pie('Olist | diagnostico por periodo de validacion', 'Ejes comunes; todos los pedidos de cada fold. Negativo = subestimacion.')
  png('05_calibracion_por_prediccion_10_modelos.png')
  cal_lim <- range(c(0, calibracion$media_predicha, calibracion$media_real))
  for (i in seq_along(modelos)) {
    plot(NA, xlim = cal_lim, ylim = cal_lim, xlab = 'Media predicha (dias)',
      ylab = 'Media real (dias)', main = etiquetas[i], asp = 1)
    abline(a = 0, b = 1, col = '#B57426', lty = 2)
    for (k in 1:4) {
      d <- calibracion[calibracion$modelo == modelos[i] & calibracion$fold == k, , drop = FALSE]
      lines(d$media_predicha, d$media_real, type = 'b', pch = 16, col = colores[k], cex = .65)
    }
  }
  pie('Olist | calibracion de la prediccion media por fold',
      'Grupos definidos por PREDICCION, no por duracion real. Colores: dic 2017, ene, feb y mar 2018. Sin intervalos inferenciales.')

  cat('[13:5/5] Certificando concordancia e integridad...\n')
  exigir(identical(huellas, tools::md5sum(historicos)), 'Se modifico un resultado historico')
  figuras_creadas <- list.files(fig, pattern='\\.png$', full.names=TRUE)
  exigir(length(figuras_creadas)==5L && all(file.info(figuras_creadas)$size>500),
    'Falta alguna figura de diagnostico o su archivo esta vacio')
  notas <- c('ETAPA 13: DIAGNOSTICO DESCRIPTIVO DE DESARROLLO',
    'RMSE, MAE, R2 y sesgo originales conservados. Cero entrenamientos nuevos.',
    '01: metricas por fold. 02: promedios simples entre folds. 03: calculos combinados APARTE.',
    'Mediana/P90 combinados no son promedios de medianas/P90 de folds.',
    'Tolerancias +/-3 y +/-5 dias ILUSTRATIVAS; requieren justificacion de negocio.',
    'Pearson/Spearman son asociaciones complementarias, no reemplazos del error. Baseline constante: NA por fold.',
    'Calibracion agrupada por prediccion; tramos de duracion REAL exclusivamente retrospectivos.',
    'Error = predicho - real; negativo = subestimacion. Todos los extremos se conservan.',
    'Sensibilidad RMSE/MAE exclusivamente INTERNA; no es validacion externa de modelos reoptimizados.',
    'Correlacion y forma de histogramas no eligen automaticamente la metrica.',
    'Periodos ya examinados: estos resultados NO constituyen un test independiente.')
  writeLines(notas, file.path(out, '09_INTERPRETACION_Y_LIMITACIONES.txt'), useBytes = TRUE)
  writeLines(c('ESTADO: APROBADO EN R', '10 alternativas; 26.279 pedidos comunes por alternativa; cuatro folds.',
    '40 metricas historicas (RMSE/MAE/R2/sesgo) cotejadas con tolerancia 1e-8.',
    paste('Comparaciones internas:', nrow(sensibilidad), '; cambios de configuracion:', sum(sensibilidad$cambia)),
    'Resultados historicos intactos (MD5). Test historico no consultado.',
    paste('Entorno:', R.version.string)), cert, useBytes = TRUE)
  cat('ETAPA 13 APROBADA: diagnostico completo, sin cambiar modelos ni metricas historicas.\n')
})
