# OLIST | ETAPA 14 | Interpretacion y experimento temporal de intervalos
# source('scripts/14_interpretacion_e_incertidumbre.R', encoding='UTF-8')
# Solo stats/splines/graphics. No modifica el candidato final ni reabre el test.
# Dos objetos DIFERENTES, identificados en cada salida:
# A) PDP/ICE: candidato FINAL Etapa 11, descripcion de su comportamiento.
# B) Importancia/intervalos: Splines del fold 2 reconstruido y verificado,
#    entrenado antes de enero, FIJO en calibracion enero y evaluacion marzo.
# Elecciones y periodos retrospectivos; ninguna garantia de cobertura futura.
local({
  set.seed(20261027L)
  raiz <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
  ruta <- function(...) file.path(raiz, ...)
  exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call. = FALSE)
  out <- ruta('resultados', '14_interpretacion_e_incertidumbre')
  fig <- ruta('graficos', '14_interpretacion_e_incertidumbre')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig, recursive = TRUE, showWarnings = FALSE)
  cert <- file.path(out, 'CONTROL_ETAPA14_APROBADO.txt')
  if (file.exists(cert)) unlink(cert)
  leer <- function(f) utils::read.csv(ruta('resultados', f), check.names = FALSE,
    stringsAsFactors = FALSE, na.strings = c('', 'NA'), fileEncoding = 'UTF-8-BOM')
  guardar <- function(d, f) utils::write.csv(d, file.path(out, f), row.names = FALSE, na = '')
  fecha <- function(x) as.POSIXct(x, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  historicos <- list.files(ruta('resultados'), full.names = TRUE, recursive = TRUE)
  historicos <- historicos[!grepl('/(13_diagnostico_complementario|14_interpretacion_e_incertidumbre)/', historicos)]
  huellas <- tools::md5sum(historicos)
  modelo_rds <- ruta('resultados', '11_modelo_candidato', 'MODELO_CANDIDATO_SPLINES_B.rds')
  exigir(file.exists(modelo_rds), 'Falta el candidato RDS de la Etapa 11')
  candidato <- readRDS(modelo_rds)
  exigir(is.function(candidato$predecir) && candidato$n_entrenamiento == 75172L,
    'Candidato final inesperado')
  vars <- candidato$predictores_originales
  b <- leer('02_variables_candidatas/01_variables_candidatas_desarrollo.csv')
  y <- leer('01_revision_datos/06_etiquetas_solo_desarrollo.csv')
  ids <- leer('03_protocolo_y_folds/04_ids_train_validacion.csv')
  maduras <- leer('02_variables_candidatas/03_etiquetas_maduras_45d_referencia.csv')
  cfg <- leer('06_splines/03_configuracion_seleccionada_por_fold.csv')
  pred6 <- leer('06_splines/05_predicciones_splines_por_pedido.csv')
  exigir(!anyDuplicated(b$order_id) && !anyDuplicated(y$order_id) &&
    all(vars %in% names(b)), 'IDs o variables B inconsistentes')
  b$y <- y$dias_entrega[match(b$order_id, y$order_id)]
  aprob <- fecha(b$fecha_aprobacion)
  entrega <- as.POSIXct(as.numeric(aprob) + 86400 * b$y, origin = '1970-01-01', tz = 'UTC')
  exigir(!anyNA(aprob) && all(aprob < fecha('2018-07-01 00:00:00')), 'Datos fuera de desarrollo')

  # La misma receta de la Etapa 11. No se ejecuta esa etapa ni se reescribe su RDS.
  receta <- function(d) {
    t <- table(d$estado_cliente); freq <- names(t)[t >= 100L]
    if (!length(freq)) freq <- names(t)[which.max(t)]
    agr <- ifelse(is.na(d$estado_cliente) | !(d$estado_cliente %in% freq), 'OTROS', d$estado_cliente)
    lev <- sort(unique(agr)); fallback <- if ('OTROS' %in% lev) 'OTROS' else names(t)[which.max(t)]
    list(freq = freq, lev = lev, fallback = fallback)
  }
  matrizB <- function(d, rec) {
    h <- as.numeric(d$hora_aprobacion); s <- as.numeric(d$dia_semana)
    m <- as.numeric(d$mes_aprobacion); hc <- as.numeric(d$horas_desde_compra)
    exigir(all(is.finite(h) & h >= 0 & h <= 23) && all(is.finite(s) & s >= 1 & s <= 7) &&
      all(is.finite(m) & m >= 1 & m <= 12) && all(is.finite(hc) & hc >= 0), 'Calendario invalido')
    x <- cbind(hora_sin = sin(2*pi*h/24), hora_cos = cos(2*pi*h/24),
      dia_sin = sin(2*pi*(s-1)/7), dia_cos = cos(2*pi*(s-1)/7),
      mes_sin = sin(2*pi*(m-1)/12), mes_cos = cos(2*pi*(m-1)/12), log_horas_compra = log1p(hc))
    g <- as.character(d$estado_cliente)
    grupo <- factor(ifelse(is.na(g) | !(g %in% rec$freq), rec$fallback, g), levels = rec$lev)
    exigir(!anyNA(grupo), 'Estado no reconocido por receta')
    if (length(rec$lev) > 1L) {
      D <- stats::model.matrix(~grupo)[, -1L, drop = FALSE]
      colnames(D) <- paste0('estado_', make.names(rec$lev[-1L])); x <- cbind(x, D)
    }
    for (z in c('n_items', 'n_vendedores', 'precio_total_brl', 'flete_total_brl', 'todos_vendedores_mismo_estado'))
      exigir(all(is.finite(d[[z]]) & d[[z]] >= 0), paste('Entrada invalida:', z))
    exigir(all(d$todos_vendedores_mismo_estado %in% c(0, 1)), 'Indicador geografico invalido')
    x <- cbind(x, log_items = log1p(d$n_items), log_vendedores = log1p(d$n_vendedores),
      log_precio = log1p(d$precio_total_brl), log_flete = log1p(d$flete_total_brl),
      vendedor_mismo_estado = d$todos_vendedores_mismo_estado)
    as.data.frame(x, check.names = FALSE)
  }
  metricas_punto <- function(real, p) c(rmse = sqrt(mean((p-real)^2)), mae = mean(abs(p-real)), sesgo = mean(p-real))
  cut_fit <- fecha('2018-01-01 00:00:00')
  cut_eval <- fecha('2018-03-01 00:00:00')
  tr_ids <- ids$order_id[ids$fold == 2 & ids$rol == 'train']
  tr <- match(tr_ids, b$order_id)
  exigir(length(tr) == 32995L && !anyNA(tr) && all(is.finite(b$y[tr])) &&
    all(aprob[tr] <= cut_fit - as.difftime(45, units = 'days')) && all(entrega[tr] < cut_fit),
    'Entrenamiento no respeta observabilidad y madurez originales')
  cat('[14:1/6] Reconstruyendo Splines fold 2 y verificando predicciones archivadas...\n')
  rec <- receta(b[tr, , drop = FALSE]); xtr <- matrizB(b[tr, , drop = FALSE], rec)
  c2 <- cfg[cfg$fold_externo == 2, , drop = FALSE]
  exigir(nrow(c2) == 1L, 'Configuracion fold 2 ambigua')
  texto <- paste0('dias ~ . - log_precio - log_flete + splines::ns(log_precio, df=',
    c2$df, ') + splines::ns(log_flete, df=', c2$df, ')')
  if (c2$interaccion_geo) texto <- paste0(texto, ' + vendedor_mismo_estado:log_flete')
  f <- stats::as.formula(texto); environment(f) <- baseenv()
  ajuste <- stats::lm(f, data = data.frame(dias = b$y[tr], xtr, check.names = FALSE),
    model = FALSE, x = FALSE, y = FALSE, na.action = stats::na.fail)
  exigir(all(is.finite(stats::coef(ajuste))) && ajuste$rank == length(stats::coef(ajuste)), 'Ajuste singular')
  predecir <- function(d) as.numeric(stats::predict(ajuste, newdata = matrizB(d, rec)))
  r2 <- pred6[pred6$fold == 2, , drop = FALSE]
  iv2 <- match(r2$order_id, b$order_id)
  diferencia <- max(abs(predecir(b[iv2, , drop = FALSE]) - r2$dias_predichos))
  exigir(!anyNA(iv2) && diferencia < 1e-7, 'La reconstruccion no reproduce Splines fold 2')

  # Enero: solo respuestas realmente conocidas antes del 1 de marzo.
  candidatos_cal <- which(aprob >= cut_fit & aprob < fecha('2018-02-01 00:00:00'))
  conocida <- is.finite(b$y[candidatos_cal]) & !is.na(entrega[candidatos_cal]) & entrega[candidatos_cal] < cut_eval
  cal <- candidatos_cal[conocida]
  ev_ids <- ids$order_id[ids$fold == 4 & ids$rol == 'validacion']
  ev <- match(ev_ids, b$order_id)
  exigir(length(cal) > 100L && length(ev) == 7083L && !anyNA(ev) &&
    !length(intersect(tr, cal)) && !length(intersect(tr, ev)) && !length(intersect(cal, ev)) &&
    all(aprob[ev] >= cut_eval & aprob[ev] < fecha('2018-04-01 00:00:00')) &&
    all(is.finite(b$y[ev])) && all(entrega[ev] < fecha('2018-07-01 00:00:00')), 'Particion temporal inconsistente')
  diseno <- data.frame(etapa = c('Entrenamiento fijo fold 2', 'Calibracion enero observable', 'Evaluacion marzo desarrollo'),
    n = c(length(tr), length(cal), length(ev)),
    aprobacion_min = vapply(list(tr, cal, ev), function(i) format(min(aprob[i]), tz = 'UTC'), character(1)),
    aprobacion_max = vapply(list(tr, cal, ev), function(i) format(max(aprob[i]), tz = 'UTC'), character(1)),
    corte_observabilidad_entrega = c('2018-01-01', '2018-03-01', '2018-07-01'),
    modelo = c2$configuracion)
  guardar(diseno, '01_diseno_temporal.csv')
  pcal <- predecir(b[cal, , drop = FALSE]); pev <- predecir(b[ev, , drop = FALSE])
  media_train <- mean(b$y[tr])
  # El registro permite recalcular los cuantiles independientemente, sin
  # publicar ni emplear las respuestas que aun no eran observables al corte.
  auditoria <- data.frame(order_id = b$order_id[candidatos_cal],
    fecha_aprobacion = b$fecha_aprobacion[candidatos_cal], elegible_calibracion = conocida,
    motivo = ifelse(conocida, 'Entrega conocida antes de marzo', 'Entrega no conocida antes de marzo; no usar respuesta'),
    duracion_real_conocida_al_corte = NA_real_, prediccion_splines = NA_real_,
    prediccion_baseline = NA_real_, score_absoluto_splines = NA_real_, score_absoluto_baseline = NA_real_)
  auditoria$duracion_real_conocida_al_corte[conocida] <- b$y[cal]
  auditoria$prediccion_splines[conocida] <- pcal
  auditoria$prediccion_baseline[conocida] <- media_train
  auditoria$score_absoluto_splines[conocida] <- abs(b$y[cal] - pcal)
  auditoria$score_absoluto_baseline[conocida] <- abs(b$y[cal] - media_train)
  guardar(auditoria, '02_auditoria_calibracion_enero.csv')
  puntos <- list(Splines_fijo_enero = list(cal = pcal, ev = pev),
    Baseline_media_train = list(cal = rep(media_train, length(cal)), ev = rep(media_train, length(ev))))
  exigir(all(is.finite(c(pcal, pev))), 'Predicciones no finitas')

  cat('[14:2/6] Calibrando intervalos 80/90% y evaluando cobertura en marzo...\n')
  # Regla de orden de split-conformal con score absoluto. Bajo dependencia
  # temporal, cambios de distribucion y seleccion de entregados NO se afirma
  # la garantia marginal que requeriria intercambiabilidad.
  objetivos <- c(.8, .9); calibrados <- list(); pred_pi <- list(); cob <- list()
  for (m in names(puntos)) for (nivel in objetivos) {
    a <- 1 - nivel; score <- abs(b$y[cal] - puntos[[m]]$cal)
    orden <- ceiling((length(score) + 1) * nivel)
    exigir(orden <= length(score), 'No hay calibracion suficiente para ese nivel')
    q <- sort(score)[orden]
    p <- puntos[[m]]$ev
    li <- pmax(0, p - q); ls <- pmax(li, p + q)
    calibrados[[length(calibrados) + 1L]] <- data.frame(modelo = m, nivel_objetivo = nivel,
      n_calibracion = length(score), orden_cuantil = orden, q_error_absoluto = q,
      fecha_limite_etiquetas_calibracion = '2018-03-01')
    d <- data.frame(modelo = m, nivel_objetivo = nivel, order_id = b$order_id[ev],
      fecha_aprobacion = b$fecha_aprobacion[ev], dias_reales = b$y[ev], dias_predichos = p,
      limite_inferior = li, limite_superior = ls, cubierto = b$y[ev] >= li & b$y[ev] <= ls,
      anchura = ls - li, todos_vendedores_mismo_estado = b$todos_vendedores_mismo_estado[ev])
    exigir(all(is.finite(as.matrix(d[, c('dias_reales', 'dias_predichos', 'limite_inferior', 'limite_superior')]))) &&
      all(d$limite_inferior >= 0 & d$limite_superior >= d$limite_inferior), 'Intervalos invalidos')
    pred_pi[[length(pred_pi) + 1L]] <- d
    grupos <- list(Todos = seq_len(nrow(d)),
      Geografia_distinta = which(d$todos_vendedores_mismo_estado == 0),
      Geografia_coincidente = which(d$todos_vendedores_mismo_estado == 1),
      Duracion_real_30_o_mas_RETROSPECTIVO = which(d$dias_reales >= 30))
    for (g in names(grupos)) {
      s <- d[grupos[[g]], , drop = FALSE]
      exigir(nrow(s) > 0, paste('Subgrupo sin observaciones:', g))
      winkler <- s$anchura + (2/a) * pmax(s$limite_inferior - s$dias_reales, 0) +
        (2/a) * pmax(s$dias_reales - s$limite_superior, 0)
      mp <- metricas_punto(s$dias_reales, s$dias_predichos)
      cob[[length(cob) + 1L]] <- data.frame(modelo = m, nivel_objetivo = nivel, subgrupo = g,
        n = nrow(s), n_cubiertos = sum(s$cubierto), cobertura_observada = mean(s$cubierto),
        anchura_media_dias = mean(s$anchura), anchura_mediana_dias = stats::median(s$anchura),
        interval_score_medio = mean(winkler), rmse_puntual = unname(mp['rmse']),
        mae_puntual = unname(mp['mae']), sesgo_puntual = unname(mp['sesgo']))
    }
  }
  calibrados <- do.call(rbind, calibrados); pred_pi <- do.call(rbind, pred_pi); cob <- do.call(rbind, cob)
  guardar(calibrados, '03_cuantiles_calibrados.csv')
  guardar(pred_pi, '04_intervalos_evaluacion_marzo_desarrollo.csv')
  guardar(cob, '05_cobertura_anchura_y_subgrupos.csv')

  cat('[14:3/6] Importancia por permutacion del modelo fijo en marzo...\n')
  # Permutacion conjunta de grupos para preservar pares internos relacionados.
  # Aun asi puede romper dependencias entre grupos; no mide causalidad.
  grupos <- list(Calendario = c('hora_aprobacion', 'dia_semana', 'mes_aprobacion'),
    Demora_aprobacion = 'horas_desde_compra',
    Geografia = c('estado_cliente', 'todos_vendedores_mismo_estado'),
    Composicion_pedido = c('n_items', 'n_vendedores'),
    Precio_y_flete = c('precio_total_brl', 'flete_total_brl'))
  evaluar <- b[ev, vars, drop = FALSE]; met0 <- metricas_punto(b$y[ev], pev)
  perm <- list()
  set.seed(20261027L)
  for (g in names(grupos)) for (j in 1:5) {
    d <- evaluar; ix <- sample.int(nrow(d)); d[, grupos[[g]]] <- evaluar[ix, grupos[[g]], drop = FALSE]
    mm <- metricas_punto(b$y[ev], predecir(d))
    perm[[length(perm) + 1L]] <- data.frame(grupo = g, repeticion = j,
      columnas = paste(grupos[[g]], collapse = ';'), n_evaluacion = nrow(d),
      delta_rmse = unname(mm['rmse'] - met0['rmse']), delta_mae = unname(mm['mae'] - met0['mae']))
  }
  perm <- do.call(rbind, perm)
  imp <- do.call(rbind, lapply(names(grupos), function(g) {
    d <- perm[perm$grupo == g, , drop = FALSE]
    data.frame(grupo = g, repeticiones = nrow(d), delta_rmse_medio = mean(d$delta_rmse),
      sd_montecarlo_delta_rmse = stats::sd(d$delta_rmse), delta_mae_medio = mean(d$delta_mae))
  }))
  imp <- imp[order(imp$delta_rmse_medio, decreasing = TRUE), , drop = FALSE]
  guardar(perm, '06_importancia_permutacion_repeticiones.csv')
  guardar(imp, '07_importancia_permutacion_grupos.csv')

  cat('[14:4/6] PDP e ICE descriptivos del candidato final conservado...\n')
  im <- match(maduras$order_id, b$order_id)
  exigir(length(im) == 75172L && !anyNA(im), 'IDs maduros del candidato no coinciden')
  set.seed(20261027L)
  ib <- sort(sample(im, min(1500L, length(im))))
  fondo <- b[ib, vars, drop = FALSE]
  ii <- seq_len(min(40L, nrow(fondo)))
  pdp <- list(); ice <- list()
  for (v in c('flete_total_brl', 'precio_total_brl')) {
    grid <- unique(as.numeric(stats::quantile(b[[v]][im], seq(.05, .95, length.out = 21), type = 7)))
    z <- vapply(grid, function(valor) {
      d <- fondo; d[[v]] <- valor; candidato$predecir(d)
    }, numeric(nrow(fondo)))
    exigir(all(is.finite(z)), 'PDP/ICE contiene predicciones no finitas')
    centro <- ceiling(length(grid)/2)
    pdp[[v]] <- data.frame(variable = v, valor = grid, prediccion_media_dias = colMeans(z),
      n_fondo = nrow(fondo), modelo = 'Candidato_final_Etapa11', configuracion = candidato$hiperparametros$configuracion)
    ice[[v]] <- do.call(rbind, lapply(ii, function(i) data.frame(variable = v,
      order_id = b$order_id[ib[i]], valor = grid, prediccion_dias = z[i, ],
      cambio_centrado_dias = z[i, ] - z[i, centro],
      todos_vendedores_mismo_estado = fondo$todos_vendedores_mismo_estado[i],
      modelo = 'Candidato_final_Etapa11')))
  }
  pdp <- do.call(rbind, pdp); ice <- do.call(rbind, ice)
  guardar(pdp, '08_PDP_candidato_final.csv'); guardar(ice, '09_ICE_candidato_final.csv')

  cat('[14:5/6] Graficando interpretacion, cobertura y amplitud...\n')
  colores <- c('#206C83', '#C07935')
  grDevices::png(file.path(fig, '01_cobertura_y_anchura_intervalos.png'), width = 2400, height = 1250, res = 180)
  par(mfrow = c(1, 2), mar = c(6, 5, 4, 2), oma = c(3, 0, 3, 0), family = 'sans')
  total <- cob[cob$subgrupo == 'Todos', , drop = FALSE]
  plot(NA, xlim = c(.77, .93), ylim = c(min(.7, total$cobertura_observada)-.04, 1.01),
    xlab = 'Cobertura objetivo', ylab = 'Cobertura observada en marzo', main = 'Cobertura en evaluacion posterior')
  abline(a = 0, b = 1, lty = 2, col = '#9B9B9B')
  for (i in seq_along(puntos)) {
    d <- total[total$modelo == names(puntos)[i], ]; d <- d[order(d$nivel_objetivo), ]
    lines(d$nivel_objetivo, d$cobertura_observada, type = 'b', pch = 15+i, col = colores[i], lwd = 2)
    text(d$nivel_objetivo, d$cobertura_observada, sprintf('%.1f%%', 100*d$cobertura_observada), pos = if (i == 1) 3 else 1, cex = .8)
  }
  legend('bottomright', c('Splines fijo enero', 'Media train fija'), col = colores, pch = 16:17, lwd = 2, bty = 'n', cex = .85)
  plot(NA, xlim = c(.77, .93), ylim = c(0, 1.15*max(total$anchura_media_dias)),
    xlab = 'Cobertura objetivo', ylab = 'Anchura media (dias)', main = 'Precision del rango ofrecido')
  for (i in seq_along(puntos)) {
    d <- total[total$modelo == names(puntos)[i], ]; d <- d[order(d$nivel_objetivo), ]
    lines(d$nivel_objetivo, d$anchura_media_dias, type = 'b', pch = 15+i, col = colores[i], lwd = 2)
    text(d$nivel_objetivo, d$anchura_media_dias, sprintf('%.1f d', d$anchura_media_dias), pos = if (i == 1) 1 else 3, cex = .8)
  }
  mtext('Intervalos | train antes de enero, calibracion enero, evaluacion marzo', outer = TRUE, side = 3, line = 1, font = 2)
  mtext('Experimento retrospectivo de desarrollo. Niveles 80/90% objetivos, sin garantia temporal de cobertura.', outer = TRUE, side = 1, line = 1, cex = .8)
  grDevices::dev.off()
  grDevices::png(file.path(fig, '02_ejemplos_intervalos_marzo.png'), width = 2300, height = 1350, res = 180)
  par(mar = c(6, 5, 5, 2), oma = c(2, 0, 1, 0), family = 'sans')
  d <- pred_pi[pred_pi$modelo == 'Splines_fijo_enero' & pred_pi$nivel_objetivo == .9, ]
  # Seleccion sistematica por ID, sin seleccionar errores favorables ni ordenar por y.
  d <- d[order(d$order_id), ]; ss <- unique(round(seq(1, nrow(d), length.out = 80)))
  d <- d[ss, ]; xx <- seq_len(nrow(d))
  plot(xx, d$dias_reales, type = 'n', xlab = 'Pedidos seleccionados por ID (sin ordenar por duracion)',
    ylab = 'Duracion (dias)', ylim = range(c(0, d$dias_reales, d$limite_superior)),
    main = 'Intervalos con objetivo 90% | muestra reproducible de marzo')
  segments(xx, d$limite_inferior, xx, d$limite_superior, col = '#AFCAD5', lwd = 2)
  points(xx, d$dias_predichos, pch = 16, col = colores[1], cex = .7)
  points(xx, d$dias_reales, pch = 4, col = ifelse(d$cubierto, '#3B515E', '#BD443B'), cex = .8)
  legend('topright', c('Prediccion puntual', 'Real cubierto', 'Real fuera del intervalo'),
    pch = c(16, 4, 4), col = c(colores[1], '#3B515E', '#BD443B'), bty = 'n', cex = .85)
  mtext('Cobertura numerica calculada con los 7.083 pedidos, no solo con esta muestra. Desarrollo ya examinado.', side = 1, line = 4.6, cex = .8)
  grDevices::dev.off()
  grDevices::png(file.path(fig, '03_importancia_grupos_modelo_fijo.png'), width = 2000, height = 1150, res = 170)
  par(mar = c(6, 12, 5, 2), family = 'sans', las = 1)
  yy <- nrow(imp):1
  plot(imp$delta_rmse_medio, yy, pch = 19, col = colores[1], cex = 1.3, yaxt = 'n',
    xlim = range(c(0, imp$delta_rmse_medio))*c(1, 1.25),
    ylim = c(.5, nrow(imp)+.5), ylab = '', xlab = 'Cambio medio de RMSE al permutar (dias)',
    main = 'Importancia por grupos | Splines fijo antes de enero')
  axis(2, at = yy, labels = gsub('_', ' ', imp$grupo), tick = FALSE)
  abline(v = 0, lty = 2, col = '#999999'); segments(0, yy, imp$delta_rmse_medio, yy, col = '#8DB6C5', lwd = 3)
  points(imp$delta_rmse_medio, yy, pch = 19, col = colores[1], cex = 1.3)
  mtext('Evaluacion marzo: 7.083 pedidos, 5 repeticiones. Asociacion predictiva; no causalidad.', side = 1, line = 4.2, cex = .8)
  grDevices::dev.off()
  for (v in unique(pdp$variable)) {
    d <- pdp[pdp$variable == v, ]; c <- ice[ice$variable == v, ]
    etiqueta <- if (v == 'flete_total_brl') 'Flete total (BRL)' else 'Precio total (BRL)'
    grDevices::png(file.path(fig, paste0('04_PDP_ICE_', v, '.png')), width = 2400, height = 1300, res = 180)
    par(mfrow = c(1, 2), mar = c(5, 5, 4, 2), oma = c(4, 0, 3, 0), family = 'sans')
    plot(d$valor, d$prediccion_media_dias, type = 'l', col = colores[1], lwd = 3,
      xlab = etiqueta, ylab = 'Prediccion media (dias)', main = 'PDP sobre 1.500 pedidos de fondo')
    rug(stats::quantile(b[[v]][im], seq(.05, .95, .05)), col = '#929DA6')
    plot(NA, xlim = range(c$valor), ylim = range(c$cambio_centrado_dias),
      xlab = etiqueta, ylab = 'Cambio frente al valor central (dias)', main = 'ICE centrado | 40 pedidos')
    for (id in unique(c$order_id)) {
      s <- c[c$order_id == id, ]
      lines(s$valor, s$cambio_centrado_dias, col = grDevices::adjustcolor(colores[1+s$todos_vendedores_mismo_estado[1]], alpha.f = .45))
    }
    abline(h = 0, lty = 2, col = '#999999')
    legend(if (v == 'precio_total_brl') 'bottomleft' else 'topleft',
      c('Algun vendedor en otro estado', 'Todos en mismo estado'), col = colores, lwd = 2, bty = 'n', cex = .8)
    mtext(paste('Comportamiento del candidato final Etapa 11 |', candidato$hiperparametros$configuracion), outer = TRUE, side = 3, line = 1, font = 2)
    mtext('Rango P5-P95 del entrenamiento. No causal. Cambiar una variable puede crear combinaciones poco frecuentes.', outer = TRUE, side = 1, line = 1.5, cex = .8)
    grDevices::dev.off()
  }

  cat('[14:6/6] Guardando limites del analisis y certificando integridad...\n')
  exigir(identical(huellas, tools::md5sum(historicos)), 'Se alteraron resultados historicos o el candidato final')
  figuras_creadas <- list.files(fig, pattern='\\.png$', full.names=TRUE)
  exigir(length(figuras_creadas)==5L && all(file.info(figuras_creadas)$size>500),
    'Falta alguna figura de interpretacion o su archivo esta vacio')
  control <- data.frame(control = c('n_train_fijo', 'n_calibracion_conocida', 'n_enero_no_observable_al_corte',
    'n_evaluacion_marzo', 'max_diferencia_vs_fold2', 'n_fondo_PDP', 'n_ICE'),
    valor = c(length(tr), length(cal), sum(!conocida), length(ev), diferencia, nrow(fondo), length(ii)))
  guardar(control, '10_controles_del_experimento.csv')
  notas <- c('ETAPA 14: ANALISIS EXPLORATORIO DE DESARROLLO, NO TEST NUEVO',
    'Intervalos/importancia: MISMO Splines del fold 2, verificado contra predicciones enero; NO reentrenar tras calibrar.',
    'Entrenamiento anterior al 01-01-2018; calibracion enero con entregas conocidas antes del 01-03-2018.',
    'Evaluacion marzo entre pedidos entregados observables al corte global 01-07-2018.',
    'Familia, periodos y analisis elegidos despues de examinar desarrollo: resultado retrospectivo.',
    'Scores absolutos y cuantil de orden ceil((n+1)*nivel); limite inferior intersectado con duraciones >=0.',
    '80%/90% son objetivos de diseno, no probabilidades garantizadas para cada pedido.',
    'Dependencia temporal, drift y exclusion de entregas no observables impiden afirmar garantia conformal estandar.',
    'Informar cobertura Y anchura Y interval score; buena cobertura con intervalo ancho puede ser poco util.',
    'Importancia por permutacion de cinco grupos en marzo; SD entre 5 repeticiones es SOLO variacion Monte Carlo.',
    'Los grupos preservan pares internos, pero permutar puede romper dependencias entre grupos. No implica causalidad.',
    'PDP/ICE: OTRO objeto, candidato FINAL Etapa 11; descripcion de su funcion, no evaluacion fuera de muestra.',
    'Fondo 1.500 pedidos maduros, ICE 40, rango P5-P95, semilla fija; variables correlacionadas pueden producir combinaciones irreales.',
    'No se cambiaron las diez entradas B, los ajustes originales, sus predicciones ni su ranking.',
    'El candidato FINAL Etapa 11 no hereda automaticamente estos intervalos del modelo fijo de enero.',
    'Julio-agosto 2018 no se leyo ni se utilizo. Evaluacion final independiente aun pendiente.',
    'Fuentes metodologicas: https://scikit-learn.org/stable/modules/partial_dependence.html',
    'https://mapie.readthedocs.io/en/stable/content/exchangeability-testing/')
  writeLines(notas, file.path(out, '11_METODOLOGIA_Y_LIMITACIONES.txt'), useBytes = TRUE)
  writeLines(c('ESTADO: APROBADO EN R', paste('Reconstruccion fold 2, diferencia maxima:', format(diferencia, digits = 16)),
    paste('Train:', length(tr), '| calibracion conocida:', length(cal), '| evaluacion marzo:', length(ev)),
    'Conjuntos disjuntos; observabilidad temporal comprobada; candidato final y resultados historicos intactos.',
    'Cero consultas del test historico. No se afirma cobertura futura ni evaluacion final independiente.',
    paste('Entorno:', R.version.string)), cert, useBytes = TRUE)
  print(total[, c('modelo', 'nivel_objetivo', 'n', 'cobertura_observada', 'anchura_media_dias')], row.names = FALSE)
  cat('ETAPA 14 APROBADA: interpretacion e intervalos de desarrollo, sin alterar la comparacion original.\n')
})
