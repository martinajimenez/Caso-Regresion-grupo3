# =============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3 | OLIST
# ETAPA 15 | EVALUACION MAS ALLA DEL R2 Y DIAGNOSTICO DE SUPUESTOS
# Responde a la orientacion docente con las herramientas vistas en el curso:
#   (1) Por que el R2 no basta: grafico real vs predicho, correlacion de
#       Spearman (cor, method = 'spearman'), error por tramo de prediccion y
#       diferencias pareadas por fold frente al baseline.
#   (2) Supuestos de la regresion con la receta del curso (supuestos1.R):
#       lm(), plot(modelo, which = 1:4), shapiro.test(), lmtest::bptest(),
#       lmtest::dwtest() y car::vif().
# No reentrena los modelos de comparacion: lee las predicciones archivadas de
# validacion (etapas 04-07). Para los supuestos reajusta con lm() el OLS B y el
# Splines B en el train del fold 4 (marzo 2018) con la misma receta train-only
# y comprueba que reproduce los coeficientes y predicciones archivados.
# No consulta el test historico julio-agosto 2018.
# Requiere: stats, splines, lmtest, car.
# Desde la raiz: source('scripts/15_evaluacion_no_lineal_y_supuestos.R', encoding='UTF-8')
# =============================================================================
local({
  # ---------------------------------------------------------------------------
  # 0. Configuracion (parametros de la etapa en un solo lugar)
  # ---------------------------------------------------------------------------
  config <- list(
    semilla = 20261104L,
    n_max_shapiro = 5000L,          # limite de stats::shapiro.test
    alfa = 0.05,
    fold_diagnostico = 4L,          # train mas grande (validacion marzo 2018)
    umbral_frecuencia_estado = 100L,
    n_tramos_calibracion = 20L,
    n_puntos_grafico = 6000L,
    tolerancia_reconstruccion = 1e-7,
    modelos = c('Baseline_media_train', 'B_pedido_geografia', 'Ridge_min',
                'Lasso_min', 'ElasticNet_min', 'Splines_B_anidado',
                'Arbol_B_anidado', 'RandomForest_B_anidado',
                'Boosting_B_anidado', 'Averaging_Splines_Boosting'),
    etiquetas = c('Baseline', 'OLS B', 'Ridge', 'Lasso', 'Elastic net',
                  'Splines', 'Arbol', 'Random Forest', 'Boosting',
                  'Averaging 50/50'),
    modelos_grafico = c('B_pedido_geografia', 'Splines_B_anidado'),
    n_validacion_esperado = 26279L
  )
  for (p in c('splines', 'lmtest', 'car')) {
    if (!requireNamespace(p, quietly = TRUE))
      stop(paste0('ETAPA 15: falta el paquete ', p, '; instalar con install.packages("', p, '").'),
           call. = FALSE)
  }
  set.seed(config$semilla)

  raiz <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
  ruta <- function(...) file.path(raiz, ...)
  exigir <- function(ok, msg) if (!isTRUE(ok)) stop(paste('ETAPA 15:', msg), call. = FALSE)
  out <- ruta('resultados', '15_evaluacion_no_lineal_y_supuestos')
  fig <- ruta('graficos', '15_evaluacion_no_lineal_y_supuestos')
  exigir(dir.exists(ruta('scripts')) && dir.exists(ruta('resultados')),
         'Ejecutar desde la raiz del proyecto (carpeta con scripts/ y resultados/).')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig, recursive = TRUE, showWarnings = FALSE)
  cert <- file.path(out, 'CONTROL_ETAPA15_APROBADO.txt')
  if (file.exists(cert)) unlink(cert)
  leer <- function(f) utils::read.csv(ruta('resultados', f), check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c('', 'NA'))
  guardar <- function(d, f) utils::write.csv(d, file.path(out, f), row.names = FALSE, na = '')

  # Huella de todo lo anterior: esta etapa no debe modificar ninguna salida previa.
  previos <- list.files(ruta('resultados'), full.names = TRUE, recursive = TRUE)
  previos <- previos[!grepl('/(15_evaluacion_no_lineal_y_supuestos|16_sensibilidad_log_respuesta)/', previos)]
  huellas_antes <- tools::md5sum(previos)

  # ---------------------------------------------------------------------------
  # 1. Predicciones de validacion archivadas de las diez alternativas
  # ---------------------------------------------------------------------------
  cat('[15:1/6] Cargando predicciones de validacion archivadas...\n')
  archivos <- c('04_baseline_y_ols/03_predicciones_validacion.csv',
                '05_regularizacion/05_predicciones_validacion_regularizados.csv',
                '06_splines/05_predicciones_splines_por_pedido.csv',
                '07_ensembles/05_predicciones_validacion.csv',
                '07_ensembles/12_averaging_predicciones.csv')
  exigir(all(file.exists(ruta('resultados', archivos))), 'Faltan predicciones archivadas de las etapas 04-07.')
  cols <- c('fold', 'order_id', 'modelo', 'dias_reales', 'dias_predichos')
  pred <- do.call(rbind, lapply(archivos, function(f) {
    d <- leer(f); exigir(all(cols %in% names(d)), paste('Columnas ausentes en', f)); d[, cols]
  }))
  pred <- pred[pred$modelo %in% config$modelos, , drop = FALSE]
  exigir(nrow(pred) == length(config$modelos) * config$n_validacion_esperado &&
           !anyNA(pred) && !anyDuplicated(pred[c('modelo', 'fold', 'order_id')]) &&
           all(is.finite(pred$dias_reales)) && all(is.finite(pred$dias_predichos)),
         'Predicciones faltantes, duplicadas o no finitas.')
  clave <- function(d) paste(d$fold, d$order_id, sep = '|')
  ref <- pred[pred$modelo == config$modelos[1], , drop = FALSE]
  for (m in config$modelos) {
    d <- pred[pred$modelo == m, , drop = FALSE]; ix <- match(clave(d), clave(ref))
    exigir(!anyNA(ix) && max(abs(d$dias_reales - ref$dias_reales[ix])) < 1e-10,
           paste('Modelos no comparables sobre los mismos pedidos:', m))
  }

  # ---------------------------------------------------------------------------
  # 2. Metricas por fold: R2 frente a Spearman y errores en dias
  # ---------------------------------------------------------------------------
  cat('[15:2/6] R2, Spearman, RMSE, MAE y recta de calibracion por fold...\n')
  metricas <- do.call(rbind, lapply(config$modelos, function(m) {
    do.call(rbind, lapply(sort(unique(pred$fold)), function(k) {
      d <- pred[pred$modelo == m & pred$fold == k, , drop = FALSE]
      y <- d$dias_reales; p <- d$dias_predichos; e <- y - p
      constante <- diff(range(p)) < 1e-12
      # Recta real = a + b * predicho (ideal a = 0, b = 1); se ajusta con lm().
      cal <- if (constante) c(NA_real_, NA_real_) else unname(stats::coef(stats::lm(y ~ p)))
      data.frame(modelo = m, fold = k, n = nrow(d),
        r2_fuera_muestra = 1 - sum(e^2) / sum((y - mean(y))^2),
        spearman = if (constante) NA_real_ else stats::cor(y, p, method = 'spearman'),
        rmse = sqrt(mean(e^2)), mae = mean(abs(e)),
        mediana_error_absoluto = stats::median(abs(e)),
        calibracion_intercepto = cal[1], calibracion_pendiente = cal[2],
        stringsAsFactors = FALSE)
    }))
  }))
  media_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  num <- setdiff(names(metricas), c('modelo', 'fold', 'n'))
  resumen <- do.call(rbind, lapply(config$modelos, function(m) {
    d <- metricas[metricas$modelo == m, , drop = FALSE]
    data.frame(modelo = m, etiqueta = config$etiquetas[match(m, config$modelos)],
               folds = nrow(d), n_validacion = sum(d$n),
               as.list(vapply(d[, num], media_na, numeric(1))),
               sd_spearman_folds = stats::sd(d$spearman),
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  resumen <- resumen[order(resumen$rmse), , drop = FALSE]; rownames(resumen) <- NULL

  # Diferencias pareadas por fold frente al baseline (mismos pedidos).
  # SE = sd / sqrt(4): descriptivo, los folds temporales no son independientes.
  base <- metricas[metricas$modelo == 'Baseline_media_train', , drop = FALSE]
  pareadas <- do.call(rbind, lapply(setdiff(config$modelos, 'Baseline_media_train'), function(m) {
    d <- metricas[metricas$modelo == m, , drop = FALSE]
    d <- d[match(base$fold, d$fold), , drop = FALSE]
    dr <- d$rmse - base$rmse; dm <- d$mae - base$mae
    data.frame(modelo = m, etiqueta = config$etiquetas[match(m, config$modelos)],
               delta_rmse_medio = mean(dr), se_delta_rmse = stats::sd(dr) / sqrt(length(dr)),
               delta_mae_medio = mean(dm), se_delta_mae = stats::sd(dm) / sqrt(length(dm)),
               folds_mejor_que_baseline_rmse = sum(dr < 0),
               mejora_rmse_pct = 100 * mean(-dr / base$rmse),
               stringsAsFactors = FALSE)
  }))
  pareadas <- pareadas[order(pareadas$delta_rmse_medio), , drop = FALSE]; rownames(pareadas) <- NULL

  # Error por tramo de prediccion (calibracion), folds combinados.
  calibracion <- do.call(rbind, lapply(config$modelos_grafico, function(m) {
    d <- pred[pred$modelo == m, , drop = FALSE]
    cortes <- unique(stats::quantile(d$dias_predichos,
                     probs = seq(0, 1, length.out = config$n_tramos_calibracion + 1L)))
    tramo <- cut(d$dias_predichos, cortes, include.lowest = TRUE, labels = FALSE)
    do.call(rbind, lapply(sort(unique(tramo)), function(t) {
      z <- d[tramo == t, , drop = FALSE]
      data.frame(modelo = m, tramo = t, n = nrow(z),
                 media_predicha = mean(z$dias_predichos), media_real = mean(z$dias_reales),
                 p10_real = unname(stats::quantile(z$dias_reales, .10)),
                 p90_real = unname(stats::quantile(z$dias_reales, .90)),
                 mae_tramo = mean(abs(z$dias_reales - z$dias_predichos)),
                 stringsAsFactors = FALSE)
    }))
  }))

  # ---------------------------------------------------------------------------
  # 3. Supuestos con la receta del curso: lm() en el train del fold 4
  # ---------------------------------------------------------------------------
  cat('[15:3/6] Reajustando OLS B y Splines B con lm() en el train del fold', config$fold_diagnostico, '...\n')
  b <- leer('02_variables_candidatas/01_variables_candidatas_desarrollo.csv')
  et <- leer('01_revision_datos/06_etiquetas_solo_desarrollo.csv')
  ids <- leer('03_protocolo_y_folds/04_ids_train_validacion.csv')
  coef04 <- leer('04_baseline_y_ols/04_coeficientes_ols_por_fold.csv')
  cfg6 <- leer('06_splines/03_configuracion_seleccionada_por_fold.csv')
  exigir(!anyDuplicated(b$order_id) && !anyDuplicated(et$order_id), 'IDs duplicados en desarrollo.')
  y_todo <- et$dias_entrega[match(b$order_id, et$order_id)]
  k <- config$fold_diagnostico
  tr_ids <- ids$order_id[ids$fold == k & ids$rol == 'train']
  va_ids <- ids$order_id[ids$fold == k & ids$rol == 'validacion']
  tr <- match(tr_ids, b$order_id); va <- match(va_ids, b$order_id)
  exigir(!anyNA(tr) && !anyNA(va) && !length(intersect(tr, va)) && all(is.finite(y_todo[c(tr, va)])),
         'Particion del fold de diagnostico invalida.')
  # Orden cronologico: dwtest evalua autocorrelacion en el orden de aprobacion.
  fecha_tr <- as.POSIXct(b$fecha_aprobacion[tr], format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  exigir(!anyNA(fecha_tr), 'Fechas de aprobacion invalidas en train.')
  tr <- tr[order(fecha_tr, b$order_id[tr])]

  receta <- function(d) {
    tab <- table(d$estado_cliente)
    freq <- names(tab)[tab >= config$umbral_frecuencia_estado]
    if (!length(freq)) freq <- names(tab)[which.max(tab)]
    agr <- ifelse(is.na(d$estado_cliente) | !(d$estado_cliente %in% freq), 'OTROS', d$estado_cliente)
    lev <- sort(unique(agr))
    list(freq = freq, lev = lev, fallback = if ('OTROS' %in% lev) 'OTROS' else names(tab)[which.max(tab)])
  }
  # Receta B identica a las etapas 04-11 (aprendida solo con el train).
  matrizB <- function(d, rec) {
    h <- as.numeric(d$hora_aprobacion); s <- as.numeric(d$dia_semana)
    m <- as.numeric(d$mes_aprobacion); hc <- as.numeric(d$horas_desde_compra)
    exigir(all(is.finite(h) & h >= 0 & h <= 23) && all(is.finite(s) & s >= 1 & s <= 7) &&
             all(is.finite(m) & m >= 1 & m <= 12) && all(is.finite(hc) & hc >= 0),
           'Variables temporales invalidas.')
    x <- cbind(hora_sin = sin(2 * pi * h / 24), hora_cos = cos(2 * pi * h / 24),
               dia_sin = sin(2 * pi * (s - 1) / 7), dia_cos = cos(2 * pi * (s - 1) / 7),
               mes_sin = sin(2 * pi * (m - 1) / 12), mes_cos = cos(2 * pi * (m - 1) / 12),
               log_horas_compra = log1p(hc))
    g0 <- as.character(d$estado_cliente)
    grupo <- factor(ifelse(is.na(g0) | !(g0 %in% rec$freq), rec$fallback, g0), levels = rec$lev)
    exigir(!anyNA(grupo), 'Estado de cliente sin nivel conocido.')
    if (length(rec$lev) > 1L) {
      D <- stats::model.matrix(~grupo)[, -1L, drop = FALSE]
      colnames(D) <- paste0('estado_', make.names(rec$lev[-1L])); x <- cbind(x, D)
    }
    for (z in c('n_items', 'n_vendedores', 'precio_total_brl', 'flete_total_brl',
                'todos_vendedores_mismo_estado')) {
      v <- as.numeric(d[[z]]); exigir(all(is.finite(v) & v >= 0), paste('Valor invalido:', z))
    }
    x <- cbind(x, log_items = log1p(as.numeric(d$n_items)),
               log_vendedores = log1p(as.numeric(d$n_vendedores)),
               log_precio = log1p(as.numeric(d$precio_total_brl)),
               log_flete = log1p(as.numeric(d$flete_total_brl)),
               vendedor_mismo_estado = as.numeric(d$todos_vendedores_mismo_estado))
    storage.mode(x) <- 'double'
    as.data.frame(x, check.names = FALSE)
  }
  rec <- receta(b[tr, , drop = FALSE])
  xtr <- matrizB(b[tr, , drop = FALSE], rec); xva <- matrizB(b[va, , drop = FALSE], rec)
  datos_tr <- data.frame(dias = y_todo[tr], xtr, check.names = FALSE)
  cg <- cfg6[cfg6$fold_externo == k, , drop = FALSE]
  exigir(nrow(cg) == 1L, 'Configuracion Splines del fold de diagnostico no encontrada.')
  f_spl <- paste0('dias ~ . - log_precio - log_flete + splines::ns(log_precio, df=', cg$df,
                  ') + splines::ns(log_flete, df=', cg$df, ')')
  if (as.logical(cg$interaccion_geo)) f_spl <- paste0(f_spl, ' + vendedor_mismo_estado:log_flete')
  modelos_lm <- list(
    B_pedido_geografia = stats::lm(dias ~ ., data = datos_tr),
    Splines_B_anidado = stats::lm(stats::as.formula(f_spl), data = datos_tr))

  # Control anti-error: lm() debe reproducir lo archivado en las etapas 04 y 06.
  c04 <- coef04[coef04$fold == k & coef04$modelo == 'B_pedido_geografia', , drop = FALSE]
  b_lm <- stats::coef(modelos_lm$B_pedido_geografia)
  exigir(nrow(c04) == length(b_lm) &&
           max(abs(c04$coeficiente[match(gsub('`', '', names(b_lm)), c04$termino)] - unname(b_lm))) < 1e-6,
         'lm() no reproduce los coeficientes OLS B archivados en la etapa 04.')
  dif_reconstruccion <- c(B_pedido_geografia = NA_real_, Splines_B_anidado = NA_real_)
  for (m in names(modelos_lm)) {
    ar <- pred[pred$modelo == m & pred$fold == k, , drop = FALSE]
    p_lm <- as.numeric(stats::predict(modelos_lm[[m]], newdata = xva))
    pos <- match(b$order_id[va], ar$order_id)
    exigir(!anyNA(pos), paste('Pedidos de validacion no archivados para', m))
    dif_reconstruccion[[m]] <- max(abs(ar$dias_predichos[pos] - p_lm))
    exigir(dif_reconstruccion[[m]] < config$tolerancia_reconstruccion,
           paste('lm() no reproduce las predicciones archivadas de', m))
  }

  cat('[15:4/6] shapiro.test, bptest, dwtest y vif (receta del curso)...\n')
  supuestos <- do.call(rbind, lapply(names(modelos_lm), function(m) {
    mod <- modelos_lm[[m]]; r <- stats::residuals(mod); n <- length(r)
    # shapiro.test acepta como maximo 5000 residuos: submuestra fija con semilla.
    set.seed(config$semilla + match(m, names(modelos_lm)))
    rs <- if (n > config$n_max_shapiro) r[sort(sample.int(n, config$n_max_shapiro))] else r
    sw <- stats::shapiro.test(rs)
    bp <- lmtest::bptest(mod)
    dw <- lmtest::dwtest(mod)
    hv <- stats::hatvalues(mod); p <- length(stats::coef(mod))
    data.frame(modelo = m, etiqueta = config$etiquetas[match(m, config$modelos)],
               fold_train = k, n_train = n, n_coeficientes = p,
               shapiro_n_submuestra = length(rs), shapiro_W = unname(sw$statistic),
               shapiro_p_valor = sw$p.value,
               bptest_BP = unname(bp$statistic), bptest_gl = unname(bp$parameter),
               bptest_p_valor = unname(bp$p.value),
               dwtest_DW = unname(dw$statistic), dwtest_p_valor = unname(dw$p.value),
               leverage_max = max(hv), n_leverage_sobre_2p_n = sum(hv > 2 * p / n),
               normalidad_rechazada_5pct = sw$p.value < config$alfa,
               homocedasticidad_rechazada_5pct = unname(bp$p.value) < config$alfa,
               stringsAsFactors = FALSE)
  }))
  # car::vif con la variable categorica como FACTOR (no como dummies sueltas):
  # asi entrega GVIF por variable. Para comparar con la regla del curso
  # (<5 bien, 5-10 moderado, >10 grave) se usa (GVIF^(1/(2*Df)))^2, que en
  # variables de 1 grado de libertad coincide con el VIF clasico.
  g0 <- as.character(b$estado_cliente[tr])
  estado <- factor(ifelse(is.na(g0) | !(g0 %in% rec$freq), rec$fallback, g0), levels = rec$lev)
  datos_vif <- data.frame(datos_tr[, !startsWith(names(datos_tr), 'estado_'), drop = FALSE],
                          estado_cliente = estado, check.names = FALSE)
  mod_vif <- stats::lm(dias ~ ., data = datos_vif)
  exigir(max(abs(stats::fitted(mod_vif) - stats::fitted(modelos_lm$B_pedido_geografia))) < 1e-6,
         'El modelo con estado como factor no coincide con OLS B.')
  gv <- car::vif(mod_vif)
  if (is.null(dim(gv))) gv <- cbind(GVIF = gv, Df = 1, `GVIF^(1/(2*Df))` = sqrt(gv))
  vif_comp <- gv[, 3]^2
  tabla_vif <- data.frame(variable = rownames(gv), GVIF = gv[, 1], gl = gv[, 2],
                          VIF_comparable = vif_comp,
                          lectura_curso = ifelse(vif_comp < 5, 'bien (<5)',
                                           ifelse(vif_comp <= 10, 'moderado (5-10)', 'grave (>10)')),
                          stringsAsFactors = FALSE, row.names = NULL)
  tabla_vif <- tabla_vif[order(-tabla_vif$VIF_comparable), , drop = FALSE]; rownames(tabla_vif) <- NULL

  # ---------------------------------------------------------------------------
  # 4. Tablas
  # ---------------------------------------------------------------------------
  cat('[15:5/6] Guardando tablas y figuras...\n')
  guardar(metricas, '01_r2_spearman_y_errores_por_fold.csv')
  guardar(resumen, '02_r2_spearman_y_errores_resumen.csv')
  guardar(pareadas, '03_diferencias_pareadas_vs_baseline.csv')
  guardar(calibracion, '04_error_por_tramo_de_prediccion.csv')
  guardar(supuestos, '05_supuestos_lm_shapiro_bptest_dwtest.csv')
  guardar(tabla_vif, '06_vif_ols_b.csv')
  guardar(data.frame(parametro = names(config),
                     valor = vapply(config, function(v) paste(v, collapse = '; '), character(1)),
                     stringsAsFactors = FALSE), '07_configuracion_etapa15.csv')

  # ---------------------------------------------------------------------------
  # 5. Figuras (R base)
  # ---------------------------------------------------------------------------
  azul <- '#16334F'; verde <- '#147F77'; gris <- '#61758B'; naranjo <- '#D5783A'
  png_seguro <- function(nombre, fun, w = 1900, h = 1000) {
    f <- file.path(fig, nombre)
    grDevices::png(f, width = w, height = h, res = 170, bg = 'white')
    tryCatch(fun(), finally = grDevices::dev.off())
    exigir(file.exists(f) && file.info(f)$size > 2000, paste('No se genero', nombre))
  }
  etq <- function(m) config$etiquetas[match(m, config$modelos)]
  lim_dias <- c(0, unname(stats::quantile(ref$dias_reales, .99)))

  png_seguro('01_real_vs_predicho_y_error_por_tramo.png', function() {
    op <- graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4.5, 1.5), family = 'sans')
    on.exit(graphics::par(op))
    for (m in config$modelos_grafico) {
      d <- pred[pred$modelo == m, , drop = FALSE]
      set.seed(config$semilla)
      ii <- sort(sample.int(nrow(d), min(nrow(d), config$n_puntos_grafico)))
      graphics::plot(d$dias_predichos[ii], d$dias_reales[ii], pch = 16, cex = .35,
        col = grDevices::adjustcolor(gris, alpha.f = .25), xlim = lim_dias, ylim = lim_dias,
        xlab = 'Dias predichos', ylab = 'Dias reales',
        main = paste(etq(m), '| validacion, 4 folds'), cex.main = 1.05)
      graphics::abline(0, 1, lty = 2, lwd = 2, col = azul)
      cz <- calibracion[calibracion$modelo == m, , drop = FALSE]
      graphics::segments(cz$media_predicha, cz$p10_real, cz$media_predicha, cz$p90_real,
                         col = grDevices::adjustcolor(naranjo, alpha.f = .6), lwd = 2)
      graphics::lines(cz$media_predicha, cz$media_real, col = verde, lwd = 3)
      graphics::points(cz$media_predicha, cz$media_real, pch = 19, col = verde, cex = .9)
      r <- resumen[resumen$modelo == m, , drop = FALSE]
      graphics::legend('topleft', bty = 'n', cex = .78,
        legend = c('Recta ideal real = predicho', 'Media real por tramo (20 tramos)',
                   'P10-P90 real por tramo',
                   sprintf('R2 = %.3f | Spearman = %.3f', r$r2_fuera_muestra, r$spearman)),
        col = c(azul, verde, naranjo, NA), lty = c(2, 1, 1, NA), lwd = c(2, 3, 2, NA))
    }
    graphics::mtext('Eje recortado al P99 de la respuesta; puntos: submuestra determinista',
                    side = 1, line = -1.2, outer = TRUE, cex = .75, col = gris)
  })

  png_seguro('02_r2_vs_spearman_por_modelo.png', function() {
    d <- resumen[!is.na(resumen$spearman), , drop = FALSE]
    d <- d[order(d$spearman), , drop = FALSE]
    graphics::par(mar = c(5, 10, 4.5, 2), family = 'sans')
    yy <- seq_len(nrow(d))
    graphics::plot(NA, xlim = c(0, 0.7), ylim = c(.5, nrow(d) + .5), yaxt = 'n',
      xlab = 'Valor de la medida (0 a 1)', ylab = '',
      main = 'El R2 subestima cuanto ordena el modelo los pedidos', cex.main = 1.1)
    graphics::axis(2, at = yy, labels = d$etiqueta, las = 1, tick = FALSE, cex.axis = .9)
    graphics::abline(v = seq(0, 0.7, .1), col = '#EBF0F4')
    graphics::segments(d$r2_fuera_muestra, yy, d$spearman, yy, col = '#CAD4DB', lwd = 2)
    graphics::points(d$r2_fuera_muestra, yy, pch = 15, col = gris, cex = 1.4)
    graphics::points(d$spearman, yy, pch = 17, col = verde, cex = 1.4)
    graphics::legend(0.24, nrow(d) * 0.45, bty = 'n', cex = .85, pch = c(15, 17),
      col = c(gris, verde), legend = c('R2 fuera de muestra', 'Spearman real vs predicho'))
    graphics::mtext('Promedio de 4 folds temporales; Baseline constante excluido',
                    side = 3, line = .4, cex = .78, col = gris)
  })

  # Diagnostico del curso: plot(modelo, which = 1:4).
  for (m in names(modelos_lm)) {
    nombre <- if (m == 'B_pedido_geografia') '03_diagnostico_lm_ols_b.png' else '04_diagnostico_lm_splines_b.png'
    png_seguro(nombre, function() {
      op <- graphics::par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), family = 'sans',
                          oma = c(0, 0, 2.2, 0))
      on.exit(graphics::par(op))
      for (w in 1:4) plot(modelos_lm[[m]], which = w, pch = 16, cex = .3, sub.caption = '',
                          col = grDevices::adjustcolor(gris, alpha.f = .35))
      graphics::mtext(paste0('plot(modelo, which = 1:4) | ', etq(m), ' | train fold ', k,
                             ' (n = ', nrow(datos_tr), ')'), outer = TRUE, cex = 1.05, font = 2)
    }, w = 1900, h = 1500)
  }

  # ---------------------------------------------------------------------------
  # 6. Interpretacion, controles e integridad
  # ---------------------------------------------------------------------------
  cat('[15:6/6] Redactando interpretacion y verificando integridad...\n')
  fmt_p <- function(p) if (is.na(p)) 'NA' else if (p < 2.2e-16) '< 2.2e-16' else sprintf('= %.2g', p)
  sp <- resumen[resumen$modelo == 'Splines_B_anidado', , drop = FALSE]
  ols <- resumen[resumen$modelo == 'B_pedido_geografia', , drop = FALSE]
  pr_sp <- pareadas[pareadas$modelo == 'Splines_B_anidado', , drop = FALSE]
  s_ols <- supuestos[supuestos$modelo == 'B_pedido_geografia', , drop = FALSE]
  s_sp <- supuestos[supuestos$modelo == 'Splines_B_anidado', , drop = FALSE]
  y_val <- ref$dias_reales
  texto <- c(
    'ETAPA 15 - EVALUACION MAS ALLA DEL R2 Y SUPUESTOS (RESPUESTA A LA ORIENTACION DOCENTE)',
    '',
    'MENSAJE PRINCIPAL: el modelo no esta mal; evaluarlo solo con R2 no corresponde.',
    '',
    '1. Por que el R2 no basta',
    sprintf('- La respuesta es asimetrica: mediana %.1f dias, media %.1f, P99 %.1f, maximo %.0f.',
            stats::median(y_val), mean(y_val), unname(stats::quantile(y_val, .99)), max(y_val)),
    '- El R2 mide la varianza explicada por una relacion lineal y esa varianza la dominan pocos pedidos muy demorados.',
    sprintf('- Splines: R2 = %.3f, pero Spearman = %.3f: el modelo ordena bien los pedidos aunque el R2 sea bajo.',
            sp$r2_fuera_muestra, sp$spearman),
    sprintf('- OLS B: R2 = %.3f, Spearman = %.3f.', ols$r2_fuera_muestra, ols$spearman),
    sprintf('- Recta real ~ predicho de Splines: pendiente %.2f (ideal 1). Las predicciones estan comprimidas',
            sp$calibracion_pendiente),
    '  y el modelo se queda corto con los pedidos mas demorados (figura 01, tramos altos).',
    sprintf('- Frente al baseline, Splines reduce el RMSE en %.3f dias en promedio (SE descriptivo %.3f), mejor en %d de 4 folds.',
            -pr_sp$delta_rmse_medio, pr_sp$se_delta_rmse, pr_sp$folds_mejor_que_baseline_rmse),
    '- Se sigue reportando el R2, pero la decision usa RMSE, MAE, diferencias pareadas frente al baseline y Spearman.',
    '',
    sprintf('2. Supuestos con la receta del curso (lm en el train del fold %d, n = %d)', k, nrow(datos_tr)),
    sprintf('- Normalidad, shapiro.test (submuestra fija de %d residuos): OLS B p %s; Splines p %s.',
            s_ols$shapiro_n_submuestra, fmt_p(s_ols$shapiro_p_valor), fmt_p(s_sp$shapiro_p_valor)),
    sprintf('- Homocedasticidad, lmtest::bptest: OLS B p %s; Splines p %s.',
            fmt_p(s_ols$bptest_p_valor), fmt_p(s_sp$bptest_p_valor)),
    sprintf('- Independencia, lmtest::dwtest (orden de aprobacion): OLS B DW = %.3f; Splines DW = %.3f.',
            s_ols$dwtest_DW, s_sp$dwtest_DW),
    sprintf('- Multicolinealidad, car::vif (OLS B, estado del cliente como factor -> GVIF): maximo %.2f (%s); %d variables sobre 5.',
            max(tabla_vif$VIF_comparable), tabla_vif$variable[1], sum(tabla_vif$VIF_comparable > 5)),
    '- Graficos plot(modelo, which = 1:4) en las figuras 03 (OLS B) y 04 (Splines).',
    '- Lectura: los residuos no son normales y la varianza no es constante. Como dice el curso, esto sugiere',
    '  transformar la respuesta o revisar la especificacion (ver etapa 16, log(dias)).',
    '- La prediccion puntual no exige normalidad; lo afectado son los intervalos clasicos de los coeficientes.',
    '',
    '3. Limitaciones',
    '- shapiro.test acepta como maximo 5000 datos: se usa una submuestra fija con semilla.',
    '- Con miles de datos las pruebas rechazan desviaciones pequenas; por eso se leen junto con los graficos.',
    '- Folds temporales ya examinados en etapas previas; no es un test nuevo. Test julio-agosto 2018 no consultado.')
  writeLines(texto, file.path(out, '08_INTERPRETACION_PARA_INFORME.txt'), useBytes = TRUE)
  capture.output(sessionInfo(), file = file.path(out, '09_sessionInfo.txt'))

  exigir(identical(unname(huellas_antes), unname(tools::md5sum(previos))),
         'Se modifico un resultado de etapas anteriores.')
  exigir(all(supuestos$shapiro_p_valor >= 0 & supuestos$shapiro_p_valor <= 1) &&
           all(is.finite(tabla_vif$VIF_comparable)), 'Resultados de supuestos invalidos.')
  writeLines(c('CONTROL APROBADO - ETAPA 15',
               paste('Modelos evaluados:', length(config$modelos)),
               paste('Pedidos de validacion por modelo:', config$n_validacion_esperado),
               'Mismos pedidos y respuestas para todos los modelos (verificado).',
               sprintf('lm() reproduce lo archivado: max dif OLS B %.3g, Splines %.3g.',
                       dif_reconstruccion[['B_pedido_geografia']], dif_reconstruccion[['Splines_B_anidado']]),
               'Receta del curso: lm, plot which 1:4, shapiro.test, lmtest::bptest, lmtest::dwtest, car::vif.',
               'Resultados de etapas 01-14 sin cambios MD5.',
               'No se reentrenaron los modelos de comparacion ni se consulto el test historico.',
               paste('R:', R.version.string)), cert, useBytes = TRUE)
  cat('ETAPA 15 COMPLETADA\n')
  print(resumen[, c('etiqueta', 'r2_fuera_muestra', 'spearman', 'calibracion_pendiente', 'rmse', 'mae')],
        row.names = FALSE, digits = 4)
  print(supuestos[, c('etiqueta', 'shapiro_p_valor', 'bptest_p_valor', 'dwtest_DW')], row.names = FALSE, digits = 4)
})
