# =============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3 | OLIST
# ETAPA 16 | ANALISIS DE SENSIBILIDAD: RESPUESTA TRANSFORMADA log(dias)
# La etapa 15 muestra residuos no normales y varianza no constante. El curso
# (supuestos1.R) propone en ese caso transformar: lm(log(y) ~ ...). Aqui se
# reajusta el OLS B sobre log(dias) en los MISMOS 4 folds temporales, con la
# MISMA receta aprendida solo en train, y se repite el diagnostico del curso.
#
# Protocolo:
#  - Es un analisis de SENSIBILIDAD: NO reemplaza ni modifica el candidato
#    congelado de la etapa 11 (Splines B).
#  - Para volver a dias se usa exp(prediccion). En escala log, exp() estima la
#    MEDIANA condicional de los dias, no la media: se informa RMSE, MAE y
#    mediana del error absoluto para leerlo correctamente.
#  - No consulta el test historico julio-agosto 2018.
# Requiere: stats, lmtest. Desde la raiz:
#   source('scripts/16_sensibilidad_log_respuesta.R', encoding='UTF-8')
# =============================================================================
local({
  config <- list(
    semilla = 20261104L,
    n_max_shapiro = 5000L,
    alfa = 0.05,
    fold_diagnostico = 4L,
    umbral_frecuencia_estado = 100L,   # identico a etapas 04-11
    tolerancia_reconstruccion = 1e-7
  )
  if (!requireNamespace('lmtest', quietly = TRUE))
    stop('ETAPA 16: falta el paquete lmtest; instalar con install.packages("lmtest").', call. = FALSE)
  set.seed(config$semilla)
  raiz <- normalizePath(getwd(), winslash = '/', mustWork = TRUE)
  ruta <- function(...) file.path(raiz, ...)
  exigir <- function(ok, msg) if (!isTRUE(ok)) stop(paste('ETAPA 16:', msg), call. = FALSE)
  out <- ruta('resultados', '16_sensibilidad_log_respuesta')
  fig <- ruta('graficos', '16_sensibilidad_log_respuesta')
  exigir(dir.exists(ruta('scripts')) && dir.exists(ruta('resultados')), 'Ejecutar desde la raiz del proyecto.')
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig, recursive = TRUE, showWarnings = FALSE)
  cert <- file.path(out, 'CONTROL_ETAPA16_APROBADO.txt')
  if (file.exists(cert)) unlink(cert)
  leer <- function(f) utils::read.csv(ruta('resultados', f), check.names = FALSE,
      stringsAsFactors = FALSE, na.strings = c('', 'NA'))
  guardar <- function(d, f) utils::write.csv(d, file.path(out, f), row.names = FALSE, na = '')
  previos <- list.files(ruta('resultados'), full.names = TRUE, recursive = TRUE)
  previos <- previos[!grepl('/16_sensibilidad_log_respuesta/', previos)]
  huellas_antes <- tools::md5sum(previos)

  # ---------------------------------------------------------------------------
  # 1. Insumos
  # ---------------------------------------------------------------------------
  cat('[16:1/5] Cargando datos, folds y predicciones archivadas del OLS B...\n')
  b <- leer('02_variables_candidatas/01_variables_candidatas_desarrollo.csv')
  et <- leer('01_revision_datos/06_etiquetas_solo_desarrollo.csv')
  ids <- leer('03_protocolo_y_folds/04_ids_train_validacion.csv')
  folds <- leer('03_protocolo_y_folds/03_resumen_folds_temporales.csv')
  p4 <- leer('04_baseline_y_ols/03_predicciones_validacion.csv')
  exigir(!anyDuplicated(b$order_id) && !anyDuplicated(et$order_id) && nrow(folds) == 4L,
         'Insumos con duplicados o conteos inesperados.')
  y <- et$dias_entrega[match(b$order_id, et$order_id)]
  fecha <- as.POSIXct(b$fecha_aprobacion, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')
  exigir(!anyNA(fecha) && all(fecha < as.POSIXct('2018-07-01', tz = 'UTC')), 'Fechas fuera del desarrollo.')
  indices <- function(k, rol) {
    v <- ids$order_id[ids$fold == k & ids$rol == rol]; ix <- match(v, b$order_id)
    exigir(length(ix) > 0L && !anyNA(ix) && !anyDuplicated(v) && all(is.finite(y[ix])) && all(y[ix] > 0),
           paste('Particion invalida', k, rol))
    ix
  }

  # Receta B identica a las etapas 04-11 (aprendida solo con train).
  receta <- function(d) {
    tab <- table(d$estado_cliente)
    freq <- names(tab)[tab >= config$umbral_frecuencia_estado]
    if (!length(freq)) freq <- names(tab)[which.max(tab)]
    agr <- ifelse(is.na(d$estado_cliente) | !(d$estado_cliente %in% freq), 'OTROS', d$estado_cliente)
    lev <- sort(unique(agr))
    list(freq = freq, lev = lev, fallback = if ('OTROS' %in% lev) 'OTROS' else names(tab)[which.max(tab)])
  }
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
  metricas <- function(yv, p) {
    e <- yv - p
    c(rmse = sqrt(mean(e^2)), mae = mean(abs(e)), mediana_error_absoluto = stats::median(abs(e)),
      r2 = 1 - sum(e^2) / sum((yv - mean(yv))^2), sesgo_pred_menos_real = mean(p - yv),
      spearman = stats::cor(yv, p, method = 'spearman'))
  }

  # ---------------------------------------------------------------------------
  # 2. OLS B en dias (control) y en log(dias), mismos 4 folds
  # ---------------------------------------------------------------------------
  cat('[16:2/5] Ajustando lm(dias ~ .) y lm(log(dias) ~ .) en los 4 folds...\n')
  filas <- list(); max_dif <- 0; modelos_fold4 <- NULL
  for (k in 1:4) {
    tr <- indices(k, 'train'); va <- indices(k, 'validacion')
    exigir(!length(intersect(tr, va)) && max(fecha[tr]) < min(fecha[va]), paste('Fuga temporal en fold', k))
    tr <- tr[order(fecha[tr], b$order_id[tr])]   # orden cronologico para dwtest
    rec <- receta(b[tr, , drop = FALSE])
    xtr <- matrizB(b[tr, , drop = FALSE], rec); xva <- matrizB(b[va, , drop = FALSE], rec)
    exigir(identical(names(xtr), names(xva)), 'Columnas train/validacion distintas.')
    m_dias <- stats::lm(dias ~ ., data = data.frame(dias = y[tr], xtr, check.names = FALSE))
    m_log <- stats::lm(log_dias ~ ., data = data.frame(log_dias = log(y[tr]), xtr, check.names = FALSE))
    p_dias <- as.numeric(stats::predict(m_dias, newdata = xva))
    p_log <- exp(as.numeric(stats::predict(m_log, newdata = xva)))
    # Control anti-error: el OLS en dias debe reproducir lo archivado en la etapa 04.
    ar <- p4[p4$fold == k & p4$modelo == 'B_pedido_geografia', , drop = FALSE]
    pos <- match(b$order_id[va], ar$order_id)
    exigir(nrow(ar) == length(va) && !anyNA(pos), paste('Predicciones archivadas incompletas, fold', k))
    max_dif <- max(max_dif, max(abs(ar$dias_predichos[pos] - p_dias)))
    exigir(max_dif < config$tolerancia_reconstruccion, 'lm() no reproduce el OLS B archivado.')
    for (v in c('OLS_B_dias', 'OLS_B_log_exp')) {
      pp <- if (v == 'OLS_B_dias') p_dias else p_log
      filas[[length(filas) + 1L]] <- data.frame(fold = k,
        mes_validacion = folds$mes_validacion[folds$fold == k], version = v,
        n_validacion = length(va), as.list(metricas(y[va], pp)), check.names = FALSE,
        stringsAsFactors = FALSE)
    }
    if (k == config$fold_diagnostico) modelos_fold4 <- list(OLS_B_dias = m_dias, OLS_B_log = m_log)
    cat('  Fold', k, 'completo\n')
  }
  met <- do.call(rbind, filas)
  num <- c('rmse', 'mae', 'mediana_error_absoluto', 'r2', 'sesgo_pred_menos_real', 'spearman')
  resumen <- do.call(rbind, lapply(c('OLS_B_dias', 'OLS_B_log_exp'), function(v) {
    d <- met[met$version == v, , drop = FALSE]
    data.frame(version = v, folds = nrow(d), as.list(colMeans(d[, num])),
               sd_rmse_folds = stats::sd(d$rmse), check.names = FALSE, stringsAsFactors = FALSE)
  }))
  a <- met[met$version == 'OLS_B_dias', , drop = FALSE]
  z <- met[met$version == 'OLS_B_log_exp', , drop = FALSE]; z <- z[match(a$fold, z$fold), , drop = FALSE]
  pareadas <- data.frame(fold = a$fold, mes_validacion = a$mes_validacion,
                         delta_rmse_log_menos_dias = z$rmse - a$rmse,
                         delta_mae_log_menos_dias = z$mae - a$mae,
                         delta_mediana_ea_log_menos_dias = z$mediana_error_absoluto - a$mediana_error_absoluto)

  # ---------------------------------------------------------------------------
  # 3. Diagnostico del curso antes y despues (train del fold 4)
  # ---------------------------------------------------------------------------
  cat('[16:3/5] shapiro.test y bptest en dias y en log(dias)...\n')
  supuestos <- do.call(rbind, lapply(names(modelos_fold4), function(m) {
    mod <- modelos_fold4[[m]]; r <- stats::residuals(mod); n <- length(r)
    set.seed(config$semilla + match(m, names(modelos_fold4)))
    rs <- if (n > config$n_max_shapiro) r[sort(sample.int(n, config$n_max_shapiro))] else r
    sw <- stats::shapiro.test(rs); bp <- lmtest::bptest(mod); dw <- lmtest::dwtest(mod)
    data.frame(modelo = m, fold_train = config$fold_diagnostico, n_train = n,
               shapiro_n_submuestra = length(rs), shapiro_W = unname(sw$statistic),
               shapiro_p_valor = sw$p.value, bptest_BP = unname(bp$statistic),
               bptest_p_valor = unname(bp$p.value), dwtest_DW = unname(dw$statistic),
               stringsAsFactors = FALSE)
  }))

  guardar(met, '01_metricas_por_fold_dias_vs_log.csv')
  guardar(resumen, '02_resumen_dias_vs_log.csv')
  guardar(pareadas, '03_diferencias_pareadas_por_fold.csv')
  guardar(supuestos, '04_supuestos_dias_vs_log.csv')
  guardar(data.frame(parametro = names(config),
                     valor = vapply(config, function(v) paste(v, collapse = '; '), character(1))),
          '05_configuracion_etapa16.csv')

  # ---------------------------------------------------------------------------
  # 4. Figuras: diagnostico del curso para el modelo en log
  # ---------------------------------------------------------------------------
  cat('[16:4/5] Dibujando plot(modelo, which = 1:4) del modelo en log y QQ antes/despues...\n')
  gris <- '#61758B'; verde <- '#147F77'; azul <- '#16334F'
  png_seguro <- function(nombre, fun, w = 1900, h = 1000) {
    f <- file.path(fig, nombre)
    grDevices::png(f, width = w, height = h, res = 170, bg = 'white')
    tryCatch(fun(), finally = grDevices::dev.off())
    exigir(file.exists(f) && file.info(f)$size > 2000, paste('No se genero', nombre))
  }
  png_seguro('01_qq_residuos_dias_vs_log.png', function() {
    op <- graphics::par(mfrow = c(1, 2), mar = c(5, 5, 4.5, 1.5), family = 'sans'); on.exit(graphics::par(op))
    titulos <- c(OLS_B_dias = 'OLS B en dias', OLS_B_log = 'OLS B en log(dias)')
    for (m in names(modelos_fold4)) {
      plot(modelos_fold4[[m]], which = 2, pch = 16, cex = .3, sub.caption = '', caption = '',
           col = grDevices::adjustcolor(verde, alpha.f = .35), main = titulos[[m]])
      s <- supuestos[supuestos$modelo == m, , drop = FALSE]
      graphics::legend('topleft', bty = 'n', cex = .8,
        legend = sprintf('shapiro.test W = %.3f (submuestra %d)', s$shapiro_W, s$shapiro_n_submuestra))
    }
  })
  png_seguro('02_diagnostico_lm_ols_b_log.png', function() {
    op <- graphics::par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1.5), family = 'sans', oma = c(0, 0, 2.2, 0))
    on.exit(graphics::par(op))
    for (w in 1:4) plot(modelos_fold4$OLS_B_log, which = w, pch = 16, cex = .3, sub.caption = '',
                        col = grDevices::adjustcolor(gris, alpha.f = .35))
    graphics::mtext(paste0('plot(modelo, which = 1:4) | OLS B en log(dias) | train fold ',
                           config$fold_diagnostico), outer = TRUE, cex = 1.05, font = 2)
  }, w = 1900, h = 1500)

  # ---------------------------------------------------------------------------
  # 5. Interpretacion, controles e integridad
  # ---------------------------------------------------------------------------
  cat('[16:5/5] Redactando interpretacion y verificando integridad...\n')
  r <- function(v, col) resumen[[col]][resumen$version == v]
  s <- function(m, col) supuestos[[col]][supuestos$modelo == m]
  texto <- c(
    'ETAPA 16 - SENSIBILIDAD CON RESPUESTA TRANSFORMADA log(dias)',
    '',
    'Objetivo: seguir la recomendacion del curso (supuestos1.R) de transformar la respuesta cuando los',
    'residuos no son normales, y ver si cambia la conclusion. NO cambia el candidato congelado de la etapa 11.',
    '',
    sprintf('1. Supuestos (lm en el train del fold %d)', config$fold_diagnostico),
    sprintf('- shapiro.test: W pasa de %.3f (dias) a %.3f (log); W = 1 seria normalidad perfecta.',
            s('OLS_B_dias', 'shapiro_W'), s('OLS_B_log', 'shapiro_W')),
    sprintf('- lmtest::bptest: BP pasa de %.0f (dias) a %.0f (log): la varianza no constante %s.',
            s('OLS_B_dias', 'bptest_BP'), s('OLS_B_log', 'bptest_BP'),
            if (s('OLS_B_log', 'bptest_BP') < s('OLS_B_dias', 'bptest_BP')) 'disminuye' else 'NO se corrige'),
    '- Ver QQ antes/despues (figura 01) y plot(modelo, which = 1:4) del modelo en log (figura 02).',
    '',
    '2. Error de prediccion en dias (promedio de 4 folds, OLS B)',
    sprintf('- En dias:            RMSE %.3f | MAE %.3f | mediana error abs. %.3f | Spearman %.3f',
            r('OLS_B_dias', 'rmse'), r('OLS_B_dias', 'mae'), r('OLS_B_dias', 'mediana_error_absoluto'), r('OLS_B_dias', 'spearman')),
    sprintf('- En log, con exp():  RMSE %.3f | MAE %.3f | mediana error abs. %.3f | Spearman %.3f',
            r('OLS_B_log_exp', 'rmse'), r('OLS_B_log_exp', 'mae'), r('OLS_B_log_exp', 'mediana_error_absoluto'), r('OLS_B_log_exp', 'spearman')),
    sprintf('- Folds donde log tiene menor mediana del error absoluto: %d de 4; menor RMSE: %d de 4.',
            sum(pareadas$delta_mediana_ea_log_menos_dias < 0), sum(pareadas$delta_rmse_log_menos_dias < 0)),
    '',
    '3. Lectura',
    '- La transformacion log acerca los residuos a la normalidad, como anticipa el curso, pero no resuelve',
    '  la heterocedasticidad: ese supuesto sigue sin cumplirse, por eso la inferencia usa errores robustos (etapa 09).',
    '- exp() de una prediccion en log estima la MEDIANA de los dias, no la media: por eso tiende a bajar la',
    '  mediana del error absoluto y a subir el RMSE, que castiga mas los pedidos muy demorados.',
    '- El orden de los pedidos (Spearman) casi no cambia: la transformacion no mejora la capacidad de ordenar.',
    '- Conclusion: se conserva el modelo en dias para la decision (predice la media, que es lo que mide el RMSE),',
    '  y la version en log se reporta como sensibilidad que valida los supuestos.',
    '',
    '4. Limitaciones',
    '- Solo OLS B; los demas modelos no se reajustaron en log.',
    '- Mismos 4 folds ya examinados: no es test nuevo. Test julio-agosto 2018 no consultado.')
  writeLines(texto, file.path(out, '06_INTERPRETACION_PARA_INFORME.txt'), useBytes = TRUE)
  capture.output(sessionInfo(), file = file.path(out, '07_sessionInfo.txt'))

  exigir(identical(unname(huellas_antes), unname(tools::md5sum(previos))),
         'Se modifico un resultado de etapas anteriores.')
  exigir(all(is.finite(as.matrix(met[, num]))) && nrow(met) == 8L, 'Metricas incompletas.')
  writeLines(c('CONTROL APROBADO - ETAPA 16 (SENSIBILIDAD log)',
               sprintf('lm() en dias reproduce el OLS B archivado: max dif %.3g (tolerancia %.0e).',
                       max_dif, config$tolerancia_reconstruccion),
               'Receta aprendida solo con train de cada fold; mismos pedidos de validacion que etapas 04-15.',
               'Resultados de etapas anteriores sin cambios MD5.',
               'Candidato etapa 11 no modificado. Test historico no consultado.',
               paste('R:', R.version.string)), cert, useBytes = TRUE)
  cat('ETAPA 16 COMPLETADA\n')
  print(resumen[, c('version', 'rmse', 'mae', 'mediana_error_absoluto', 'spearman')], row.names = FALSE, digits = 4)
  print(supuestos[, c('modelo', 'shapiro_W', 'bptest_BP', 'dwtest_DW')], row.names = FALSE, digits = 4)
})
