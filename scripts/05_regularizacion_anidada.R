# =============================================================================
# UNIVERSIDAD DE SANTIAGO DE CHILE | ESTADISTICA APLICADA | GRUPO 3
# ETAPA 5: RIDGE, LASSO Y ELASTIC-NET | VERSION 1.0
# Comparacion controlada con OLS B y baseline de etapa 4; solo DESARROLLO.
# REQUIERE: R >= 4.0 y paquete glmnet. No instala nada automaticamente.
# Ejecutar desde la raiz del proyecto, en una sesion limpia:
# source("scripts/05_regularizacion_anidada.R", encoding = "UTF-8")
# =============================================================================

local({
  set.seed(20261023L)
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("Falta 'glmnet'. Instalar una vez con install.packages('glmnet'), luego repetir.",
         call. = FALSE)
  }
  options(stringsAsFactors = FALSE)
  raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  dir2 <- file.path(raiz, "resultados", "02_variables_candidatas")
  dir3 <- file.path(raiz, "resultados", "03_protocolo_y_folds")
  dir4 <- file.path(raiz, "resultados", "04_baseline_y_ols")
  dir5 <- file.path(raiz, "resultados", "05_regularizacion")
  fig5 <- file.path(raiz, "graficos", "05_regularizacion")
  req <- c(
    base = file.path(dir2, "01_variables_candidatas_desarrollo.csv"),
    etiquetas = file.path(raiz, "resultados", "01_revision_datos", "06_etiquetas_solo_desarrollo.csv"),
    ids = file.path(dir3, "04_ids_train_validacion.csv"),
    folds = file.path(dir3, "03_resumen_folds_temporales.csv"),
    especificaciones = file.path(dir3, "02_especificaciones_previas.csv"),
    pred_ols = file.path(dir4, "03_predicciones_validacion.csv"),
    coef_ols = file.path(dir4, "04_coeficientes_ols_por_fold.csv"),
    certificado4 = file.path(dir4, "11_CONTROL_COMPARABILIDAD_APROBADO.txt"),
    dataset = file.path(raiz, "datos_originales", "dataset.zip")
  )
  faltantes <- req[!file.exists(req)]
  if (length(faltantes)) {
    stop(paste("Faltan dependencias de las etapas 1-4:",
               paste(names(faltantes), collapse = ", ")), call. = FALSE)
  }
  dir.create(dir5, recursive = TRUE, showWarnings = FALSE)
  dir.create(fig5, recursive = TRUE, showWarnings = FALSE)
  # La visualizacion es independiente del ajuste: tambien admite modo solo graficos.
  graficar_etapa5 <- function(principal, fig5) {
    dir.create(fig5, showWarnings = FALSE, recursive = TRUE)
    requeridas <- c("modelo", "rmse_promedio", "r2_promedio")
    if (!all(requeridas %in% names(principal))) stop("Faltan columnas del resumen 05")
    nombres <- c(Baseline_media_train = "Media historica", OLS_B = "OLS B",
                 Ridge_min = "Ridge", Lasso_min = "Lasso",
                 ElasticNet_min = "Elastic-Net")
    if (nrow(principal) != length(nombres) ||
        !setequal(principal$modelo, names(nombres)) ||
        anyDuplicated(principal$modelo) ||
        any(!is.finite(principal$rmse_promedio)) ||
        any(!is.finite(principal$r2_promedio)))
      stop("Comparacion de Etapa 5 incompleta o inconsistente")
    ref <- principal$rmse_promedio[principal$modelo == "OLS_B"]
    reg <- principal[principal$modelo %in% c("Ridge_min", "Lasso_min", "ElasticNet_min"), ]
    reg <- reg[order(reg$rmse_promedio), ]
    orden <- c("OLS_B", reg$modelo)
    d <- principal[match(orden, principal$modelo), ]
    y <- rev(seq_len(nrow(d)))
    delta_min <- (d$rmse_promedio - ref) * 24 * 60
    # Figura 1: pequenos cambios respecto de la referencia; sin barras truncadas.
    grDevices::png(file.path(fig5, "01_rmse_modelos_misma_especificacion.png"),
                   width = 1750L, height = 980L, res = 165L, bg = "white")
    op <- par(no.readonly = TRUE)
    tryCatch({
      par(mar = c(7, 11, 6, 4), family = "sans", fg = "#19364B",
          col.axis = "#40566B", las = 1, xaxs = "i")
      xmin <- min(-8, min(delta_min) - 2)
      xmax <- max(8, max(delta_min) + 2)
      plot(NA, xlim = c(xmin, xmax), ylim = c(0.4, 4.6),
           xlab = "Diferencia de RMSE frente a OLS B (minutos)",
           ylab = "", xaxt = "n", yaxt = "n", bty = "n")
      ticks <- pretty(c(xmin, xmax), n = 7)
      ticks <- ticks[ticks >= xmin & ticks <= xmax]
      abline(v = ticks, col = "#EBF0F4")
      abline(v = 0, col = "#52687A", lty = 2, lwd = 2)
      axis(1, at = ticks, labels = format(ticks, trim = TRUE),
           col = "#CBD6DF", col.axis = "#40566B")
      axis(2, at = y, labels = unname(nombres[d$modelo]), tick = FALSE,
           cex.axis = 1.03, las = 1)
      colors <- ifelse(d$modelo == "OLS_B", "#167F78", "#275B8E")
      segments(0, y, delta_min, y, col = "#A8C8D0", lwd = 5)
      points(delta_min, y, pch = 19, cex = 1.7, col = colors)
      # Numeros precisos junto al punto: los cuatro algoritmos comparten escala.
      for (j in seq_along(y)) {
        txt <- sprintf("RMSE %.4f d", d$rmse_promedio[j])
        text(xmax - 0.12, y[j], labels = txt, adj = 1, cex = 0.9,
             font = if (d$modelo[j] == "OLS_B") 2 else 1,
             col = "#19364B")
      }
      title(main = "Regularizacion: diferencias frente a OLS B",
            cex.main = 1.30, col.main = "#19364B", line = 2.5)
      mtext("Mismos diez predictores y cuatro folds temporales; menor RMSE es mejor.",
            side = 3, line = 0.55, col = "#54697D", cex = 0.88)
      mtext("Diferencias muy pequenas: no prueban superioridad estadistica. La media historica no se incluye en este zoom.",
            side = 1, line = 4.8, col = "#697B8C", cex = 0.75)
    }, finally = {par(op); grDevices::dev.off()})

    # Figura 2: sustituimos el grafico de barras R2 casi identicas por tabla.
    d <- principal[match(c("Baseline_media_train", "OLS_B", "Ridge_min",
                            "Lasso_min", "ElasticNet_min"), principal$modelo), ]
    grDevices::png(file.path(fig5, "02_r2_modelos_misma_especificacion.png"),
                   width = 1650L, height = 930L, res = 165L, bg = "white")
    op <- par(no.readonly = TRUE)
    tryCatch({
      par(mar = c(2, 2, 2, 2), family = "sans", fg = "#19364B")
      plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
      text(0.04, 0.92, "Resumen de regularizacion", adj = 0, cex = 1.50,
           font = 2, col = "#19364B")
      text(0.04, 0.855, "Misma especificacion B y mismos pedidos; promedios de cuatro folds",
           adj = 0, cex = 0.87, col = "#556B7E")
      rect(0.025, 0.71, 0.975, 0.78, col = "#E8F0F5", border = NA)
      xs <- c(0.06, 0.57, 0.80)
      headers <- c("Modelo", "RMSE (dias)", "R2 medio")
      for (j in seq_along(xs))
        text(xs[j], 0.745, headers[j], adj = 0,
             col = "#19364B", font = 2, cex = 1.00)
      yy <- seq(0.65, 0.21, length.out = nrow(d))
      for (i in seq_len(nrow(d))) {
        if (i %% 2 == 0) rect(0.025, yy[i] - 0.045, 0.975, yy[i] + 0.045,
                             col = "#F4F7FA", border = NA)
        colr <- if (d$modelo[i] == "OLS_B") "#167F78" else "#19364B"
        text(xs[1], yy[i], nombres[d$modelo[i]], adj = 0, cex = 1.01,
             font = if (d$modelo[i] == "OLS_B") 2 else 1, col = colr)
        text(xs[2], yy[i], sprintf("%.4f", d$rmse_promedio[i]), adj = 0,
             cex = 1.01, col = colr)
        text(xs[3], yy[i], sprintf("%.4f", d$r2_promedio[i]), adj = 0,
             cex = 1.01, col = colr)
      }
      text(0.04, 0.095,
           "R2 fuera de muestra puede ser negativo; pequenas diferencias no implican significancia.",
           adj = 0, cex = 0.84, col = "#5C6F82")
    }, finally = {par(op); grDevices::dev.off()})
    arch <- file.path(fig5, c("01_rmse_modelos_misma_especificacion.png",
                              "02_r2_modelos_misma_especificacion.png"))
    if (any(!file.exists(arch)) || any(file.info(arch)$size < 500))
      stop("No se pudieron generar los graficos de Etapa 5")
    invisible(arch)
  }
  # Modo opcional: solo graficos, sin tocar calculos ni certificados existentes.
  if (isTRUE(getOption("olist.solo_graficos", FALSE))) {
    f <- file.path(dir5, "06_comparacion_principal_mismo_B.csv")
    if (!file.exists(f)) stop("Ejecute primero la Etapa 5: falta ", f)
    md5_antes <- unname(tools::md5sum(f))
    principal <- utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
    graficar_etapa5(principal, fig5)
    if (!identical(md5_antes, unname(tools::md5sum(f))))
      stop("El resumen estadistico cambio inesperadamente")
    cat("ETAPA 5: GRAFICOS ACTUALIZADOS; SIN ENTRENAR MODELOS\n")
  } else {
  certificado5 <- file.path(dir5, "12_CONTROL_ETAPA5_APROBADO.txt")
  if (file.exists(certificado5)) file.remove(certificado5)
  leer <- function(p) utils::read.csv(p, stringsAsFactors = FALSE,
                                      check.names = FALSE, na.strings = c("", "NA"))
  guardar <- function(x, nombre) utils::write.csv(x, file.path(dir5, nombre),
                                                  row.names = FALSE, na = "")
  falla <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)
  metrica <- function(y, p) {
    falla(length(y) == length(p) && length(y) > 1L &&
          all(is.finite(y)) && all(is.finite(p)), "Valores invalidos de metricas.")
    e <- p - y
    sst <- sum((y - mean(y))^2)
    falla(sst > 0, "R2 indefinido; respuesta constante.")
    c(rmse = sqrt(mean(e * e)), mae = mean(abs(e)),
      r2 = 1 - sum(e * e) / sst, sesgo = mean(e))
  }
  as_fecha <- function(x) as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")

  cat("[1/8] Cargando fuentes y verificando integridad del trabajo anterior...\n")
  b <- leer(req[["base"]]); yy <- leer(req[["etiquetas"]]); ids <- leer(req[["ids"]])
  folds <- leer(req[["folds"]]); specs <- leer(req[["especificaciones"]])
  pols <- leer(req[["pred_ols"]]); cols <- leer(req[["coef_ols"]])
  falla(nrow(b) == 86162L && nrow(yy) == 81950L && nrow(folds) == 4L,
        "Conteos iniciales inesperados; no mezclar proyectos.")
  falla(!anyDuplicated(b$order_id) && !anyDuplicated(yy$order_id) &&
          !anyDuplicated(paste(ids$fold, ids$rol, ids$order_id, sep = "|")),
        "Duplicados en pedidos o particiones.")
  falla(setequal(unique(ids$rol), c("train", "validacion")) &&
          setequal(unique(ids$fold), seq_len(4L)), "Particiones incompatibles.")
  falla("B_pedido_geografia" %in% specs$especificacion,
        "La especificacion B de etapa 3 no esta definida.")
  B <- c("hora_aprobacion", "dia_semana", "mes_aprobacion", "horas_desde_compra",
         "estado_cliente", "n_items", "n_vendedores", "precio_total_brl",
         "flete_total_brl", "todos_vendedores_mismo_estado")
  vars_B <- specs$variables[specs$especificacion == "B_pedido_geografia"]
  falla(length(vars_B) == 1L &&
          setequal(trimws(strsplit(vars_B, ";", fixed = TRUE)[[1L]]), B),
        "Variables B incompatibles con el protocolo de etapa 3.")
  falla(all(B %in% names(b)), "Faltan variables en base de desarrollo.")
  fecha <- as_fecha(b$fecha_aprobacion)
  falla(!anyNA(fecha) && all(fecha < as_fecha("2018-07-01 00:00:00")),
        "Fechas invalidas o datos del test en la base de desarrollo.")
  etiqueta <- yy$dias_entrega[match(b$order_id, yy$order_id)]
  falla(all(is.finite(etiqueta[match(yy$order_id, b$order_id)])) &&
          !anyNA(match(yy$order_id, b$order_id)), "Etiquetas mal alineadas.")
  # Verificacion de etiquetas contra el CSV original, por order_id; no consulta test.
  con <- unz(req[["dataset"]], "olist_orders_dataset.csv", open = "r")
  ord <- tryCatch(utils::read.csv(con, colClasses = "character",
                                  stringsAsFactors = FALSE), finally = close(con))
  campos <- c("order_id", "order_approved_at", "order_delivered_customer_date",
              "order_status")
  falla(all(campos %in% names(ord)) && !anyDuplicated(ord$order_id),
        "CSV original de pedidos invalido.")
  pos0 <- match(b$order_id, ord$order_id)
  falla(!anyNA(pos0), "Hay pedidos de desarrollo ausentes en Olist original.")
  ord <- ord[pos0, campos, drop = FALSE]
  app0 <- as_fecha(ord$order_approved_at)
  entrega0 <- as_fecha(ord$order_delivered_customer_date)
  falla(!anyNA(app0) && max(abs(as.numeric(difftime(app0, fecha,
              units = "secs")))) < 0.5, "Fechas originales no coinciden.")
  iy <- match(yy$order_id, b$order_id)
  real0 <- as.numeric(difftime(entrega0[iy], fecha[iy], units = "days"))
  falla(all(is.finite(real0)) && all(ord$order_status[iy] == "delivered") &&
          all(entrega0[iy] < as_fecha("2018-07-01 00:00:00")) &&
          max(abs(real0 - yy$dias_entrega)) < 1e-08,
        "La respuesta no coincide con las fechas reales originales.")

  indices <- function(k, rol) {
    v <- ids$order_id[ids$fold == k & ids$rol == rol]
    ix <- match(v, b$order_id)
    falla(length(ix) > 0L && !anyNA(ix) && !anyDuplicated(v),
          paste("Particion invalida:", k, rol))
    ix
  }
  # Receta IDENTICA a la especificacion B de la etapa 4, sin leer informacion
  # de la validacion para ajustar categorias; glmnet estandariza SOLO train.
  receta_B <- function(dtr) {
    tab <- table(dtr$estado_cliente)
    frecuentes <- names(tab)[tab >= 100L]
    if (!length(frecuentes)) frecuentes <- names(tab)[which.max(tab)]
    g <- ifelse(is.na(dtr$estado_cliente) |
                  !(dtr$estado_cliente %in% frecuentes),
                "OTROS", dtr$estado_cliente)
    niveles <- sort(unique(g))
    fallback <- if ("OTROS" %in% niveles) "OTROS" else
      names(tab)[which.max(tab)]
    list(frecuentes = frecuentes, niveles = niveles, fallback = fallback)
  }
  matriz_B <- function(d, rec) {
    h <- as.numeric(d$hora_aprobacion)
    s <- as.numeric(d$dia_semana)
    m <- as.numeric(d$mes_aprobacion)
    hc <- as.numeric(d$horas_desde_compra)
    falla(all(is.finite(h) & h >= 0 & h <= 23) &&
            all(is.finite(s) & s >= 1 & s <= 7) &&
            all(is.finite(m) & m >= 1 & m <= 12) &&
            all(is.finite(hc) & hc >= 0), "Variables de tiempo incorrectas.")
    x <- cbind(hora_sin = sin(2 * pi * h / 24),
               hora_cos = cos(2 * pi * h / 24),
               dia_sin = sin(2 * pi * (s - 1) / 7),
               dia_cos = cos(2 * pi * (s - 1) / 7),
               mes_sin = sin(2 * pi * (m - 1) / 12),
               mes_cos = cos(2 * pi * (m - 1) / 12),
               log_horas_compra = log1p(hc))
    g0 <- as.character(d$estado_cliente)
    g <- ifelse(is.na(g0) | !(g0 %in% rec$frecuentes), rec$fallback, g0)
    grupo <- factor(g, levels = rec$niveles)
    falla(!anyNA(grupo), "Categoria de cliente sin nivel conocido.")
    if (length(rec$niveles) > 1L) {
      D <- stats::model.matrix(~ grupo)[, -1L, drop = FALSE]
      colnames(D) <- paste0("estado_", make.names(rec$niveles[-1L]))
      x <- cbind(x, D)
    }
    for (z in c("n_items", "n_vendedores", "precio_total_brl",
                "flete_total_brl", "todos_vendedores_mismo_estado")) {
      v <- as.numeric(d[[z]])
      falla(all(is.finite(v) & v >= 0), paste("Faltante o valor invalido:", z))
    }
    geo <- as.numeric(d$todos_vendedores_mismo_estado)
    falla(all(geo %in% c(0, 1)), "Relacion entre estados debe ser 0/1.")
    x <- cbind(x,
               log_items = log1p(as.numeric(d$n_items)),
               log_vendedores = log1p(as.numeric(d$n_vendedores)),
               log_precio = log1p(as.numeric(d$precio_total_brl)),
               log_flete = log1p(as.numeric(d$flete_total_brl)),
               vendedor_mismo_estado = geo)
    storage.mode(x) <- "double"
    falla(nrow(x) == nrow(d) && all(is.finite(x)) && !anyNA(x),
          "Matriz de predictores contiene NA/Inf.")
    x
  }
  # 31 lambdas fijas, sin informacion del test y sin construir la grilla con
  # los targets de la validacion. Incluye regularizacion casi nula y fuerte.
  lambdas <- 10 ^ seq(-3, 2, length.out = 31L)
  alphas <- c(Ridge = 0, Lasso = 1,
              ENet_025 = 0.25, ENet_050 = 0.50, ENet_075 = 0.75)
  falla(all(diff(lambdas) > 0), "Grilla lambda incorrecta.")
  glmfit <- function(x, y, alpha) {
    falla(nrow(x) == length(y) && ncol(x) > 1L && all(is.finite(y)),
          "Argumentos del ajuste glmnet invalidos.")
    glmnet::glmnet(x = x, y = y, family = "gaussian", alpha = alpha,
                   lambda = rev(lambdas), standardize = TRUE, intercept = TRUE,
                   control = list(thresh = 1e-07, maxit = 100000L))
  }
  predpath <- function(fit, x, ls) {
    p <- as.matrix(stats::predict(fit, newx = x, s = ls))
    falla(nrow(p) == nrow(x) && ncol(p) == length(ls) && all(is.finite(p)),
          "Predicciones glmnet invalidas o incompletas.")
    p
  }
  comparar <- function(yk, p) {
    fallo <- sweep(p, 1L, yk, "-")
    sqrt(colMeans(fallo * fallo))
  }

  cat("[2/8] Verificando folds externos, fechas y comparacion original de OLS B...\n")
  for (k in seq_len(4L)) {
    tr <- indices(k, "train"); va <- indices(k, "validacion")
    sub <- folds[folds$fold == k, , drop = FALSE]
    falla(nrow(sub) == 1L && length(tr) == sub$n_train &&
            length(va) == sub$n_validacion && !length(intersect(tr, va)),
          "Folds no corresponden al protocolo original.")
    ini <- as_fecha(sub$fecha_corte_entrenamiento)
    fin <- seq(ini, by = "month", length.out = 2L)[2L]
    falla(all(fecha[tr] <= ini - as.difftime(45, units = "days")) &&
            all(entrega0[tr] < ini) && all(fecha[va] >= ini) &&
            all(fecha[va] < fin) && all(is.finite(etiqueta[c(tr, va)])),
          "Fuga temporal en particion externa o etiqueta no disponible.")
    pb <- pols[pols$fold == k & pols$modelo == "B_pedido_geografia", ]
    falla(nrow(pb) == length(va) && !anyDuplicated(pb$order_id),
          "Predicciones OLS B no coinciden con numero de pedidos del fold.")
    ix <- match(b$order_id[va], pb$order_id)
    falla(!anyNA(ix) && max(abs(pb$dias_reales[ix] - etiqueta[va])) < 1e-09,
          "OLS B no utiliza la misma respuesta ni los mismos pedidos.")
    cb <- cols[cols$fold == k & cols$modelo == "B_pedido_geografia", ]
    xtr <- matriz_B(b[tr, , drop = FALSE], receta_B(b[tr, , drop = FALSE]))
    falla(nrow(cb) == ncol(xtr) + 1L &&
            identical(as.character(cb$termino), c("(Intercept)", colnames(xtr))),
          "Diseno B de etapa 5 NO coincide con columnas de etapa 4.")
  }

  cat("[3/8] Construyendo validaciones temporales internas de cada fold...\n")
  inner_info <- list(); cache_in <- vector("list", 4L)
  for (k in seq_len(4L)) {
    outer_tr <- indices(k, "train")
    ini_outer <- as_fecha(folds$fecha_corte_entrenamiento[folds$fold == k])
    limite <- ini_outer - as.difftime(45, units = "days")
    # Ultimo mes COMPLETO incluido en train: anterior al mes del limite.
    primer_dia_limite <- as_fecha(paste0(format(limite, "%Y-%m"), "-01 00:00:00"))
    fin_mes <- seq(primer_dia_limite, by = "-1 month", length.out = 2L)[2L]
    meses_int <- rev(seq(fin_mes, by = "-1 month", length.out = 3L))
    cache_in[[k]] <- vector("list", length(meses_int))
    for (j in seq_along(meses_int)) {
      inicio <- meses_int[j]
      fin <- seq(inicio, by = "month", length.out = 2L)[2L]
      tr <- outer_tr[fecha[outer_tr] <= inicio - as.difftime(45, units = "days") &
                     entrega0[outer_tr] < inicio]
      va <- outer_tr[fecha[outer_tr] >= inicio & fecha[outer_tr] < fin]
      falla(length(tr) >= 1500L && length(va) >= 500L &&
              !length(intersect(tr, va)) && all(is.finite(etiqueta[c(tr, va)])) &&
              all(entrega0[tr] < inicio) && all(fecha[tr] < fecha[va][1L]),
            paste("Fold interno incorrecto", k, j))
      cache_in[[k]][[j]] <- list(tr = tr, va = va, mes = format(inicio, "%Y-%m"))
      inner_info[[length(inner_info) + 1L]] <- data.frame(
        fold_externo = k, fold_interno = j, mes_validacion = format(inicio, "%Y-%m"),
        n_train = length(tr), n_validacion = length(va),
        ultima_aprobacion_train = format(max(fecha[tr]), "%Y-%m-%d %H:%M:%S"),
        primera_aprobacion_validacion = format(min(fecha[va]), "%Y-%m-%d %H:%M:%S"))
    }
  }

  cat("[4/8] Seleccionando lambda/alpha SOLO con validacion interna temporal...\n")
  filas_cv <- list(); selecciones <- list(); pred_filas <- list()
  metr_filas <- list(); coef_filas <- list(); sub_filas <- list(); complejidad_filas <- list()
  for (k in seq_len(4L)) {
    cat("   Fold externo", k, "de 4...\n")
    cv_k <- list()
    for (j in seq_along(cache_in[[k]])) {
      ind <- cache_in[[k]][[j]]
      dtr <- b[ind$tr, , drop = FALSE]; dva <- b[ind$va, , drop = FALSE]
      rec <- receta_B(dtr)
      xtr <- matriz_B(dtr, rec); xva <- matriz_B(dva, rec)
      falla(identical(colnames(xtr), colnames(xva)),
            "Codificacion interna difiere entre entrenamiento y validacion.")
      yt <- etiqueta[ind$tr]; yv <- etiqueta[ind$va]
      for (nombre in names(alphas)) {
        fit <- glmfit(xtr, yt, alphas[[nombre]])
        p <- predpath(fit, xva, lambdas)
        rmse <- comparar(yv, p)
        mae <- colMeans(abs(sweep(p, 1L, yv, "-")))
        fila <- data.frame(fold_externo = k, fold_interno = j,
                           mes_interno = ind$mes, algoritmo = nombre,
                           alpha_enet = unname(alphas[[nombre]]),
                           lambda = lambdas, rmse = rmse, mae = mae)
        cv_k[[length(cv_k) + 1L]] <- fila
      }
    }
    cv_k <- do.call(rbind, cv_k)
    filas_cv[[k]] <- cv_k
    # Cada lambda se evalua en EXACTAMENTE las mismas 3 ventanas internas.
    for (familia in c("Ridge", "Lasso", "ElasticNet")) {
      candidatos <- if (familia == "ElasticNet")
        cv_k[grepl("^ENet_", cv_k$algoritmo), ] else
        cv_k[cv_k$algoritmo == familia, ]
      grupos <- split(candidatos,
                     paste(candidatos$algoritmo, format(candidatos$lambda,
                                                      digits = 16), sep = "|"))
      agreg <- do.call(rbind, lapply(grupos, function(g) {
        falla(nrow(g) == 3L && setequal(g$fold_interno, 1:3),
              "Se perdio un fold interno para lambda/alpha.")
        data.frame(algoritmo = g$algoritmo[1L], alpha_enet = g$alpha_enet[1L],
                   lambda = g$lambda[1L], media_rmse = mean(g$rmse),
                   sd_rmse = stats::sd(g$rmse),
                   error_estandar = stats::sd(g$rmse) / sqrt(nrow(g)))
      }))
      # Regla principal: menor RMSE medio; empate, mayor lambda.
      agreg <- agreg[order(agreg$media_rmse, -agreg$lambda, agreg$alpha_enet), ]
      mejor <- agreg[1L, ]
      # Regla 1-SE: SOLO dentro del alpha elegido en el minimo RMSE.
      misma_alpha <- agreg[agreg$algoritmo == mejor$algoritmo, ]
      posible <- misma_alpha[misma_alpha$media_rmse <=
                               mejor$media_rmse + mejor$error_estandar + 1e-12, ]
      falla(nrow(posible) > 0L, "Regla 1-SE no encontro candidatos.")
      uno_se <- posible[which.max(posible$lambda), ]
      selecciones[[length(selecciones) + 1L]] <- data.frame(
        fold_externo = k, familia = familia, alpha_seleccionado = mejor$alpha_enet,
        lambda_min = mejor$lambda, lambda_1se = uno_se$lambda,
        media_rmse_interna_min = mejor$media_rmse,
        error_estandar_interno_min = mejor$error_estandar,
        media_rmse_interna_1se = uno_se$media_rmse,
        enet_combinacion_elegida = mejor$algoritmo,
        n_folds_internos = 3L,
        lambda_min_en_borde = mejor$lambda %in% range(lambdas))
      outer_tr <- indices(k, "train"); outer_va <- indices(k, "validacion")
      rec_full <- receta_B(b[outer_tr, , drop = FALSE])
      xtr <- matriz_B(b[outer_tr, , drop = FALSE], rec_full)
      xva <- matriz_B(b[outer_va, , drop = FALSE], rec_full)
      falla(identical(colnames(xtr), colnames(xva)),
            "Columnas incompatibles en outer fold.")
      yt <- etiqueta[outer_tr]; yv <- etiqueta[outer_va]
      fit <- glmfit(xtr, yt, mejor$alpha_enet)
      for (regla in c("min", "1se")) {
        lam <- if (regla == "min") mejor$lambda else uno_se$lambda
        pp <- as.vector(predpath(fit, xva, lam))
        nombre <- paste0(familia, "_", regla)
        met <- metrica(yv, pp)
        metr_filas[[length(metr_filas) + 1L]] <- data.frame(
          fold = k, mes_validacion = folds$mes_validacion[folds$fold == k],
          modelo = nombre, especificacion = "B_pedido_geografia",
          n_train = length(outer_tr), n_validacion = length(outer_va),
          alpha_enet = mejor$alpha_enet, lambda = lam,
          rmse = unname(met["rmse"]), mae = unname(met["mae"]),
          r2 = unname(met["r2"]), sesgo = unname(met["sesgo"]))
        pred_filas[[length(pred_filas) + 1L]] <- data.frame(
          fold = k, order_id = b$order_id[outer_va], modelo = nombre,
          dias_reales = yv, dias_predichos = pp,
          error_predicho_menos_real = pp - yv)
        # Resultados geograficos por separado, para cumplir pauta sin recargar tabla.
        grupo <- b$todos_vendedores_mismo_estado[outer_va]
        for (g in 0:1) {
          ig <- which(!is.na(grupo) & grupo == g)
          if (length(ig) >= 2L) {
            mg <- metrica(yv[ig], pp[ig])
            sub_filas[[length(sub_filas) + 1L]] <- data.frame(
              fold = k, modelo = nombre,
              grupo = if (g == 0) "al_menos_un_vendedor_otro_estado" else
                "todos_vendedores_mismo_estado",
              n = length(ig), rmse = unname(mg["rmse"]),
              mae = unname(mg["mae"]))
          }
        }
      }
      # Coeficientes SOLO del modelo lambda.min; intercepto sin penalizar.
      bmin <- as.matrix(stats::coef(fit, s = mejor$lambda))
      complejidad_filas[[length(complejidad_filas) + 1L]] <- data.frame(
        fold = k, familia = familia, alpha_enet = mejor$alpha_enet,
        lambda_min = mejor$lambda,
        predictores_no_cero = sum(abs(bmin[-1L, 1L]) > 1e-08),
        predictores_totales = nrow(bmin) - 1L)
      coef_filas[[length(coef_filas) + 1L]] <- data.frame(
        fold = k, familia = familia, termino = rownames(bmin),
        coeficiente = as.numeric(bmin[, 1L]), alpha_enet = mejor$alpha_enet,
        lambda = mejor$lambda)
    }
  }

  cat("[5/8] Consolidando comparacion sin mezclar predicciones...\n")
  cv <- do.call(rbind, filas_cv)
  inner <- do.call(rbind, inner_info)
  tuning <- do.call(rbind, selecciones)
  metr <- do.call(rbind, metr_filas)
  pred <- do.call(rbind, pred_filas)
  coefi <- do.call(rbind, coef_filas)
  sub <- do.call(rbind, sub_filas)
  complejidad <- do.call(rbind, complejidad_filas)
  falla(nrow(tuning) == 12L && nrow(metr) == 24L &&
          nrow(pred) == 6L * sum(folds$n_validacion) &&
          !anyDuplicated(paste(metr$fold, metr$modelo, sep = "|")) &&
          !anyDuplicated(paste(pred$fold, pred$modelo, pred$order_id, sep = "|")),
        "Datos finales duplicados o dimensiones incorrectas.")
  # Recalcula CADA metrica a partir de CADA prediccion nueva.
  for (k in seq_len(4L)) {
    ov <- indices(k, "validacion"); yr <- etiqueta[ov]
    referencia <- b$order_id[ov]
    for (nombre in unique(metr$modelo)) {
      pp <- pred[pred$fold == k & pred$modelo == nombre, ]
      posicion <- match(referencia, pp$order_id)
      falla(nrow(pp) == length(ov) && !anyNA(posicion) &&
              max(abs(pp$dias_reales[posicion] - yr)) < 1e-09,
            "Pedido o respuesta desalineados en metricas regularizadas.")
      mm <- metrica(yr, pp$dias_predichos[posicion])
      grabado <- metr[metr$fold == k & metr$modelo == nombre, ]
      falla(nrow(grabado) == 1L && grabado$n_validacion == length(ov) &&
              all(abs(as.numeric(grabado[1L, names(mm)]) - mm) < 1e-09),
            "Resumen de metricas no coincide con predicciones por pedido.")
    }
  }
  reg_resumen <- do.call(rbind, lapply(unique(metr$modelo), function(nombre) {
    z <- metr[metr$modelo == nombre, ]
    falla(nrow(z) == 4L, "Faltan folds externos.")
    data.frame(modelo = nombre, rmse_promedio = mean(z$rmse),
               sd_rmse_folds = stats::sd(z$rmse),
               mae_promedio = mean(z$mae), r2_promedio = mean(z$r2),
               sesgo_promedio = mean(z$sesgo))
  }))
  # Tabla principal: MISMA ESPECIFICACION B, mismas validaciones, regla MIN.
  ref_nombres <- c("Baseline_media_train", "B_pedido_geografia")
  ref <- pols[pols$modelo %in% ref_nombres, ]
  # Usa las metricas originales, pero recalcula para no copiar resumen externo.
  fila_ref <- list()
  for (nom in ref_nombres) {
    filas <- list()
    for (k in seq_len(4L)) {
      v <- indices(k, "validacion")
      p <- ref[ref$fold == k & ref$modelo == nom, ]
      idx <- match(b$order_id[v], p$order_id)
      falla(nrow(p) == length(v) && !anyNA(idx) &&
              max(abs(p$dias_reales[idx] - etiqueta[v])) < 1e-09,
            "Baseline / OLS B desalineado.")
      mm <- metrica(etiqueta[v], p$dias_predichos[idx])
      filas[[k]] <- data.frame(fold = k, rmse = mm["rmse"], mae = mm["mae"],
                               r2 = mm["r2"], sesgo = mm["sesgo"])
    }
    z <- do.call(rbind, filas)
    fila_ref[[length(fila_ref) + 1L]] <- data.frame(
      modelo = if (nom == "B_pedido_geografia") "OLS_B" else "Baseline_media_train",
      rmse_promedio = mean(z$rmse), sd_rmse_folds = stats::sd(z$rmse),
      mae_promedio = mean(z$mae), r2_promedio = mean(z$r2),
      sesgo_promedio = mean(z$sesgo))
  }
  principal <- rbind(do.call(rbind, fila_ref),
                     reg_resumen[grepl("_min$", reg_resumen$modelo), ])
  principal <- principal[order(principal$rmse_promedio), ]
  rownames(principal) <- NULL
  regla1se <- reg_resumen[grepl("_1se$", reg_resumen$modelo), ]
  rownames(regla1se) <- NULL

  cat("[6/8] Guardando pruebas y trazabilidad reproducible...\n")
  guardar(inner, "01_folds_internos.csv")
  guardar(cv, "02_grilla_resultados_validacion_interna.csv")
  guardar(tuning, "03_hiperparametros_elegidos.csv")
  guardar(metr, "04_metricas_por_fold_regularizados.csv")
  guardar(pred, "05_predicciones_validacion_regularizados.csv")
  guardar(principal, "06_comparacion_principal_mismo_B.csv")
  guardar(regla1se, "07_sensibilidad_regla_1se.csv")
  guardar(coefi, "08_coeficientes_regulados_lambda_min.csv")
  guardar(sub, "09_subgrupos_aparte.csv")
  guardar(complejidad, "13_complejidad_por_fold.csv")
  manifest <- data.frame(entrada = names(req), archivo = unname(req),
                         md5 = unname(tools::md5sum(req)), stringsAsFactors = FALSE)
  guardar(manifest, "10_huellas_archivos_entrada.csv")
  writeLines(c(paste("R:", R.version.string),
               paste("glmnet:", as.character(utils::packageVersion("glmnet"))),
               paste("Semilla:", 20261023L),
               paste("Folds externos:", 4L),
               paste("Folds internos:", 3L),
               paste("Lambda grilla: 10 ^ seq(-3, 2, length.out=31)"),
               paste("Alpha grilla Elastic Net: 0.25, 0.50, 0.75"),
               "Regla principal: lambda.min; lambda.1se solo sensibilidad.",
               "ElasticNet alpha seleccionado por RMSE interno minimo.",
               "Mismas diez variables B y misma poblacion validada para todos.",
               "Todas las agrupaciones de estados y estandarizacion: SOLO train.",
               "Folds internos temporales anidados SOLO en train externo.",
               "La tabla principal NO mezcla A, B, C, subgrupos ni 1-SE.",
               "Si lambda_min_en_borde es TRUE, ampliar grilla en siguiente iteracion sin test.",
               "NO se usa test historico; el desarrollo actual es exploratorio posterior al test original.",
               "La disponibilidad historica exacta de datos de vendedor/pedido es supuesto no probado.",
               "La media de RMSE de folds es descriptiva; no prueba superioridad estadistica.",
               "El nivel alfa de significancia 0.05 NO es glmnet alpha ni un umbral de RMSE."),
             file.path(dir5, "11_metodologia_y_versiones.txt"), useBytes = TRUE)

  if (any(tuning$lambda_min_en_borde)) {
    warning("Algunos lambdas optimos estan en el borde de la grilla. Ver 03_hiperparametros_elegidos.csv; no afirmar optimalidad global.")
  }
  cat("[7/8] Dibujando figuras comparativas en R base...\n")
  graficar_etapa5(principal, fig5)

  cat("[8/8] Certificando comparabilidad y finalizando...\n")
  esperado_arch <- c("01_folds_internos.csv", "02_grilla_resultados_validacion_interna.csv",
                     "03_hiperparametros_elegidos.csv", "04_metricas_por_fold_regularizados.csv",
                     "05_predicciones_validacion_regularizados.csv",
                     "06_comparacion_principal_mismo_B.csv", "07_sensibilidad_regla_1se.csv",
                     "08_coeficientes_regulados_lambda_min.csv", "09_subgrupos_aparte.csv",
                     "10_huellas_archivos_entrada.csv", "11_metodologia_y_versiones.txt",
                     "13_complejidad_por_fold.csv")
  falla(all(file.exists(file.path(dir5, esperado_arch))) &&
          all(file.info(file.path(dir5, esperado_arch))$size > 0),
        "Falta algun resultado de Etapa 5.")
  falla(all(file.exists(file.path(fig5, c("01_rmse_modelos_misma_especificacion.png",
                                         "02_r2_modelos_misma_especificacion.png")))),
        "Faltan los graficos de Etapa 5.")
  falla(max(abs(unlist(metr[c("rmse", "mae", "r2", "sesgo")])) ) < 1e05,
        "Metricas numericamente inverosimiles; revisar datos.")
  writeLines(c("CONTROL APROBADO - ETAPA 5 v1.0",
               "4 folds externos identicos a Etapa 4.",
               "3 validaciones internas cronologicas, dentro de cada train externo.",
               "Preprocesamiento de estado y glmnet standardize solo en train.",
               "Diseno de predictores B cotejado con coeficientes de OLS etapa 4.",
               "Grilla de alpha/lambda y reglas min/1se documentadas.",
               "Se recalcularon metricas desde predicciones individuales por ID.",
               "Comparacion principal: baseline, OLS B y regularizados lambda.min.",
               "Ni fechas futuras ni test historico participaron en seleccionar modelos."),
             certificado5, useBytes = TRUE)
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("ETAPA 5 COMPLETADA - REGULARIZACION; SOLO DESARROLLO\n")
  print(principal, row.names = FALSE, digits = 5)
  cat("\nRESULTADOS:", dir5, "\nGRAFICOS:", fig5, "\n")
  cat("CERTIFICADO:", certificado5, "\n")
  cat(strrep("=", 70), "\n", sep = "")
  }
})
