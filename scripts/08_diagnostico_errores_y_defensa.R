# Funciones de graficos de Etapa 8. No modifican predicciones ni calculos.
graficar_diagnosticos_08 <- function(resumen, tramos, pred, graficos) {
  dir.create(graficos, showWarnings = FALSE, recursive = TRUE)
  requerido <- c("modelo", "sesgo_promedio")
  if (!all(requerido %in% names(resumen))) stop("Falta resumen de sesgo de Etapa 8")
  if (anyNA(resumen$sesgo_promedio) || any(!is.finite(resumen$sesgo_promedio)))
    stop("Sesgo no finito")
  # Figura 1: sesgo medio de todos los modelos, cero como referencia.
  grDevices::png(file.path(graficos, "01_sesgo_medio_por_modelo.png"),
                 width = 1700L, height = 1050L, res = 170L, bg = "white")
  op <- par(no.readonly = TRUE)
  tryCatch({
    par(mar = c(6, 15, 5.5, 4), family = "sans", fg = "#1B3146",
        col.axis = "#334A60", las = 1)
    n <- nrow(resumen)
    yy <- n:1
    xmin <- min(c(-3.6, resumen$sesgo_promedio)) - 0.2
    xmax <- 0.35
    plot(NA, xlim = c(xmin, xmax), ylim = c(0.5, n + 0.5),
         yaxt = "n", xaxt = "n", xlab = "Sesgo (predicho - real, dias)",
         ylab = "", bty = "n")
    abline(v = seq(ceiling(xmin), 0, by = 1), col = "#E5EBF0")
    abline(v = 0, col = "#314961", lwd = 1.5, lty = 2)
    axis(1, at = seq(ceiling(xmin), 0, by = 1), col.axis = "#334A60")
    axis(2, at = yy, labels = gsub("_", " ", resumen$modelo),
         las = 1, tick = FALSE, cex.axis = 0.85)
    segments(0, yy, resumen$sesgo_promedio, yy, lwd = 5, col = "#B9DBDC")
    points(resumen$sesgo_promedio, yy, pch = 19, cex = 1.6, col = "#087D82")
    text(resumen$sesgo_promedio, yy,
         labels = sprintf("%.2f", resumen$sesgo_promedio),
         pos = 2, offset = 0.6, cex = 0.78, col = "#153A56", font = 2)
    title(main = "Sesgo medio de prediccion por modelo",
          col.main = "#12304A", cex.main = 1.32, line = 2)
    mtext("Promedio de cuatro folds. Negativo = subestimacion media, no necesariamente la mayoria de pedidos.",
          side = 3, line = 0.4, cex = 0.76, col = "#56687B")
  }, finally = {par(op); grDevices::dev.off()})

  # Figura 2: error firmado por tramo de DURACION REAL, SOLO descriptivo.
  spl <- tramos[tramos$modelo == "Splines_B_anidado", ]
  ord <- c("Menos de 7 dias", "7 a menos de 14 dias",
           "14 a menos de 30 dias", "30 dias o mas")
  spl <- spl[match(ord, spl$tramo_real), ]
  if (nrow(spl) != 4L || anyNA(spl$sesgo) || anyNA(spl$n) ||
      any(!is.finite(spl$sesgo)) || any(spl$n <= 0))
    stop("No hay cuatro tramos validos de Splines en Etapa 8")
  grDevices::png(file.path(graficos, "02_error_por_duracion_real_splines.png"),
                 width = 1750L, height = 1020L, res = 165L, bg = "white")
  op <- par(no.readonly = TRUE)
  tryCatch({
    par(mar = c(7, 14, 6, 4), family = "sans", fg = "#19364B",
        col.axis = "#40566B", las = 1)
    yy <- 4:1
    xmin <- min(-30, min(spl$sesgo) - 4)
    xmax <- max(12, max(spl$sesgo) + 4)
    plot(NA, xlim = c(xmin, xmax), ylim = c(0.3, 4.7),
         xlab = "Error medio: prediccion menos entrega real (dias)",
         ylab = "", xaxt = "n", yaxt = "n", bty = "n")
    ticks <- pretty(c(xmin, xmax), n = 9)
    ticks <- ticks[ticks >= xmin & ticks <= xmax]
    abline(v = ticks, col = "#E8EDF2")
    abline(v = 0, lty = 2, lwd = 2, col = "#40566B")
    axis(1, at = ticks, col.axis = "#40566B", col = "#CBD8E2")
    axis(2, at = yy, labels = c("Menos de 7 dias", "7 a menos de 14 dias",
                              "14 a menos de 30 dias", "30 dias o mas"),
         tick = FALSE, las = 1, cex.axis = 0.94)
    colores <- ifelse(spl$sesgo < 0, "#CA7048", "#187E80")
    segments(0, yy, spl$sesgo, yy, lwd = 12, col = colores)
    points(spl$sesgo, yy, pch = 19, cex = 1.4, col = colores)
    for (j in seq_len(4L)) {
      pos <- if (spl$sesgo[j] < 0) 2 else 4
      text(spl$sesgo[j], yy[j], sprintf("%+.2f dias", spl$sesgo[j]),
           pos = pos, offset = 0.65, font = 2, cex = 0.93, col = "#19364B")
      # Los tamanos de grupo se colocan por ENCIMA de cada barra, evitando
      # superposicion con la etiqueta del error en el tramo mas corto.
      # Separadores distintos: miles="." y decimal=","; evita avisos de prettyNum.
      n_etiqueta <- format(spl$n[j], big.mark = ".", decimal.mark = ",",
                           trim = TRUE, scientific = FALSE)
      text(xmax - 0.3, yy[j] + 0.27, sprintf("n = %s", n_etiqueta),
           adj = 1, cex = 0.78, col = "#566B7C")
    }
    title(main = "De donde proviene el sesgo de Splines",
          col.main = "#19364B", cex.main = 1.34, line = 2.4)
    mtext("Diagnostico retrospectivo por duracion REAL; no son grupos conocidos al predecir.",
          side = 3, line = 0.55, cex = 0.87, col = "#566B7C")
    mtext("Valor negativo: subestimacion; positivo: sobreestimacion. No se excluyen observaciones.",
          side = 1, line = 5.0, cex = 0.78, col = "#566B7C")
  }, finally = {par(op); grDevices::dev.off()})

  # Figura 3: dos series con medias reales y predichas por el mismo fold.
  pd <- pred[pred$modelo == "Splines_B_anidado", , drop = FALSE]
  if (!all(c("fold", "dias_reales", "dias_predichos") %in% names(pd)) ||
      nrow(pd) != 26279L || any(!is.finite(pd$dias_reales)) ||
      any(!is.finite(pd$dias_predichos)))
    stop("Predicciones de Splines incompletas para figura de medias")
  medias <- stats::aggregate(cbind(dias_reales, dias_predichos) ~ fold,
                              data = pd, FUN = mean)
  medias <- medias[order(medias$fold), ]
  if (nrow(medias) != 4L || !identical(as.integer(medias$fold), 1:4))
    stop("Folds de Splines fuera del protocolo")
  grDevices::png(file.path(graficos, "03_media_real_vs_predicha_por_fold.png"),
                 width = 1750L, height = 970L, res = 165L, bg = "white")
  op <- par(no.readonly = TRUE)
  tryCatch({
    par(mar = c(6.8, 7.3, 6, 5), family = "sans", fg = "#19364B",
        col.axis = "#40566B", las = 1)
    x <- seq_len(4L)
    ylim <- range(c(medias$dias_reales, medias$dias_predichos)) + c(-2, 2)
    plot(x, medias$dias_reales, type = "n", xlim = c(0.85, 4.45), ylim = ylim,
         xlab = "Mes de validacion", ylab = "Tiempo medio de entrega (dias)",
         xaxt = "n", bty = "n")
    abline(h = pretty(ylim), col = "#EAF0F4")
    axis(1, at = x, labels = c("Dic 2017", "Ene 2018", "Feb 2018", "Mar 2018"),
         col.axis = "#40566B", tick = FALSE)
    lines(x, medias$dias_reales, type = "b", pch = 19, cex = 1.45,
          lwd = 3, col = "#193F6C")
    lines(x, medias$dias_predichos, type = "b", pch = 17, cex = 1.5,
          lwd = 3, col = "#CE7546")
    text(x, medias$dias_reales, labels = sprintf("%.2f", medias$dias_reales),
         pos = 3, offset = 0.65, cex = 0.88, font = 2, col = "#193F6C")
    text(x, medias$dias_predichos,
         labels = sprintf("%.2f", medias$dias_predichos),
         pos = 1, offset = 0.70, cex = 0.88, font = 2, col = "#B65F35")
    title(main = "Duracion real frente a prediccion media de Splines",
          col.main = "#19364B", cex.main = 1.27, line = 2.5)
    mtext("Mismas observaciones por mes; comparacion descriptiva fuera de muestra.",
          side = 3, line = 0.5, cex = 0.85, col = "#5A7082")
    legend("topleft", legend = c("Media real", "Media predicha"),
           col = c("#193F6C", "#CE7546"), pch = c(19, 17),
           lwd = 3, bty = "n", cex = 0.85, horiz = TRUE, inset = c(0.005, 0.01))
  }, finally = {par(op); grDevices::dev.off()})

  files <- file.path(graficos, c("01_sesgo_medio_por_modelo.png",
                                  "02_error_por_duracion_real_splines.png",
                                  "03_media_real_vs_predicha_por_fold.png"))
  if (any(!file.exists(files)) || any(file.info(files)$size < 500))
    stop("No se pudieron generar las tres figuras de Etapa 8")
  invisible(files)
}

# Modo solo graficos: lee CSV existentes, sin reentrenar ni reescribir tablas.
if (isTRUE(getOption("olist.solo_graficos", FALSE))) {
  raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  ruta <- file.path(raiz, "resultados", "08_diagnostico_errores")
  pred6 <- file.path(raiz, "resultados", "06_splines", "05_predicciones_splines_por_pedido.csv")
  f <- c(file.path(ruta, "01_comparacion_resumida.csv"),
         file.path(ruta, "03_diagnostico_por_duracion_real.csv"), pred6)
  if (any(!file.exists(f))) stop("Faltan los resultados existentes de Etapas 6 u 8")
  md5_antes <- unname(tools::md5sum(f))
  resumen <- utils::read.csv(f[1], stringsAsFactors = FALSE)
  tramos <- utils::read.csv(f[2], stringsAsFactors = FALSE)
  pred <- utils::read.csv(f[3], stringsAsFactors = FALSE)
  graficar_diagnosticos_08(resumen, tramos, pred,
    file.path(raiz, "graficos", "08_diagnostico_errores"))
  if (!identical(md5_antes, unname(tools::md5sum(f))))
    stop("Un CSV de entrada cambio inesperadamente")
  cat("ETAPA 8: GRAFICOS ACTUALIZADOS; SIN REENTRENAR MODELOS\n")
} else {
# ETAPA 8 - Diagnostico descriptivo de errores fuera de muestra
# Proyecto Olist - Caso 1, Estadistica Aplicada
# Solo usa PREDICCIONES GUARDADAS de desarrollo (etapas 4 a 7).
# No ajusta modelos, no modifica salidas anteriores y NO consulta el test historico.
# Requiere unicamente R base (stats, graphics, grDevices, utils).

cat('[1/7] Verificando fuentes y certificados...\n')
raiz <- getwd()
fuentes <- c(
  folds = 'resultados/03_protocolo_y_folds/04_ids_train_validacion.csv',
  pred04 = 'resultados/04_baseline_y_ols/03_predicciones_validacion.csv',
  pred05 = 'resultados/05_regularizacion/05_predicciones_validacion_regularizados.csv',
  pred06 = 'resultados/06_splines/05_predicciones_splines_por_pedido.csv',
  pred07 = 'resultados/07_ensembles/05_predicciones_validacion.csv',
  resumen07 = 'resultados/07_ensembles/07_comparacion_comun_resumen.csv',
  certificado07 = 'resultados/07_ensembles/11_CONTROL_ETAPA7_APROBADO.txt'
)
if (any(!file.exists(fuentes))) stop('Faltan entradas de etapas previas: ', paste(names(fuentes)[!file.exists(fuentes)],collapse=', '))
huellas_antes <- unname(tools::md5sum(fuentes))
if (anyNA(huellas_antes)) stop('No se pudieron calcular las huellas MD5.')
leer <- function(p) utils::read.csv(p, stringsAsFactors=FALSE, check.names=FALSE, fileEncoding='UTF-8-BOM')

cat('[2/7] Leyendo solo los modelos de la comparacion B...\n')
seleccion <- list(
 pred04=c('Baseline_media_train','B_pedido_geografia'),
 pred05=c('Ridge_min','Lasso_min','ElasticNet_min'),
 pred06='Splines_B_anidado',
 pred07=c('Arbol_B_anidado','RandomForest_B_anidado','Boosting_B_anidado')
)
modelos <- unlist(seleccion, use.names=FALSE)
columnas <- c('fold','order_id','modelo','dias_reales','dias_predichos')
pred <- do.call(rbind, lapply(names(seleccion), function(archivo) {
  d <- leer(fuentes[[archivo]])
  if (!all(columnas %in% names(d))) stop('Columnas ausentes en ', archivo)
  d <- d[d$modelo %in% seleccion[[archivo]], columnas, drop=FALSE]
  for (m in seleccion[[archivo]]) if (sum(d$modelo==m) == 0L) stop('No se encontro ',m)
  d
}))
row.names(pred) <- NULL
if (nrow(pred) != 26279L * length(modelos)) stop('Numero inesperado de predicciones: ',nrow(pred))
if (anyNA(pred[,columnas]) || any(!is.finite(pred$dias_reales)) || any(!is.finite(pred$dias_predichos))) stop('Predicciones o etiquetas faltantes/no finitas')
if (!identical(sort(unique(pred$modelo)), sort(modelos))) stop('Familias de modelos inesperadas')
if (!identical(sort(unique(as.integer(pred$fold))), 1:4)) stop('Folds fuera del protocolo')
clave <- function(d) paste(d$fold, d$order_id,sep='|')
if (anyDuplicated(paste(pred$modelo,clave(pred),sep='|'))) stop('Duplicacion modelo-fold-pedido')

cat('[3/7] Comprobando IDs y respuestas reales comunes...\n')
ids <- leer(fuentes[['folds']])
if (!all(c('fold','order_id','rol') %in% names(ids))) stop('Esquema inesperado de los folds')
valid <- ids[ids$rol=='validacion',c('fold','order_id'),drop=FALSE]
# Se admite en el protocolo el nombre "validacion" sin tilde exclusivamente.
if (nrow(valid) == 0L) stop('No se reconocieron filas de validacion')
if (anyDuplicated(clave(valid))) stop('IDs duplicados en el protocolo')
modelo_ref <- 'Baseline_media_train'
ref <- pred[pred$modelo==modelo_ref,,drop=FALSE]
clave_ref <- clave(ref)
if (length(clave_ref)!=nrow(valid) || !setequal(clave_ref,clave(valid))) stop('Los IDs del baseline no coinciden con validacion')
for (m in modelos) {
 d <- pred[pred$modelo==m,,drop=FALSE]
 kk <- clave(d)
 if (length(kk)!=length(clave_ref) || !setequal(kk,clave_ref)) stop('Pedidos diferentes en ',m)
 pos <- match(kk,clave_ref)
 if (anyNA(pos) || max(abs(d$dias_reales-ref$dias_reales[pos]))>1e-9) stop('Respuestas reales no coinciden en ',m)
}

cat('[4/7] Calculando errores por fold y diagnosticos exploratorios...\n')
pred$error <- pred$dias_predichos-pred$dias_reales
metricas <- function(d) {
  er <- d$error; y<-d$dias_reales
  ss <- sum((y-mean(y))^2)
  if (!is.finite(ss) || ss<=0) stop('No se puede calcular R2 en esta muestra')
  c(n=length(er), rmse=sqrt(mean(er^2)), mae=mean(abs(er)),
    r2=1-sum(er^2)/ss, sesgo=mean(er),
    p_subestima=mean(er < 0), p_errores_mayores_7=mean(abs(er)>7))
}
por_fold <- do.call(rbind,lapply(modelos,function(m)do.call(rbind,lapply(1:4,function(f){
  d<-pred[pred$modelo==m & pred$fold==f,,drop=FALSE]
  data.frame(modelo=m,fold=f,as.list(metricas(d)),check.names=FALSE)
}))))
row.names(por_fold)<-NULL
por_fold$n<-as.integer(por_fold$n)
resumen <- do.call(rbind,lapply(modelos,function(m) {
 d<-por_fold[por_fold$modelo==m,,drop=FALSE]
 p<-pred[pred$modelo==m,,drop=FALSE]
 data.frame(modelo=m, rmse_promedio=mean(d$rmse),sd_rmse_folds=sd(d$rmse),
            mae_promedio=mean(d$mae),r2_promedio=mean(d$r2),
            sesgo_promedio=mean(d$sesgo), n_validacion=nrow(p),
            porcentaje_subestimaciones=100*mean(p$error<0),
            porcentaje_entregas_largas=100*mean(p$dias_reales>=30),
            porcentaje_sse_entregas_largas=100*sum((p$error[p$dias_reales>=30])^2)/sum(p$error^2))
}))
resumen<-resumen[order(resumen$rmse_promedio),,drop=FALSE]
row.names(resumen)<-NULL
# Comprobar que el resultado resumido coincide con el certificado numerico de la etapa 7.
archivado <- leer(fuentes[['resumen07']])
for (m in modelos){
 a<-archivado[archivado$modelo==m,,drop=FALSE]
 b<-resumen[resumen$modelo==m,,drop=FALSE]
 if (nrow(a)!=1L || nrow(b)!=1L || max(abs(unlist(a[c('rmse_promedio','mae_promedio','r2_promedio','sesgo_promedio')])-unlist(b[c('rmse_promedio','mae_promedio','r2_promedio','sesgo_promedio')])))>1e-9)
 stop('El recalculo no coincide con etapa 7: ',m)
}

# Segmentacion SOLO diagnostica. No borra datos ni participa en la seleccion.
pred$tramo_real <- cut(pred$dias_reales,breaks=c(-Inf,7,14,30,Inf),
                        labels=c('Menos de 7 dias','7 a menos de 14 dias','14 a menos de 30 dias','30 dias o mas'),right=FALSE)
tramos <- do.call(rbind,lapply(modelos,function(m) do.call(rbind,lapply(levels(pred$tramo_real),function(g){
 d<-pred[pred$modelo==m & pred$tramo_real==g,,drop=FALSE]
 data.frame(modelo=m,tramo_real=g,n=nrow(d),rmse=sqrt(mean(d$error^2)),
            mae=mean(abs(d$error)),sesgo=mean(d$error),
            porcentaje_subestimacion=100*mean(d$error<0))
}))))

# Diferencias emparejadas por fold; solo DESCRIPTIVAS por tener cuatro ventanas.
base_rmse <- por_fold[por_fold$modelo=='Splines_B_anidado',c('fold','rmse')]
pareadas <- merge(por_fold[,c('fold','modelo','rmse')],base_rmse,by='fold',suffixes=c('_modelo','_splines'),sort=FALSE)
pareadas$diferencia_rmse_con_splines<-pareadas$rmse_modelo-pareadas$rmse_splines
pareadas<-pareadas[order(pareadas$modelo,pareadas$fold),]

cat('[5/7] Guardando tablas y una figura sin paquetes adicionales...\n')
salida<-file.path(raiz,'resultados','08_diagnostico_errores')
graficos<-file.path(raiz,'graficos','08_diagnostico_errores')
dir.create(salida,showWarnings=FALSE,recursive=TRUE)
dir.create(graficos,showWarnings=FALSE,recursive=TRUE)
escribir <- function(x,n) utils::write.csv(x,file.path(salida,n),row.names=FALSE,na='')
escribir(resumen,'01_comparacion_resumida.csv')
escribir(por_fold,'02_metricas_por_fold.csv')
escribir(tramos,'03_diagnostico_por_duracion_real.csv')
escribir(pareadas,'04_diferencias_emparejadas_splines.csv')
manifiesto <- data.frame(archivo=names(fuentes),ruta=as.character(fuentes),md5=huellas_antes)
escribir(manifiesto,'05_huellas_entradas.csv')

# Visualizaciones de Etapa 8, incluido el diagnostico explicativo de Splines.
graficar_diagnosticos_08(resumen, tramos, pred, graficos)

cat('[6/7] Verificando integridad de las entradas y archivos escritos...\n')
huellas_despues<-unname(tools::md5sum(fuentes))
if (!identical(huellas_antes,huellas_despues)) stop('Un archivo previo fue modificado durante esta etapa')
archivos_creados<-c(file.path(salida,c('01_comparacion_resumida.csv','02_metricas_por_fold.csv',
 '03_diagnostico_por_duracion_real.csv','04_diferencias_emparejadas_splines.csv','05_huellas_entradas.csv')),
 file.path(graficos,c('01_sesgo_medio_por_modelo.png',
                        '02_error_por_duracion_real_splines.png',
                        '03_media_real_vs_predicha_por_fold.png')))
if (any(!file.exists(archivos_creados)) || any(file.info(archivos_creados)$size<200)) stop('Archivo de salida no valido')
if (any(!is.finite(resumen$rmse_promedio)) || any(!is.finite(por_fold$r2)))stop('Metricas no finitas')

nota<-c(
 'ETAPA 8: DIAGNOSTICO DESCRIPTIVO POSTERIOR A VALIDACION TEMPORAL',
 'Se utilizan exclusivamente predicciones de desarrollo de las etapas 4 a 7; no se ajustan modelos.',
 'Todas las alternativas tienen las mismas 26279 observaciones evaluadas en cuatro folds.',
 'El RMSE y R2 mostrados en la tabla principal son MEDIAS de metricas por fold, no metricas de un conjunto combinado.',
 'El sesgo = prediccion - duracion real. Negativo significa subestimacion.',
 'Los tramos por DURACION REAL se usan solo para diagnostico retrospectivo: no se pueden asignar a un pedido sin saber la entrega.',
 'La concentracion del error cuadratico en entregas largas no autoriza suprimirlas de la evaluacion.',
 'Con cuatro folds temporales no afirmamos superioridad estadistica por pequenas diferencias de RMSE.',
 'Predictores de articulo/vendedor sujetos a supuesto de disponibilidad al aprobar el pedido.',
 'Los periodos de test de julio-agosto de 2018 ya se conocieron en el proyecto historico:',
 'no se consultan ni se consideran test virgen de esta reconstruccion.',
 'Para la defensa: un R2 externo negativo es posible y se conserva en la comparacion.',
 paste('R ejecutado:',R.version.string)
)
writeLines(nota,file.path(salida,'06_interpretacion_y_limitaciones.txt'),useBytes=TRUE)
writeLines(c('ETAPA 8 CERTIFICADA - SOLO DIAGNOSTICO DE DESARROLLO',
              'Mismos IDs y respuestas para nueve modelos; 26279 pedidos por modelo.',
              'Metricas recalculadas desde predicciones y cotejadas con etapa 7.',
              'Sin modificar entradas previas (MD5); sin usar test; sin instalar paquetes.'),
           file.path(salida,'07_CONTROL_ETAPA8_APROBADO.txt'),useBytes=TRUE)
# Diagnostico complementario de Averaging: no reescribe las nueve filas
# historicas ni afecta la comparacion que utiliza la Etapa 10.
fuente_avg <- file.path(raiz,'resultados','07_ensembles','12_averaging_predicciones.csv')
fuente_avg_metrics <- file.path(raiz,'resultados','07_ensembles','14_averaging_resumen.csv')
if (!file.exists(fuente_avg) || !file.exists(fuente_avg_metrics))
  stop('Faltan las salidas de averaging de Etapa 7; ejecute primero 07.')
xa <- leer(fuente_avg)
xa_resumen <- leer(fuente_avg_metrics)
if (nrow(xa) != nrow(ref) || anyDuplicated(clave(xa)) ||
    !setequal(clave(xa),clave(ref))) stop('IDs de Averaging no comparables')
pos <- match(clave(xa),clave(ref))
if (anyNA(pos) || max(abs(xa$dias_reales-ref$dias_reales[pos]))>1e-9)
  stop('Respuesta real de Averaging inconsistente')
xa$error <- xa$dias_predichos-xa$dias_reales
m_av <- do.call(rbind,lapply(1:4,function(k){
  d <- xa[xa$fold==k,,drop=FALSE]
  data.frame(fold=k,as.list(metricas(d)),check.names=FALSE)
}))
ra <- c(rmse_promedio=mean(m_av$rmse),mae_promedio=mean(m_av$mae),
        r2_promedio=mean(m_av$r2),sesgo_promedio=mean(m_av$sesgo))
if (nrow(xa_resumen)!=1L ||
    max(abs(as.numeric(unlist(xa_resumen[1,names(ra)],use.names=FALSE))-unname(ra)))>1e-8)
  stop('Metricas Averaging no coinciden con Etapa 7')
res_av <- data.frame(modelo='Averaging_Splines_Boosting',
  t(ra),n_validacion=nrow(xa),
  porcentaje_subestimaciones=100*mean(xa$error<0),
  porcentaje_sse_entregas_largas=100*sum(xa$error[xa$dias_reales>=30]^2)/sum(xa$error^2))
escribir(res_av,'08_averaging_resumen_diagnostico.csv')
writeLines(c('AVERAGING: DIAGNOSTICO COMPLEMENTARIO REVISADO',
  'Peso fijo 0.5 Splines y 0.5 Boosting. Comparacion retrospectiva.',
  'Mismos pedidos en los cuatro folds; test historico no consultado.',
  'El ranking original de nueve modelos no se reescribe.'),
  file.path(salida,'09_AVERAGING_METODOLOGIA.txt'))
cat('Comparacion complementaria averaging: ver archivo 08_averaging_resumen_diagnostico.csv.\n',
    file=file.path(salida,'07_CONTROL_ETAPA8_APROBADO.txt'),append=TRUE)

cat('[7/7] ETAPA 8 COMPLETADA - DIAGNOSTICOS; SIN MODELOS NI TEST\n')
print(resumen[,c('modelo','rmse_promedio','r2_promedio','sesgo_promedio')],row.names=FALSE)
cat('Resultados: ',salida,'\nGraficos: ',graficos,'\n',sep='')

}
