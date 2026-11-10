# ETAPA 10 - Decision metodologica documentada, sin entrenar ni evaluar test.
# Caso 1 Olist - Estadistica Aplicada (USACH), Grupo 3.
# Dependencias: R base unicamente.
# Ejecucion desde la RAIZ del proyecto:
# source('scripts/10_decision_modelo_y_trazabilidad.R', encoding='UTF-8')
# Se crean exclusivamente resultados/10_decision_modelo y graficos/10_decision_modelo.
# Los resultados 01-09 no se modifican. El modelo no se reentrena ni congela aun.

local({
  raiz <- getwd()
  resultados <- file.path(raiz, 'resultados')
  fuentes <- list(
    comparacion = '07_ensembles/07_comparacion_comun_resumen.csv',
    por_fold = '07_ensembles/06_comparacion_comun_por_fold.csv',
    diagnostico = '08_diagnostico_errores/01_comparacion_resumida.csv',
    selec_variables = '04_seleccion_variables_exploratoria/03_seleccion_interna_por_fold.csv',
    variables = '03_protocolo_y_folds/02_especificaciones_previas.csv',
    folds = '03_protocolo_y_folds/03_resumen_folds_temporales.csv',
    hiper_splines = '06_splines/03_configuracion_seleccionada_por_fold.csv',
    inferencia = '09_inferencia_ols/02_resumen_ajuste_SOLO_train.csv',
    pred_04 = '04_baseline_y_ols/03_predicciones_validacion.csv',
    pred_05 = '05_regularizacion/05_predicciones_validacion_regularizados.csv',
    pred_06 = '06_splines/05_predicciones_splines_por_pedido.csv',
    pred_07 = '07_ensembles/05_predicciones_validacion.csv',
    disponibilidad = '02_variables_candidatas/06_disponibilidad_candidatas.csv'
  )
  ruta_fuente <- function(nombre) file.path(resultados, fuentes[[nombre]])
  faltantes <- vapply(fuentes, function(x) !file.exists(file.path(resultados, x)), logical(1))
  if (any(faltantes)) stop('Faltan insumos de etapas anteriores: ', paste(fuentes[faltantes], collapse='; '))
  certificados <- c(
    '04_baseline_y_ols/11_CONTROL_COMPARABILIDAD_APROBADO.txt',
    '05_regularizacion/12_CONTROL_ETAPA5_APROBADO.txt',
    '06_splines/11_CONTROL_ETAPA6_APROBADO.txt',
    '07_ensembles/11_CONTROL_ETAPA7_APROBADO.txt',
    '08_diagnostico_errores/07_CONTROL_ETAPA8_APROBADO.txt',
    '09_inferencia_ols/09_CONTROL_ETAPA9_APROBADO.txt'
  )
  if (any(!file.exists(file.path(resultados, certificados)))) {
    stop('No se encontraron todos los certificados originales de las etapas 4-9.')
  }
  # Se toman huellas de los archivos previos ANTES de generar nuevos productos.
  anteriores <- list.files(resultados, recursive=TRUE, full.names=TRUE, all.files=FALSE)
  anteriores <- anteriores[file.exists(anteriores) & !dir.exists(anteriores)]
  rel <- substring(anteriores, nchar(normalizePath(resultados, winslash='/', mustWork=TRUE))+2L)
  es_nuevo <- grepl('^10_decision_modelo/', gsub('\\\\','/',rel))
  anteriores <- anteriores[!es_nuevo]
  rel <- rel[!es_nuevo]
  huella_antes <- unname(tools::md5sum(anteriores))
  names(huella_antes) <- rel
  if (anyNA(huella_antes)) stop('No se pudieron calcular las huellas de todas las entradas.')
  cat('[1/7] Leyendo las comparaciones verificadas y protegiendo los archivos anteriores...\n')
  leer <- function(nombre) read.csv(ruta_fuente(nombre), stringsAsFactors=FALSE,
                                    check.names=FALSE, fileEncoding='UTF-8')
  resumen <- leer('comparacion')
  folds <- leer('por_fold')
  diag <- leer('diagnostico')
  seleccion <- leer('selec_variables')
  especificaciones <- leer('variables')
  calendario <- leer('folds')
  hiper <- leer('hiper_splines')
  inferencia <- leer('inferencia')
  disp <- leer('disponibilidad')
  requerido_res <- c('modelo','rmse_promedio','sd_rmse_folds','mae_promedio','r2_promedio','sesgo_promedio')
  requerido_fold <- c('fold','modelo','n_validacion','rmse','mae','r2','sesgo')
  if (!all(requerido_res %in% names(resumen)) || !all(requerido_fold %in% names(folds))) {
    stop('Las tablas 07 no contienen las columnas esperadas.')
  }
  modelos <- c('Splines_B_anidado','Ridge_min','Lasso_min','ElasticNet_min',
               'B_pedido_geografia','Boosting_B_anidado','Arbol_B_anidado',
               'RandomForest_B_anidado','Baseline_media_train')
  if (nrow(resumen)!=9L || !setequal(resumen$modelo, modelos) ||
      anyDuplicated(resumen$modelo)) stop('Comparacion general incompleta o duplicada.')
  if (nrow(folds)!=36L || !setequal(folds$fold,1:4) ||
      anyDuplicated(paste(folds$fold,folds$modelo,sep='::'))) {
    stop('Las 36 combinaciones modelo-fold no son unicas o completas.')
  }
  if (!all(vapply(split(folds$modelo, folds$fold),
                  function(x) setequal(x, modelos), logical(1)))) {
    stop('Faltan modelos en al menos un fold.')
  }
  for (f in 1:4) {
    g <- folds[folds$fold==f,]
    if (length(unique(g$n_validacion)) != 1L ||
        unique(g$n_validacion) != calendario$n_validacion[calendario$fold==f]) {
      stop('Numero de observaciones de validacion incompatible en fold ',f)
    }
  }
  # Auditoria estricta a nivel de order_id: las nueve alternativas deben predecir
  # exactamente las mismas 26.279 observaciones, con la misma respuesta real.
  # Esta auditoria NO lee registros del test historico.
  cat('      Contraste de IDs, etiquetas y metricas desde predicciones por pedido...\n')
  pred04 <- leer('pred_04'); pred05 <- leer('pred_05')
  pred06 <- leer('pred_06'); pred07 <- leer('pred_07')
  pred <- rbind(pred04[,c('fold','order_id','modelo','dias_reales','dias_predichos','error_predicho_menos_real')],
                pred05[,c('fold','order_id','modelo','dias_reales','dias_predichos','error_predicho_menos_real')],
                pred06[,c('fold','order_id','modelo','dias_reales','dias_predichos','error_predicho_menos_real')],
                pred07[,c('fold','order_id','modelo','dias_reales','dias_predichos','error_predicho_menos_real')])
  pred <- pred[pred$modelo %in% modelos,]
  if (nrow(pred)!=26279L*9L || anyNA(pred[,c('fold','order_id','modelo','dias_reales','dias_predichos')])) {
    stop('No se recuperaron exactamente 9 x 26.279 predicciones validas.')
  }
  llave <- paste(pred$fold,pred$modelo,pred$order_id,sep='::')
  if (anyDuplicated(llave)) stop('Hay predicciones duplicadas por fold-modelo-pedido.')
  if (max(abs(pred$dias_predichos-pred$dias_reales-
              pred$error_predicho_menos_real))>1e-8) {
    stop('Errores individuales incompatibles con las predicciones.')
  }
  base <- pred[pred$modelo=='B_pedido_geografia', c('fold','order_id','dias_reales')]
  llave_base <- paste(base$fold,base$order_id,sep='::')
  if (nrow(base)!=26279L || anyDuplicated(llave_base)) stop('Referencia OLS B incompleta.')
  for (m in modelos) {
    z <- pred[pred$modelo==m,]
    pos <- match(paste(z$fold,z$order_id,sep='::'),llave_base)
    if (nrow(z)!=nrow(base) || anyNA(pos) ||
        max(abs(z$dias_reales-base$dias_reales[pos]))>1e-8) {
      stop('No coinciden IDs o etiquetas con OLS B para: ',m)
    }
    for (f in 1:4) {
      g <- z[z$fold==f,]
      if (nrow(g)!=unique(folds$n_validacion[folds$fold==f])) {
        stop('Cantidad de pedidos no coincide para ',m,' fold ',f)
      }
      obs <- folds[folds$modelo==m & folds$fold==f,]
      e <- g$dias_predichos-g$dias_reales
      rmse_pred <- sqrt(mean(e^2))
      mae_pred <- mean(abs(e))
      r2_pred <- 1-sum(e^2)/sum((g$dias_reales-mean(g$dias_reales))^2)
      if (max(abs(c(rmse_pred-obs$rmse,mae_pred-obs$mae,
                    r2_pred-obs$r2,mean(e)-obs$sesgo)))>1e-8) {
        stop('Metricas por fold no reproducibles para ',m,' fold ',f)
      }
    }
  }
  # No se conservan predicciones cargadas para reducir memoria.
  rm(pred,pred04,pred05,pred06,pred07,base,llave,llave_base)
  num <- c('rmse','mae','r2','sesgo')
  if (any(!is.finite(as.matrix(folds[,num]))) || any(folds$rmse<0) || any(folds$mae<0)) {
    stop('Metrícas faltantes, infinitas o inadmisibles.')
  }
  cat('[2/7] Recalculando cada promedio, dispersion y metrica por fold...\n')
  por_modelo <- split(folds, folds$modelo)
  calcula <- do.call(rbind,lapply(por_modelo, function(x) {
    data.frame(modelo=x$modelo[1], rmse_promedio=mean(x$rmse),
               sd_rmse_folds=sd(x$rmse), mae_promedio=mean(x$mae),
               r2_promedio=mean(x$r2), sesgo_promedio=mean(x$sesgo),
               stringsAsFactors=FALSE)
  }))
  rownames(calcula) <- NULL
  contraste <- merge(resumen,calcula,by='modelo',suffixes=c('_archivado','_recalculado'),sort=FALSE)
  if (nrow(contraste)!=9L) stop('Error al contrastar las nueve alternativas.')
  columnas_metricas <- setdiff(requerido_res,'modelo')
  diferencias <- unlist(lapply(columnas_metricas,function(n) {
    abs(contraste[[paste0(n,'_archivado')]] - contraste[[paste0(n,'_recalculado')]])
  }))
  tolerancia <- 1e-8
  if (any(!is.finite(diferencias)) || max(diferencias)>tolerancia) {
    stop('Los promedios archivados NO se reproducen desde las metricas por fold.')
  }
  if (!all(c('modelo','rmse_promedio','r2_promedio','sesgo_promedio') %in% names(diag))) {
    stop('No se puede contrastar la etapa de diagnosticos 08.')
  }
  contraste08 <- merge(resumen,diag,by='modelo',suffixes=c('_07','_08'),sort=FALSE)
  if (nrow(contraste08)!=9L ||
      max(abs(contraste08$rmse_promedio_07-contraste08$rmse_promedio_08))>tolerancia ||
      max(abs(contraste08$r2_promedio_07-contraste08$r2_promedio_08))>tolerancia) {
    stop('Las etapas 07 y 08 no coinciden en RMSE o R2.')
  }
  cat('[3/7] Comprobando seleccion de variables, protocolos y limite de inferencia...\n')
  if (nrow(seleccion)!=4L || !all(seleccion$especificacion=='B_original_10') ||
      !all(seleccion$n_variables==10L) || !setequal(seleccion$fold_externo,1:4)) {
    stop('La seleccion B de 10 variables no coincide con los resultados archivados.')
  }
  esp_b <- especificaciones[especificaciones$especificacion=='B_pedido_geografia',]
  if (nrow(esp_b)!=1L || esp_b$numero_variables!=10L) {
    stop('Especificacion B original no identificada inequívocamente.')
  }
  if (nrow(hiper)!=4L || !setequal(hiper$fold_externo,1:4)) {
    stop('Configuraciones anidadas de Splines incompletas.')
  }
  if (nrow(inferencia)!=1L || inferencia$n_train!=46300L) {
    stop('La inferencia OLS no corresponde al entrenamiento del cuarto fold.')
  }
  if (!('condicion' %in% names(disp))) stop('Falta el registro de disponibilidad.')
  if (!any(grepl('VERIFICAR',disp$condicion,fixed=TRUE))) {
    stop('La disponibilidad historica de ciertos predictores debe quedar advertida.')
  }
  # La decision se elabora retrospectivamente sobre VALIDACION; NO es evidencia de test nuevo.
  resumen <- resumen[order(resumen$rmse_promedio),,drop=FALSE]
  rownames(resumen) <- NULL
  elegido <- as.character(resumen$modelo[1])
  if (elegido!='Splines_B_anidado') stop('El lider observado ha cambiado; revisar antes de decidir.')
  r_s <- resumen$rmse_promedio[resumen$modelo==elegido]
  r_base <- resumen$rmse_promedio[resumen$modelo=='Baseline_media_train']
  r_ols <- resumen$rmse_promedio[resumen$modelo=='B_pedido_geografia']
  mejora_baseline <- 100*(1-r_s/r_base)
  comp <- resumen
  comp$delta_rmse_vs_splines <- comp$rmse_promedio-r_s
  comp$mejora_rmse_vs_media_historica_pct <- 100*(1-comp$rmse_promedio/r_base)
  comp$ranking_descriptivo <- seq_len(nrow(comp))
  # Contrastes por fold emparejados, sin p-values no sustentados por cuatro folds.
  ref <- folds[folds$modelo==elegido,c('fold','rmse','mae','r2')]
  names(ref)[-1] <- c('rmse_splines','mae_splines','r2_splines')
  dif <- merge(folds, ref, by='fold', sort=FALSE)
  dif$delta_rmse_vs_splines <- dif$rmse-dif$rmse_splines
  dif$delta_mae_vs_splines <- dif$mae-dif$mae_splines
  dif <- dif[order(match(dif$modelo, modelos),dif$fold),]
  resumen_delta <- aggregate(dif$delta_rmse_vs_splines,
                            by=list(modelo=dif$modelo), FUN=function(x) sum(x>0))
  names(resumen_delta)[2] <- 'folds_peor_rmse_que_splines'
  comp <- merge(comp,resumen_delta,by='modelo',all.x=TRUE,sort=FALSE)
  comp <- comp[order(comp$ranking_descriptivo),]
  # Salidas aisladas: jamas escribir en las etapas anteriores.
  salida <- file.path(resultados,'10_decision_modelo')
  graficos <- file.path(raiz,'graficos','10_decision_modelo')
  dir.create(salida, recursive=TRUE, showWarnings=FALSE)
  dir.create(graficos, recursive=TRUE, showWarnings=FALSE)
  escribir_csv <- function(d,n) write.csv(d,file.path(salida,n),row.names=FALSE,na='')
  escribir_txt <- function(lines,n) writeLines(enc2utf8(lines),file.path(salida,n),useBytes=TRUE)
  cat('[4/7] Guardando comparaciones y decision metodologica retrospectiva...\n')
  escribir_csv(comp,'01_comparacion_y_ranking_descriptivo.csv')
  escribir_csv(dif,'02_diferencias_emparejadas_por_fold.csv')
  decision <- data.frame(
    tipo_decision=c('Candidato principal','Modelo para interpretacion','Referencia simple'),
    modelo=c('Splines_B_anidado','B_pedido_geografia','Baseline_media_train'),
    razon=c('Menor RMSE medio de los cuatro folds externos de desarrollo; ventaja pequena frente a Ridge y OLS.',
            'Regresion lineal interpretable, con inferencia HC3 condicionada a supuestos y limitaciones.',
            'Media historica calculada solamente con entrenamiento; comparador operativo minimo.'),
    estado=c('Recomendacion retrospectiva; NO es un modelo final ya reentrenado ni un test independiente.',
             'Apoyo explicativo; no sustituye la validacion predictiva.',
             'Comparador; no utilizar la media real del mes futuro.'),
    stringsAsFactors=FALSE)
  escribir_csv(decision,'03_decision_y_alternativas.csv')
  escribir_csv(data.frame(fold_externo=hiper$fold_externo,
                         configuracion_seleccionada=hiper$configuracion,
                         df=hiper$df,interaccion_geo=hiper$interaccion_geo,
                         rmse_interno=hiper$rmse_interno),
              '04_configuracion_splines_por_fold.csv')
  texto <- c(
    'ETAPA 10 - DECISION METODOLOGICA RETROSPECTIVA SOBRE DESARROLLO',
    'Caso 1 Olist | Grupo 3 | Estadistica Aplicada',
    '',
    'OBJETIVO Y ALCANCE',
    'Se documenta una recomendacion a partir de metricas archivadas, no se ajusta un nuevo modelo.',
    'Los cuatro meses de validacion son diciembre 2017 y enero-marzo 2018.',
    'Se comparan nueve alternativas en la misma especificacion B cuando corresponde:',
    '10 variables originales, con folds externos comunes y ajustes de hiperparametros internos.',
    '',
    'ELECCION DOCUMENTADA',
    'Candidato de mejor RMSE medio observado: Splines_B_anidado.',
    sprintf('RMSE medio de Splines: %.6f dias.',r_s),
    sprintf('R2 medio de Splines: %.6f.',resumen$r2_promedio[resumen$modelo==elegido]),
    sprintf('RMSE de OLS B: %.6f dias; diferencia OLS menos Splines: %.6f dias.', r_ols, r_ols-r_s),
    sprintf('RMSE de baseline media historica: %.6f dias; mejora relativa Splines: %.3f%%.',r_base,mejora_baseline),
    'La ventaja frente a OLS/Ridge es pequena; no se presenta como superioridad estadistica demostrada.',
    'Los cuatro folds no son replicaciones independientes: hay ventanas de entrenamiento superpuestas.',
    'No se calcula valor p para comparar modelos con cuatro folds correlacionados.',
    '',
    'VARIABLES Y AJUSTE',
    'La seleccion exploratoria con validacion interna conservo las 10 variables B en los cuatro folds.',
    'La matriz de correlaciones sirve como diagnostico; correlacion no equivale a importancia predictiva.',
    'La eleccion de grados de libertad/interacciones de Splines VARIO por fold:',
    paste(sprintf('fold %s: %s',hiper$fold_externo,hiper$configuracion),collapse='; '),
    'Por tanto, NO corresponde congelar automaticamente una de esas cuatro configuraciones.',
    'Para un ajuste final sobre desarrollo completo debe realizarse una nueva seleccion interna temporal',
    'y guardar formula, receta de predictores, transformaciones, objeto RDS, semillas y huellas MD5.',
    '',
    'RESTRICCIONES QUE DEBEN DECLARARSE',
    '1) Los resultados corresponden a VALIDACION DE DESARROLLO y su inspeccion ha guiado revisiones posteriores.',
    '2) El test julio-agosto 2018 ya se evaluo en el proyecto antiguo: NO es un nuevo test independiente.',
    '3) La evaluacion verdaderamente independiente requiere nuevos datos o acuerdo explicito con el profesor.',
    '4) Varios predictores B tienen disponibilidad historica por registro NO demostrada.',
    '5) La respuesta se calcula solo sobre pedidos entregados: hay sesgo de seleccion posible.',
    '6) Las entregas mas largas presentan grandes errores y sesgo de subestimacion.',
    '7) La inferencia OLS HC3 del fold 4 no prueba que Splines sea causal ni superior.',
    '',
    'DECISION FINAL DEL PROYECTO',
    'Candidato prioritario para refinamiento/documentacion: Splines_B_anidado (especificacion B).',
    'Comparador interpretable y respaldo: OLS B; comparador minimo: media historica.',
    'ESTADO: decision sobre desarrollo documentada; ajuste final nuevo y test independiente PENDIENTES.',
    'No modificar resultados archivados ni reconsultar el test antiguo para optimizar.',
    ''
  )
  escribir_txt(texto,'05_acta_seleccion_y_limitaciones.txt')
  cat('[5/7] Dibujando una comparacion emparejada y clara de RMSE por fold...\n')
  png_file <- file.path(graficos,'01_diferencias_rmse_con_splines_por_fold.png')
  modelos_grafico <- c('Ridge_min','Lasso_min','ElasticNet_min','B_pedido_geografia',
                       'Boosting_B_anidado','Arbol_B_anidado','RandomForest_B_anidado')
  nombres <- c('Ridge','Lasso','Elastic-Net','OLS B','Gradient Boosting',
               'Arbol CART','Random Forest')
  colores <- c('#23659A','#318B87','#DC8A34','#7A5A98')
  dibujo <- function() {
    par(mar=c(6,11,4.9,2.5), xaxs='r',yaxs='i',family='sans')
    x <- dif[dif$modelo %in% modelos_grafico,]
    vx <- x$delta_rmse_vs_splines
    xr <- range(c(0,vx), finite=TRUE)
    ancho <- diff(xr)
    xr <- xr + c(-0.09,0.12)*max(ancho,0.15)
    plot(NA,NA,xlim=xr,ylim=c(0.4,length(modelos_grafico)+0.6),
         axes=FALSE, xlab='',ylab='',main='Comparacion emparejada de RMSE por mes',
         sub='',cex.main=1.24,col.main='#173553')
    abline(v=0,col='#334155',lwd=1.6)
    abline(h=seq_along(modelos_grafico),col='#EDF1F5',lwd=1)
    eje <- pretty(xr,n=6)
    axis(1,at=eje,labels=format(round(eje,2),decimal.mark=',',trim=TRUE),
         cex.axis=0.85,col.axis='#425466')
    axis(2, at=rev(seq_along(modelos_grafico)),labels=nombres,las=1,
         tick=FALSE,cex.axis=0.97,col.axis='#173553',hadj=1)
    for(j in seq_along(modelos_grafico)) {
      m <- modelos_grafico[j]
      datos <- x[x$modelo==m,]
      for(i in seq_len(nrow(datos))) {
        k <- as.integer(datos$fold[i]); ypos <- length(modelos_grafico)-j+1+(k-2.5)*0.105
        points(datos$delta_rmse_vs_splines[i],ypos,pch=19,cex=1.10,col=colores[k])
      }
    }
    mtext('RMSE de alternativa menos RMSE de Splines (dias)',side=1,line=3,cex=1.05,col='#173553')
    legend('topright',legend=c('Dic 2017','Ene 2018','Feb 2018','Mar 2018'),
           pch=19,col=colores,bty='n',cex=0.78,horiz=TRUE,inset=c(0,0.04))
    mtext('Valores positivos: Splines tuvo menor RMSE en ese mes. Valores negativos: la alternativa tuvo menor RMSE.',
          side=1,line=4.6,cex=0.71,col='#607086')
    box(bty='l',col='#BCC8D4')
  }
  grDevices::png(filename=png_file,width=1800,height=960,res=160,bg='white')
  tryCatch(dibujo(),finally=grDevices::dev.off())
  if (!file.exists(png_file) || file.info(png_file)$size<2000) stop('No se genero el grafico PNG.')
  cat('[6/7] Certificando que las etapas anteriores permanecen intactas...\n')
  huella_despues <- unname(tools::md5sum(anteriores))
  if (anyNA(huella_despues) || !identical(as.character(huella_antes),as.character(huella_despues))) {
    stop('ALERTA: cambiaron archivos de resultados anteriores durante la Etapa 10.')
  }
  manifiesto <- data.frame(archivo_original=rel,md5=unname(huella_antes),stringsAsFactors=FALSE)
  escribir_csv(manifiesto,'06_manifiesto_resultados_anteriores_MD5.csv')
  decision_md5 <- unname(tools::md5sum(file.path(salida,'05_acta_seleccion_y_limitaciones.txt')))
  escribir_txt(c('CONTROL ETAPA 10: APROBADO',
                paste('Archivos de etapas anteriores sin cambios (MD5):',length(anteriores)),
                paste('Modelos y folds:',nrow(folds),'=',length(modelos),'x 4'),
                'Predicciones comprobadas por order_id y respuesta real: 236511',
                paste('Diferencia maxima entre resumen y folds:',format(max(diferencias),digits=5)),
                paste('Modelo con menor RMSE medio observado:',elegido),
                paste('MD5 del acta de decision:',decision_md5),
                'No se reentreno ningun modelo.',
                'No se leyo ni evaluo el test julio-agosto 2018.',
                'No se genero un modelo predictivo final nuevo: esa etapa esta pendiente.'),
              '07_CONTROL_ETAPA10_APROBADO.txt')
  # Decima alternativa agregada como comparacion RETROSPECTIVA, sin
  # cambiar la eleccion documentada de Splines ni la receta del RDS congelado.
  avruta <- file.path(resultados,'07_ensembles','14_averaging_resumen.csv')
  avdiag <- file.path(resultados,'08_diagnostico_errores','08_averaging_resumen_diagnostico.csv')
  avcert <- file.path(resultados,'07_ensembles','16_CONTROL_AVERAGING_APROBADO.txt')
  if (!all(file.exists(c(avruta,avdiag,avcert))))
    stop('Faltan resultados certificados del Averaging en Etapas 07 y 08.')
  av <- read.csv(avruta,stringsAsFactors=FALSE,check.names=FALSE)
  avd <- read.csv(avdiag,stringsAsFactors=FALSE,check.names=FALSE)
  if(nrow(av)!=1L || nrow(avd)!=1L ||
     !identical(as.character(av$modelo), 'Averaging_Splines_Boosting') ||
     max(abs(as.numeric(unlist(av[1,c('rmse_promedio','mae_promedio','r2_promedio','sesgo_promedio')],use.names=FALSE))-
             as.numeric(unlist(avd[1,c('rmse_promedio','mae_promedio','r2_promedio','sesgo_promedio')],use.names=FALSE))))>1e-8)
    stop('Averaging no coincide con el diagnostico de Etapa 08')
  diez <- rbind(resumen[,requerido_res],av[,requerido_res])
  diez <- diez[order(diez$rmse_promedio),,drop=FALSE]
  diez$ranking_descriptivo <- seq_len(nrow(diez))
  diez$estado <- ifelse(diez$modelo=='Averaging_Splines_Boosting',
     'Combinacion 50/50 complementaria, analizada retrospectivamente',
     'Alternativa original de desarrollo')
  if (nrow(diez)!=10L || anyDuplicated(diez$modelo))
    stop('La tabla de diez alternativas quedo incompleta')
  escribir_csv(diez,'08_comparacion_10_alternativas.csv')
  diferencia <- av$rmse_promedio-r_s
  escribir_txt(c('ETAPA 10 - ADDENDUM DE AVERAGING COMPLEMENTARIO',
     sprintf('RMSE averaging 50/50: %.8f dias',av$rmse_promedio),
     sprintf('RMSE Splines: %.8f dias',r_s),
     sprintf('Diferencia averaging menos Splines: %.8f dias',diferencia),
     'El averaging no cambia el modelo RDS congelado Splines.',
     'La eleccion de las dos familias es retrospectiva al examen de desarrollo.',
     'No se usa test historico ni se demuestra superioridad estadistica.',
     'Las tablas historicas con nueve modelos se conservan como evidencia.'),
     '09_averaging_decision_complementaria.txt')
  # Puntos ordenados, etiquetas directas, cero visual para comparar diez modelos.
  img10 <- file.path(graficos,'02_rmse_diez_alternativas.png')
  grDevices::png(img10,width=1800,height=1020,res=160,bg='white')
  tryCatch({
    op <- par(mar=c(6,18,5,5),family='sans')
    gg <- diez[order(diez$rmse_promedio,decreasing=TRUE),]
    nam <- c(Splines_B_anidado='Splines',Ridge_min='Ridge',Lasso_min='Lasso',
       ElasticNet_min='Elastic-Net',B_pedido_geografia='OLS B',
       Boosting_B_anidado='Gradient Boosting',Arbol_B_anidado='Arbol CART',
       RandomForest_B_anidado='Random Forest',Baseline_media_train='Media historica',
       Averaging_Splines_Boosting='Averaging 50/50')
    yp <- seq_len(nrow(gg))
    plot(gg$rmse_promedio,yp,xlim=c(0,max(gg$rmse_promedio)*1.12),
         ylim=c(0.5,nrow(gg)+0.5),yaxt='n',xlab='RMSE medio (dias; menor es mejor)',
         ylab='',pch=19,cex=1.5,col=ifelse(gg$modelo=='Averaging_Splines_Boosting',
         '#147F7C','#567A9E'),bty='n',main='Comparacion de diez alternativas en desarrollo')
    axis(2,at=yp,labels=nam[gg$modelo],las=1,tick=FALSE)
    abline(h=yp,col='#F0F3F6')
    points(gg$rmse_promedio,yp,pch=19,cex=1.5,
           col=ifelse(gg$modelo=='Averaging_Splines_Boosting','#147F7C','#567A9E'))
    text(gg$rmse_promedio+0.13,yp,labels=sprintf('%.3f',gg$rmse_promedio),
         pos=4,cex=0.82,col='#213D56',xpd=NA)
    mtext('Averaging es una comparacion retrospectiva; diferencias pequenas no prueban superioridad.',
          side=1,line=4.5,cex=0.72,col='#566A7F')
    par(op)
  },finally=grDevices::dev.off())
  if (!file.exists(img10) || file.info(img10)$size<2000)
    stop('No se genero el grafico de diez alternativas')
  cat('Modelo con menor RMSE medio (diez alternativas, comparacion retrospectiva): ',
      diez$modelo[1], '\nLa seleccion de candidato Splines se mantiene de acuerdo con el protocolo previo.\n',
      file=file.path(salida,'07_CONTROL_ETAPA10_APROBADO.txt'),append=TRUE,sep='')

  cat('[7/7] ETAPA 10 COMPLETADA - SELECCION METODOLOGICA DOCUMENTADA; SIN TEST.\n')
  cat('Candidato por minimo RMSE medio: ',elegido,'\n',sep='')
  cat(sprintf('RMSE: %.4f dias | R2 medio: %.4f | diferencia respecto OLS B: %.4f dias\n',
              r_s, resumen$r2_promedio[resumen$modelo==elegido],r_ols-r_s))
  cat(sprintf('Mejora frente a media historica: %.2f%% en RMSE de validacion.\n',mejora_baseline))
  cat('Estado: seleccion retrospectiva en desarrollo, NO test independiente ni modelo final reentrenado.\n')
  cat('Salidas: ',salida,'\nGrafico: ',png_file,'\n',sep='')
})
