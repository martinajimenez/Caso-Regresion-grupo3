# =============================================================================
# USACH | Estadistica Aplicada | Grupo 3 | Caso 1: Regresion Olist
# ETAPA 11 - Ajuste del candidato Splines B, receta portable y trazabilidad
# Entrenar UNA vez a partir de desarrollo maduro al 2018-07-01 (75.172 pedidos).
# Seleccion final de complejidad: cuatro ventanas historicas de DESARROLLO.
# ATENCION: las ventanas ya se examinaron en el estudio; NO son test nuevo.
# R base + splines (paquete recomendado incluido con R).
# Ejecutar DESDE la raiz de Proyecto_Olist_caso1:
# source('scripts/11_ajuste_candidato_splines_y_reproducibilidad.R', encoding='UTF-8')
# El modelo resultante es un CANDIDATO; no se infiere rendimiento test nuevo.
# =============================================================================
local({
  set.seed(20261023L)
  stopifnot(requireNamespace('splines', quietly=TRUE))
  raiz <- normalizePath(getwd(), winslash='/', mustWork=TRUE)
  ruta <- function(...) file.path(raiz, ...)
  salida <- ruta('resultados', '11_modelo_candidato')
  dir.create(salida, showWarnings=FALSE, recursive=TRUE)
  cert <- file.path(salida, '07_CONTROL_ETAPA11_APROBADO.txt')
  if (file.exists(cert)) stop('La Etapa 11 ya esta certificada. No sobrescribir un candidato congelado. Conserve su RDS.',call.=FALSE)
  ok <- function(p, mensaje) if (!isTRUE(p)) stop(mensaje, call.=FALSE)
  leer <- function(p) utils::read.csv(p, stringsAsFactors=FALSE, check.names=FALSE, na.strings=c('', 'NA'))
  guardar <- function(tabla, n) utils::write.csv(tabla,file.path(salida,n),row.names=FALSE,na='')
  fecha_utc <- function(x) as.POSIXct(x,format='%Y-%m-%d %H:%M:%S',tz='UTC')
  metrica <- function(y, p) {
    ok(length(y)==length(p) && length(y)>1L && all(is.finite(y)) && all(is.finite(p)), 'Predicciones o respuestas invalidas')
    c(rmse=sqrt(mean((p-y)^2)),mae=mean(abs(p-y)),sesgo=mean(p-y))
  }
  entradas <- c(
    desarrollo=ruta('resultados','02_variables_candidatas','01_variables_candidatas_desarrollo.csv'),
    maduras=ruta('resultados','02_variables_candidatas','03_etiquetas_maduras_45d_referencia.csv'),
    etiquetas=ruta('resultados','01_revision_datos','06_etiquetas_solo_desarrollo.csv'),
    fold=ruta('resultados','03_protocolo_y_folds','03_resumen_folds_temporales.csv'),
    ids=ruta('resultados','03_protocolo_y_folds','04_ids_train_validacion.csv'),
    especificaciones=ruta('resultados','03_protocolo_y_folds','02_especificaciones_previas.csv'),
    configuracion6=ruta('resultados','06_splines','03_configuracion_seleccionada_por_fold.csv'),
    predicciones6=ruta('resultados','06_splines','05_predicciones_splines_por_pedido.csv'),
    eleccion10=ruta('resultados','10_decision_modelo','07_CONTROL_ETAPA10_APROBADO.txt')
  )
  ok(all(file.exists(entradas)), paste('Faltan entradas: ',paste(names(entradas)[!file.exists(entradas)],collapse=', ')))
  ok(any(grepl('Modelo con menor RMSE medio observado: Splines_B_anidado',
               readLines(entradas[['eleccion10']],warn=FALSE),fixed=TRUE)),
     'Etapa 10 no certifica Splines como candidato; detener y revisar')
  cat('[1/8] Verificando entradas, IDs y la decision documentada...\n')
  b<-leer(entradas[['desarrollo']]); maduras<-leer(entradas[['maduras']]); etiq<-leer(entradas[['etiquetas']])
  ids<-leer(entradas[['ids']]); folds<-leer(entradas[['fold']]); specs<-leer(entradas[['especificaciones']])
  cfg6<-leer(entradas[['configuracion6']]); pred6<-leer(entradas[['predicciones6']])
  ok(nrow(b)==86162L && nrow(maduras)==75172L && nrow(etiq)==81950L && nrow(folds)==4L,
     'Conteos distintos a los certificados de etapas previas')
  ok(!anyDuplicated(b$order_id) && !anyDuplicated(maduras$order_id) && !anyDuplicated(etiq$order_id),
     'Existen order_id duplicados')
  original_B <- c('hora_aprobacion','dia_semana','mes_aprobacion','horas_desde_compra',
                  'estado_cliente','n_items','n_vendedores','precio_total_brl',
                  'flete_total_brl','todos_vendedores_mismo_estado')
  filaB<-specs$variables[specs$especificacion=='B_pedido_geografia']
  ok(length(filaB)==1L && setequal(trimws(strsplit(filaB, ';', fixed=TRUE)[[1L]]),original_B),
     'La especificacion B ya no coincide con los diez predictores originales')
  ok(all(original_B %in% names(b)), 'Faltan variables de entrada del modelo B')
  aprobacion<-fecha_utc(b$fecha_aprobacion)
  ok(!anyNA(aprobacion) && all(aprobacion < fecha_utc('2018-07-01 00:00:00')),
     'Se encontraron fechas desconocidas o posteriores al corte')
  y<-etiq$dias_entrega[match(b$order_id,etiq$order_id)]
  entrega<-as.POSIXct(as.numeric(aprobacion)+86400*y,origin='1970-01-01',tz='UTC')
  ok(!anyNA(match(maduras$order_id,b$order_id)) &&
     max(abs(y[match(maduras$order_id,b$order_id)]-maduras$dias_entrega))<1e-9,
     'Las etiquetas maduras difieren del desarrollo original')

  # Huella de todos los resultados anteriores (no tocar ni modificar).
  raiz_resultados<-ruta('resultados')
  previo<-list.files(raiz_resultados,recursive=TRUE,full.names=TRUE,include.dirs=FALSE)
  previo<-previo[file.exists(previo) & !dir.exists(previo)]
  previo<-previo[!startsWith(normalizePath(previo,winslash='/',mustWork=TRUE),
                            normalizePath(salida,winslash='/',mustWork=TRUE))]
  md5_antes<-unname(tools::md5sum(previo))
  ok(all(!is.na(md5_antes)), 'No es posible registrar integridad de entradas')
  nombres_previos<-substring(normalizePath(previo,winslash='/',mustWork=TRUE),
                             nchar(normalizePath(raiz_resultados,winslash='/',mustWork=TRUE))+2L)

  cat('[2/8] Reconstruyendo la receta de las diez variables del OLS B...\n')
  receta <- function(d) {
    tab<-table(d$estado_cliente)
    freq<-names(tab)[tab>=100L]
    if(!length(freq)) freq<-names(tab)[which.max(tab)]
    agr<-ifelse(is.na(d$estado_cliente)|!(d$estado_cliente %in% freq),'OTROS',d$estado_cliente)
    lev<-sort(unique(agr))
    fallback<-if('OTROS' %in% lev) 'OTROS' else names(tab)[which.max(tab)]
    list(freq=freq,lev=lev,fallback=fallback)
  }
  matrizB <- function(d, rec) {
    ok(is.data.frame(d) && nrow(d)>0L && all(original_B %in% names(d)),
       'Datos de prediccion incompletos: se requieren las diez columnas B')
    h<-as.numeric(d$hora_aprobacion); s<-as.numeric(d$dia_semana)
    m<-as.numeric(d$mes_aprobacion); hc<-as.numeric(d$horas_desde_compra)
    ok(all(is.finite(h)&h>=0&h<=23) && all(is.finite(s)&s>=1&s<=7) &&
       all(is.finite(m)&m>=1&m<=12) && all(is.finite(hc)&hc>=0),
       'Fechas/horas transformadas invalidas')
    x<-cbind(hora_sin=sin(2*pi*h/24),hora_cos=cos(2*pi*h/24),
             dia_sin=sin(2*pi*(s-1)/7),dia_cos=cos(2*pi*(s-1)/7),
             mes_sin=sin(2*pi*(m-1)/12),mes_cos=cos(2*pi*(m-1)/12),
             log_horas_compra=log1p(hc))
    g0<-as.character(d$estado_cliente)
    grupo<-factor(ifelse(is.na(g0)|!(g0 %in% rec$freq),rec$fallback,g0),levels=rec$lev)
    ok(!anyNA(grupo),'Estado de cliente incompatible con el entrenamiento')
    if(length(rec$lev)>1L){
      D<-stats::model.matrix(~grupo)[,-1L,drop=FALSE]
      colnames(D)<-paste0('estado_',make.names(rec$lev[-1L]))
      x<-cbind(x,D)
    }
    for(z in c('n_items','n_vendedores','precio_total_brl','flete_total_brl','todos_vendedores_mismo_estado')) {
      vv<-as.numeric(d[[z]])
      ok(all(is.finite(vv)&vv>=0),paste('Faltantes o negativos en',z))
    }
    geo<-as.numeric(d$todos_vendedores_mismo_estado)
    ok(all(geo %in% c(0,1)),'Geografia binaria invalida')
    x<-cbind(x,log_items=log1p(as.numeric(d$n_items)),
             log_vendedores=log1p(as.numeric(d$n_vendedores)),
             log_precio=log1p(as.numeric(d$precio_total_brl)),
             log_flete=log1p(as.numeric(d$flete_total_brl)),
             vendedor_mismo_estado=geo)
    storage.mode(x)<-'double'
    ok(nrow(x)==nrow(d) && all(is.finite(x)), 'Matriz de predictores invalida')
    as.data.frame(x,check.names=FALSE)
  }
  # df es el mismo numero de bases de los splines naturales de la Etapa 6.
  formula_spline <- function(df, geo) {
    texto<-paste0('dias ~ . - log_precio - log_flete + ',
                  'splines::ns(log_precio, df=',df,') + ',
                  'splines::ns(log_flete, df=',df,')')
    if(geo) texto<-paste0(texto,' + vendedor_mismo_estado:log_flete')
    f<-stats::as.formula(texto)
    environment(f)<-baseenv()  # evitar serializar toda la sesion de entrenamiento
    f
  }
  ajustar <- function(x, y, df, geo) {
    ajuste<-stats::lm(formula_spline(df,geo),
                       data=data.frame(dias=y,x,check.names=FALSE),
                       na.action=stats::na.fail,model=FALSE,x=FALSE,y=FALSE)
    ok(length(y)==nrow(x) && all(is.finite(stats::coef(ajuste))) &&
       ajuste$rank==length(stats::coef(ajuste)), 'Ajuste con rango deficiente o coeficientes invalidos')
    ajuste
  }
  grilla<-expand.grid(df=c(3L,4L,5L),interaccion_geo=c(FALSE,TRUE),KEEP.OUT.ATTRS=FALSE)
  grilla<-grilla[order(grilla$df,grilla$interaccion_geo),,drop=FALSE]
  grilla$configuracion<-paste0('ns_df',grilla$df,'_geo',as.integer(grilla$interaccion_geo))
  ok(nrow(grilla)==6L && nrow(cfg6)==4L,'Grilla o configuraciones historicas invalidas')

  cat('[3/8] Recalculando seis configuraciones en los cuatro folds de desarrollo...\n')
  obtener <- function(k,rol) {
    o<-ids$order_id[ids$fold==k & ids$rol==rol]
    ix<-match(o,b$order_id)
    ok(length(ix)>0L && !anyNA(ix) && !anyDuplicated(o),'Folds/IDs no validos')
    ix
  }
  fila_cv<-list(); max_diferencia_reconstruccion<-0
  for(k in 1:4){
    tr<-obtener(k,'train'); va<-obtener(k,'validacion')
    fold<-folds[folds$fold==k,,drop=FALSE]
    corte<-fecha_utc(fold$fecha_corte_entrenamiento)
    fin<-seq(corte,by='month',length.out=2L)[2L]
    ok(nrow(fold)==1L && length(tr)==fold$n_train && length(va)==fold$n_validacion &&
       !length(intersect(tr,va)), 'Tamano o cruce de folds invalidos')
    ok(all(aprobacion[tr]<=corte-as.difftime(45,units='days')) &&
       all(entrega[tr]<corte) && all(aprobacion[va]>=corte & aprobacion[va]<fin) &&
       all(is.finite(y[c(tr,va)])), 'Fuga de informacion o corte temporal invalido')
    rec<-receta(b[tr,,drop=FALSE]); xtr<-matrizB(b[tr,,drop=FALSE],rec)
    xva<-matrizB(b[va,,drop=FALSE],rec)
    ok(identical(names(xtr),names(xva)),'Columnas dispares entrenamiento/validacion')
    cat('  Fold ',k,'/4: ',length(tr),' train y ',length(va),' validacion...\n',sep='')
    for(j in seq_len(nrow(grilla))){
      g<-grilla[j,]
      fit<-ajustar(xtr,y[tr],g$df,g$interaccion_geo)
      p<-as.numeric(stats::predict(fit,newdata=xva))
      er<-metrica(y[va],p)
      fila_cv[[length(fila_cv)+1L]]<-data.frame(fold=k,mes_validacion=fold$mes_validacion,
        configuracion=g$configuracion,df=g$df,interaccion_geo=g$interaccion_geo,
        n_train=length(tr),n_validacion=length(va),rmse=unname(er['rmse']),
        mae=unname(er['mae']),sesgo=unname(er['sesgo']))
      # Prueba independiente: reproduce las predicciones de Splines etapa 6
      # para la configuracion que esa etapa eligio en el fold k.
      cg<-cfg6[cfg6$fold_externo==k,,drop=FALSE]
      if(nrow(cg)==1L && g$configuracion==cg$configuracion){
        ref<-pred6[pred6$fold==k,,drop=FALSE]
        pos<-match(b$order_id[va],ref$order_id)
        ok(nrow(ref)==length(va) && !anyNA(pos) && !anyDuplicated(ref$order_id) &&
           max(abs(ref$dias_reales[pos]-y[va]))<1e-9,
           'Predicciones Etapa 6 no corresponden a los mismos pedidos')
        dif<-max(abs(ref$dias_predichos[pos]-p))
        max_diferencia_reconstruccion<-max(max_diferencia_reconstruccion,dif)
        ok(is.finite(dif) && dif<1e-7,'La receta Splines no reproduce la Etapa 6')
      }
    }
  }
  cv<-do.call(rbind,fila_cv)
  ok(nrow(cv)==24L && all(table(cv$configuracion)==4L),'Validacion de configuraciones incompleta')
  resumen<-do.call(rbind,lapply(split(cv,cv$configuracion),function(z){
    data.frame(configuracion=z$configuracion[1],df=z$df[1],interaccion_geo=z$interaccion_geo[1],
               rmse_medio=mean(z$rmse),sd_rmse=stats::sd(z$rmse),mae_medio=mean(z$mae))
  }))
  resumen<-resumen[order(resumen$rmse_medio,resumen$df,resumen$interaccion_geo),,drop=FALSE]
  rownames(resumen)<-NULL
  elegido<-resumen[1L,,drop=FALSE]
  # Estas ventanas YA fueron examinadas en la Etapa 6; solo se usan para ajuste
  # del candidato, NUNCA como evaluacion independiente del modelo nuevo.

  cat('[4/8] Ajustando una sola vez el candidato con los 75.172 pedidos maduros...\n')
  indices<-match(maduras$order_id,b$order_id)
  idx_etiq<-match(maduras$order_id,etiq$order_id)
  ok(!anyNA(indices) && !anyNA(idx_etiq),'Pedido maduro sin respaldo en desarrollo')
  ultimo_corte<-fecha_utc('2018-07-01 00:00:00')
  ok(all(aprobacion[indices]<=ultimo_corte-as.difftime(45,units='days')) &&
     all(entrega[indices]<ultimo_corte) &&
     all(is.finite(maduras$dias_entrega)) && all(maduras$dias_entrega>=0),
     'Etiquetas maduras con incumplimiento del corte o respuesta no valida')
  base_train<-b[indices,,drop=FALSE]
  rec_final<-receta(base_train)
  x_final<-matrizB(base_train,rec_final)
  ok(nrow(x_final)==75172L,'Cantidad de registros en matriz final incorrecta')
  fit_final<-ajustar(x_final,maduras$dias_entrega,elegido$df,elegido$interaccion_geo)
  pred_train<-as.numeric(stats::predict(fit_final,newdata=x_final))
  train_m<-metrica(maduras$dias_entrega,pred_train)
  ok(length(pred_train)==75172L && all(is.finite(pred_train)), 'El ajuste final no predice todas las filas')

  cat('[5/8] Guardando una funcion de prediccion portatil junto al modelo...\n')
  # El entorno de esta funcion contiene SOLAMENTE fit/rec/columnas; no copia
  # objetos de la sesion ni depende de los scripts de preparacion originales.
  crear_predictor<-function(modelo,rec,columnas){
    entorno<-new.env(parent=baseenv())
    entorno$fit<-modelo; entorno$rec<-rec; entorno$columnas<-columnas
    funcion<-function(nuevos){
      if(!is.data.frame(nuevos) || nrow(nuevos)<1L ||
         !all(columnas %in% names(nuevos))) stop('Nuevos datos incompletos: faltan columnas B',call.=FALSE)
      h<-as.numeric(nuevos$hora_aprobacion)
      s<-as.numeric(nuevos$dia_semana); m<-as.numeric(nuevos$mes_aprobacion)
      hc<-as.numeric(nuevos$horas_desde_compra)
      if(!all(is.finite(h)&h>=0&h<=23) || !all(is.finite(s)&s>=1&s<=7) ||
         !all(is.finite(m)&m>=1&m<=12) || !all(is.finite(hc)&hc>=0))
        stop('Datos temporales invalidos',call.=FALSE)
      x<-cbind(hora_sin=sin(2*pi*h/24),hora_cos=cos(2*pi*h/24),
               dia_sin=sin(2*pi*(s-1)/7),dia_cos=cos(2*pi*(s-1)/7),
               mes_sin=sin(2*pi*(m-1)/12),mes_cos=cos(2*pi*(m-1)/12),
               log_horas_compra=log1p(hc))
      g0<-as.character(nuevos$estado_cliente)
      grupo<-factor(ifelse(is.na(g0)|!(g0 %in% rec$freq),rec$fallback,g0),levels=rec$lev)
      if(anyNA(grupo)) stop('Estado no reconocido por receta',call.=FALSE)
      if(length(rec$lev)>1L){
        D<-stats::model.matrix(~grupo)[,-1L,drop=FALSE]
        colnames(D)<-paste0('estado_',make.names(rec$lev[-1L]))
        x<-cbind(x,D)
      }
      for(z in c('n_items','n_vendedores','precio_total_brl','flete_total_brl','todos_vendedores_mismo_estado')){
        v<-as.numeric(nuevos[[z]])
        if(!all(is.finite(v)&v>=0)) stop(paste('Invalidos o faltantes en',z),call.=FALSE)
      }
      geo<-as.numeric(nuevos$todos_vendedores_mismo_estado)
      if(!all(geo %in% c(0,1))) stop('Geografia invalida',call.=FALSE)
      x<-cbind(x,log_items=log1p(as.numeric(nuevos$n_items)),
               log_vendedores=log1p(as.numeric(nuevos$n_vendedores)),
               log_precio=log1p(as.numeric(nuevos$precio_total_brl)),
               log_flete=log1p(as.numeric(nuevos$flete_total_brl)),
               vendedor_mismo_estado=geo)
      storage.mode(x)<-'double'
      x<-as.data.frame(x,check.names=FALSE)
      if(any(!is.finite(as.matrix(x)))) stop('Predictores no finitos',call.=FALSE)
      p<-as.numeric(stats::predict(fit,newdata=x))
      if(any(!is.finite(p))) stop('Predicciones no finitas',call.=FALSE)
      p # sin truncamiento: conserva exactamente la escala del modelo validado
    }
    environment(funcion)<-entorno
    funcion
  }
  predictor<-crear_predictor(fit_final,rec_final,original_B)
  prueba_nuevos<-predictor(base_train[1:100,,drop=FALSE])
  ok(max(abs(prueba_nuevos-pred_train[1:100]))<1e-7,
     'Receta de prediccion exportada no coincide con el modelo')
  paquete<-list(
    modelo=fit_final,
    receta=rec_final,
    predictores_originales=original_B,
    columnas_modelo=names(x_final),
    hiperparametros=elegido,
    n_entrenamiento=length(indices),
    corte_utc='2018-07-01 00:00:00',
    variable_objetivo='dias desde aprobacion hasta entrega (solo entregados)',
    estimacion= 'CANDIDATO; ninguna metrica de test nuevo',
    advertencia='Disponibilidad historica de atributos de articulo y vendedor no certificada',
    predecir=predictor
  )
  ruta_rds<-file.path(salida,'MODELO_CANDIDATO_SPLINES_B.rds')
  saveRDS(paquete,ruta_rds,compress='xz',version=3)
  externo<-readRDS(ruta_rds)
  ok(externo$n_entrenamiento==75172L &&
     max(abs(externo$predecir(base_train[1:100,,drop=FALSE])-prueba_nuevos))<1e-9,
     'El RDS no reproduce las predicciones al volver a abrirlo')

  cat('[6/8] Guardando configuraciones, integridad y limitaciones...\n')
  guardar(cv,'01_validacion_desarrollo_por_fold.csv')
  guardar(resumen,'02_configuraciones_y_eleccion.csv')
  guardar(data.frame(n_entrenamiento=length(indices),n_coeficientes=length(stats::coef(fit_final)),
      rango=fit_final$rank,configuracion=elegido$configuracion,
      rmse_train_no_independiente=unname(train_m['rmse']),
      mae_train_no_independiente=unname(train_m['mae']),
      sesgo_train_no_independiente=unname(train_m['sesgo']),
      diferencia_maxima_reconstruccion_etapa6=max_diferencia_reconstruccion),
      '03_resumen_ajuste_candidato.csv')
  guardar(data.frame(nombre=names(entradas),ruta=unname(entradas),md5=unname(tools::md5sum(entradas))),
          '04_huellas_fuentes.csv')
  captura<-utils::capture.output(sessionInfo())
  writeLines(captura,file.path(salida,'05_entorno_y_versiones.txt'),useBytes=TRUE)
  notas<-c(
    'ETAPA 11 | AJUSTE DE UN CANDIDATO SPLINES PARA ENTREGA | NO TEST INDEPENDIENTE',
    'Se utiliza solo Olist de desarrollo con etiqueta conocida y aprobacion madura >=45 dias al 01/07/2018.',
    'Muestra de ajuste definitiva: 75.172 pedidos entregados elegibles.',
    'Se mantienen las diez entradas originales B, incluida la geografia del vendedor.',
    'La grilla df=3,4,5 y geografia x log(flete) con/sin interaccion es EXACTAMENTE la Etapa 6.',
    'La receta (estados raros y transformaciones) se aprende dentro de cada entrenamiento.',
    'La configuracion final se escoge con cuatro ventanas de desarrollo diciembre 2017-marzo 2018.',
    'Estas ventanas YA fueron examinadas en la investigacion. Sus errores no son test nuevo.',
    'La seleccion de la familia Splines fue retrospectiva a las comparaciones de desarrollo.',
    'El RDS contiene el ajuste, la receta y una funcion $predecir(datos_B).',
    'La funcion conserva predicciones originales sin truncamiento; cualquier regla operativa debe validarse por separado.',
    'La disponibilidad historica de geografia de vendedor, articulos, flete y precio al aprobar',
    'es una hipotesis de negocio que debe validarse antes de uso operativo.',
    'La poblacion se limita a pedidos entregados, no cancelados o sin entrega observada.',
    'El test historico de julio-agosto 2018 ya fue utilizado y NO se leyo ni consulto aqui.',
    'El candidato NO cuenta con estimacion imparcial de rendimiento sobre test nuevo.',
    'No se realizo inferencia causal ni contraste de superioridad entre algoritmos.'
  )
  writeLines(notas,file.path(salida,'06_protocolo_y_limitaciones.txt'),useBytes=TRUE)

  cat('[7/8] Certificando que los archivos de Etapas 1-10 siguen intactos...\n')
  md5_despues<-unname(tools::md5sum(previo))
  ok(identical(md5_antes,md5_despues),'Algunos archivos anteriores fueron modificados')
  guardar(data.frame(archivo=nombres_previos,md5=md5_antes),'08_huellas_anteriores_protegidas.csv')
  ok(file.exists(ruta_rds) && file.info(ruta_rds)$size>1000,
     'El archivo RDS no fue creado correctamente')
  cat('[8/8] Certificado final y conclusiones...\n')
  texto<-c('ETAPA 11 APROBADA - CANDIDATO SPLINES B ENTRENADO SOLO CON DESARROLLO',
           paste('Fecha de corte (UTC): 2018-07-01 00:00:00'),
           paste('Pedidos de entrenamiento:',length(indices)),
           paste('Configuracion elegida:',elegido$configuracion),
           paste('RMSE media de ventanas de desarrollo examinadas:',sprintf('%.6f',elegido$rmse_medio)),
           'ADVERTENCIA: esa media NO es una estimacion test independiente del modelo final.',
           paste('Maxima diferencia etapa6:',sprintf('%.3g',max_diferencia_reconstruccion)),
           paste('Modelo MD5:',unname(tools::md5sum(ruta_rds))),
           'Resultados de Etapas 1-10 sin cambios MD5.',
           'El test historico NO fue consultado o recalculado.')
  writeLines(texto,cert,useBytes=TRUE)
  cat('\n',paste(texto,collapse='\n'),'\n',sep='')
  cat('Archivos Etapa 11:',salida,'\n')
  cat('Ejemplo en R: z <- readRDS("resultados/11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds")\n')
  cat('              z$predecir(nuevos_pedidos_con_las_10_columnas_B)\n')
})
