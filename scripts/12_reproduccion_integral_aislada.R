# Modo rapido: reproduce SOLO las etapas nuevas desde insumos archivados,
# en otro proceso y carpeta. No vuelve a entrenar los modelos originales.
# options(olist.solo_complementos=TRUE); source('scripts/12_reproduccion_integral_aislada.R',encoding='UTF-8')
# options(olist.solo_complementos=FALSE)
if (isTRUE(getOption('olist.solo_complementos', FALSE))) {
local({
  exigir <- function(ok, msg) if (!isTRUE(ok)) stop(msg, call.=FALSE)
  raiz <- normalizePath(getwd(), winslash='/', mustWork=TRUE)
  carpetas <- c('13_diagnostico_complementario','14_interpretacion_e_incertidumbre',
                '15_evaluacion_no_lineal_y_supuestos','16_sensibilidad_log_respuesta')
  scripts <- paste0(carpetas,'.R')
  exigir(all(file.exists(file.path(raiz,'scripts',scripts))), 'Faltan scripts 13/14/15/16')
  tablas <- unlist(lapply(carpetas,function(ca) file.path('resultados',ca,
    list.files(file.path(raiz,'resultados',ca),pattern='\\.csv$'))),use.names=FALSE)
  exigir(length(tablas)==30L, 'Primero deben existir las 30 tablas de referencia de 13-16')
  figuras <- unlist(lapply(carpetas,function(ca) file.path('graficos',ca,
    list.files(file.path(raiz,'graficos',ca),pattern='\\.png$'))),use.names=FALSE)
  exigir(length(figuras)==16L, 'Faltan las dieciseis figuras de referencia de 13-16')
  originales <- c(list.files(file.path(raiz,'scripts'),recursive=TRUE,full.names=TRUE),
    list.files(file.path(raiz,'resultados'),recursive=TRUE,full.names=TRUE),
    list.files(file.path(raiz,'graficos'),recursive=TRUE,full.names=TRUE),
    file.path(raiz,'datos_originales','dataset.zip'))
  h0 <- tools::md5sum(originales)
  exigir(!anyNA(h0), 'No se puede comprobar integridad original')
  copia <- file.path(dirname(raiz),paste0(basename(raiz),'__COMPLEMENTOS_',format(Sys.time(),'%Y%m%d_%H%M%S')))
  exigir(!file.exists(copia), 'La carpeta de reproduccion ya existe')
  dir.create(file.path(copia,'scripts'),recursive=TRUE)
  dir.create(file.path(copia,'12_control_complementos'))
  exigir(all(file.copy(file.path(raiz,'scripts',scripts),file.path(copia,'scripts'))), 'Fallo al copiar scripts')
  archivos <- list.files(file.path(raiz,'resultados'),recursive=TRUE)
  archivos <- archivos[!grepl('^(13_diagnostico_complementario|14_interpretacion_e_incertidumbre|15_evaluacion_no_lineal_y_supuestos|16_sensibilidad_log_respuesta)/',archivos)]
  for (f in archivos) {
    destino <- file.path(copia,'resultados',f)
    dir.create(dirname(destino),recursive=TRUE,showWarnings=FALSE)
    exigir(file.copy(file.path(raiz,'resultados',f),destino),paste('No se pudo copiar',f))
  }
  worker <- file.path(copia,'12_control_complementos','01_ejecutar.R')
  writeLines(c('args <- commandArgs(trailingOnly=TRUE)', 'setwd(args[1])',
    "source('scripts/13_diagnostico_complementario.R',encoding='UTF-8')",
    "source('scripts/14_interpretacion_e_incertidumbre.R',encoding='UTF-8')",
    "source('scripts/15_evaluacion_no_lineal_y_supuestos.R',encoding='UTF-8')",
    "source('scripts/16_sensibilidad_log_respuesta.R',encoding='UTF-8')",
    "capture.output(sessionInfo(),file='12_control_complementos/sessionInfo.txt')"),worker)
  rscript <- file.path(R.home('bin'),'Rscript')
  if (.Platform$OS.type=='windows') rscript <- paste0(rscript,'.exe')
  exigir(file.exists(rscript), 'No se encontro Rscript')
  stdout <- file.path(copia,'12_control_complementos','02_stdout.log')
  stderr <- file.path(copia,'12_control_complementos','03_stderr.log')
  cat('Reproduciendo complementos en proceso nuevo:',copia,'\n')
  estado <- system2(rscript,c('--vanilla',shQuote(worker),shQuote(copia)),stdout=stdout,stderr=stderr)
  exigir(identical(as.integer(estado),0L),paste('Error en 13-16; consultar',stderr))
  iguales <- function(a,b) {
    if(!identical(names(a),names(b)) || nrow(a)!=nrow(b)) return(FALSE)
    for(z in names(a)) {
      x<-a[[z]]; y<-b[[z]]
      if(is.numeric(x)&&is.numeric(y)) {
        if(!identical(is.na(x),is.na(y))) return(FALSE)
        ok<-!is.na(x)
        if(any(!is.finite(x[ok]))||any(!is.finite(y[ok]))||
          any(abs(x[ok]-y[ok])>1e-6+1e-8*abs(x[ok]))) return(FALSE)
      } else if(!identical(as.character(x),as.character(y))) return(FALSE)
    }
    TRUE
  }
  comp <- do.call(rbind,lapply(tablas,function(f) {
    a<-utils::read.csv(file.path(raiz,f),check.names=FALSE,stringsAsFactors=FALSE,na.strings=c('','NA'))
    b<-utils::read.csv(file.path(copia,f),check.names=FALSE,stringsAsFactors=FALSE,na.strings=c('','NA'))
    data.frame(archivo=f,coincide=iguales(a,b))
  }))
  utils::write.csv(comp,file.path(copia,'12_control_complementos','04_comparacion_30_tablas.csv'),row.names=FALSE)
  rutas_fig <- file.path(copia,figuras)
  ok_fig <- all(file.exists(rutas_fig)) && all(file.info(rutas_fig)$size>500)
  h1 <- tools::md5sum(originales)
  intactos <- identical(unname(h0),unname(h1)) && all(!is.na(h1))
  huellas_control <- data.frame(archivo=substring(originales,nchar(raiz)+2L),
    md5_antes=unname(h0),md5_despues=unname(h1),coincide=unname(h0)==unname(h1))
  utils::write.csv(huellas_control,file.path(copia,'12_control_complementos','06_integridad_archivos.csv'),row.names=FALSE)
  if (!intactos) print(huellas_control[is.na(huellas_control$coincide)|!huellas_control$coincide,,drop=FALSE])
  certs <- file.path(copia,'resultados',carpetas,c('CONTROL_ETAPA13_APROBADO.txt','CONTROL_ETAPA14_APROBADO.txt',
                                                    'CONTROL_ETAPA15_APROBADO.txt',
                                                    'CONTROL_ETAPA16_APROBADO.txt'))
  aprobado <- all(comp$coincide) && ok_fig && intactos && all(file.exists(certs))
  texto <- c(paste('ESTADO:',if(aprobado) 'APROBADO' else 'NO APROBADO'),
    paste('Comparaciones CSV:',sum(comp$coincide),'/',nrow(comp)),
    paste('Figuras nuevas regeneradas:',sum(file.exists(rutas_fig)),'/',length(figuras)),
    paste('Proyecto de origen intacto (MD5):',intactos),
    'Etapas 13-16 ejecutadas en Rscript --vanilla separado.',
    'Este control no reentrena 01-11 ni sustituye su evidencia historica.',
    'No es un test predictivo independiente. No se consulto julio-agosto 2018.',
    paste('R:',R.version.string))
  writeLines(texto,file.path(copia,'12_control_complementos','05_RESULTADO_COMPLEMENTOS.txt'))
  cat(paste(texto,collapse='\n'),'\n')
  exigir(aprobado,paste('Revisar la evidencia en',copia))
  invisible(copia)
})
} else {
# ==============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3 | OLIST
# ETAPA 12 | REPRODUCCION INTEGRAL EN PROCESO Y CARPETA AISLADOS
#  - No modifica el proyecto original
#  - Nunca reevalua el test historico como test independiente
#  - No instala paquetes automaticamente
#  - Rscript --vanilla: sesion nueva sin objetos de RStudio
#  - Comprueba insumos, certificados, tablas y lectura de RDS desde otra sesion
#  - Incluye Etapas 13-16 despues del candidato; 45 tablas en modo integral
#  - Etapas 15-16 requieren ademas los paquetes lmtest y car
# REQUISITO: carpeta scripts/12_reproduccion_integral_aislada.R
# DESDE LA RAIZ DEL PROYECTO: source('scripts/12_reproduccion_integral_aislada.R',encoding='UTF-8')
# ==============================================================================
local({
  fallo <- function(mensaje) stop(paste0('ETAPA 12: ', mensaje), call. = FALSE)
  ruta <- function(...) file.path(...)
  nombres_paquetes <- c('stats','splines','glmnet','rpart','ranger','gbm','lmtest','car')
  nombres_scripts <- c(
    '01_importacion_y_revision_datos.R',
    '01A_estilo_graficos_etapa1.R',
    '02_integracion_y_variables_candidatas.R',
    '03_protocolo_variables_y_folds.R',
    '04_baseline_y_ols_temporal.R',
    '04B_regenerar_graficos_datacamp.R',
    '05_regularizacion_anidada.R',
    '06_splines_validacion_anidada.R',
    '07_arbol_rf_boosting_anidados.R',
    '08_diagnostico_errores_y_defensa.R',
    '09_inferencia_y_diagnostico_ols.R',
    '10_decision_modelo_y_trazabilidad.R',
    '11_ajuste_candidato_splines_y_reproducibilidad.R',
    '12_reproduccion_integral_aislada.R',
    '13_diagnostico_complementario.R',
    '14_interpretacion_e_incertidumbre.R',
    '15_evaluacion_no_lineal_y_supuestos.R',
    '16_sensibilidad_log_respuesta.R'
  )
  lista_certificados <- c(
    'resultados/03_protocolo_y_folds/correlaciones/07_CONTROL_CORRELACIONES_APROBADO.txt',
    'resultados/04_baseline_y_ols/11_CONTROL_COMPARABILIDAD_APROBADO.txt',
    'resultados/05_regularizacion/12_CONTROL_ETAPA5_APROBADO.txt',
    'resultados/04_seleccion_variables_exploratoria/09_CONTROL_SELECCION_APROBADO.txt',
    'resultados/06_splines/11_CONTROL_ETAPA6_APROBADO.txt',
    'resultados/07_ensembles/11_CONTROL_ETAPA7_APROBADO.txt',
    'resultados/08_diagnostico_errores/07_CONTROL_ETAPA8_APROBADO.txt',
    'resultados/09_inferencia_ols/09_CONTROL_ETAPA9_APROBADO.txt',
    'resultados/10_decision_modelo/07_CONTROL_ETAPA10_APROBADO.txt',
    'resultados/11_modelo_candidato/07_CONTROL_ETAPA11_APROBADO.txt'
  )
  comandos <- commandArgs(trailingOnly = TRUE)
  modo_aislado <- length(comandos) >= 1L && identical(comandos[1L], '--aislado')

  if (modo_aislado) {
    # Esta rama SOLO corre en Rscript --vanilla. Nunca en el RStudio original.
    if (length(comandos) < 2L) fallo('Falta argumento con la ruta aislada.')
    trabajo <- normalizePath(comandos[2L], winslash='/', mustWork=TRUE)
    setwd(trabajo)
    librerias <- readLines('12_control/00_librerias_R.txt',warn=FALSE)
    librerias <- librerias[dir.exists(librerias)]
    if (length(librerias)) .libPaths(unique(c(librerias,.libPaths())))
    opciones <- c('olist.seleccion_variables','olist.solo_correlaciones',
                  'olist.solo_graficos','olist.permitir_reemplazar_seleccion',
                  'olist.reproducir_etapa4_historica')
    for (z in opciones) options(structure(list(FALSE),names=z))
    Sys.setenv(TZ='UTC')
    cat('=== ETAPA 12: EJECUCION AISLADA, Rscript --vanilla ===\n')
    cat('Carpeta:',getwd(),'\nR:',R.version.string,'\n')
    capture.output(sessionInfo(),file='12_sessionInfo_proceso_aislado.txt')
    ejecutar <- function(nombre, etiqueta, certificado = NULL) {
      cat('\n==>', etiqueta, '\n')
      archivo <- file.path('scripts',nombre)
      if (!file.exists(archivo)) fallo(paste('No existe',archivo))
      # Se usa source() en una sesion limpia: no depende del entorno de RStudio.
      source(archivo, encoding='UTF-8', local=FALSE)
      if (!is.null(certificado) && !file.exists(certificado))
        fallo(paste('No se creo certificado:',certificado))
    }
    tryCatch({
      ejecutar('01_importacion_y_revision_datos.R','01 - Datos originales')
      ejecutar('02_integracion_y_variables_candidatas.R','02 - Variables y madurez')
      ejecutar('03_protocolo_variables_y_folds.R','03 - Protocolo y folds')
      options(olist.solo_correlaciones=TRUE)
      ejecutar('03_protocolo_variables_y_folds.R','03 - Correlaciones train',lista_certificados[1L])
      options(olist.solo_correlaciones=FALSE)
      ejecutar('04_baseline_y_ols_temporal.R','04 - Baseline y OLS',lista_certificados[2L])
      ejecutar('05_regularizacion_anidada.R','05 - Regularizacion',lista_certificados[3L])
      options(olist.seleccion_variables=TRUE)
      ejecutar('04_baseline_y_ols_temporal.R','04 - Seleccion de variables interna',lista_certificados[4L])
      options(olist.seleccion_variables=FALSE)
      ejecutar('06_splines_validacion_anidada.R','06 - Splines',lista_certificados[5L])
      ejecutar('07_arbol_rf_boosting_anidados.R','07 - Arboles y ensembles',lista_certificados[6L])
      if(!file.exists('resultados/07_ensembles/16_CONTROL_AVERAGING_APROBADO.txt'))
        fallo('El averaging complementario no fue certificado por la Etapa 07.')
      ejecutar('08_diagnostico_errores_y_defensa.R','08 - Diagnosticos',lista_certificados[7L])
      if(!file.exists('resultados/08_diagnostico_errores/08_averaging_resumen_diagnostico.csv'))
        fallo('Falta el diagnostico del averaging en Etapa 08.')
      ejecutar('09_inferencia_y_diagnostico_ols.R','09 - Inferencia',lista_certificados[8L])
      ejecutar('10_decision_modelo_y_trazabilidad.R','10 - Decision',lista_certificados[9L])
      if(!file.exists('resultados/10_decision_modelo/08_comparacion_10_alternativas.csv'))
        fallo('Falta comparacion de diez alternativas en Etapa 10.')
      ejecutar('11_ajuste_candidato_splines_y_reproducibilidad.R','11 - Candidato final',lista_certificados[10L])
      modelo <- readRDS('resultados/11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds')
      if (!identical(modelo$n_entrenamiento,75172L) || !is.function(modelo$predecir))
        fallo('El RDS no contiene la muestra y funcion de prediccion esperadas.')
      ejecutar('13_diagnostico_complementario.R','13 - Diagnostico complementario',
        'resultados/13_diagnostico_complementario/CONTROL_ETAPA13_APROBADO.txt')
      ejecutar('14_interpretacion_e_incertidumbre.R','14 - Interpretacion e intervalos',
        'resultados/14_interpretacion_e_incertidumbre/CONTROL_ETAPA14_APROBADO.txt')
      ejecutar('15_evaluacion_no_lineal_y_supuestos.R','15 - Evaluacion no lineal y supuestos',
        'resultados/15_evaluacion_no_lineal_y_supuestos/CONTROL_ETAPA15_APROBADO.txt')
      ejecutar('16_sensibilidad_log_respuesta.R','16 - Sensibilidad log(dias)',
        'resultados/16_sensibilidad_log_respuesta/CONTROL_ETAPA16_APROBADO.txt')
      cat('\nETAPA 12: RECONSTRUCCION AISLADA DE ETAPAS 01-11 Y 13-16 COMPLETADA.\n')
      writeLines(c('REPRODUCCION ETAPAS 01-11 Y 13-16: COMPLETADA EN PROCESO AISLADO',
                   paste('R:',R.version.string),
                   'NO se evalua aqui el test historico como muestra independiente.'),
                 '12_RECONSTRUCCION_COMPLETA.txt',useBytes=TRUE)
    }, error=function(e) {
      cat('\nREPRODUCCION AISLADA DETENIDA: ',conditionMessage(e),'\n',sep='')
      quit(save='no',status=1L,runLast=FALSE)
    })
    quit(save='no',status=0L,runLast=FALSE)
  }

  # RAMA PRINCIPAL: solo inspecciona/copia; NO ejecuta modelos en esta sesion.
  raiz <- normalizePath(getwd(), winslash='/',mustWork=TRUE)
  if (!dir.exists(file.path(raiz,'scripts')) ||
      !file.exists(file.path(raiz,'datos_originales','dataset.zip')))
    fallo('Abre RStudio en la carpeta raiz Proyecto_Olist_caso1 antes de source().')
  faltan <- nombres_scripts[!file.exists(file.path(raiz,'scripts',nombres_scripts))]
  if (length(faltan)) fallo(paste('Faltan scripts:',paste(faltan,collapse=', ')))
  txt04 <- readLines(file.path(raiz,'scripts','04_baseline_y_ols_temporal.R'),
                     warn=FALSE,encoding='UTF-8')
  if (!any(grepl('ETAPA 4 UNIFICADA',txt04,fixed=TRUE)) ||
      !any(grepl('olist.seleccion_variables',txt04,fixed=TRUE)))
    fallo('El script 04 aun NO es la version unificada de dos modos. Instala primero su correccion.')
  for (p in nombres_paquetes) {
    if (!requireNamespace(p,quietly=TRUE)) {
      fallo(paste('Falta el paquete',p,'; ejecuta install.packages(',
                  shQuote(p),') antes de reproducir.'))
    }
  }
  deps <- data.frame(paquete=nombres_paquetes,
                     version=vapply(nombres_paquetes,function(x)
                       as.character(utils::packageVersion(x)),character(1L)))
  print(deps,row.names=FALSE)
  rscript_posibles <- c(file.path(R.home('bin'),'Rscript.exe'),
                         file.path(R.home('bin'),'Rscript'),
                         Sys.which('Rscript'), Sys.which('Rscript.exe'))
  rscript_posibles <- rscript_posibles[nzchar(rscript_posibles) & file.exists(rscript_posibles)]
  if (!length(rscript_posibles))
    fallo('No se localizo Rscript; comprueba la instalacion completa de R en este PC.')
  rscript <- normalizePath(rscript_posibles[1L],winslash='/',mustWork=TRUE)

  # Se crea una carpeta HERMANA, no una subcarpeta del proyecto de entrega.
  marca <- format(Sys.time(),'%Y%m%d_%H%M%S')
  aislamiento <- file.path(dirname(raiz),paste0(basename(raiz),'__REPRODUCCION_',marca))
  if (file.exists(aislamiento) || dir.exists(aislamiento))
    fallo('La carpeta aislada ya existe; no se sobrescribira.')
  if (!dir.create(aislamiento,recursive=TRUE,showWarnings=FALSE))
    fallo('No se pudo crear la carpeta aislada.')
  dir.create(file.path(aislamiento,'scripts'))
  dir.create(file.path(aislamiento,'datos_originales'))
  dir.create(file.path(aislamiento,'12_control'))

  # Manifestar todos los archivos de entrada de la carpeta original.
  inventariar <- function(base) {
    rutas <- c(file.path(base,'datos_originales','dataset.zip'),
               list.files(file.path(base,'scripts'),full.names=TRUE,recursive=TRUE),
               if (dir.exists(file.path(base,'resultados')))
                 list.files(file.path(base,'resultados'),full.names=TRUE,recursive=TRUE),
               if (dir.exists(file.path(base,'graficos')))
                 list.files(file.path(base,'graficos'),full.names=TRUE,recursive=TRUE))
    rutas[file.exists(rutas) & !dir.exists(rutas)]
  }
  originales <- inventariar(raiz)
  huella_antes <- tools::md5sum(originales)
  if (anyNA(huella_antes)) fallo('No se pudieron calcular los MD5 iniciales.')
  if (!all(file.copy(file.path(raiz,'scripts',nombres_scripts),
                     file.path(aislamiento,'scripts'),overwrite=FALSE)))
    fallo('No se pudieron copiar todos los scripts a la carpeta aislada.')
  if (!file.copy(file.path(raiz,'datos_originales','dataset.zip'),
                 file.path(aislamiento,'datos_originales','dataset.zip'),overwrite=FALSE))
    fallo('No se pudo copiar el dataset original.')
  if (!identical(unname(tools::md5sum(file.path(raiz,'datos_originales','dataset.zip'))),
                 unname(tools::md5sum(file.path(aislamiento,'datos_originales','dataset.zip')))))
    fallo('El dataset copiado no coincide bit a bit con el original.')
  writeLines(.libPaths(),file.path(aislamiento,'12_control','00_librerias_R.txt'),useBytes=TRUE)
  utils::write.csv(deps,file.path(aislamiento,'12_control','01_dependencias.csv'),row.names=FALSE)
  cat('\nProyecto original protegido:',raiz,'\n')
  cat('Carpeta aislada:',aislamiento,'\n')
  cat('Proceso R independiente:',rscript,'\n')
  cat('Inicio de reproduccion de desarrollo, sin test historico...\n')

  salida_log <- file.path(aislamiento,'12_control','02_ejecucion_stdout.log')
  error_log <- file.path(aislamiento,'12_control','03_ejecucion_stderr.log')
  comando <- c('--vanilla',
               shQuote(file.path(aislamiento,'scripts','12_reproduccion_integral_aislada.R')),
               '--aislado',shQuote(aislamiento))
  estado <- tryCatch(suppressWarnings(system2(rscript,args=comando,stdout=salida_log,
                                    stderr=error_log,wait=TRUE)),error=function(e)e)
  if (inherits(estado,'error'))
    fallo(paste('No se pudo iniciar Rscript:',conditionMessage(estado),
                'Carpeta de diagnostico:',aislamiento))
  if (length(estado)!=1L || is.na(estado) || estado!=0L ||
      !file.exists(file.path(aislamiento,'12_RECONSTRUCCION_COMPLETA.txt'))) {
    cat('La reproduccion NO paso. Revisa:',salida_log,'y',error_log,'\n')
    fallo(paste('Proceso Rscript terminado con codigo',paste(estado,collapse=','),
                '. Se preserva la carpeta aislada para diagnostico.'))
  }

  # Se comparan resultados sustantivos, NO el MD5 del RDS (puede variar en R/OS).
  comparar_csv <- c(
    'resultados/01_revision_datos/06_etiquetas_solo_desarrollo.csv',
    'resultados/03_protocolo_y_folds/04_ids_train_validacion.csv',
    'resultados/03_protocolo_y_folds/correlaciones/02_correlacion_numericas_vs_respuesta_train.csv',
    'resultados/04_baseline_y_ols/02_resumen_comparativo_principal.csv',
    'resultados/05_regularizacion/06_comparacion_principal_mismo_B.csv',
    'resultados/04_seleccion_variables_exploratoria/03_seleccion_interna_por_fold.csv',
    'resultados/06_splines/07_comparacion_comun_resumen.csv',
    'resultados/07_ensembles/07_comparacion_comun_resumen.csv',
    'resultados/08_diagnostico_errores/01_comparacion_resumida.csv',
    'resultados/09_inferencia_ols/01_coeficientes_IC95_HC3_y_clasico.csv',
    'resultados/10_decision_modelo/01_comparacion_y_ranking_descriptivo.csv',
    'resultados/11_modelo_candidato/02_configuraciones_y_eleccion.csv',
    'resultados/07_ensembles/14_averaging_resumen.csv',
    'resultados/08_diagnostico_errores/08_averaging_resumen_diagnostico.csv',
    'resultados/10_decision_modelo/08_comparacion_10_alternativas.csv'
  )
  for (ca in c('13_diagnostico_complementario','14_interpretacion_e_incertidumbre',
               '15_evaluacion_no_lineal_y_supuestos','16_sensibilidad_log_respuesta')) {
    nuevas <- list.files(file.path(raiz,'resultados',ca),pattern='\\.csv$')
    comparar_csv <- c(comparar_csv,file.path('resultados',ca,nuevas))
  }
  if(length(comparar_csv)!=45L) fallo('Se esperaban 15 tablas historicas y 30 complementarias (13-16).')
  comparar_tablas <- function(a,b) {
    if (!identical(names(a),names(b)) || nrow(a)!=nrow(b)) return(FALSE)
    for (campo in names(a)) {
      x<-a[[campo]];y<-b[[campo]]
      if (is.numeric(x)&&is.numeric(y)) {
        if (!identical(is.na(x),is.na(y))) return(FALSE)
        comparables<-!is.na(x)
        if (any(!is.finite(x[comparables])) || any(!is.finite(y[comparables]))) return(FALSE)
        if (any(abs(x[comparables]-y[comparables])>
                (1e-6+1e-8*abs(x[comparables])))) return(FALSE)
      } else if (!identical(as.character(x),as.character(y))) return(FALSE)
    }
    TRUE
  }
  resultados_comparacion <- lapply(comparar_csv,function(f) {
    ruta_old<-file.path(raiz,f); ruta_new<-file.path(aislamiento,f)
    if (!file.exists(ruta_old)||!file.exists(ruta_new))
      return(data.frame(archivo=f,coincide=FALSE,detalle='Falta origen o reconstruccion'))
    tryCatch({
      x<-utils::read.csv(ruta_old,check.names=FALSE,stringsAsFactors=FALSE,
                          na.strings=c('','NA'))
      y<-utils::read.csv(ruta_new,check.names=FALSE,stringsAsFactors=FALSE,
                          na.strings=c('','NA'))
      bien<-comparar_tablas(x,y)
      data.frame(archivo=f,coincide=bien,detalle=if(bien) 'OK' else 'Valores/estructura difieren')
    },error=function(e)data.frame(archivo=f,coincide=FALSE,
                                  detalle=conditionMessage(e)))
  })
  tabla_comparacion <- do.call(rbind,resultados_comparacion)
  utils::write.csv(tabla_comparacion,file.path(aislamiento,'12_control',
                         '04_comparacion_con_resultados_originales.csv'),row.names=FALSE)

  # Ultima comprobacion: lectura/prediccion en OTRA sesion nueva Rscript.
  lector <- file.path(aislamiento,'12_control','05_verificar_rds_en_nueva_sesion.R')
  writeLines(c(
    'args <- commandArgs(trailingOnly=TRUE)',
    'stopifnot(length(args)>=1L)',
    'setwd(args[1L])',
    "z <- readRDS('resultados/11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds')",
    "stopifnot(is.list(z),is.function(z$predecir),identical(z$n_entrenamiento,75172L))",
    "b <- read.csv('resultados/02_variables_candidatas/01_variables_candidatas_desarrollo.csv',check.names=FALSE,stringsAsFactors=FALSE)",
    'vars <- z$predictores_originales',
    'stopifnot(length(vars)==10L,all(vars %in% names(b)))',
    'idx <- which(complete.cases(b[,vars,drop=FALSE]))[1:100]',
    'stopifnot(length(idx)==100L,!anyNA(idx))',
    'p <- z$predecir(b[idx,,drop=FALSE])',
    'stopifnot(length(p)==100L,all(is.finite(p)))',
    "writeLines(c('RDS ABIERTO EN PROCESO NUEVO','100 predicciones finitas',",
    "             paste('Configuracion:',as.character(z$hiperparametros$configuracion))),",
    "             '12_control/06_RDS_NUEVA_SESION_APROBADO.txt')",
    "cat('RDS LEIDO Y PREDICHO EN NUEVA SESION SIN OBJETOS PREVIOS\\n')"
  ),lector,useBytes=TRUE)
  test_log<-file.path(aislamiento,'12_control','07_RDS_verificacion_stdout.log')
  test_err<-file.path(aislamiento,'12_control','08_RDS_verificacion_stderr.log')
  status_rds <- tryCatch(suppressWarnings(system2(rscript,args=c('--vanilla',shQuote(lector),
                                    shQuote(aislamiento)),stdout=test_log,stderr=test_err,
                                    wait=TRUE)),error=function(e)e)
  rds_aprobado <- !inherits(status_rds,'error') && length(status_rds)==1L &&
     !is.na(status_rds) && status_rds==0L &&
     file.exists(file.path(aislamiento,'12_control','06_RDS_NUEVA_SESION_APROBADO.txt'))

  huella_despues <- tools::md5sum(originales)
  origen_intacto <- identical(unname(huella_antes),unname(huella_despues)) &&
                    all(!is.na(huella_despues))
  ok <- all(tabla_comparacion$coincide) && rds_aprobado && origen_intacto
  informe <- c(
    paste('ESTADO:',if(ok) 'APROBADO' else 'NO APROBADO'),
    paste('Comparaciones CSV:',sum(tabla_comparacion$coincide),'/',nrow(tabla_comparacion)),
    paste('Lectura RDS en segunda sesion:',if(rds_aprobado) 'APROBADA' else 'FALLIDA'),
    paste('Archivos del proyecto original intactos (MD5):',origen_intacto),
    paste('Ruta copia aislada:',aislamiento),
    'No se consulto un test independiente nuevo ni se reclamo un nuevo RMSE de test.',
    'Un proceso Rscript --vanilla aislado no demuestra por si solo portabilidad entre computadores.'
  )
  writeLines(informe,file.path(aislamiento,'12_control','09_RESULTADO_REPRODUCIBILIDAD.txt'),
             useBytes=TRUE)
  cat('\n',paste(informe,collapse='\n'),'\n',sep='')
  if (!ok) fallo(paste('Revisar reportes en',file.path(aislamiento,'12_control')))
})
}
