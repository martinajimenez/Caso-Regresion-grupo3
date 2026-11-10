# =============================================================================
# GRUPO 3 | ESTADISTICA APLICADA | OLIST
# ETAPA 14 | Verificador de entorno de SOLO LECTURA. R BASE.
# No instala paquetes, no entrena, no modifica el proyecto.
# Uso: desde la raiz del proyecto, source('scripts/00_verificar_entorno.R', encoding='UTF-8')
# =============================================================================
local({
  raiz <- normalizePath(getwd(), winslash='/', mustWork=TRUE)
  esenciales <- c('scripts/01_importacion_y_revision_datos.R',
                  'scripts/01A_estilo_graficos_etapa1.R',
                  'scripts/04B_regenerar_graficos_datacamp.R',
                  'scripts/04_baseline_y_ols_temporal.R',
                  'scripts/05_regularizacion_anidada.R',
                  'scripts/12_reproduccion_integral_aislada.R',
                  'datos_originales/dataset.zip')
  ausentes <- esenciales[!file.exists(file.path(raiz, esenciales))]
  if (length(ausentes)) stop('Faltan archivos necesarios: ',
                            paste(ausentes,collapse=', '),call.=FALSE)
  paquetes <- c('stats','splines','glmnet','rpart','ranger','gbm')
  referencia <- c('4.6.1','4.6.1','5.1','4.1.27','0.18.0','2.3.1')
  presentes <- vapply(paquetes,requireNamespace,logical(1L),quietly=TRUE)
  versiones <- vapply(seq_along(paquetes),function(i) {
    if (!presentes[i]) return('NO INSTALADO')
    as.character(utils::packageVersion(paquetes[i]))
  },character(1L))
  tabla <- data.frame(paquete=paquetes,instalado=presentes,
                      version_local=versiones,version_referencia=referencia,
                      stringsAsFactors=FALSE)
  cat('\n--- COMPROBACION DEL ENTORNO ---\n')
  cat('Carpeta:',raiz,'\nR:',R.version.string,'\n')
  print(tabla,row.names=FALSE)
  cat('MD5 del ZIP de Olist: ',
      unname(tools::md5sum(file.path(raiz,'datos_originales','dataset.zip'))),'\n',sep='')
  lista <- tryCatch(utils::unzip(file.path(raiz,'datos_originales','dataset.zip'),
                                 list=TRUE)$Name,error=function(e)character(0))
  esperados <- c('olist_customers_dataset.csv','olist_geolocation_dataset.csv',
                 'olist_order_items_dataset.csv','olist_order_payments_dataset.csv',
                 'olist_order_reviews_dataset.csv','olist_orders_dataset.csv',
                 'olist_products_dataset.csv','olist_sellers_dataset.csv',
                 'product_category_name_translation.csv')
  faltan_csv <- setdiff(esperados, basename(lista))
  if (length(faltan_csv)) stop('Faltan CSV originales dentro de dataset.zip: ',
                              paste(faltan_csv,collapse=', '),call.=FALSE)
  rscript_posibles <- c(file.path(R.home('bin'),'Rscript.exe'),
                         file.path(R.home('bin'),'Rscript'),
                         Sys.which('Rscript'),Sys.which('Rscript.exe'))
  rscript_ok <- any(nzchar(rscript_posibles) & file.exists(rscript_posibles))
  if (!rscript_ok) stop('Rscript no esta disponible; revisar la instalacion de R.',call.=FALSE)
  faltantes <- paquetes[!presentes]
  if (length(faltantes)) stop('Faltan paquetes: ',paste(faltantes,collapse=', '),
      '. Instalar primero con install.packages(c(',
      paste(sprintf('"%s"',faltantes),collapse=','),'))',call.=FALSE)
  distintos <- paquetes[presentes & versiones != referencia]
  if (length(distintos)) warning('Versiones distintas a la reproduccion de referencia: ',
      paste(distintos,collapse=', '), '. No implica necesariamente fallo, pero debe documentarse.')
  cat('9 CSV originales: OK. Rscript: OK. Paquetes: OK.\n')
  cat('ENTORNO LISTO PARA INTENTAR LA REPRODUCCION AISLADA.\n')
  cat('Esta prueba no reemplaza la Etapa 12 ni demuestra portabilidad entre PC.\n')
})
