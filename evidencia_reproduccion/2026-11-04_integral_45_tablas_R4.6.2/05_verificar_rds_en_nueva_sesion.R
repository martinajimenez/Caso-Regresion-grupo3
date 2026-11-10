args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)>=1L)
setwd(args[1L])
z <- readRDS('resultados/11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds')
stopifnot(is.list(z),is.function(z$predecir),identical(z$n_entrenamiento,75172L))
b <- read.csv('resultados/02_variables_candidatas/01_variables_candidatas_desarrollo.csv',check.names=FALSE,stringsAsFactors=FALSE)
vars <- z$predictores_originales
stopifnot(length(vars)==10L,all(vars %in% names(b)))
idx <- which(complete.cases(b[,vars,drop=FALSE]))[1:100]
stopifnot(length(idx)==100L,!anyNA(idx))
p <- z$predecir(b[idx,,drop=FALSE])
stopifnot(length(p)==100L,all(is.finite(p)))
writeLines(c('RDS ABIERTO EN PROCESO NUEVO','100 predicciones finitas',
             paste('Configuracion:',as.character(z$hiperparametros$configuracion))),
             '12_control/06_RDS_NUEVA_SESION_APROBADO.txt')
cat('RDS LEIDO Y PREDICHO EN NUEVA SESION SIN OBJETOS PREVIOS\n')
