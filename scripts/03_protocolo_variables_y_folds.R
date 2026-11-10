# =============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3
# ETAPA 3 v1.1 - Correlaciones exploratorias sin fuga temporal
# =============================================================================
# OPCION RECOMENDADA SOBRE PROYECTO YA EJECUTADO HASTA ETAPA 8:
# options(olist.solo_correlaciones = TRUE)
# source("scripts/03_protocolo_variables_y_folds.R", encoding = "UTF-8")
# options(olist.solo_correlaciones = FALSE)
# Esta opcion solo LEE etapas 01/02/03 y AGREGA resultados/03.../correlaciones.
# NO sobrescribe los folds, NO entrena modelos, NO consulta el test.
# Sin opcion, ejecuta el protocolo original de Etapa 3 sin alterar su logica.
# =============================================================================

if (isTRUE(getOption("olist.solo_correlaciones", FALSE))) {
  local({
    options(stringsAsFactors = FALSE)
    raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    salida_original <- file.path(raiz, "resultados", "03_protocolo_y_folds")
    resultados <- file.path(salida_original, "correlaciones")
    graficos <- file.path(raiz, "graficos", "03_correlaciones")
    entradas <- c(
      variables = file.path(raiz,"resultados","02_variables_candidatas", "01_variables_candidatas_desarrollo.csv"),
      etiquetas = file.path(raiz,"resultados","01_revision_datos", "06_etiquetas_solo_desarrollo.csv"),
      folds = file.path(salida_original,"04_ids_train_validacion.csv"),
      especificaciones = file.path(salida_original,"02_especificaciones_previas.csv"))
    if (!all(file.exists(entradas))) {
      stop(paste("Faltan entradas:", paste(entradas[!file.exists(entradas)], collapse="; ")),
           call.=FALSE)
    }
    # Fotografia de TODOS los resultados previos (CSV, TXT, RDS), para garantizar
    # que esta etapa exploratoria no modifique archivos de modelamiento.
    carpeta_res <- file.path(raiz,"resultados")
    previos <- list.files(carpeta_res, pattern="\\.(csv|txt|rds)$",
                          full.names=TRUE, recursive=TRUE, ignore.case=TRUE)
    previos <- previos[!startsWith(previos, paste0(resultados, "/"))]
    md5_antes <- if (length(previos)) tools::md5sum(previos) else character(0)

    cat("[1/5] Leyendo variables, etiquetas y folds archivados...\n")
    b <- utils::read.csv(entradas["variables"], stringsAsFactors=FALSE,
                         na.strings=c("", "NA"), check.names=FALSE)
    y <- utils::read.csv(entradas["etiquetas"], stringsAsFactors=FALSE,
                         na.strings=c("", "NA"), check.names=FALSE)
    ids <- utils::read.csv(entradas["folds"], stringsAsFactors=FALSE,
                           na.strings=c("", "NA"), check.names=FALSE)
    var_num <- c("horas_desde_compra","n_items","n_vendedores",
                 "precio_total_brl","flete_total_brl","peso_total_g",
                 "volumen_total_cm3","todos_vendedores_mismo_estado")
    var_cat <- c("hora_aprobacion", "dia_semana", "mes_aprobacion", "estado_cliente")
    req_b <- c("order_id","fecha_aprobacion",var_num,var_cat)
    if (!all(req_b %in% names(b)) ||
        !all(c("order_id","dias_entrega") %in% names(y)) ||
        !all(c("fold","rol","order_id") %in% names(ids))) {
      stop("Las columnas no coinciden con el protocolo de Etapas 1-3.",call.=FALSE)
    }
    if (nrow(b)!=86162L || nrow(y)!=81950L ||
        anyDuplicated(b$order_id) || anyDuplicated(y$order_id) ||
        anyDuplicated(paste(ids$fold,ids$rol,ids$order_id,sep="|"))) {
      stop("Inconsistencia de conteos o IDs.",call.=FALSE)
    }
    if (!identical(sort(unique(ids$fold)),1:4) ||
        !setequal(unique(ids$rol),c("train","validacion"))) {
      stop("Folds archivados no coinciden con el protocolo original.",call.=FALSE)
    }
    if (any(!vapply(b[var_num],is.numeric,logical(1)))) {
      stop("Alguna variable numerica contiene datos no numericos.",call.=FALSE)
    }
    dir.create(resultados,recursive=TRUE,showWarnings=FALSE)
    dir.create(graficos,recursive=TRUE,showWarnings=FALSE)
    write_csv <- function(x,nombre) utils::write.csv(x,file.path(resultados,nombre),
                                                      row.names=FALSE,na="")

    # Para correlaciones simultaneas se usa UNA MISMA muestra de casos completos
    # por fold. No se imputa utilizando otros periodos, no se filtra segun test.
    # Por diseno, las 4 variables de calendario/estado se analizan APARTE mediante
    # eta-cuadrado (asociacion descriptiva, NO importancia predictiva).
    cor_pearson <- vector("list",4L); cor_spearman <- vector("list",4L)
    objetivo <- vector("list",4L); categ <- vector("list",4L)
    conteos <- vector("list",4L)
    nombres_meses <- c("2017-12","2018-01","2018-02","2018-03")
    cat("[2/5] Calculando correlaciones solo en el TRAIN de cada fold...\n")
    for (k in 1:4) {
      id_train <- ids$order_id[ids$fold==k & ids$rol=="train"]
      id_valid <- ids$order_id[ids$fold==k & ids$rol=="validacion"]
      if (length(id_train)<1000L || anyDuplicated(id_train) ||
          length(intersect(id_train,id_valid))) {
        stop(paste("Train/validacion invalidos en fold",k),call.=FALSE)
      }
      ib <- match(id_train,b$order_id)
      iy <- match(id_train,y$order_id)
      if (anyNA(ib) || anyNA(iy)) {
        stop(paste("Pedido sin datos/etiqueta disponible en train del fold",k),call.=FALSE)
      }
      tb <- b[ib,,drop=FALSE]
      resp <- y$dias_entrega[iy]
      if (!is.numeric(resp) || anyNA(resp) || any(!is.finite(resp)) || any(resp<=0)) {
        stop("Respuesta de TRAIN invalida.",call.=FALSE)
      }
      lim <- as.POSIXct(paste0(nombres_meses[k],"-01 00:00:00"),tz="UTC")
      fecha <- as.POSIXct(tb$fecha_aprobacion,format="%Y-%m-%d %H:%M:%S",tz="UTC")
      if (anyNA(fecha) || any(fecha > lim - as.difftime(45,units="days"))) {
        stop(paste("Madurez temporal invalida en fold",k),call.=FALSE)
      }
      num <- tb[,var_num,drop=FALSE]
      cc <- stats::complete.cases(num) & is.finite(resp)
      dat <- num[cc,,drop=FALSE]
      yy <- resp[cc]
      if (nrow(dat)<1000L || any(vapply(dat,function(v) stats::sd(v)==0,logical(1)))) {
        stop(paste("Muestra completa insuficiente o variables constantes en fold",k),call.=FALSE)
      }
      p <- stats::cor(dat,method="pearson")
      s <- stats::cor(dat,method="spearman")
      cor_pearson[[k]] <- p; cor_spearman[[k]] <- s
      objetivo[[k]] <- data.frame(fold=k,mes_validacion=nombres_meses[k],
         variable=var_num,n_casos_completos=nrow(dat),
         pearson=vapply(dat,function(v) stats::cor(v,yy,method="pearson"),numeric(1)),
         spearman=vapply(dat,function(v) stats::cor(v,yy,method="spearman"),numeric(1)),
         stringsAsFactors=FALSE)
      # eta2: proporcion descriptiva de variabilidad entre categorias de TRAIN.
      # No comparar directamente eta2 con r de Pearson ni usarlo como filtro.
      eta2 <- function(categoria,resp) {
        f <- as.character(categoria)
        f[is.na(f)|trimws(f)==""] <- "DESCONOCIDO"
        n <- table(f)
        medias <- tapply(resp,f,mean)
        gran_media <- mean(resp)
        sst <- sum((resp-gran_media)^2)
        c(eta2=if (sst>0) sum(as.numeric(n)*(medias-gran_media)^2)/sst else NA_real_,
          categorias=length(n))
      }
      ec <- t(vapply(var_cat,function(v) eta2(tb[[v]],resp),numeric(2)))
      categ[[k]] <- data.frame(fold=k,mes_validacion=nombres_meses[k],
                             variable=var_cat,n_train=length(resp),
                             numero_categorias=as.integer(ec[,"categorias"]),
                             eta_cuadrado=ec[,"eta2"],stringsAsFactors=FALSE)
      conteos[[k]] <- data.frame(fold=k,mes_validacion=nombres_meses[k],
        n_train=length(resp),n_completos=nrow(dat),
        n_omitidos_solo_para_correlacion=length(resp)-nrow(dat),
        n_validacion_NO_utilizados=length(id_valid),stringsAsFactors=FALSE)
      cat(sprintf("  Fold %d (%s): TRAIN=%d; completos=%d; validacion no usada=%d\n",
                  k,nombres_meses[k],length(resp),nrow(dat),length(id_valid)))
    }
    # Datos de matriz en formato largo, por fold y por metodo
    larga <- function(mat,k,metodo) {
      z <- as.data.frame(as.table(mat),stringsAsFactors=FALSE)
      names(z) <- c("variable_fila","variable_columna","r")
      z$fold <- k; z$mes_validacion <- nombres_meses[k]; z$metodo <- metodo
      z[,c("fold","mes_validacion","metodo","variable_fila","variable_columna","r")]
    }
    mats <- do.call(rbind,c(lapply(1:4,function(k) larga(cor_pearson[[k]],k,"Pearson")),
                            lapply(1:4,function(k) larga(cor_spearman[[k]],k,"Spearman"))))
    obj <- do.call(rbind,objetivo)
    catdf <- do.call(rbind,categ)
    ndf <- do.call(rbind,conteos)
    write_csv(mats,"01_matrices_correlacion_train_por_fold.csv")
    write_csv(obj,"02_correlacion_numericas_vs_respuesta_train.csv")
    write_csv(catdf,"03_asociacion_categoricas_eta2_train.csv")
    write_csv(ndf,"04_conteos_train_y_casos_completos.csv")
    # Colinealidad descriptiva: asociaciones absolutas por pares (no incluye respuesta).
    cols <- data.frame()
    for (k in 1:4) {
      for (i in seq_len(length(var_num)-1L)) for (j in (i+1L):length(var_num)) {
        cols <- rbind(cols,data.frame(fold=k,mes_validacion=nombres_meses[k],
          variable_1=var_num[i],variable_2=var_num[j],
          pearson=cor_pearson[[k]][i,j],
          spearman=cor_spearman[[k]][i,j]))
      }
    }
    write_csv(cols,"05_asociaciones_entre_predictores_train.csv")

    # Figuras limpias en R base, sin ggplot2/corrplot ni instalaciones extra.
    # Mostrar ultima muestra TRAIN por ser la mas grande, NUNCA validacion.
    etiquetas <- c("Horas compra", "N. articulos", "N. vendedores", "Precio (BRL)",
                   "Flete (BRL)", "Peso (g)", "Volumen", "Mismo estado")
    relleno <- function(v) {
      if (is.na(v)) return("#E8EDF1")
      if (v<0) grDevices::colorRampPalette(c("#B44E3B","#FFFFFF"))(101L)[min(101L,max(1L,round((v+1)*100)+1L))]
      else grDevices::colorRampPalette(c("#FFFFFF","#116E69"))(101L)[min(101L,max(1L,round(v*100)+1L))]
    }
    dibujar_matriz <- function(mat,metodo) {
      m <- nrow(mat)
      graphics::par(mar=c(8.5,11,5,2),family="sans",cex=1)
      graphics::plot.new()
      graphics::plot.window(xlim=c(.5,m+.5),ylim=c(.5,m+.5),asp=1)
      for (i in seq_len(m)) for (j in seq_len(i)) {
        yy <- m-i+1
        graphics::rect(j-.48,yy-.48,j+.48,yy+.48,col=relleno(mat[i,j]),border="white",lwd=1.5)
        vv <- mat[i,j]
        graphics::text(j,yy,if (is.finite(vv)) sprintf("%+.2f",vv) else "NA",
                       cex=.72,col=if(is.finite(vv) && abs(vv)>.63) "white" else "#203344")
      }
      graphics::axis(1,at=seq_len(m),labels=etiquetas,las=2,cex.axis=.8,tick=FALSE)
      graphics::axis(2,at=m:1,labels=etiquetas,las=1,cex.axis=.8,tick=FALSE)
      graphics::title(main=paste("Correlaciones",metodo,"entre predictores"),cex.main=1.3)
      graphics::mtext("TRAIN del fold marzo 2018 | Triangulo inferior",side=3,line=.2,cex=.83,col="#526D7B")
      graphics::mtext("Asociacion descriptiva: no mide causalidad ni importancia predictiva",side=1,
                      line=7.2,cex=.75,col="#53697A")
    }
    cat("[3/5] Dibujando matrices Pearson y Spearman y asociaciones con respuesta...\n")
    dibujar_en_dispositivos <- function(nombre,fun,width=1800,height=1500) {
      fp <- file.path(graficos,paste0(nombre,".png"))
      grDevices::png(fp,width=width,height=height,res=180)
      tryCatch(fun(),finally=grDevices::dev.off())
      grDevices::pdf(file.path(graficos,paste0(nombre,".pdf")),width=width/180,height=height/180,
                     family="Helvetica")
      tryCatch(fun(),finally=grDevices::dev.off())
      if (!file.exists(fp) || file.info(fp)$size<1000) stop("PNG no generado correctamente.")
    }
    dibujar_en_dispositivos("01_matriz_pearson_train_fold4",
                           function() dibujar_matriz(cor_pearson[[4]],"Pearson"))
    dibujar_en_dispositivos("02_matriz_spearman_train_fold4",
                           function() dibujar_matriz(cor_spearman[[4]],"Spearman"))
    ultimo <- objetivo[[4]]
    # Grafico de puntos, escala simetrica (no barras truncadas).
    grafico_respuesta <- function() {
      lim <- max(.15,ceiling(max(abs(c(ultimo$pearson,ultimo$spearman)))*12)/10)
      nn <- nrow(ultimo)
      graphics::par(mar=c(6,11,5,3),family="sans")
      graphics::plot(NA,xlim=c(-lim,lim),ylim=c(.5,nn+.5),axes=FALSE,
                     xlab="",ylab="")
      graphics::abline(v=0,lty=2,col="#9AA8B4",lwd=1.3)
      graphics::axis(1,cex.axis=.85,col.axis="#34475A")
      graphics::axis(2,at=nn:1,labels=etiquetas,las=1,tick=FALSE,cex.axis=.87)
      graphics::segments(ultimo$pearson,nn:1,ultimo$spearman,nn:1,col="#CAD4DB",lwd=2)
      graphics::points(ultimo$pearson,nn:1,pch=16,cex=1.45,col="#146D79")
      graphics::points(ultimo$spearman,nn:1,pch=17,cex=1.25,col="#D5783A")
      graphics::title(main="Asociacion de predictores con dias de entrega",cex.main=1.2)
      graphics::mtext("TRAIN marzo 2018 | Comparacion Pearson y Spearman",side=3,line=.2,cex=.85,col="#526D7B")
      graphics::mtext("Coeficiente de correlacion (−1 a +1); no equivale a importancia predictiva",
                      side=1,line=3.2,cex=.8,col="#53697A")
      graphics::mtext("Pearson: punto azul  |  Spearman: triangulo naranja",side=1,
                      line=4.7,cex=.82,col="#53697A")
    }
    dibujar_en_dispositivos("03_correlacion_vs_respuesta_train_fold4",grafico_respuesta,
                           width=1700,height=1100)
    cat("[4/5] Documentando limitaciones y comprobando integridad...\n")
    notas <- c(
      "ETAPA 3 v1.1: CORRELACIONES DESCRIPTIVAS SOLO DE ENTRENAMIENTO",
      "Las matrices estan disponibles por fold y metodo (CSV); las figuras ilustran TRAIN del fold 4.",
      "Todos los calculos con dias_entrega usan SOLO TRAIN de cada fold (sin validation/test).",
      "Correlaciones de numericas: Pearson lineal y Spearman monotona/por rangos.",
      "La variable binaria mismo_estado admite asociacion punto-biserial via Pearson.",
      "Hora, dia de semana, mes y estado cliente se tratan como CATEGORICAS (eta2);",
      "no asignar numeros arbitrarios a los estados para aplicar correlacion Pearson.",
      "Los valores NA de las matrices no se imputan: muestra comun de casos completos",
      "exclusivamente para el diagnostico y con su tamano informado por fold.",
      "Eta-cuadrado no se interpreta directamente como coeficiente de Pearson y",
      "puede verse favorecido por muchas categorias; no mide importancia del predictor.",
      "No existen umbrales de r o p-valores para eliminar automaticamente variables.",
      "La seleccion de variables futura debera hacerse SOLO en TRAIN de cada fold",
      "externo, con validaciones INTERNAS para elegir especificaciones; mantener",
      "identicos pedidos de validacion para comparar los modelos.",
      "B y C tienen supuestos pendientes de disponibilidad historica al aprobar.",
      "El test historico julio-agosto 2018 ya es conocido y NO fue utilizado.",
      "NINGUN MODELO ni hiperparametro se ha cambiado en esta ampliacion.",
      "Etapas 01-08: archivos de resultados previos deben conservar sus MD5.")
    writeLines(notas,file.path(resultados,"06_interpretacion_y_reglas.txt"),useBytes=TRUE)
    md5_despues <- if (length(previos)) tools::md5sum(previos) else character(0)
    if (!identical(unname(md5_antes),unname(md5_despues))) {
      stop("ALERTA: cambio algun resultado anterior; revisar antes de continuar.",call.=FALSE)
    }
    ctrl <- c("CONTROL DE INTEGRIDAD ETAPA 3 v1.1: APROBADO",
              paste("Archivos historicos protegidos por MD5:",length(previos)),
              paste("Filas de matrices:",nrow(mats)),
              paste("Folds:",paste(ndf$fold,collapse=",")),
              "Entradas antiguas: sin modificacion.",
              "Validacion externa y test: no utilizados en correlaciones.",
              "Modelos, predicciones y metricas originales: intactos.")
    writeLines(ctrl,file.path(resultados,"07_CONTROL_CORRELACIONES_APROBADO.txt"),useBytes=TRUE)
    cat("[5/5] ETAPA 3 v1.1: CORRELACIONES COMPLETADAS; SOLO ENTRENAMIENTO\n")
    print(ndf,row.names=FALSE)
    cat("Matrices y tablas:",resultados,"\nGraficos:",graficos,"\n")
    cat("NO se modificaron los resultados de Etapas 1-8.\n")
  })
} else {
# =============================================================================
# UNIVERSIDAD DE SANTIAGO DE CHILE | ESTADISTICA APLICADA | GRUPO 3
# OLIST - ETAPA 3: PROTOCOLO DE VARIABLES Y FOLDS TEMPORALES
# VERSION 1.0 | R BASE | NO AJUSTA MODELOS | NO EVALUA TEST
# =============================================================================
# Ejecutar desde la raiz Proyecto_Olist_caso1 en una SESION LIMPIA:
# source("scripts/03_protocolo_variables_y_folds.R", encoding = "UTF-8")
# Requisitos: haber ejecutado los scripts 01 y 02 en esta carpeta.
# Entrada: dataset ZIP ORIGINAL; salidas de etapas 01 y 02.
# Salida: resultados/03_protocolo_y_folds/ (todo autogenerado).
# No elige variables por RMSE/R2; registra especificaciones de negocio.
# El test de julio-agosto 2018 NO se consulta ni se usa para optimizar.
# =============================================================================

options(stringsAsFactors = FALSE)
raiz03 <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
r1 <- file.path(raiz03, "resultados", "01_revision_datos")
r2 <- file.path(raiz03, "resultados", "02_variables_candidatas")
salida03 <- file.path(raiz03, "resultados", "03_protocolo_y_folds")
zip03 <- file.path(raiz03, "datos_originales", "dataset.zip")
variables_arch <- file.path(r2, "01_variables_candidatas_desarrollo.csv")
etiquetas_arch <- file.path(r1, "06_etiquetas_solo_desarrollo.csv")
for (archivo in c(zip03, variables_arch, etiquetas_arch,
                  file.path(r2, "04_faltantes_por_variable.csv"))) {
  if (!file.exists(archivo)) stop(paste("Falta archivo requerido:", archivo), call. = FALSE)
}
dir.create(salida03, recursive = TRUE, showWarnings = FALSE)

# Utilidades de lectura y control
fecha03 <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
leer_zip03 <- function(nombre) {
  con <- unz(zip03, nombre, open = "r")
  on.exit(close(con), add = TRUE)
  utils::read.csv(con, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, stringsAsFactors = FALSE)
}
escribir03 <- function(tabla, nombre) {
  utils::write.csv(tabla, file.path(salida03, nombre),
                   row.names = FALSE, na = "")
}
mensaje03 <- function(txt) message(txt)

mensaje03("[1/6] Comprobando consistencia de etapas 1 y 2...")
b <- utils::read.csv(variables_arch, stringsAsFactors = FALSE,
                     check.names = FALSE, na.strings = c("", "NA"))
y <- utils::read.csv(etiquetas_arch, stringsAsFactors = FALSE,
                     check.names = FALSE, na.strings = c("", "NA"))
vars03 <- c("hora_aprobacion", "dia_semana", "mes_aprobacion",
            "horas_desde_compra", "estado_cliente", "n_items", "n_vendedores",
            "precio_total_brl", "flete_total_brl", "peso_total_g",
            "volumen_total_cm3", "todos_vendedores_mismo_estado")
req03 <- c("order_id", "fecha_aprobacion", vars03)
if (length(setdiff(req03, names(b))) ||
    length(setdiff(c("order_id", "fecha_aprobacion", "dias_entrega"), names(y)))) {
  stop("Faltan columnas necesarias en resultados previos.", call. = FALSE)
}
if (nrow(b) != 86162L || nrow(y) != 81950L ||
    anyDuplicated(b$order_id) || anyDuplicated(y$order_id)) {
  stop("Conteo/identificadores incompatibles con Etapas 1 y 2.", call. = FALSE)
}
if (!all(y$order_id %in% b$order_id)) {
  stop("Hay etiquetas que no pertenecen a desarrollo.", call. = FALSE)
}
a <- fecha03(b$fecha_aprobacion)
if (anyNA(a) || any(a < fecha03("2017-01-01 00:00:00")) ||
    any(a >= fecha03("2018-07-01 00:00:00"))) {
  stop("Fechas de aprobacion fuera de la ventana de desarrollo.", call. = FALSE)
}
if (anyNA(y$dias_entrega) || any(!is.finite(y$dias_entrega)) || any(y$dias_entrega <= 0)) {
  stop("Etiquetas de respuesta invalidas.", call. = FALSE)
}

mensaje03("[2/6] Definiendo tres especificaciones previas al modelamiento...")
# A: informacion temporal claramente observable al aprobar (referencia).
# B: detalle de pedido y geografia, con disponibilidad historica no demostrada.
# C: agrega metadatos del catalogo de productos; uso EXPLORATORIO.
# Ninguna especificacion se declara superior antes de validacion.
A <- c("hora_aprobacion", "dia_semana", "mes_aprobacion", "horas_desde_compra")
B <- c(A, "estado_cliente", "n_items", "n_vendedores",
       "precio_total_brl", "flete_total_brl", "todos_vendedores_mismo_estado")
C <- c(B, "peso_total_g", "volumen_total_cm3")
if (!setequal(C, vars03)) stop("No se incluyeron las 12 candidatas.", call. = FALSE)

politica <- data.frame(
  variable = vars03,
  tipo = c("numerica_hora", "categorica_dia", "categorica_mes",
           "numerica_no_negativa", "categorica_estado", "conteo", "conteo",
           "numerica_brl", "numerica_brl", "numerica_gramos",
           "numerica_cm3", "binaria"),
  especificaciones = vapply(vars03, function(z) {
    paste(c(if (z %in% A) "A", if (z %in% B) "B", if (z %in% C) "C"),
          collapse = "/")
  }, character(1)),
  transformacion_propuesta = c("factor o seno/coseno (ajustar solo en train)",
        "factor semana (no tratar como escala continua automaticamente)",
        "factor mes (no tratar como escala continua automaticamente)",
        "log1p o escala robusta si lo justifica validacion",
        "categorica con categoria DESCONOCIDO para faltantes",
        "conteo, revisar sesgo y posible log1p",
        "conteo, revisar sesgo y posible log1p",
        "log1p, transformacion fija sin informacion del test",
        "log1p, transformacion fija sin informacion del test",
        "imputar media TRAIN de cada fold; indicador de faltante a evaluar",
        "imputar media TRAIN de cada fold; indicador de faltante a evaluar",
        "factor 0/1; no imputar media como categoria"),
  regla_faltantes = c(rep("sin faltantes detectados; verificar en train", 4),
      "categoria DESCONOCIDO solo si faltan estados",
      rep("faltante estructural: estudiar y justificar; no inventar items", 4),
      "media aritmetica en train del fold; no usar toda la base",
      "media aritmetica en train del fold; no usar toda la base",
      "categoria DESCONOCIDO si falta; no imputar 0"),
  disponibilidad_al_aprobar = c(rep("derivable al aprobar", 4),
      rep("supuesto historico pendiente de verificar", 8)),
  stringsAsFactors = FALSE
)
if (nrow(politica) != 12L) stop("Error en plan de 12 variables.", call. = FALSE)
escribir03(politica, "01_plan_de_variables.csv")

defspec <- data.frame(
  especificacion = c("A_temporal", "B_pedido_geografia", "C_catalogo_exploratoria"),
  numero_variables = c(length(A), length(B), length(C)),
  variables = c(paste(A, collapse = "; "), paste(B, collapse = "; "),
                paste(C, collapse = "; ")),
  justificacion = c("referencia con atributos directamente fechados al aprobar",
      "anade informacion de pedido y rutas; supuesto de disponibilidad historica",
      "anade peso y volumen del catalogo; riesgo adicional de vigencia historica"),
  stringsAsFactors = FALSE
)
escribir03(defspec, "02_especificaciones_previas.csv")

mensaje03("[3/6] Reconstruyendo la observabilidad de etiquetas en cada corte...")
ord <- leer_zip03("olist_orders_dataset.csv")
reqord <- c("order_id", "order_approved_at", "order_delivered_customer_date", "order_status")
if (length(setdiff(reqord, names(ord))) || anyDuplicated(ord$order_id)) {
  stop("Orders original tiene esquema o IDs inconsistentes.", call. = FALSE)
}
idx <- match(b$order_id, ord$order_id)
if (anyNA(idx)) stop("Candidatas sin pedido en orders original.", call. = FALSE)
ord <- ord[idx, reqord, drop = FALSE]
ord_ap <- fecha03(ord$order_approved_at)
ord_ent <- fecha03(ord$order_delivered_customer_date)
if (anyNA(ord_ap) || any(abs(as.numeric(difftime(ord_ap, a, units="secs"))) > 0)) {
  stop("Las fechas de aprobacion difieren entre etapas.", call. = FALSE)
}
cut_global <- fecha03("2018-07-01 00:00:00")
etiqueta_historica <- ord$order_status == "delivered" &
   !is.na(ord_ent) & ord_ent < cut_global & ord_ent > ord_ap
etiqueta_historica[is.na(etiqueta_historica)] <- FALSE
if (sum(etiqueta_historica) != nrow(y) ||
    !setequal(b$order_id[etiqueta_historica], y$order_id)) {
  stop("Las etiquetas historicas no coinciden con Etapa 1.", call. = FALSE)
}
# Los pedidos sin articulos no tienen etiqueta historica en esta muestra.
# No se excluyen por inercia: la politica aplica a train/validacion concretos.
if (any(etiqueta_historica & is.na(b$n_items))) {
  stop("Aparecieron pedidos etiquetados sin articulos: revisar faltantes.", call. = FALSE)
}

mensaje03("[4/6] Creando folds temporales y garantizando train anterior a validacion...")
# Ventanas de evaluacion: diciembre 2017 a marzo 2018.
# Train: pedido aprobado >= 45 dias ANTES del fold, entregado ANTES del inicio.
# Validacion: aprobado en el mes; etiqueta conocida a fecha global de desarrollo.
# Recomendacion metodologica: esta validacion SOLO representa pedidos entregados.
# Su seguimiento temporal NO es identico entre meses y puede sesgar el extremo.
comienzos <- as.POSIXct(c("2017-12-01 00:00:00", "2018-01-01 00:00:00",
                          "2018-02-01 00:00:00", "2018-03-01 00:00:00"), tz="UTC")
finales <- as.POSIXct(c("2018-01-01 00:00:00", "2018-02-01 00:00:00",
                       "2018-03-01 00:00:00", "2018-04-01 00:00:00"), tz="UTC")
meses <- c("2017-12", "2018-01", "2018-02", "2018-03")
fold_resumen <- vector("list", length(meses))
fold_filas <- vector("list", length(meses))
for (k in seq_along(meses)) {
  ini <- comienzos[k]
  fin <- finales[k]
  limite_antiguedad <- ini - as.difftime(45, units="days")
  train <- which(a <= limite_antiguedad & etiqueta_historica & ord_ent < ini)
  validar <- which(a >= ini & a < fin & etiqueta_historica)
  if (!length(train) || !length(validar)) stop("Fold sin train o validacion.", call. = FALSE)
  if (length(intersect(train, validar)) ||
      any(a[train] > limite_antiguedad) || any(ord_ent[train] >= ini) ||
      any(a[validar] < ini) || any(a[validar] >= fin)) {
    stop("Fuga temporal o solapamiento detectados.", call. = FALSE)
  }
  if (anyNA(b$n_items[c(train, validar)])) {
    stop("Faltantes estructurales de items en train/validacion del fold; decidir tratamiento.",
         call. = FALSE)
  }
  miss_peso_train <- sum(is.na(b$peso_total_g[train]))
  miss_peso_valid <- sum(is.na(b$peso_total_g[validar]))
  miss_vol_train <- sum(is.na(b$volumen_total_cm3[train]))
  miss_vol_valid <- sum(is.na(b$volumen_total_cm3[validar]))
  fold_resumen[[k]] <- data.frame(
    fold = k, mes_validacion = meses[k],
    fecha_corte_entrenamiento = format(ini, "%Y-%m-%d %H:%M:%S", tz="UTC"),
    ultima_aprobacion_elegible = format(limite_antiguedad, "%Y-%m-%d %H:%M:%S", tz="UTC"),
    n_train = length(train), n_validacion = length(validar),
    faltantes_peso_train = miss_peso_train, faltantes_peso_validacion = miss_peso_valid,
    faltantes_volumen_train = miss_vol_train, faltantes_volumen_validacion = miss_vol_valid,
    stringsAsFactors = FALSE
  )
  fold_filas[[k]] <- rbind(
    data.frame(fold=k, mes_validacion=meses[k], rol="train", order_id=b$order_id[train],
               stringsAsFactors=FALSE),
    data.frame(fold=k, mes_validacion=meses[k], rol="validacion", order_id=b$order_id[validar],
               stringsAsFactors=FALSE)
  )
}
res03 <- do.call(rbind, fold_resumen)
ids03 <- do.call(rbind, fold_filas)
if (anyDuplicated(paste(ids03$fold, ids03$rol, ids03$order_id, sep="|"))) {
  stop("IDs duplicados dentro de un rol/fold.", call. = FALSE)
}
escribir03(res03, "03_resumen_folds_temporales.csv")
escribir03(ids03, "04_ids_train_validacion.csv")

mensaje03("[5/6] Verificando que la madurez global y los faltantes coincidan...")
maduro_global <- a <= cut_global - as.difftime(45, units="days")
if (sum(maduro_global & etiqueta_historica) != 75172L) {
  stop("El total de etiquetas maduras no coincide con Etapa 2.", call. = FALSE)
}
faltantes <- utils::read.csv(file.path(r2, "04_faltantes_por_variable.csv"),
                             stringsAsFactors = FALSE)
for (z in vars03) {
  esperado <- faltantes$n_faltante[match(z, faltantes$variable)]
  if (length(esperado) != 1L || is.na(esperado) || sum(is.na(b[[z]])) != esperado) {
    stop(paste("Faltantes no coinciden para",z), call. = FALSE)
  }
}

mensaje03("[6/6] Escribiendo reglas estadisticas y limitaciones...")
nota03 <- c(
  "ETAPA 3: PROTOCOLO PREFIJADO ANTES DE ENTRENAR MODELOS NUEVOS",
  "Se usan exclusivamente pedidos aprobados en desarrollo: 2017-01-01 a 2018-06-30.",
  "El test historico julio-agosto de 2018 es CONOCIDO: no volvera a ser independiente.",
  "Se compararan las mismas familias bajo LOS MISMOS folds de validacion.",
  "Cuatro validaciones: dic-2017, ene-2018, feb-2018, mar-2018.",
  "Train de cada fold: aprobacion con al menos 45 dias de antiguedad respecto al corte",
  "Y entrega real observada antes del inicio del mes validado.",
  "Validacion: aprobacion en su mes y etiqueta conocida al corte global 2018-07-01.",
  "LIMITACION: solo pedidos entregados; censura y seguimiento desigual entre meses.",
  "Especificacion A: solo fechas/demora compra-aprobacion; evidencia directa al aprobar.",
  "Especificacion B: incluye geografia y caracteristicas de pedido; vigencia por verificar.",
  "Especificacion C: incluye metadatos del catalogo; vigencia historica no demostrada.",
  "No atribuir causalidad a diferencias geograficas descriptivas.",
  "Pedido sin detalle de items: faltante ESTRUCTURAL, no completar inventando media.",
  "Solo 16 etiquetas maduras carecen de peso/volumen completos; revisar en cada fold.",
  "Si se usa peso/volumen: media aritmetica exclusivamente de TRAIN de cada fold;",
  "verificar no negatividad y considerar alternativa sin esas variables o casos completos.",
  "NO usar promedio de toda la base; NO imputar fecha real de entrega ni respuesta.",
  "Los mismos IDs de validacion se usaran para comparar todas las especificaciones.",
  "Las conversiones, codificaciones, imputaciones y selecciones deben ajustarse en TRAIN.",
  "Reglas de complejidad e hiperparametros se definirán y ejecutarán dentro del train.",
  "alpha=0.05 es significancia para inferencia (IC 95%): NO criterio para filtrar R2.",
  "alpha de elastic-net es un HIPERPARAMETRO distinto de la significancia 0.05.",
  "La tardanza no se usa como predictor de la duracion: requiere la entrega futura.",
  "No hay modelos ajustados ni RMSE, MAE, R2 o test calculados en esta etapa.",
  "Siguiente: ETAPA 4 - preprocesamiento seguro, modelos y comparacion de validacion."
)
writeLines(nota03, file.path(salida03, "05_protocolo_y_limitaciones.txt"), useBytes=TRUE)
cat("\n",strrep("=",71),"\n",sep="")
cat("ETAPA 3 COMPLETADA - PROTOCOLO Y FOLDS; SIN MODELOS NI TEST\n")
cat("Especificaciones A/B/C:",length(A),"/",length(B),"/",length(C),"variables\n")
for(k in seq_len(nrow(res03))) {
  cat("Fold",k, res03$mes_validacion[k], "| train:", res03$n_train[k],
      "| validacion:",res03$n_validacion[k],"\n")
}
cat("Etiquetas maduras al corte global:", sum(maduro_global & etiqueta_historica),"\n")
cat("Carpeta de resultados:",salida03,"\n")
cat(strrep("=",71),"\n",sep="")

} # Fin modo original / correlaciones v1.1
