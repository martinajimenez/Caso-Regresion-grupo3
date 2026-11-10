# =============================================================================
# USACH - ESTADISTICA APLICADA - GRUPO 3 / OLIST
# ETAPA 09 | INFERENCIA ASOCIATIVA Y DIAGNOSTICOS DEL OLS B
# R BASE. NO REENTRENA MODELOS DE COMPARACION NI LEE EL TEST HISTORICO.
# Muestra inferencial: TRAIN del fold 4 (marzo 2018), con madurez de 45 dias.
# El modelo se reconstruye con la RECETA ORIGINAL de la Etapa 04 y se exige
# coincidencia de TODOS los coeficientes con el archivo de la Etapa 04.
# AVISO: analisis posterior a la primera evaluacion del test. Exploratorio.
# Ejecutar DESDE LA RAIZ DEL PROYECTO:
# source("scripts/09_inferencia_y_diagnostico_ols.R", encoding="UTF-8")
# =============================================================================
local({
  options(stringsAsFactors = FALSE)
  raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  base_result <- file.path(raiz, "resultados")
  salida <- file.path(base_result, "09_inferencia_ols")
  salida_fig <- file.path(raiz, "graficos", "09_inferencia_ols")
  insumos <- c(
    variables = file.path(base_result, "02_variables_candidatas", "01_variables_candidatas_desarrollo.csv"),
    respuesta = file.path(base_result, "01_revision_datos", "06_etiquetas_solo_desarrollo.csv"),
    particiones = file.path(base_result, "03_protocolo_y_folds", "04_ids_train_validacion.csv"),
    resumen_folds = file.path(base_result, "03_protocolo_y_folds", "03_resumen_folds_temporales.csv"),
    coefs_originales = file.path(base_result, "04_baseline_y_ols", "04_coeficientes_ols_por_fold.csv"),
    certificado_4 = file.path(base_result, "04_baseline_y_ols", "11_CONTROL_COMPARABILIDAD_APROBADO.txt")
  )
  if (!dir.exists(file.path(raiz, "scripts")))
    stop("Directorio de trabajo incorrecto: use la raiz Proyecto_Olist_caso1.", call. = FALSE)
  if (any(!file.exists(insumos)))
    stop(paste("Faltan archivos requeridos:", paste(names(insumos)[!file.exists(insumos)], collapse=", ")), call. = FALSE)
  afirmar <- function(ok, texto) if (!isTRUE(ok)) stop(texto, call. = FALSE)
  leer <- function(ruta) utils::read.csv(ruta, check.names=FALSE, stringsAsFactors=FALSE,
                                          na.strings=c("", "NA"))
  escribir <- function(tabla, nombre) utils::write.csv(tabla, file.path(salida,nombre),
                                                      row.names=FALSE, na="")
  fecha <- function(z) as.POSIXct(z, tz="UTC", format="%Y-%m-%d %H:%M:%S")
  cat("[1/7] Verificando fuentes, certificados y proteccion de resultados...\n")
  anteriores <- list.files(base_result, recursive=TRUE, full.names=TRUE,
                           pattern="\\.(csv|txt|rds)$", ignore.case=TRUE)
  ruta_salida <- normalizePath(salida, winslash="/", mustWork=FALSE)
  anteriores <- anteriores[!startsWith(normalizePath(anteriores,winslash="/",mustWork=FALSE),
                                       paste0(ruta_salida,"/"))]
  hash_antes <- tools::md5sum(anteriores)
  dir.create(salida,recursive=TRUE,showWarnings=FALSE)
  dir.create(salida_fig,recursive=TRUE,showWarnings=FALSE)
  certificado <- file.path(salida,"09_CONTROL_ETAPA9_APROBADO.txt")
  if (file.exists(certificado)) file.remove(certificado)
  b <- leer(insumos[["variables"]])
  y <- leer(insumos[["respuesta"]])
  ids <- leer(insumos[["particiones"]])
  folds <- leer(insumos[["resumen_folds"]])
  coefs_originales <- leer(insumos[["coefs_originales"]])
  vars <- c("hora_aprobacion", "dia_semana", "mes_aprobacion", "horas_desde_compra",
            "estado_cliente", "n_items", "n_vendedores", "precio_total_brl",
            "flete_total_brl", "todos_vendedores_mismo_estado")
  afirmar(nrow(b)==86162L && nrow(y)==81950L && nrow(folds)==4L &&
          all(c("order_id","fecha_aprobacion",vars) %in% names(b)) &&
          all(c("order_id","dias_entrega") %in% names(y)) &&
          all(c("fold","rol","order_id") %in% names(ids)),
          "Estructura incompatible con el proyecto verificado.")
  afirmar(!anyDuplicated(b$order_id) && !anyDuplicated(y$order_id) &&
          !anyDuplicated(paste(ids$fold,ids$rol,ids$order_id,sep="|")),
          "Identificadores duplicados: no se prosigue.")
  afirmar(all(fecha(b$fecha_aprobacion) < fecha("2018-07-01 00:00:00")),
          "Se encontro un pedido fuera de desarrollo.")

  cat("[2/7] Recuperando exclusivamente el entrenamiento del fold 4...\n")
  id_train <- ids$order_id[ids$fold == 4L & ids$rol == "train"]
  id_val <- ids$order_id[ids$fold == 4L & ids$rol == "validacion"]
  pos <- match(id_train, b$order_id)
  assert_ok <- length(pos) == 46300L && !anyNA(pos) && !anyDuplicated(id_train) &&
               !length(intersect(id_train,id_val))
  afirmar(assert_ok,"IDs del entrenamiento del fold 4 no coinciden con los originales.")
  d <- b[pos,,drop=FALSE]
  iy <- match(id_train,y$order_id)
  afirmar(!anyNA(iy), "Faltan etiquetas en el entrenamiento.")
  yy <- y$dias_entrega[iy]
  tiempos <- fecha(d$fecha_aprobacion)
  corte <- fecha("2018-03-01 00:00:00")
  afirmar(all(is.finite(yy) & yy > 0) &&
          all(tiempos <= corte - as.difftime(45,units="days")) &&
          all(tiempos + yy*86400 < corte),
          "Hay etiquetas o fechas no observables en el entrenamiento del fold 4.")
  afirmar(all(abs(as.numeric(difftime(fecha(y$fecha_aprobacion[iy]),tiempos,
                                     units="secs")))<0.5),
          "Las fechas de aprobacion no coinciden con las etiquetas.")

  cat("[3/7] Reconstruyendo la matriz de OLS B con la receta original...\n")
  # Reglas EXACTAS del script 04: agrupacion de estados desde TRAIN, ciclos
  # horarios, log1p y no imputacion de variables de B.
  frecuencia <- table(d$estado_cliente)
  frecuentes <- names(frecuencia)[frecuencia >= 100L]
  if (!length(frecuentes)) frecuentes <- names(frecuencia)[which.max(frecuencia)]
  estado <- ifelse(is.na(d$estado_cliente) | !(d$estado_cliente %in% frecuentes),
                   "OTROS", as.character(d$estado_cliente))
  niveles <- sort(unique(estado))
  factor_estado <- factor(estado,levels=niveles)
  afirmar(!anyNA(factor_estado) && length(niveles)>1L, "Estados invalidos.")
  dummy <- stats::model.matrix(~ factor_estado)[,-1,drop=FALSE]
  colnames(dummy) <- paste0("estado_", make.names(niveles[-1]))
  h <- as.numeric(d$hora_aprobacion)
  ds <- as.numeric(d$dia_semana)
  mes <- as.numeric(d$mes_aprobacion)
  hc <- as.numeric(d$horas_desde_compra)
  afirmar(all(is.finite(h) & h>=0 & h<=23) &&
          all(is.finite(ds) & ds>=1 & ds<=7) &&
          all(is.finite(mes) & mes>=1 & mes<=12) &&
          all(is.finite(hc) & hc>=0),"Predictores temporales invalidos.")
  afirmar(all(vapply(d[,c("n_items","n_vendedores","precio_total_brl","flete_total_brl")],
                     function(z) all(is.finite(z) & z>=0),logical(1))) &&
          all(is.finite(d$todos_vendedores_mismo_estado) &
              d$todos_vendedores_mismo_estado %in% c(0,1)),
          "Hay predictores de OLS B faltantes o incorrectos: no imputar sin justificar.")
  X <- cbind("(Intercept)"=rep(1,nrow(d)),
             hora_sin=sin(2*pi*h/24),hora_cos=cos(2*pi*h/24),
             dia_sin=sin(2*pi*(ds-1)/7),dia_cos=cos(2*pi*(ds-1)/7),
             mes_sin=sin(2*pi*(mes-1)/12),mes_cos=cos(2*pi*(mes-1)/12),
             log_horas_compra=log1p(hc), dummy,
             log_items=log1p(d$n_items),
             log_vendedores=log1p(d$n_vendedores),
             log_precio=log1p(d$precio_total_brl),
             log_flete=log1p(d$flete_total_brl),
             vendedor_mismo_estado=as.numeric(d$todos_vendedores_mismo_estado))
  storage.mode(X) <- "double"
  n <- nrow(X); p <- ncol(X)
  afirmar(n==46300L && p < n && all(is.finite(X)), "Matriz mal construida.")
  ajustado <- stats::lm.fit(X, yy, tol=1e-9)
  afirmar(ajustado$rank==p && all(is.finite(ajustado$coefficients)),
          "Matriz singular: no procede inferencia.")
  beta <- as.numeric(ajustado$coefficients)
  names(beta) <- colnames(X)
  guardado <- coefs_originales[coefs_originales$fold==4L &
                                  coefs_originales$modelo=="B_pedido_geografia",,drop=FALSE]
  ix <- match(names(beta), guardado$termino)
  afirmar(nrow(guardado)==p && !anyNA(ix) && !anyDuplicated(guardado$termino),
          "No coincide el conjunto de coeficientes del OLS B archivado.")
  max_dif_beta <- max(abs(beta - guardado$coeficiente[ix]))
  afirmar(is.finite(max_dif_beta) && max_dif_beta < 1e-8,
          paste("No coincide el OLS B original. Maximo error:",max_dif_beta))

  cat("[4/7] Calculando errores estandar clasicos y robustos HC3...\n")
  resid <- as.vector(ajustado$residuals)
  fit <- as.vector(ajustado$fitted.values)
  df <- n-p
  inv <- solve(crossprod(X))
  inv <- (inv+t(inv))/2
  hh <- rowSums((X %*% inv)*X)
  afirmar(all(is.finite(hh)) && all(hh>=-1e-8) && all(hh < 1-1e-9),
          "Leverage invalido; no se puede aplicar HC3.")
  sigma2 <- sum(resid^2)/df
  se_clasico <- sqrt(diag(inv)*sigma2)
  # HC3: Var(beta) = (X'X)^-1 X' diag[e_i^2/(1-h_ii)^2] X (X'X)^-1
  esc <- resid/(1-hh)
  Vhc3 <- inv %*% crossprod(X * esc) %*% inv
  Vhc3 <- (Vhc3+t(Vhc3))/2
  se_hc3 <- sqrt(diag(Vhc3))
  afirmar(all(is.finite(se_hc3) & se_hc3 > 0) &&
          all(is.finite(se_clasico) & se_clasico > 0),
          "Error estandar no definido.")
  alpha <- 0.05
  critico <- stats::qt(1-alpha/2,df=df)
  tabla <- data.frame(termino=names(beta),coeficiente_dias=unname(beta),
                      error_estandar_HC3=se_hc3,
                      t_HC3=unname(beta)/se_hc3,
                      p_valor_HC3=2*stats::pt(-abs(unname(beta)/se_hc3),df=df),
                      IC95_HC3_inf=unname(beta)-critico*se_hc3,
                      IC95_HC3_sup=unname(beta)+critico*se_hc3,
                      error_estandar_clasico=se_clasico,
                      p_valor_clasico=2*stats::pt(-abs(unname(beta)/se_clasico),df=df),
                      stringsAsFactors=FALSE)
  tabla$significativo_5pct_HC3 <- tabla$p_valor_HC3 < alpha
  escribir(tabla,"01_coeficientes_IC95_HC3_y_clasico.csv")

  cat("[5/7] Diagnosticos: colinealidad, residuos, heterocedasticidad e influencia...\n")
  R2_train <- 1-sum(resid^2)/sum((yy-mean(yy))^2)
  metricas_train <- data.frame(n_train=n,p_coeficientes=p,grados_libertad=df,
                               RMSE_ajuste_train=sqrt(mean(resid^2)),
                               MAE_ajuste_train=mean(abs(resid)),
                               R2_ajuste_train=R2_train,
                               R2_ajustado_train=1-(1-R2_train)*(n-1)/df,
                               sesgo_train=mean(fit-yy),
                               max_dif_coef_archivado=max_dif_beta,
                               stringsAsFactors=FALSE)
  escribir(metricas_train,"02_resumen_ajuste_SOLO_train.csv")
  Z <- X[,-1,drop=FALSE]
  Zc <- sweep(Z,2,colMeans(Z),"-")
  C <- crossprod(Zc)
  vif <- diag(solve(C))*diag(C)
  tabla_vif <- data.frame(termino=colnames(Z),VIF_columna=as.numeric(vif),
                          stringsAsFactors=FALSE)
  tabla_vif <- tabla_vif[order(-tabla_vif$VIF_columna),,drop=FALSE]
  afirmar(all(is.finite(vif) & vif>=0.9999),"VIF inestable o invalido.")
  escribir(tabla_vif,"03_VIF_por_columna_transformada.csv")

  # Diagnostico auxiliar de heterocedasticidad tipo Breusch-Pagan (1 regresor):
  # residuo^2 ~ valor_ajustado, LM=n*R2_aux, referencia chi^2(1).
  # Es orientativo: supuesto de normalidad/independencia puede fallar en logistica.
  w <- resid^2
  XA <- cbind(intercepto=rep(1,n),prediccion=fit)
  aux <- stats::lm.fit(XA,w)
  Rsq_aux <- 1-sum(aux$residuals^2)/sum((w-mean(w))^2)
  LM <- n*Rsq_aux
  p_BP <- stats::pchisq(LM,df=1,lower.tail=FALSE)
  e_centrado <- resid-mean(resid)
  m2 <- mean(e_centrado^2)
  asimetria <- mean(e_centrado^3)/m2^(3/2)
  curtosis_exceso <- mean(e_centrado^4)/m2^2-3
  cooks <- resid^2/(p*sigma2) * hh/(1-hh)^2
  diagn <- data.frame(n=n,p=p,
                       diagnostico="BP_auxiliar_resid2_sobre_prediccion",
                       estadistico_LM=LM,grados_libertad_BP=1,p_valor_BP_aprox=p_BP,
                       asimetria_residuos=asimetria,
                       exceso_curtosis_residuos=curtosis_exceso,
                       max_leverage=max(hh),
                       n_leverage_superior_2p_sobre_n=sum(hh > 2*p/n),
                       max_Cook=max(cooks),
                       n_Cook_superior_4_sobre_n=sum(cooks>4/n),
                       stringsAsFactors=FALSE)
  escribir(diagn,"04_diagnosticos_residuos_e_influencia.csv")
  # Guardar SOLO un subconjunto determinista para graficos, no bases masivas.
  set.seed(20261019L)
  it_plot <- sort(sample.int(n,min(n,4500L)))
  muestra <- data.frame(order_id=id_train[it_plot],fecha_aprobacion=d$fecha_aprobacion[it_plot],
                        real_dias=yy[it_plot],pred_dias=fit[it_plot],
                        residuo_real_menos_pred=resid[it_plot],
                        leverage=hh[it_plot],stringsAsFactors=FALSE)
  escribir(muestra,"05_muestra_diagnostica_determinista.csv")
  escribir(data.frame(archivo=names(insumos),ruta=unname(insumos),
                     md5=unname(tools::md5sum(insumos)),stringsAsFactors=FALSE),
           "06_versiones_y_huellas_de_fuentes.csv")

  cat("[6/7] Generando graficos descriptivos en R base, sin paquetes...\n")
  dir.create(salida, recursive=TRUE,showWarnings=FALSE)
  dir.create(salida_fig, recursive=TRUE,showWarnings=FALSE)
  azul <- "#16334F"; verde <- "#147F77"; gris <- "#61758B"
  # Un archivo por dispositivo; tambien PDF vectorial para la presentacion.
  plotear <- function(nombre,funcion) {
    for (ext in c("png","pdf")) {
      archivo <- file.path(salida_fig,paste0(nombre,".",ext))
      if (ext=="png") grDevices::png(archivo,width=1700,height=1100,res=155)
      else grDevices::pdf(archivo,width=11,height=7.1,onefile=FALSE)
      tryCatch(funcion(),finally=grDevices::dev.off())
    }
  }
  esenciales <- c("log_flete","vendedor_mismo_estado","log_precio",
                  "log_items","log_vendedores")
  co <- tabla[match(esenciales,tabla$termino),,drop=FALSE]
  afirmar(!anyNA(co$coeficiente_dias),"Faltan terminos para la figura.")
  etiquetas <- c("Flete total (log1p)","Vendedor en mismo estado (0/1)",
                 "Precio total (log1p)","Numero de articulos (log1p)",
                 "Numero de vendedores (log1p)")
  plotear("01_coeficientes_seleccionados_IC95_HC3",function(){
    op <- par(mar=c(6.2,15.3,5.0,2.5),xaxs="i",las=1,fg=azul,cex.axis=0.94)
    on.exit(par(op),add=TRUE)
    yl <- rev(seq_along(etiquetas))
    lim <- range(c(co$IC95_HC3_inf,co$IC95_HC3_sup,0))
    margen <- max(diff(lim)*0.12,0.2)
    plot(NA, xlim=lim+c(-margen,margen),ylim=c(.5,5.5),
         axes=FALSE,xlab="",ylab="")
    abline(v=0,col="#A5B4C2",lty=2,lwd=1.6)
    segments(co$IC95_HC3_inf,yl,co$IC95_HC3_sup,yl,col=verde,lwd=3)
    points(co$coeficiente_dias,yl,pch=19,col=azul,cex=1.25)
    axis(1,col.axis=azul)
    axis(2,at=yl,labels=etiquetas,tick=FALSE,las=1)
    title(main="Coeficientes seleccionados del OLS B e IC del 95 % (HC3)",
          col.main=azul,cex.main=1.15)
    mtext("Cambio en dias por unidad del predictor transformado (NO comparar magnitudes como importancia)",
          side=1,line=3.5,col=gris,cex=.78)
    mtext("Asociaciones condicionales | Train fold 4 | Intervalos robustos HC3, no causales",
          side=1,line=5.0,col=gris,cex=.76)
    box(col="#DDE5EB")
  })
  plotear("02_residuos_vs_prediccion_train",function(){
    op <- par(mar=c(6.2,6.6,5,2.3),las=1,fg=azul,cex.axis=.9)
    on.exit(par(op),add=TRUE)
    xx<-fit[it_plot]; rr<-resid[it_plot]
    plot(xx,rr,pch=16,cex=.34,col=grDevices::adjustcolor(verde,alpha.f=.22),
         xlab="Duracion predicha (dias)",ylab="Residuo: real - predicho (dias)",
         main="Residuos frente a prediccion | OLS B (train fold 4)")
    abline(h=0,col="#D06D3D",lty=2,lwd=2)
    mtext("Muestra aleatoria fija de 4.500 pedidos; residuos extremos no eliminados",
          side=1,line=4.6,col=gris,cex=.82)
    box(col="#DDE5EB")
  })
  plotear("03_qq_residuos_train",function(){
    op <- par(mar=c(6.0,6.2,5,2.2),fg=azul,cex.axis=.91)
    on.exit(par(op),add=TRUE)
    rr <- resid[it_plot]
    stats::qqnorm(rr,pch=16,cex=.35,col=grDevices::adjustcolor(verde,alpha.f=.35),
                  xlab="Cuantiles teoricos de la normal",ylab="Cuantiles de residuos (dias)",
                  main="Diagnostico grafico de normalidad | OLS B")
    stats::qqline(rr,col="#D06D3D",lwd=2)
    mtext("La normalidad es un supuesto a evaluar; HC3 no corrige dependencia temporal",
          side=1,line=4.5,col=gris,cex=.82)
    box(col="#DDE5EB")
  })

  interpretacion <- c(
    "ETAPA 9 - INFERENCIA Y DIAGNOSTICOS (NO ES UNA NUEVA EVALUACION DEL TEST)",
    "Unidad: pedido; respuesta: dias entre aprobacion y entrega real; SOLO TRAIN fold 4.",
    paste0("n=",n,"; coeficientes (incl. intercepto)=",p,"; alpha=0.05; IC nominal 95 %."),
    "Modelo: OLS B original de pedido y geografia, construido con su receta y contrastado por coeficientes.",
    sprintf("Diferencia maxima vs coeficientes archivados: %.12g",max_dif_beta),
    "La inferencia es CONDICIONAL/ASOCIATIVA; no demuestra causalidad ni importancia predictiva individual.",
    "Errores HC3: ayudan frente a heterocedasticidad, NO corrigen dependencia temporal, sesgo de seleccion",
    "ni incertidumbre de cambios de distribucion. p-valores nominales son exploratorios ante muchos contrastes.",
    "VIF se calcula por COLUMNA de diseno; en estados dummies NO equivale a GVIF por variable categórica.",
    "Breusch-Pagan auxiliar usa residuo^2 sobre valor ajustado (1 df); p-valor asintotico orientativo.",
    "Con n grande, los p-valores pueden ser muy pequenos para asociaciones operativamente modestas.",
    "In-sample R2, RMSE y residuos del fold 4 NO se comparan con metricas externas de validacion.",
    "Diagnosticos graficos usan muestra aleatoria reproducible; NO se eliminaron outliers.",
    "No se calcularon metricas del test julio-agosto 2018 ni se seleccionaron modelos en esta etapa.",
    "Analisis realizado despues de conocer el test del proyecto historico; no es una evaluacion nueva independiente.",
    "Predictores de items, flete y ubicacion suponen disponibilidad al aprobar; no verificable fila a fila.",
    "Para la defensa: informar coeficientes, IC, heterocedasticidad, colinealidad, influencia y limites.",
    "Paquetes externos: NINGUNO (stats y graphics incluidos en R base)."
  )
  writeLines(interpretacion,file.path(salida,"07_interpretacion_y_advertencias.txt"),useBytes=TRUE)
  capture.output(sessionInfo(),file=file.path(salida,"08_sessionInfo.txt"))
  cat("[7/7] Certificando integridad y consistencia de archivos...\n")
  hash_despues <- tools::md5sum(anteriores)
  afirmar(identical(names(hash_antes),names(hash_despues)) &&
          identical(unname(hash_antes),unname(hash_despues)),
          "Se modifico un resultado historico. Detener y revisar antes de continuar.")
  dibujos <- file.path(salida_fig,paste0(rep(c("01_coeficientes_seleccionados_IC95_HC3",
                 "02_residuos_vs_prediccion_train","03_qq_residuos_train"),each=2),
                 ".",rep(c("png","pdf"),3)))
  afirmar(all(file.exists(dibujos)) && all(file.info(dibujos)$size>1000),
          "Falto generar alguna figura; no se certifica la etapa.")
  afirmar(nrow(tabla)==p && all(tabla$IC95_HC3_inf <= tabla$coeficiente_dias) &&
          all(tabla$IC95_HC3_sup >= tabla$coeficiente_dias) &&
          all(tabla$p_valor_HC3>=0 & tabla$p_valor_HC3<=1),
          "Inconsistencia de intervalos o pruebas.")
  certificacion <- c("ETAPA 09 APROBADA (CONTROLES AUTOMATICOS)",
                    "No se modificaron archivos de resultados anteriores (MD5).",
                    paste0("n train fold4: ",n),
                    paste0("Coeficientes: ",p),
                    sprintf("Max diferencia beta vs OLS B Etapa 4: %.12g",max_dif_beta),
                    "Regla alfa=0.05, IC=95%, errores HC3 y clasicos.",
                    "NO se consulto el test historico. NO se cambiaron modelos anteriores.",
                    "Control de ejecucion RStudio adicional a revisiones independientes.")
  writeLines(certificacion,file.path(salida,"09_CONTROL_ETAPA9_APROBADO.txt"),useBytes=TRUE)
  cat("\nETAPA 9 COMPLETADA - INFERENCIA OLS B Y DIAGNOSTICOS; SOLO TRAIN\n")
  print(metricas_train,row.names=FALSE)
  cat("Coeficiente log_flete (HC3):\n")
  print(tabla[tabla$termino=="log_flete",c("coeficiente_dias","IC95_HC3_inf",
                                         "IC95_HC3_sup","p_valor_HC3")],row.names=FALSE)
  cat("BP auxiliar p (orientativo): ",format(p_BP,digits=4),"\n",sep="")
  cat("RESULTADOS: ",salida,"\nGRAFICOS: ",salida_fig,"\n",sep="")
})
