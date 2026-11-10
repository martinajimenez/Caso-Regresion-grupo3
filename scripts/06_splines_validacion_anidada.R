# =============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3 | CASO 1: REGRESION OLIST
# ETAPA 6 v1.0 - SPLINES NATURALES, VALIDACION TEMPORAL ANIDADA
# Requiere: R base (incluido paquete recomendado 'splines'), etapas 1-5.
# NO instala paquetes, NO ejecuta el test historico, NO modifica resultados 1-5.
# Ejecutar desde la raiz: source("scripts/06_splines_validacion_anidada.R", encoding="UTF-8")
# =============================================================================
local({
  set.seed(20261023L)
  stopifnot(requireNamespace("splines", quietly=TRUE))
  raiz <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
  r <- function(...) file.path(raiz,...)
  salida <- r("resultados","06_splines")
  figuras <- r("graficos","06_splines")
  requeridos <- c(
    base=r("resultados","02_variables_candidatas","01_variables_candidatas_desarrollo.csv"),
    etiquetas=r("resultados","01_revision_datos","06_etiquetas_solo_desarrollo.csv"),
    ids=r("resultados","03_protocolo_y_folds","04_ids_train_validacion.csv"),
    folds=r("resultados","03_protocolo_y_folds","03_resumen_folds_temporales.csv"),
    specs=r("resultados","03_protocolo_y_folds","02_especificaciones_previas.csv"),
    pred4=r("resultados","04_baseline_y_ols","03_predicciones_validacion.csv"),
    pred5=r("resultados","05_regularizacion","05_predicciones_validacion_regularizados.csv"),
    internos5=r("resultados","05_regularizacion","01_folds_internos.csv"),
    cert4=r("resultados","04_baseline_y_ols","11_CONTROL_COMPARABILIDAD_APROBADO.txt"),
    cert5=r("resultados","05_regularizacion","12_CONTROL_ETAPA5_APROBADO.txt"),
    original=r("datos_originales","dataset.zip")
  )
  faltan <- requeridos[!file.exists(requeridos)]
  if(length(faltan)) stop("Faltan archivos requeridos: ", paste(names(faltan), collapse=", "),call.=FALSE)
  dir.create(salida, showWarnings=FALSE, recursive=TRUE)
  dir.create(figuras, showWarnings=FALSE, recursive=TRUE)
  certificado <- file.path(salida,"11_CONTROL_ETAPA6_APROBADO.txt")
  if(file.exists(certificado)) file.remove(certificado)
  falla <- function(ok, msg) if(!isTRUE(ok)) stop(msg,call.=FALSE)
  leer <- function(p) utils::read.csv(p, stringsAsFactors=FALSE,check.names=FALSE,na.strings=c("NA",""))
  guardar <- function(x,n) utils::write.csv(x,file.path(salida,n),row.names=FALSE,na="")
  metrica <- function(y,p) {
    falla(length(y)==length(p) && length(y)>1 && all(is.finite(y)) && all(is.finite(p)),"Metricas: valores no finitos")
    sst <- sum((y-mean(y))^2)
    falla(sst>0, "R2 indefinido: respuesta constante")
    d <- p-y
    c(rmse=sqrt(mean(d^2)),mae=mean(abs(d)),r2=1-sum(d^2)/sst,sesgo=mean(d))
  }
  fecha_utc <- function(x) as.POSIXct(x, format="%Y-%m-%d %H:%M:%S",tz="UTC")

  cat("[1/8] Cargando datos y controles de Etapas 1 a 5...\n")
  b<-leer(requeridos[["base"]]);y0<-leer(requeridos[["etiquetas"]]);ids<-leer(requeridos[["ids"]]);
  folds<-leer(requeridos[["folds"]]);specs<-leer(requeridos[["specs"]]);
  p4<-leer(requeridos[["pred4"]]);p5<-leer(requeridos[["pred5"]]);
  internos_previos<-leer(requeridos[["internos5"]]);
  falla(nrow(b)==86162L && nrow(y0)==81950L && nrow(folds)==4L, "Conteos incompatibles: revisar carpetas")
  falla(!anyDuplicated(b$order_id) && !anyDuplicated(y0$order_id) &&
        !anyDuplicated(paste(ids$fold,ids$rol,ids$order_id,sep="|")),"Duplicados en datos o folds")
  original_B <- c("hora_aprobacion","dia_semana","mes_aprobacion","horas_desde_compra",
                  "estado_cliente","n_items","n_vendedores","precio_total_brl",
                  "flete_total_brl","todos_vendedores_mismo_estado")
  specB <- specs$variables[specs$especificacion=="B_pedido_geografia"]
  falla(length(specB)==1L && setequal(trimws(strsplit(specB,";",fixed=TRUE)[[1L]]),original_B),
        "La especificacion B fue modificada")
  falla(all(original_B %in% names(b)), "Faltan variables B")
  fecha <- fecha_utc(b$fecha_aprobacion)
  falla(!anyNA(fecha) && all(fecha<fecha_utc("2018-07-01 00:00:00")),"Se encontro fecha de prueba o NA")
  y <- y0$dias_entrega[match(b$order_id,y0$order_id)]
  # Fecha de entrega reconstruida SOLO de etiquetas conocidas de desarrollo.
  # Es NECESARIO filtrar train por entrega observable antes de cada corte interno.
  entrega <- as.POSIXct(as.numeric(fecha)+86400*y,origin="1970-01-01",tz="UTC")
  falla(!anyNA(match(y0$order_id,b$order_id)) && all(is.finite(y[match(y0$order_id,b$order_id)])),
        "Etiquetas de desarrollo incorrectas")
  obtener_idx <- function(k,rol){
    v<-ids$order_id[ids$fold==k & ids$rol==rol]
    ix<-match(v,b$order_id)
    falla(length(ix)>0L && !anyNA(ix) && !anyDuplicated(v),"IDs incorrectos por fold")
    ix
  }

  # Misma receta de OLS B y Ridge/Lasso/ElasticNet, ajustada SOLO con training.
  receta <- function(d){
    tab<-table(d$estado_cliente)
    freq<-names(tab)[tab>=100L]
    if(!length(freq)) freq<-names(tab)[which.max(tab)]
    agr<-ifelse(is.na(d$estado_cliente)|!(d$estado_cliente %in% freq),"OTROS",d$estado_cliente)
    lev<-sort(unique(agr))
    fallback<-if("OTROS" %in% lev) "OTROS" else names(tab)[which.max(tab)]
    list(freq=freq,lev=lev,fallback=fallback)
  }
  matrizB <- function(d, rec){
    h<-as.numeric(d$hora_aprobacion);s<-as.numeric(d$dia_semana)
    m<-as.numeric(d$mes_aprobacion);hc<-as.numeric(d$horas_desde_compra)
    falla(all(is.finite(h)&h>=0&h<=23) && all(is.finite(s)&s>=1&s<=7) &&
            all(is.finite(m)&m>=1&m<=12) && all(is.finite(hc)&hc>=0),"Variables temporales no validas")
    x<-cbind(hora_sin=sin(2*pi*h/24),hora_cos=cos(2*pi*h/24),
             dia_sin=sin(2*pi*(s-1)/7),dia_cos=cos(2*pi*(s-1)/7),
             mes_sin=sin(2*pi*(m-1)/12),mes_cos=cos(2*pi*(m-1)/12),
             log_horas_compra=log1p(hc))
    g0<-as.character(d$estado_cliente)
    grupo<-factor(ifelse(is.na(g0)|!(g0 %in% rec$freq),rec$fallback,g0),levels=rec$lev)
    falla(!anyNA(grupo),"Niveles desconocidos en estado_cliente")
    if(length(rec$lev)>1L){
      D<-stats::model.matrix(~grupo)[,-1L,drop=FALSE]
      colnames(D)<-paste0("estado_",make.names(rec$lev[-1L]))
      x<-cbind(x,D)
    }
    for(z in c("n_items","n_vendedores","precio_total_brl","flete_total_brl","todos_vendedores_mismo_estado")){
      vv<-as.numeric(d[[z]])
      falla(all(is.finite(vv)&vv>=0),paste("Faltantes/valores invalidos:",z))
    }
    geo<-as.numeric(d$todos_vendedores_mismo_estado)
    falla(all(geo %in% c(0,1)),"Variable geografia no es binaria")
    x<-cbind(x,log_items=log1p(as.numeric(d$n_items)),
             log_vendedores=log1p(as.numeric(d$n_vendedores)),
             log_precio=log1p(as.numeric(d$precio_total_brl)),
             log_flete=log1p(as.numeric(d$flete_total_brl)),
             vendedor_mismo_estado=geo)
    storage.mode(x)<-"double"
    falla(nrow(x)==nrow(d) && all(is.finite(x)),"Matriz contiene NA/Inf")
    as.data.frame(x,check.names=FALSE)
  }

  # Las transformaciones naturales ns() se ajustan con los X del training de cada fold.
  # predict.lm() reutiliza knots y limites aprendidos del training (predict-safe).
  formula_spline <- function(df, interaccion){
    base <- paste0("dias ~ . - log_precio - log_flete + ",
                   "splines::ns(log_precio, df=",df,") + ",
                   "splines::ns(log_flete, df=",df,")")
    if(interaccion) base<-paste0(base," + vendedor_mismo_estado:log_flete")
    stats::as.formula(base)
  }
  entrenar <- function(dtr, yt, df, interaccion){
    falla(nrow(dtr)==length(yt) && all(is.finite(yt)),"Respuesta mal alineada")
    datos<-data.frame(dias=yt,dtr,check.names=FALSE)
    fit<-stats::lm(formula_spline(df,interaccion),data=datos,na.action=stats::na.fail,
                   model=TRUE, x=FALSE,y=FALSE)
    falla(all(is.finite(stats::coef(fit))) && fit$rank==length(stats::coef(fit)),
          paste("Ajuste spline con rango deficiente (df=",df,", interaccion=",interaccion,")",sep=""))
    fit
  }
  # Hipotesis moderada: relaciones NO lineales de flete y precio; mismo origen B.
  grilla<-expand.grid(df=c(3L,4L,5L),interaccion_geo=c(FALSE,TRUE),KEEP.OUT.ATTRS=FALSE)
  grilla<-grilla[order(grilla$df,grilla$interaccion_geo),,drop=FALSE]
  grilla$configuracion<-paste0("ns_df",grilla$df,"_geo",as.integer(grilla$interaccion_geo))
  falla(nrow(grilla)==6L,"Grilla spline invalida")

  cat("[2/8] Revalidando IDs, fechas, madurez y muestra de OLS B...\n")
  indices_valid <- vector("list",4L)
  for(k in 1:4){
    tr<-obtener_idx(k,"train");va<-obtener_idx(k,"validacion")
    f<-folds[folds$fold==k,,drop=FALSE]
    falla(nrow(f)==1 && length(tr)==f$n_train && length(va)==f$n_validacion && !length(intersect(tr,va)),
          "Folds externos incongruentes")
    ini<-fecha_utc(f$fecha_corte_entrenamiento)
    fin<-seq(ini,by="month",length.out=2L)[2L]
    falla(all(fecha[tr]<=ini-as.difftime(45,units="days")) &&
            all(entrega[tr]<ini) && all(fecha[va]>=ini & fecha[va]<fin) &&
            all(is.finite(y[c(tr,va)])),
          "Fuga temporal / etiquetas incompletas en folds externos")
    ix<-match(b$order_id[va],p4$order_id[p4$fold==k & p4$modelo=="B_pedido_geografia"])
    pp<-p4[p4$fold==k & p4$modelo=="B_pedido_geografia",]
    falla(nrow(pp)==length(va) && !anyNA(ix) && !anyDuplicated(pp$order_id) &&
            max(abs(pp$dias_reales[ix]-y[va]))<1e-09, "OLS B no coincide con validacion")
    indices_valid[[k]]<-list(tr=tr,va=va,ini=ini,fin=fin)
  }

  cat("[3/8] Construyendo tres validaciones internas por cada fold externo...\n")
  cache<-vector("list",4L);internos<-list()
  for(k in 1:4){
    tr_outer<-indices_valid[[k]]$tr
    ini_outer<-indices_valid[[k]]$ini
    limite<-ini_outer-as.difftime(45,units="days")
    primero_limite<-fecha_utc(paste0(format(limite,"%Y-%m"),"-01 00:00:00"))
    ultimo_mes<-seq(primero_limite,by="-1 month",length.out=2L)[2L]
    meses<-rev(seq(ultimo_mes,by="-1 month",length.out=3L))
    cache[[k]]<-vector("list",3L)
    for(j in 1:3){
      ini<-meses[j];fin<-seq(ini,by="month",length.out=2L)[2L]
      tr<-tr_outer[fecha[tr_outer]<=ini-as.difftime(45,units="days") &
                   entrega[tr_outer]<ini]
      va<-tr_outer[fecha[tr_outer]>=ini & fecha[tr_outer]<fin]
      falla(length(tr)>=1500 && length(va)>=500 && !length(intersect(tr,va)) &&
              all(is.finite(y[c(tr,va)])) && all(entrega[tr]<ini),"Fold interno invalido")
      comparador<-internos_previos[internos_previos$fold_externo==k &
                                   internos_previos$fold_interno==j,]
      falla(nrow(comparador)==1L && comparador$n_train==length(tr) &&
            comparador$n_validacion==length(va) &&
            comparador$mes_validacion==format(ini,"%Y-%m"),
            "Fold interno no corresponde a la Etapa 5")
      cache[[k]][[j]]<-list(tr=tr,va=va,mes=format(ini,"%Y-%m"))
      internos[[length(internos)+1L]]<-data.frame(fold_externo=k,fold_interno=j,
           mes_validacion=format(ini,"%Y-%m"),n_train=length(tr),n_validacion=length(va))
    }
  }
  # No se leen etiquetas futuras del test; las etiquetas internas pertenecen a train externo.

  cat("[4/8] Midiendo splines candidatos SOLO con validacion interna...\n")
  cvfilas<-list();selecc<-list();preds<-list();metrs<-list();geof <- list()
  for(k in 1:4){
    cat("   Fold externo ",k," de 4...\n",sep="")
    prueba<-list()
    for(j in 1:3){
      ind<-cache[[k]][[j]]
      rec<-receta(b[ind$tr,,drop=FALSE])
      xtr<-matrizB(b[ind$tr,,drop=FALSE],rec);xva<-matrizB(b[ind$va,,drop=FALSE],rec)
      falla(identical(names(xtr),names(xva)),"Matriz interna con columnas distintas")
      for(z in 1:nrow(grilla)){
        cfg<-grilla[z,]
        fit<-entrenar(xtr,y[ind$tr],cfg$df,cfg$interaccion_geo)
        pp<-as.numeric(stats::predict(fit,newdata=xva))
        mm<-metrica(y[ind$va],pp)
        prueba[[length(prueba)+1L]]<-data.frame(
          fold_externo=k,fold_interno=j,mes_interno=ind$mes,
          configuracion=cfg$configuracion,df=cfg$df,
          interaccion_geo=cfg$interaccion_geo,n_train=length(ind$tr),n_validacion=length(ind$va),
          rmse=unname(mm["rmse"]),mae=unname(mm["mae"]))
      }
    }
    cur<-do.call(rbind,prueba)
    cvfilas[[k]]<-cur
    agg<-do.call(rbind,lapply(split(cur,cur$configuracion),function(d){
      falla(nrow(d)==3L && setequal(d$fold_interno,1:3),"Folds internos perdidos")
      data.frame(configuracion=d$configuracion[1],df=d$df[1],
        interaccion_geo=d$interaccion_geo[1],rmse_interno=mean(d$rmse),
        sd_rmse_interno=stats::sd(d$rmse),mae_interno=mean(d$mae))
    }))
    # Elige menor RMSE interno. En empate virtual, prefiere menos df y SIN interaccion.
    agg<-agg[order(agg$rmse_interno,agg$df,agg$interaccion_geo),,drop=FALSE]
    best<-agg[1,,drop=FALSE]
    selecc[[k]]<-data.frame(fold_externo=k,best,
                            margen_rmse_vs_segundo=agg$rmse_interno[2]-agg$rmse_interno[1])
    ind<-indices_valid[[k]]
    rec<-receta(b[ind$tr,,drop=FALSE]);xtr<-matrizB(b[ind$tr,,drop=FALSE],rec)
    xva<-matrizB(b[ind$va,,drop=FALSE],rec)
    fit<-entrenar(xtr,y[ind$tr],best$df,best$interaccion_geo)
    pp<-as.numeric(stats::predict(fit,newdata=xva));
    mm<-metrica(y[ind$va],pp)
    predsk<-data.frame(fold=k,mes_validacion=folds$mes_validacion[folds$fold==k],
        order_id=b$order_id[ind$va],modelo="Splines_B_anidado",
        dias_reales=y[ind$va],dias_predichos=pp,error_predicho_menos_real=pp-y[ind$va])
    preds[[k]]<-predsk
    metrs[[k]]<-data.frame(fold=k,mes_validacion=folds$mes_validacion[folds$fold==k],
        modelo="Splines_B_anidado",n_train=length(ind$tr),n_validacion=length(ind$va),
        df=best$df,interaccion_geo=best$interaccion_geo,
        configuracion=best$configuracion,
        rmse=unname(mm["rmse"]),mae=unname(mm["mae"]),
        r2=unname(mm["r2"]),sesgo=unname(mm["sesgo"]))
    geof[[k]]<-do.call(rbind,lapply(0:1,function(g){
      ig<-which(b$todos_vendedores_mismo_estado[ind$va]==g)
      if(length(ig)<2) return(NULL)
      mg<-metrica(y[ind$va][ig],pp[ig])
      data.frame(fold=k,modelo="Splines_B_anidado",
        segmento=if(g==0) "vendedor_en_otro_estado" else "todos_vendedores_mismo_estado",
        n=length(ig),rmse=unname(mg["rmse"]),mae=unname(mg["mae"]))
    }))
  }

  cat("[5/8] Comprobando que la comparacion contiene EXACTAMENTE los mismos pedidos...\n")
  pred<-do.call(rbind,preds); metr<-do.call(rbind,metrs)
  cv<-do.call(rbind,cvfilas); tuning<-do.call(rbind,selecc)
  falla(nrow(metr)==4L && nrow(pred)==sum(folds$n_validacion) &&
        !anyDuplicated(paste(pred$fold,pred$order_id,sep="|")),
        "Predicciones splines faltantes o duplicadas")
  for(k in 1:4){
    val<-indices_valid[[k]]$va; g<-pred[pred$fold==k,];
    pp<-g[match(b$order_id[val],g$order_id),]
    falla(nrow(g)==length(val) && !anyNA(pp$order_id) &&
            max(abs(pp$dias_reales-y[val]))<1e-09, "Pedido/etiqueta spline desalineado")
    mm<-metrica(y[val],pp$dias_predichos)
    grabado<-metr[metr$fold==k,]
    falla(all(abs(unname(mm)-unname(as.numeric(grabado[1,names(mm)])))<1e-08),
          "Las metricas spline no se reproducen")
  }
  # Referencias existentes, seleccionadas SOLO por los mismos IDs de cada validacion.
  anterior<-rbind(
    p4[p4$modelo %in% c("B_pedido_geografia","Baseline_media_train"),
       c("fold","order_id","modelo","dias_reales","dias_predichos")],
    p5[p5$modelo %in% c("Ridge_min","Lasso_min","ElasticNet_min"),
       c("fold","order_id","modelo","dias_reales","dias_predichos")],
    pred[,c("fold","order_id","modelo","dias_reales","dias_predichos")]
  )
  referencias<-c("B_pedido_geografia","Baseline_media_train","Ridge_min","Lasso_min","ElasticNet_min","Splines_B_anidado")
  compar_folds<-list()
  for(k in 1:4){
    va<-indices_valid[[k]]$va
    refIDs<-b$order_id[va]
    for(nom in referencias){
      g<-anterior[anterior$fold==k & anterior$modelo==nom,]
      pos<-match(refIDs,g$order_id)
      falla(nrow(g)==length(va) && !anyNA(pos) && !anyDuplicated(g$order_id),
            paste("Cantidad de pedidos distinta para",nom,"fold",k))
      falla(max(abs(g$dias_reales[pos]-y[va]))<1e-09,
            "Respuesta no corresponde a los mismos pedidos")
      mm<-metrica(y[va],g$dias_predichos[pos])
      compar_folds[[length(compar_folds)+1L]]<-data.frame(fold=k,modelo=nom,
        n_validacion=length(va),rmse=unname(mm["rmse"]),mae=unname(mm["mae"]),
        r2=unname(mm["r2"]),sesgo=unname(mm["sesgo"]))
    }
  }
  cfold<-do.call(rbind,compar_folds)
  resumen<-do.call(rbind,lapply(split(cfold,cfold$modelo),function(z){
    falla(nrow(z)==4L,"Modelo sin los cuatro folds")
    data.frame(modelo=z$modelo[1],rmse_promedio=mean(z$rmse),
               sd_rmse_folds=stats::sd(z$rmse),mae_promedio=mean(z$mae),
               r2_promedio=mean(z$r2),sesgo_promedio=mean(z$sesgo))
  }))
  resumen<-resumen[order(resumen$rmse_promedio),];rownames(resumen)<-NULL

  cat("[6/8] Guardando resultados, decisiones y huellas de entrada...\n")
  guardar(do.call(rbind,internos),"01_folds_internos.csv")
  guardar(cv,"02_seleccion_interna_candidatos.csv")
  guardar(tuning,"03_configuracion_seleccionada_por_fold.csv")
  guardar(metr,"04_metricas_splines_por_fold.csv")
  guardar(pred,"05_predicciones_splines_por_pedido.csv")
  guardar(cfold,"06_comparacion_comun_por_fold.csv")
  guardar(resumen,"07_comparacion_comun_resumen.csv")
  guardar(do.call(rbind,geof),"08_subgrupos_aparte.csv")
  guardar(data.frame(entrada=names(requeridos),ruta=unname(requeridos),
                    md5=unname(tools::md5sum(requeridos))),"09_huellas_entradas.csv")
  writeLines(c("ETAPA 6 - SPLINES NATURALES v1.0",
   "R base + paquete 'splines' incluido con R. Sin instalaciones nuevas.",
   "Misma especificacion B: diez variables originales, codificacion de estado entrenada en cada train.",
   "Formula: OLS B con splines naturales de log(flete) y log(precio), df=3,4,5.",
   "Se evalua con/sin interaccion previamente definida de geografia x log(flete).",
   "Seis configuraciones escogidas SOLO en validacion interna temporal (tres folds).",
   "Cuatro folds externos identicos a las Etapas 4 y 5; no se usan sus valores para elegir splines.",
   "Se conservan por separado errores del subgrupo geografico.",
   "La comparacion con Ridge, Lasso, ElasticNet y OLS B tiene los mismos IDs de validacion.",
   "Las medias de metricas de cuatro folds no garantizan superioridad estadistica.",
   "La disponibilidad al aprobar del estado del vendedor es supuesto historico sin corroborar.",
   "Los resultados son validaciones exploratorias posteriores a conocer el test ORIGINAL; no test nuevo.",
   "No se consulto el test historico, no se ajusto ningun modelo definitivo nuevo."),
   file.path(salida,"10_notas_metodologicas.txt"),useBytes=TRUE)

  cat("[7/8] Generando grafico simple y portable en R base...\n")
  png(file.path(figuras,"01_rmse_comparacion_algoritmos.png"),width=1800,height=1050,res=160,bg="white")
  tryCatch({
    op<-par(mar=c(6,15,5,6),family="sans")
    orden<-resumen[order(resumen$rmse_promedio,decreasing=TRUE),]
    nom<-c(Splines_B_anidado="Splines naturales (B)",B_pedido_geografia="OLS B",
           Baseline_media_train="Media historica",Ridge_min="Ridge",Lasso_min="Lasso",
           ElasticNet_min="Elastic-Net")
    etiquetas<-unname(nom[orden$modelo])
    falla(!anyNA(etiquetas),"Modelo sin nombre en grafico")
    col<-ifelse(orden$modelo=="Splines_B_anidado","#137C7A",
           ifelse(orden$modelo=="Baseline_media_train","#9DAAB8","#6B8CAD"))
    pos<-barplot(orden$rmse_promedio,horiz=TRUE,names.arg=etiquetas,las=1,
             col=col,border=NA,xlim=c(0,max(orden$rmse_promedio)*1.18),axes=FALSE,
             cex.names=0.92,main="Comparacion de RMSE en validacion temporal",
             sub="Misma especificacion B | Promedio de cuatro folds | Menor es mejor")
    axis(1,at=pretty(c(0,max(orden$rmse_promedio))),las=1)
    text(orden$rmse_promedio+0.12,pos,labels=sprintf("%.3f",orden$rmse_promedio),
         pos=4,cex=0.95,font=2,col="#17324A",xpd=NA)
    mtext("RMSE promedio de validacion (dias)",1,line=3.3)
    box(bty="l",col="#C4D2E0")
    par(op)
  },finally=dev.off())

  cat("[8/8] Certificando archivos y recalculo por ID...\n")
  expected<-c("01_folds_internos.csv","02_seleccion_interna_candidatos.csv",
    "03_configuracion_seleccionada_por_fold.csv","04_metricas_splines_por_fold.csv",
    "05_predicciones_splines_por_pedido.csv","06_comparacion_comun_por_fold.csv",
    "07_comparacion_comun_resumen.csv","08_subgrupos_aparte.csv",
    "09_huellas_entradas.csv","10_notas_metodologicas.txt")
  falla(all(file.exists(file.path(salida,expected))) &&
          all(file.info(file.path(salida,expected))$size>0) &&
          file.exists(file.path(figuras,"01_rmse_comparacion_algoritmos.png")),
        "Faltan archivos de salida")
  writeLines(c("CONTROL APROBADO ETAPA 6 v1.0",
     "Revisadas fechas e IDs de los cuatro folds, y codificacion solo train.",
     "Tres folds internos anidados para seleccionar df e interaccion.",
     "Splines calculados solo en datos de entrenamiento de cada ventana.",
     "Recalculadas metricas desde cada prediccion individual por order_id.",
     "Comparacion con OLS B y regularizados sobre los mismos pedidos por fold.",
     "No se consulto test historico ni se modificaron Etapas 1-5."),
     certificado,useBytes=TRUE)
  cat("\nETAPA 6 COMPLETADA - SPLINES; SOLO DESARROLLO\n")
  print(resumen,row.names=FALSE,digits=5)
  cat("\nRESULTADOS: ",salida,"\nGRAFICOS: ",figuras,"\nCERTIFICADO: ",certificado,"\n",sep="")
})
