# =============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3 | OLIST
# ETAPA 7 v1.1 - CART, RANDOM FOREST, GRADIENT BOOSTING
# Desarrollo exclusivo; mismos folds y especificacion B que etapas 4, 5, 6.
# No revisa el test de julio-agosto 2018 ni reajusta modelos anteriores.
# Requiere rpart, ranger y gbm. Ejecutar desde la raiz del proyecto:
# source("scripts/07_arbol_rf_boosting_anidados.R", encoding="UTF-8")
# =============================================================================
local({
  set.seed(20261023L)
  paquetes <- c("rpart", "ranger", "gbm")
  faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly=TRUE)]
  if (length(faltan)) stop("Faltan paquetes: ",paste(faltan,collapse=", "),
                          ". Instalarlos previamente con install.packages(...).",call.=FALSE)
  raiz <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
  r <- function(...) file.path(raiz,...)
  dir7 <- r("resultados","07_ensembles")
  img7 <- r("graficos","07_ensembles")
  req <- c(
    base=r("resultados","02_variables_candidatas","01_variables_candidatas_desarrollo.csv"),
    y=r("resultados","01_revision_datos","06_etiquetas_solo_desarrollo.csv"),
    ids=r("resultados","03_protocolo_y_folds","04_ids_train_validacion.csv"),
    folds=r("resultados","03_protocolo_y_folds","03_resumen_folds_temporales.csv"),
    specs=r("resultados","03_protocolo_y_folds","02_especificaciones_previas.csv"),
    p4=r("resultados","04_baseline_y_ols","03_predicciones_validacion.csv"),
    p5=r("resultados","05_regularizacion","05_predicciones_validacion_regularizados.csv"),
    p6=r("resultados","06_splines","05_predicciones_splines_por_pedido.csv"),
    cv5=r("resultados","05_regularizacion","01_folds_internos.csv"),
    cert4=r("resultados","04_baseline_y_ols","11_CONTROL_COMPARABILIDAD_APROBADO.txt"),
    cert5=r("resultados","05_regularizacion","12_CONTROL_ETAPA5_APROBADO.txt"),
    cert6=r("resultados","06_splines","11_CONTROL_ETAPA6_APROBADO.txt"),
    original=r("datos_originales","dataset.zip")
  )
  missing <- req[!file.exists(req)]
  if (length(missing)) stop("Faltan entradas: ",paste(names(missing),collapse=", "),call.=FALSE)
  dir.create(dir7,recursive=TRUE,showWarnings=FALSE)
  dir.create(img7,recursive=TRUE,showWarnings=FALSE)
  certificado <- file.path(dir7,"11_CONTROL_ETAPA7_APROBADO.txt")
  if(file.exists(certificado)) file.remove(certificado)
  ok <- function(x,msg) if(!isTRUE(x)) stop(msg,call.=FALSE)
  rd <- function(p) utils::read.csv(p,stringsAsFactors=FALSE,check.names=FALSE,
                                   na.strings=c("NA",""))
  wr <- function(x,name) utils::write.csv(x,file.path(dir7,name),row.names=FALSE,na="")
  dt <- function(x) as.POSIXct(x,format="%Y-%m-%d %H:%M:%S",tz="UTC")
  score <- function(y,p) {
    ok(length(y)==length(p) && length(y)>2L && all(is.finite(y)) &&
       all(is.finite(p)),"Predicciones o respuesta no finitas")
    sst <- sum((y-mean(y))^2)
    ok(sst>0,"R2 indefinido; varianza cero en respuesta")
    e<-p-y
    c(rmse=sqrt(mean(e^2)),mae=mean(abs(e)),r2=1-sum(e^2)/sst,sesgo=mean(e))
  }

  cat("[1/8] Validando archivos, especificacion B y fechas...\n")
  b<-rd(req[["base"]]);yy<-rd(req[["y"]]); ids<-rd(req[["ids"]]);
  folds<-rd(req[["folds"]]);specs<-rd(req[["specs"]]);
  p4<-rd(req[["p4"]]);p5<-rd(req[["p5"]]);p6<-rd(req[["p6"]]);cv5<-rd(req[["cv5"]]);
  ok(nrow(b)==86162L && nrow(yy)==81950L && nrow(folds)==4L,
     "Recuento de datos original alterado")
  ok(!anyDuplicated(b$order_id) && !anyDuplicated(yy$order_id) &&
     !anyDuplicated(paste(ids$fold,ids$rol,ids$order_id)),"Duplicados en entradas")
  B<-c("hora_aprobacion","dia_semana","mes_aprobacion","horas_desde_compra",
        "estado_cliente","n_items","n_vendedores","precio_total_brl",
        "flete_total_brl","todos_vendedores_mismo_estado")
  vars<-specs$variables[specs$especificacion=="B_pedido_geografia"]
  ok(length(vars)==1L && setequal(trimws(strsplit(vars,";",fixed=TRUE)[[1]]),B),
     "La especificacion B ya no coincide con Etapas 4-6")
  ok(all(B %in% names(b)),"Faltan predictores B")
  fecha<-dt(b$fecha_aprobacion)
  ok(!anyNA(fecha) && all(fecha<dt("2018-07-01 00:00:00")),
     "Entraron fechas posteriores al desarrollo")
  y<-yy$dias_entrega[match(b$order_id,yy$order_id)]
  ok(!anyNA(match(yy$order_id,b$order_id)) &&
     all(is.finite(y[match(yy$order_id,b$order_id)])),"Etiquetas no alineadas")
  entrega<-as.POSIXct(as.numeric(fecha)+86400*y,origin="1970-01-01",tz="UTC")
  getidx<-function(k,rol) {
    v<-ids$order_id[ids$fold==k & ids$rol==rol]
    ii<-match(v,b$order_id)
    ok(length(ii)>0L && !anyNA(ii) && !anyDuplicated(v),
       paste("Particion de IDs invalida",k,rol))
    ii
  }

  # Receta de especificacion B identica a 4-6; aprendida solo con train.
  receta<-function(d) {
    tab<-table(d$estado_cliente)
    frecuentes<-names(tab)[tab>=100L]
    if(!length(frecuentes)) frecuentes<-names(tab)[which.max(tab)]
    g<-ifelse(is.na(d$estado_cliente)|!(d$estado_cliente %in% frecuentes),
              "OTROS",d$estado_cliente)
    lev<-sort(unique(g))
    fallback<-if("OTROS" %in% lev) "OTROS" else names(tab)[which.max(tab)]
    list(freq=frecuentes,lev=lev,fallback=fallback)
  }
  Xmat<-function(d,rc) {
    h<-as.numeric(d$hora_aprobacion); s<-as.numeric(d$dia_semana)
    m<-as.numeric(d$mes_aprobacion);hc<-as.numeric(d$horas_desde_compra)
    ok(all(is.finite(h)&h>=0&h<=23) && all(is.finite(s)&s>=1&s<=7) &&
       all(is.finite(m)&m>=1&m<=12) && all(is.finite(hc)&hc>=0),
       "Variables temporales B invalidas")
    xx<-cbind(hora_sin=sin(2*pi*h/24),hora_cos=cos(2*pi*h/24),
          dia_sin=sin(2*pi*(s-1)/7),dia_cos=cos(2*pi*(s-1)/7),
          mes_sin=sin(2*pi*(m-1)/12),mes_cos=cos(2*pi*(m-1)/12),
          log_horas_compra=log1p(hc))
    g<-as.character(d$estado_cliente)
    g<-factor(ifelse(is.na(g)|!(g %in% rc$freq),rc$fallback,g),levels=rc$lev)
    ok(!anyNA(g),"Categorias geograficas desconocidas")
    if(length(rc$lev)>1L){
      D<-stats::model.matrix(~g)[,-1L,drop=FALSE]
      colnames(D)<-paste0("estado_",make.names(rc$lev[-1L]))
      xx<-cbind(xx,D)
    }
    for(z in c("n_items","n_vendedores","precio_total_brl",
               "flete_total_brl","todos_vendedores_mismo_estado")){
      v<-as.numeric(d[[z]])
      ok(all(is.finite(v)&v>=0),paste("Dato faltante o invalido:",z))
    }
    geo<-as.numeric(d$todos_vendedores_mismo_estado)
    ok(all(geo %in% c(0,1)),"Indicador geografico no binario")
    xx<-cbind(xx,log_items=log1p(as.numeric(d$n_items)),
              log_vendedores=log1p(as.numeric(d$n_vendedores)),
              log_precio=log1p(as.numeric(d$precio_total_brl)),
              log_flete=log1p(as.numeric(d$flete_total_brl)),
              vendedor_mismo_estado=geo)
    storage.mode(xx)<-"double"
    ok(nrow(xx)==nrow(d) && all(is.finite(xx)),"Matriz B contiene NA o Inf")
    as.data.frame(xx,check.names=FALSE)
  }

  # Grillas CORTAS y declaradas antes de ver las metricas externas.
  treegrid<-data.frame(config=c("CART_d4_b30","CART_d6_b30","CART_d8_b50"),
                        profundidad=c(4L,6L,8L),minbucket=c(30L,30L,50L))
  rfgrid<-data.frame(config=c("RF_sqrt_b10","RF_tercio_b10","RF_tercio_b30"),
                     mtry_regla=c("sqrt","tercio","tercio"),minnode=c(10L,10L,30L))
  boostgrid<-data.frame(config=c("GBM_d1_lr05","GBM_d2_lr05","GBM_d3_lr03"),
                  profundidad=c(1L,2L,3L),lr=c(0.05,0.05,0.03),
                  arboles=c(350L,350L,450L))
  familias<-c("CART","RandomForest","Boosting")
  configure<-list(CART=treegrid,RandomForest=rfgrid,Boosting=boostgrid)
  # Cada ajuste usa un identificador determinista para reproducir RF/GBM.
  ajustar<-function(modelo,cfg,x,y,semilla) {
    set.seed(as.integer(semilla))
    d<-data.frame(dias=y,x,check.names=FALSE)
    if(modelo=="CART") {
      fit<-rpart::rpart(dias~.,data=d,method="anova",
               control=rpart::rpart.control(cp=0,xval=0,maxdepth=cfg$profundidad,
                 minbucket=cfg$minbucket,minsplit=max(60L,2L*cfg$minbucket),
                 maxcompete=0,maxsurrogate=0,usesurrogate=0))
    } else if(modelo=="RandomForest") {
      pp<-ncol(x)
      mm<-if(cfg$mtry_regla=="sqrt") max(2L,as.integer(floor(sqrt(pp))))
           else max(2L,as.integer(floor(pp/3)))
      mm<-min(pp,mm)
      fit<-ranger::ranger(dependent.variable.name="dias",data=d,
             num.trees=200L,mtry=mm,min.node.size=cfg$minnode,
             importance="none",splitrule="variance",replace=TRUE,
             write.forest=TRUE,seed=as.integer(semilla),num.threads=1L)
    } else if(modelo=="Boosting") {
      # Misma matriz de X e Y que el resto: minobsinnode evita hojas muy pequeñas.
      # nTrain sustituye train.fraction=1 (deprecado en gbm >= 2.3).
      # Se utilizan TODOS los datos recibidos por el ajuste; no cambia el protocolo.
      fit<-gbm::gbm.fit(x=as.matrix(x),y=y,distribution="gaussian",
             n.trees=cfg$arboles,interaction.depth=cfg$profundidad,
             n.minobsinnode=20L,shrinkage=cfg$lr,bag.fraction=0.8,
             nTrain=length(y),keep.data=FALSE,verbose=FALSE)
    } else stop("Familia desconocida")
    fit
  }
  predecir<-function(modelo,fit,nuevo,cfg) {
    pred<-if(modelo=="CART") stats::predict(fit,newdata=nuevo)
      else if(modelo=="RandomForest")
        stats::predict(fit,data=nuevo)$predictions
      else stats::predict(fit,newdata=as.data.frame(nuevo),n.trees=cfg$arboles)
    pp<-as.numeric(pred)
    ok(length(pp)==nrow(nuevo) && all(is.finite(pp)),
       paste("Error predictivo en",modelo,cfg$config))
    pp
  }

  cat("[2/8] Comprobando los cuatro folds externos y sus IDs...\n")
  outer<-vector("list",4L)
  for(k in 1:4){
    tr<-getidx(k,"train");va<-getidx(k,"validacion")
    f<-folds[folds$fold==k,,drop=FALSE]
    ok(nrow(f)==1L && length(tr)==f$n_train && length(va)==f$n_validacion &&
       !length(intersect(tr,va)),"Tamano o cruce de folds externos")
    ini<-dt(f$fecha_corte_entrenamiento)
    fin<-seq(ini,by="month",length.out=2L)[2L]
    ok(all(fecha[tr]<=ini-as.difftime(45,units="days")) &&
       all(entrega[tr]<ini) && all(fecha[va]>=ini & fecha[va]<fin) &&
       all(is.finite(y[c(tr,va)])),"Fuga temporal en folds externos")
    pold<-p4[p4$fold==k & p4$modelo=="B_pedido_geografia",]
    pos<-match(b$order_id[va],pold$order_id)
    ok(nrow(pold)==length(va) && !anyDuplicated(pold$order_id) &&
       !anyNA(pos) && max(abs(pold$dias_reales[pos]-y[va]))<1e-09,
       "OLS B no usa los mismos IDs/respuesta")
    outer[[k]]<-list(tr=tr,va=va,ini=ini)
  }

  cat("[3/8] Replicando validaciones internas de las etapas 5-6...\n")
  inner<-vector("list",4L);ci<-list()
  for(k in 1:4){
    trouter<-outer[[k]]$tr
    limite<-outer[[k]]$ini-as.difftime(45,units="days")
    primer_limite<-dt(paste0(format(limite,"%Y-%m"),"-01 00:00:00"))
    ultimo<-seq(primer_limite,by="-1 month",length.out=2L)[2L]
    meses<-rev(seq(ultimo,by="-1 month",length.out=3L))
    inner[[k]]<-vector("list",3L)
    for(j in 1:3){
      ini<-meses[j];fin<-seq(ini,by="month",length.out=2L)[2L]
      tr<-trouter[fecha[trouter]<=ini-as.difftime(45,units="days") & entrega[trouter]<ini]
      va<-trouter[fecha[trouter]>=ini & fecha[trouter]<fin]
      referencia<-cv5[cv5$fold_externo==k & cv5$fold_interno==j,]
      ok(length(tr)>=1500L && length(va)>=500L &&
         !length(intersect(tr,va)) && all(is.finite(y[c(tr,va)])) &&
         all(entrega[tr]<ini) && nrow(referencia)==1L &&
         referencia$n_train==length(tr) && referencia$n_validacion==length(va) &&
         referencia$mes_validacion==format(ini,"%Y-%m"),
         paste("Fold interno incorrecto",k,j))
      inner[[k]][[j]]<-list(tr=tr,va=va,mes=format(ini,"%Y-%m"))
      ci[[length(ci)+1L]]<-data.frame(fold_externo=k,fold_interno=j,
        mes_validacion=format(ini,"%Y-%m"),n_train=length(tr),n_validacion=length(va))
    }
  }

  cat("[4/8] Seleccionando configuraciones SOLO con validacion interna...\n")
  cvfilas<-list();elegidas<-list();metfilas<-list();predfilas<-list();geof<-list()
  for(k in 1:4){
    cat("  Fold externo ",k," de 4...\n",sep="")
    # Cachea exactamente el mismo preprocesamiento train-only para todas las familias.
    cv_k<-list()
    for(j in 1:3){
      ind<-inner[[k]][[j]]
      rec<-receta(b[ind$tr,,drop=FALSE])
      xt<-Xmat(b[ind$tr,,drop=FALSE],rec)
      xv<-Xmat(b[ind$va,,drop=FALSE],rec)
      ok(identical(names(xt),names(xv)),"Columnas train/validacion distintas")
      for(modelo in familias){
        grid<-configure[[modelo]]
        for(z in 1:nrow(grid)){
          cfg<-grid[z,,drop=FALSE]
          sem<-20261023L+k*1000L+j*100L+match(modelo,familias)*10L+z
          fit<-ajustar(modelo,cfg,xt,y[ind$tr],sem)
          pp<-predecir(modelo,fit,xv,cfg)
          met<-score(y[ind$va],pp)
          cv_k[[length(cv_k)+1L]]<-data.frame(fold_externo=k,fold_interno=j,
             mes_interno=ind$mes,modelo=modelo,config=cfg$config,
             n_train=length(ind$tr),n_validacion=length(ind$va),
             rmse=unname(met["rmse"]),mae=unname(met["mae"]))
          rm(fit)
        }
      }
      gc(verbose=FALSE)
    }
    cvcur<-do.call(rbind,cv_k); cvfilas[[k]]<-cvcur
    for(modelo in familias){
      d<-cvcur[cvcur$modelo==modelo,]
      resumen<-do.call(rbind,lapply(split(d,d$config),function(v){
        ok(nrow(v)==3L && setequal(v$fold_interno,1:3),"Falta validacion interna")
        data.frame(modelo=modelo,config=v$config[1],
          rmse_interno=mean(v$rmse),sd_rmse_interno=stats::sd(v$rmse),
          mae_interno=mean(v$mae))
      }))
      # Si el RMSE interno empata, se prefiere la primera opcion del orden fijado.
      orden<-match(resumen$config,configure[[modelo]]$config)
      resumen<-resumen[order(resumen$rmse_interno,orden),,drop=FALSE]
      best<-resumen[1,,drop=FALSE]
      elegidas[[length(elegidas)+1L]]<-data.frame(fold_externo=k,best,
          margen_rmse_vs_segundo=resumen$rmse_interno[2]-resumen$rmse_interno[1])
      id<-outer[[k]]
      rec<-receta(b[id$tr,,drop=FALSE]);xt<-Xmat(b[id$tr,,drop=FALSE],rec)
      xv<-Xmat(b[id$va,,drop=FALSE],rec)
      ok(identical(names(xt),names(xv)),"Columnas externas diferentes")
      grid<-configure[[modelo]]
      cfg<-grid[match(best$config,grid$config),,drop=FALSE]
      sem<-20261023L+k*1000L+match(modelo,familias)*10L
      fit<-ajustar(modelo,cfg,xt,y[id$tr],sem)
      pp<-predecir(modelo,fit,xv,cfg)
      met<-score(y[id$va],pp)
      etiqueta<-c(CART="Arbol_B_anidado",RandomForest="RandomForest_B_anidado",
                  Boosting="Boosting_B_anidado")[[modelo]]
      predfilas[[length(predfilas)+1L]]<-data.frame(fold=k,
         mes_validacion=folds$mes_validacion[folds$fold==k],
         order_id=b$order_id[id$va],modelo=etiqueta,
         dias_reales=y[id$va],dias_predichos=pp,
         error_predicho_menos_real=pp-y[id$va])
      metfilas[[length(metfilas)+1L]]<-data.frame(fold=k,
         mes_validacion=folds$mes_validacion[folds$fold==k],modelo=etiqueta,
         configuracion=cfg$config,n_train=length(id$tr),n_validacion=length(id$va),
         rmse=unname(met["rmse"]),mae=unname(met["mae"]),
         r2=unname(met["r2"]),sesgo=unname(met["sesgo"]))
      for(g in 0:1){
        ii<-which(b$todos_vendedores_mismo_estado[id$va]==g)
        if(length(ii)>1){
          mg<-score(y[id$va][ii],pp[ii])
          geof[[length(geof)+1L]]<-data.frame(fold=k,modelo=etiqueta,
             segmento=if(g==0) "vendedor_en_otro_estado" else "todos_vendedores_mismo_estado",
             n=length(ii),rmse=unname(mg["rmse"]),mae=unname(mg["mae"]))
        }
      }
      rm(fit)
    }
    gc(verbose=FALSE)
  }

  cat("[5/8] Verificando identificadores y reconstruyendo metricas por pedido...\n")
  cv<-do.call(rbind,cvfilas);hparam<-do.call(rbind,elegidas)
  met<-do.call(rbind,metfilas);pr<-do.call(rbind,predfilas)
  ok(nrow(met)==12L && nrow(pr)==3L*sum(folds$n_validacion) &&
     !anyDuplicated(paste(pr$fold,pr$modelo,pr$order_id,sep="|")),
     "Faltan predicciones o hay duplicados")
  for(k in 1:4){
    val<-outer[[k]]$va
    for(nom in unique(met$modelo)){
      d<-pr[pr$fold==k & pr$modelo==nom,]
      ii<-match(b$order_id[val],d$order_id)
      ok(nrow(d)==length(val) && !anyNA(ii) &&
         max(abs(d$dias_reales[ii]-y[val]))<1e-09,
         "Desalineacion de pedidos/etiquetas")
      m0<-score(y[val],d$dias_predichos[ii])
      mf<-met[met$fold==k & met$modelo==nom,]
      ok(nrow(mf)==1L && all(abs(m0-unlist(mf[1,names(m0)],use.names=FALSE))<1e-08),
         "Metricas guardadas incompatibles con predicciones")
    }
  }
  anteriores<-rbind(
    p4[p4$modelo %in% c("Baseline_media_train","B_pedido_geografia"),
       c("fold","order_id","modelo","dias_reales","dias_predichos")],
    p5[p5$modelo %in% c("Ridge_min","Lasso_min","ElasticNet_min"),
       c("fold","order_id","modelo","dias_reales","dias_predichos")],
    p6[,c("fold","order_id","modelo","dias_reales","dias_predichos")],
    pr[,c("fold","order_id","modelo","dias_reales","dias_predichos")])
  referencias<-c("Baseline_media_train","B_pedido_geografia","Ridge_min",
    "Lasso_min","ElasticNet_min","Splines_B_anidado",
    "Arbol_B_anidado","RandomForest_B_anidado","Boosting_B_anidado")
  compfilas<-list()
  for(k in 1:4){
    val<-outer[[k]]$va
    for(nom in referencias){
      d<-anteriores[anteriores$fold==k & anteriores$modelo==nom,]
      ii<-match(b$order_id[val],d$order_id)
      ok(nrow(d)==length(val) && !anyDuplicated(d$order_id) && !anyNA(ii) &&
         max(abs(d$dias_reales[ii]-y[val]))<1e-09,
         paste("No comparables:",nom,"fold",k))
      s<-score(y[val],d$dias_predichos[ii])
      compfilas[[length(compfilas)+1L]]<-data.frame(fold=k,modelo=nom,
        n_validacion=length(val),rmse=unname(s["rmse"]),mae=unname(s["mae"]),
        r2=unname(s["r2"]),sesgo=unname(s["sesgo"]))
    }
  }
  comp<-do.call(rbind,compfilas)
  prom<-do.call(rbind,lapply(split(comp,comp$modelo),function(d){
    ok(nrow(d)==4L,"Modelo no tiene los cuatro folds")
    data.frame(modelo=d$modelo[1],rmse_promedio=mean(d$rmse),
      sd_rmse_folds=stats::sd(d$rmse),mae_promedio=mean(d$mae),
      r2_promedio=mean(d$r2),sesgo_promedio=mean(d$sesgo))
  }))
  prom<-prom[order(prom$rmse_promedio),];rownames(prom)<-NULL

  cat("[6/8] Guardando CV, predicciones y decisiones de modelamiento...\n")
  wr(do.call(rbind,ci),"01_folds_internos.csv")
  wr(cv,"02_validaciones_internas.csv")
  wr(hparam,"03_hiperparametros_seleccionados.csv")
  wr(met,"04_metricas_por_fold.csv")
  wr(pr,"05_predicciones_validacion.csv")
  wr(comp,"06_comparacion_comun_por_fold.csv")
  wr(prom,"07_comparacion_comun_resumen.csv")
  wr(do.call(rbind,geof),"08_diagnostico_geografico_aparte.csv")
  wr(data.frame(archivo=names(req),ruta=unname(req),md5=unname(tools::md5sum(req))),
     "09_manifest_entradas.csv")
  writeLines(c("ETAPA 7 - CART, RANDOM FOREST Y GRADIENT BOOSTING v1.1",
    "Todas las familias reciben las diez variables ORIGINALES de B transformadas con la misma receta train-only.",
    "Se comparan exactamente los mismos pedidos por fold.",
    "Cuatro folds externos y tres internos por fold (mismas ventanas que etapas 5 y 6).",
    "Cada algoritmo escoge 1 de 3 configuraciones con RMSE de validacion INTERNA.",
    "CART usa rpart sin xval interno adicional; Random Forest usa ranger con semilla y un hilo.",
    "Boosting usa gbm con gaussian, bag.fraction 0.8, semilla y validacion temporal anidada.",
    "Subgrupo geografico se guarda APARTE y no determina la seleccion.",
    "Las medias de cuatro folds son descriptivas; no prueban superioridad estadistica.",
    "La categoria de estado del vendedor se basa en informacion historica asumida al aprobar.",
    "RESULTADOS exploratorios de desarrollo posterior a la consulta del test original.",
    "No se volvio a ejecutar ni evaluar el test historico.",
    paste("Version R:",getRversion()),
    paste("Paquetes:",paste(vapply(paquetes,function(p)
         paste0(p,"=",as.character(utils::packageVersion(p))),character(1)),collapse="; "))),
    file.path(dir7,"10_metodologia_y_limitaciones.txt"))

  cat("[7/8] Generando un grafico claro en R base...\n")
  img<-file.path(img7,"01_rmse_comparacion_familias.png")
  grDevices::png(img,width=2000,height=1100,res=175,bg="white")
  tryCatch({
    op<-par(mar=c(6,18,6,5),family="sans")
    pp<-prom[order(prom$rmse_promedio,decreasing=TRUE),]
    rotulos<-c(Baseline_media_train="Media historica",B_pedido_geografia="OLS B",
       Ridge_min="Ridge",Lasso_min="Lasso",ElasticNet_min="Elastic-Net",
       Splines_B_anidado="Splines naturales",Arbol_B_anidado="Arbol de regresion",
       RandomForest_B_anidado="Random Forest",Boosting_B_anidado="Gradient Boosting")
    labs<-unname(rotulos[pp$modelo])
    ok(!anyNA(labs),"Modelo sin etiqueta en grafico")
    cols<-ifelse(pp$modelo %in% c("Arbol_B_anidado","RandomForest_B_anidado",
                                  "Boosting_B_anidado"),"#137F7C",
                 ifelse(pp$modelo=="Baseline_media_train","#A1AAB5","#7B94AC"))
    positions<-barplot(pp$rmse_promedio,horiz=TRUE,names.arg=labs,las=1,
          col=cols,border=NA,axes=FALSE,xlim=c(0,max(pp$rmse_promedio)*1.18),
          cex.names=0.9,main="RMSE medio | cuatro folds temporales")
    axis(1,at=pretty(c(0,max(pp$rmse_promedio))))
    text(pp$rmse_promedio+0.15,positions,
         labels=sprintf("%.3f",pp$rmse_promedio),pos=4,cex=0.88,col="#19324C",xpd=NA)
    mtext("RMSE (dias; menor es mejor)",side=1,line=3.4)
    box(bty="l",col="#CAD2DC")
    par(op)
  },finally=grDevices::dev.off())

  cat("[8/8] Certificando sin tocar resultados previos...\n")
  expected<-c("01_folds_internos.csv","02_validaciones_internas.csv",
   "03_hiperparametros_seleccionados.csv","04_metricas_por_fold.csv",
   "05_predicciones_validacion.csv","06_comparacion_comun_por_fold.csv",
   "07_comparacion_comun_resumen.csv","08_diagnostico_geografico_aparte.csv",
   "09_manifest_entradas.csv","10_metodologia_y_limitaciones.txt")
  ok(all(file.exists(file.path(dir7,expected))) &&
     all(file.info(file.path(dir7,expected))$size>0) &&
     file.exists(img) && file.info(img)$size>0,
     "Faltan salidas al terminar")
  writeLines(c("ETAPA 7 CERTIFICADA v1.1",
    "Misma especificacion B y IDs de las etapas 4-6.",
    "Modelos CART, ranger y gbm seleccionados solo con 3 folds internos por corte.",
    "Metrica por pedido recalculada y cotejada para las 9 alternativas.",
    "Solo desarrollo; no se evalua el test original ni se alteran datos previos."),certificado)
  # -------------------------------------------------------------------------
  # AVERAGING: combinacion fija 50/50. Se agregan resultados COMPLEMENTARIOS
  # sin cambiar los nueve CSV historicos ni escoger ponderaciones con validacion.
  # Ambos componentes fueron ajustados SOLO con train de cada fold externo.
  # La eleccion de usar estos dos algoritmos es retrospectiva; las metricas
  # NO constituyen una evaluacion independiente del ensemble seleccionado.
  # -------------------------------------------------------------------------
  cat("[AVERAGING] Emparejando Splines y Boosting por ID y fold...\n")
  nombre_avg <- "Averaging_Splines_Boosting"
  pa <- list(); ma <- list()
  for (k in 1:4) {
    spl <- p6[p6$fold == k & p6$modelo == "Splines_B_anidado", , drop=FALSE]
    bst <- pr[pr$fold == k & pr$modelo == "Boosting_B_anidado", , drop=FALSE]
    idref <- b$order_id[outer[[k]]$va]
    ok(nrow(spl) == length(idref) && nrow(bst) == length(idref) &&
         !anyDuplicated(spl$order_id) && !anyDuplicated(bst$order_id),
       paste("Averaging: tamanos o duplicados, fold", k))
    is <- match(idref, spl$order_id)
    ib <- match(idref, bst$order_id)
    ok(!anyNA(is) && !anyNA(ib) &&
         max(abs(spl$dias_reales[is]-y[outer[[k]]$va])) < 1e-9 &&
         max(abs(bst$dias_reales[ib]-y[outer[[k]]$va])) < 1e-9,
       paste("Averaging: error de identificadores o etiquetas, fold",k))
    estimado <- 0.5*spl$dias_predichos[is] + 0.5*bst$dias_predichos[ib]
    ok(length(estimado)==length(idref) && all(is.finite(estimado)),
       paste("Averaging: predicciones no finitas, fold",k))
    metricas <- score(y[outer[[k]]$va], estimado)
    pa[[k]] <- data.frame(fold=k, mes_validacion=folds$mes_validacion[folds$fold==k],
       order_id=idref, modelo=nombre_avg, dias_reales=y[outer[[k]]$va],
       dias_predichos=estimado,
       error_predicho_menos_real=estimado-y[outer[[k]]$va])
    ma[[k]] <- data.frame(fold=k,mes_validacion=folds$mes_validacion[folds$fold==k],
       modelo=nombre_avg, n_validacion=length(idref),
       rmse=unname(metricas["rmse"]),mae=unname(metricas["mae"]),
       r2=unname(metricas["r2"]),sesgo=unname(metricas["sesgo"]))
  }
  avg_pred <- do.call(rbind,pa)
  avg_fold <- do.call(rbind,ma)
  ok(nrow(avg_pred)==sum(folds$n_validacion) && nrow(avg_fold)==4L &&
     !anyDuplicated(paste(avg_pred$fold,avg_pred$order_id)),
     "Averaging: faltan pedidos o hay duplicados")
  avg_res <- data.frame(modelo=nombre_avg,
    rmse_promedio=mean(avg_fold$rmse),sd_rmse_folds=stats::sd(avg_fold$rmse),
    mae_promedio=mean(avg_fold$mae),r2_promedio=mean(avg_fold$r2),
    sesgo_promedio=mean(avg_fold$sesgo))
  wr(avg_pred, "12_averaging_predicciones.csv")
  wr(avg_fold, "13_averaging_metricas_por_fold.csv")
  wr(avg_res, "14_averaging_resumen.csv")
  writeLines(c(
    "AVERAGING COMPLEMENTARIO 50% SPLINES + 50% GRADIENT BOOSTING",
    "Pesos fijos, elegidos sin optimizacion sobre validaciones externas.",
    "Mismo order_id, respuesta y fold para ambos componentes; comprobados.",
    "Cada componente respeta su propio ajuste con TRAIN y seleccion interna.",
    "Es una comparacion retrospectiva de desarrollo, posterior a examinar los modelos.",
    "No se declara ganancia significativa ni se sustituye automaticamente el RDS Splines.",
    "No constituye un nuevo test independiente; test julio-agosto 2018 no consultado.",
    "Las nueve comparaciones historicas y sus CSV se conservan como referencia."
  ),file.path(dir7,"15_averaging_metodologia.txt"))
  writeLines(c("AVERAGING: APROBADO - SOLO DESARROLLO",
               "4 folds; mismos 26279 pedidos; pesos predeterminados de 0.5 y 0.5",
               paste("RMSE medio:",sprintf("%.10f",avg_res$rmse_promedio)),
               "Sin nuevos ajustes de modelos ni consulta de test historico"),
             file.path(dir7,"16_CONTROL_AVERAGING_APROBADO.txt"))
  ok(all(file.exists(file.path(dir7,c("12_averaging_predicciones.csv",
         "13_averaging_metricas_por_fold.csv","14_averaging_resumen.csv",
         "15_averaging_metodologia.txt","16_CONTROL_AVERAGING_APROBADO.txt")))),
      "Faltan resultados complementarios de averaging")
  # Gráfico por fold de diferencia de RMSE, sin barras truncadas.
  figura_avg <- file.path(img7,"02_averaging_vs_splines_por_fold.png")
  ref_rmse <- comp[comp$modelo=="Splines_B_anidado",c("fold","rmse")]
  ref_rmse <- ref_rmse[match(1:4,ref_rmse$fold),]
  delta <- avg_fold$rmse-ref_rmse$rmse
  grDevices::png(figura_avg,width=1550,height=900,res=160,bg="white")
  tryCatch({
    op <- par(mar=c(6,10,5,3),family="sans")
    rango <- range(c(0,delta))
    ancho <- max(diff(rango),0.05)
    plot(delta,4:1,xlim=rango+c(-0.2,0.25)*ancho,ylim=c(0.4,4.6),
         xlab="RMSE averaging menos RMSE Splines (dias)",ylab="",yaxt="n",
         pch=19,cex=1.8,col="#147E84",bty="n",
         main="Averaging: diferencia de RMSE por mes")
    axis(2,at=4:1,labels=c("Dic 2017","Ene 2018","Feb 2018","Mar 2018"),
         las=1,tick=FALSE)
    abline(v=0,lty=2,col="#46566A")
    segments(0,4:1,delta,4:1,col="#98B7B9",lwd=3)
    points(delta,4:1,pch=19,cex=1.6,col="#147E84")
    mtext("Valores negativos: averaging obtuvo menor RMSE en ese fold.",
          side=1,line=4.5,cex=0.82,col="#53657A")
    par(op)
  },finally=grDevices::dev.off())
  ok(file.exists(figura_avg) && file.info(figura_avg)$size>2000,
     "No se creo el grafico averaging")
  cat(sprintf("[AVERAGING] RMSE medio: %.6f dias; Splines: %.6f dias.\n",
      avg_res$rmse_promedio,mean(ref_rmse$rmse)))
  cat("AVERAGING 50/50 complementario certificado aparte (16_CONTROL_AVERAGING_APROBADO.txt).\n",
      file=certificado,append=TRUE)

  cat("\nETAPA 7 COMPLETADA - ARBOLES Y ENSEMBLES; SOLO DESARROLLO\n")
  print(prom,row.names=FALSE,digits=5)
  cat("\nRESULTADOS: ",dir7,"\nGRAFICOS: ",img7,
      "\nCERTIFICADO: ",certificado,"\n",sep="")
})
