# =============================================================================
# USACH | Estadistica Aplicada | Grupo 3 | Proyecto Olist
# ETAPA 4 UNIFICADA: OLS original + seleccion exploratoria posterior
# Se mantienen INTACTAS las dos implementaciones estadisticas ya ejecutadas.
#
# MODO A (POR DEFECTO, SOLO CUANDO LA ETAPA 5 TODAVIA NO EXISTE):
#   source("scripts/04_baseline_y_ols_temporal.R", encoding="UTF-8")
#   Crea resultados/04_baseline_y_ols y sus graficos. Requiere script 04B
#   ya existente para generar las figuras de la comparacion original.
#
# MODO B (DESPUES DE EJECUTAR LA ETAPA 5):
#   options(olist.seleccion_variables=TRUE)
#   source("scripts/04_baseline_y_ols_temporal.R", encoding="UTF-8")
#   options(olist.seleccion_variables=FALSE)
#   Crea resultados/04_seleccion_variables_exploratoria; usa exclusivamente
#   desarrollo y los folds internos YA GENERADOS por la Etapa 5.
#
# Orden de reproduccion: 01, 02, 03, 04(A), 05, 04(B), 06, 07, 08, 09, 10, 11.
# Si etapa 03 tiene analisis opcional de correlaciones, reproducirlo despues
# de su ejecucion principal con la opcion documentada en dicho script.
#
# PROTECCION: en proyectos con resultados posteriores, el modo A NO
# sobrescribe la comparacion original salvo autorizacion explicita.
# El modo B tampoco sobrescribe un certificado ya existente por defecto.
# No se lee el test historico de julio-agosto 2018.
# =============================================================================

modo_seleccion_04 <- isTRUE(getOption("olist.seleccion_variables", FALSE))
if (modo_seleccion_04 && isTRUE(getOption("olist.reproducir_etapa4_historica", FALSE))) {
  stop("Elija solo un modo de Etapa 4 (historico o seleccion).", call. = FALSE)
}
raiz_04 <- normalizePath(getwd(), winslash="/", mustWork=TRUE)
if (modo_seleccion_04) {
  if (file.exists(file.path(raiz_04, "resultados", "04_seleccion_variables_exploratoria",
                            "09_CONTROL_SELECCION_APROBADO.txt")) &&
      !isTRUE(getOption("olist.permitir_reemplazar_seleccion", FALSE))) {
    stop(paste("La seleccion exploratoria ya existe y esta protegida.",
               "NO es necesario reejecutarla en tu proyecto actual.",
               "Para reproducir desde cero, utiliza una carpeta aislada."), call. = FALSE)
  }
} else {
  if ((dir.exists(file.path(raiz_04, "resultados", "05_regularizacion")) ||
       dir.exists(file.path(raiz_04, "resultados", "11_modelo_candidato"))) &&
      !isTRUE(getOption("olist.reproducir_etapa4_historica", FALSE))) {
    stop(paste("Proteccion: ya existen modelos posteriores a la Etapa 4.",
               "No sobrescribas OLS B y sus metricas en este proyecto.",
               "La reproduccion integral debe hacerse en una COPIA AISLADA."), call. = FALSE)
  }
}
rm(raiz_04)

if (modo_seleccion_04) {

  local({
    options(stringsAsFactors = FALSE)
    raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
    r <- file.path(raiz, "resultados")
    destino <- file.path(r, "04_seleccion_variables_exploratoria")
    figuras <- file.path(raiz, "graficos", "04_seleccion_variables_exploratoria")
    entradas <- c(
      datos = file.path(r, "02_variables_candidatas", "01_variables_candidatas_desarrollo.csv"),
      etiquetas = file.path(r, "01_revision_datos", "06_etiquetas_solo_desarrollo.csv"),
      folds = file.path(r, "03_protocolo_y_folds", "04_ids_train_validacion.csv"),
      fechas_folds = file.path(r, "03_protocolo_y_folds", "03_resumen_folds_temporales.csv"),
      internos_referencia = file.path(r, "05_regularizacion", "01_folds_internos.csv"),
      predicciones_B_archivadas = file.path(r, "04_baseline_y_ols", "03_predicciones_validacion.csv"),
      zip_original = file.path(raiz, "datos_originales", "dataset.zip")
    )
    if (any(!file.exists(entradas))) stop(paste("Faltan entradas:",
      paste(names(entradas)[!file.exists(entradas)], collapse=", ")), call.=FALSE)
    if (!dir.exists(file.path(raiz,"scripts"))) stop("Directorio de trabajo incorrecto.",call.=FALSE)
    # Fotografia de la ejecucion archivada, EXCLUYENDO solo esta extension.
    anteriores <- list.files(r, pattern="\\.(csv|txt|rds)$", full.names=TRUE,
                            recursive=TRUE, ignore.case=TRUE)
    anteriores <- anteriores[!startsWith(normalizePath(anteriores,winslash="/",mustWork=FALSE),
                                  paste0(normalizePath(destino,winslash="/",mustWork=FALSE),"/"))]
    md5_antes <- tools::md5sum(anteriores)
    dir.create(destino, recursive=TRUE, showWarnings=FALSE)
    dir.create(figuras, recursive=TRUE, showWarnings=FALSE)
    certificado <- file.path(destino,"09_CONTROL_SELECCION_APROBADO.txt")
    if (file.exists(certificado)) file.remove(certificado)
    guardar <- function(x,n) utils::write.csv(x,file.path(destino,n), row.names=FALSE, na="")
    leer <- function(n) utils::read.csv(n, stringsAsFactors=FALSE,
                                      check.names=FALSE, na.strings=c("", "NA"))
    validar <- function(cond,msg) if (!isTRUE(cond)) stop(msg,call.=FALSE)
    parsear <- function(x) as.POSIXct(x,format="%Y-%m-%d %H:%M:%S",tz="UTC")
    mostrar <- function(x) cat(x,"\n",sep="")

    mostrar("[1/7] Cargando datos archivados y protegiendo los resultados anteriores...")
    b <- leer(entradas[["datos"]]); y <- leer(entradas[["etiquetas"]])
    ids <- leer(entradas[["folds"]]); folds <- leer(entradas[["fechas_folds"]])
    internos_ref <- leer(entradas[["internos_referencia"]])
    pred_B <- leer(entradas[["predicciones_B_archivadas"]])
    columnas <- c("hora_aprobacion","dia_semana","mes_aprobacion",
                  "horas_desde_compra","estado_cliente","n_items","n_vendedores",
                  "precio_total_brl","flete_total_brl","todos_vendedores_mismo_estado")
    validar(nrow(b)==86162L && nrow(y)==81950L && nrow(folds)==4L &&
            all(c("order_id","fecha_aprobacion", columnas) %in% names(b)) &&
            all(c("order_id","dias_entrega") %in% names(y)) &&
            all(c("fold","rol","order_id") %in% names(ids)),
            "Incompatibilidad con los resultados archivados de Etapas 1-3.")
    validar(!anyDuplicated(b$order_id) && !anyDuplicated(y$order_id) &&
            !anyDuplicated(paste(ids$fold,ids$rol,ids$order_id,sep="|")),
            "Duplicacion de order_id o de particiones.")
    validar(setequal(unique(ids$fold),1:4) &&
            setequal(unique(ids$rol),c("train","validacion")),"Folds invalidos.")
    fechas <- parsear(b$fecha_aprobacion)
    yy <- y$dias_entrega[match(b$order_id,y$order_id)]
    validar(!anyNA(fechas) && all(fechas < parsear("2018-07-01 00:00:00")),
            "Fechas fuera del desarrollo.")
    # Orders original: verificacion SOLO de las etiquetas de desarrollo conocidas.
    con <- unz(entradas[["zip_original"]], "olist_orders_dataset.csv",open="r")
    ord <- tryCatch(utils::read.csv(con,colClasses="character",na.strings=c("","NA")),
                    finally=close(con))
    ix <- match(b$order_id,ord$order_id)
    validar(!anyNA(ix) && !anyDuplicated(ord$order_id), "Datos originales de pedidos invalidos.")
    ord <- ord[ix,,drop=FALSE]
    entrega <- parsear(ord$order_delivered_customer_date)
    validar(max(abs(as.numeric(difftime(parsear(ord$order_approved_at),fechas,units="secs"))))<0.5,
            "Aprobacion no coincide con Orders original.")
    identificados <- which(is.finite(yy))
    validar(length(identificados)==81950L &&
            all(ord$order_status[identificados]=="delivered") &&
            all(entrega[identificados] < parsear("2018-07-01 00:00:00")) &&
            max(abs(as.numeric(difftime(entrega[identificados],fechas[identificados],
                                        units="days"))-yy[identificados]))<1e-8,
            "La respuesta difiere de las fechas originales.")

    # Los siguientes candidatos son hipotesis EXPLORATORIAS, inspiradas en el
    # analisis de correlaciones, nunca se escogen por resultados del fold externo.
    propuestas <- list(
      B_original_10 = columnas,
      R1_geografia_flete = c("estado_cliente","todos_vendedores_mismo_estado","flete_total_brl"),
      R2_geo_flete_tiempo = c("estado_cliente","todos_vendedores_mismo_estado",
                               "flete_total_brl","mes_aprobacion","dia_semana","horas_desde_compra"),
      R3_geo_flete_pedido = c("estado_cliente","todos_vendedores_mismo_estado",
                               "flete_total_brl","precio_total_brl","n_items","n_vendedores"),
      R4_geo_flete_pedido_mes = c("estado_cliente","todos_vendedores_mismo_estado",
                                   "flete_total_brl","precio_total_brl","n_items",
                                   "n_vendedores","mes_aprobacion"),
      R5_sin_estado_cliente = setdiff(columnas,"estado_cliente")
    )
    validar(all(vapply(propuestas,function(v) all(v %in% columnas),logical(1))),
            "Propuesta contiene variables desconocidas.")
    candidatos <- data.frame(especificacion=names(propuestas),
      n_variables_originales=vapply(propuestas,length,integer(1)),
      variables=vapply(propuestas,paste,collapse="; ",FUN.VALUE=character(1)),
      stringsAsFactors=FALSE)

    # Solo las familias categorizadas requieren una receta train. Los niveles
    # infrecuentes se agrupan con una regla identica a la Etapa 4 original.
    receta <- function(dt) {
      tb <- table(dt$estado_cliente)
      frecuentes <- names(tb)[tb>=100L]
      if (!length(frecuentes)) frecuentes<-names(tb)[which.max(tb)]
      gs <- ifelse(is.na(dt$estado_cliente) | !(dt$estado_cliente %in% frecuentes),
                   "OTROS",as.character(dt$estado_cliente))
      niveles <- sort(unique(gs))
      fallback <- if ("OTROS" %in% niveles) "OTROS" else names(tb)[which.max(tb)]
      list(frecuentes=frecuentes,niveles=niveles,fallback=fallback)
    }
    matriz <- function(d,vars,rec) {
      n<-nrow(d); x<-matrix(numeric(0),nrow=n,ncol=0)
      agregar <- function(a) x <<- cbind(x,a)
      if ("hora_aprobacion" %in% vars) {
        h<-d$hora_aprobacion
        validar(all(is.finite(h) & h>=0 & h<=23),"Hora invalida.")
        agregar(cbind(hora_sin=sin(2*pi*h/24),hora_cos=cos(2*pi*h/24)))
      }
      if ("dia_semana" %in% vars) {
        z<-d$dia_semana
        validar(all(is.finite(z) & z>=1 & z<=7),"Dia invalido.")
        agregar(cbind(dia_sin=sin(2*pi*(z-1)/7),dia_cos=cos(2*pi*(z-1)/7)))
      }
      if ("mes_aprobacion" %in% vars) {
        z<-d$mes_aprobacion
        validar(all(is.finite(z) & z>=1 & z<=12),"Mes invalido.")
        agregar(cbind(mes_sin=sin(2*pi*(z-1)/12),mes_cos=cos(2*pi*(z-1)/12)))
      }
      if ("horas_desde_compra" %in% vars) {
        z<-d$horas_desde_compra
        validar(all(is.finite(z) & z>=0),"Horas desde compra invalidas.")
        agregar(cbind(log_horas_compra=log1p(z)))
      }
      if ("estado_cliente" %in% vars) {
        raw<-as.character(d$estado_cliente)
        grupo<-ifelse(is.na(raw) | !(raw %in% rec$frecuentes),rec$fallback,raw)
        factor_grupo<-factor(grupo,levels=rec$niveles)
        validar(!anyNA(factor_grupo),"Estado desconocido sin fallback.")
        if (length(rec$niveles)>1L) {
          dummy<-stats::model.matrix(~factor_grupo)[,-1,drop=FALSE]
          colnames(dummy)<-paste0("estado_",make.names(rec$niveles[-1]))
          agregar(dummy)
        }
      }
      nombres<-c(n_items="log_items",n_vendedores="log_vendedores",
                 precio_total_brl="log_precio",flete_total_brl="log_flete")
      for (z in names(nombres)) if (z %in% vars) {
        valor<-as.numeric(d[[z]])
        validar(all(is.finite(valor) & valor>=0),paste("Valor invalido:",z))
        temp<-matrix(log1p(valor),ncol=1)
        colnames(temp)<-nombres[[z]]
        agregar(temp)
      }
      if ("todos_vendedores_mismo_estado" %in% vars) {
        geo<-as.numeric(d$todos_vendedores_mismo_estado)
        validar(all(is.finite(geo) & geo %in% 0:1),"Indicador geografico invalido.")
        agregar(cbind(vendedor_mismo_estado=geo))
      }
      x<-cbind("(Intercept)"=rep(1,n),x)
      storage.mode(x)<-"double"
      validar(all(is.finite(x)) && !anyNA(x),"Matriz de diseno con NA/Inf.")
      x
    }
    metricas <- function(real,pred) {
      err <- real-pred
      sst<-sum((real-mean(real))^2)
      validar(all(is.finite(pred)) && is.finite(sst) && sst>0,"Prediccion/R2 invalido.")
      c(rmse=sqrt(mean(err^2)),mae=mean(abs(err)),
        r2=1-sum(err^2)/sst,sesgo=mean(pred-real))
    }
    predecir <- function(it,iv,vars) {
      dt<-b[it,,drop=FALSE]; dv<-b[iv,,drop=FALSE]
      rec<-if ("estado_cliente" %in% vars) receta(dt) else NULL
      xt<-matriz(dt,vars,rec); xv<-matriz(dv,vars,rec)
      validar(identical(colnames(xt),colnames(xv)),"Columnas distintas train/validacion.")
      fit<-stats::lm.fit(xt,yy[it],tol=1e-9)
      validar(fit$rank==ncol(xt) && all(is.finite(fit$coefficients)),
              "OLS singular o coeficientes invalidos.")
      as.vector(xv %*% as.numeric(fit$coefficients))
    }
    obtener <- function(k,rol) {
      v<-ids$order_id[ids$fold==k & ids$rol==rol]
      ix<-match(v,b$order_id)
      validar(length(ix)>100L && !anyNA(ix) && !anyDuplicated(v) &&
              all(is.finite(yy[ix])),"Ids o etiquetas invalidas en particion.")
      ix
    }
    comparar_archivado <- function(k,iv) {
      elegir<-function(nom) {
        p<-pred_B[pred_B$fold==k & pred_B$modelo==nom,,drop=FALSE]
        pos<-match(b$order_id[iv],p$order_id)
        validar(nrow(p)==length(iv) && !anyNA(pos) &&
                !anyDuplicated(p$order_id) &&
                max(abs(p$dias_reales[pos]-yy[iv]))<1e-9,
                paste("Predicciones historicas invalidas:",nom,k))
        p$dias_predichos[pos]
      }
      list(B=elegir("B_pedido_geografia"),
           media=elegir("Baseline_media_train"))
    }

    mostrar("[2/7] Reconstruyendo las 12 ventanas internas de la Etapa 5...")
    resultados_internos<-list(); decisiones<-list(); metricas_externas<-list()
    preds_externas<-list(); detalles<-list()
    for (k in 1:4) {
      it<-obtener(k,"train"); iv<-obtener(k,"validacion")
      f<-folds[folds$fold==k,,drop=FALSE]
      validar(nrow(f)==1L && length(it)==f$n_train && length(iv)==f$n_validacion &&
              !length(intersect(it,iv)),"Particion externa diferente de la original.")
      ini<-parsear(f$fecha_corte_entrenamiento)
      fin<-seq(ini,by="month",length.out=2L)[2L]
      validar(all(fechas[it] <= ini-as.difftime(45,units="days")) &&
              all(entrega[it]<ini) && all(fechas[iv]>=ini) && all(fechas[iv]<fin) &&
              all(is.finite(yy[c(it,iv)])), "Temporalidad externa no valida.")
      hist<-comparar_archivado(k,iv)
      # La regresion B completa debe ser EXACTAMENTE la original (tolerancia FP).
      pB<-predecir(it,iv,propuestas[["B_original_10"]])
      validar(max(abs(pB-hist$B))<1e-7,
              "El OLS B reconstruido difiere del original: no continuar.")
      limite<-ini-as.difftime(45,units="days")
      primer<-parsear(paste0(format(limite,"%Y-%m"),"-01 00:00:00"))
      ultimo_mes<-seq(primer,by="-1 month",length.out=2L)[2L]
      meses<-rev(seq(ultimo_mes,by="-1 month",length.out=3L))
      cat("  Fold externo",k,":",format(ini,"%Y-%m"),"\n")
      datos_k<-list()
      for (j in 1:3) {
        corte<-meses[j]; hasta<-seq(corte,by="month",length.out=2L)[2L]
        itr<-it[fechas[it]<=corte-as.difftime(45,units="days") & entrega[it]<corte]
        iva<-it[fechas[it]>=corte & fechas[it]<hasta]
        validar(length(itr)>=1500L && length(iva)>=500L &&
                !length(intersect(itr,iva)) && all(is.finite(yy[c(itr,iva)])),
                "Poblacion interna invalida.")
        referencia<-internos_ref[internos_ref$fold_externo==k &
                                 internos_ref$fold_interno==j,,drop=FALSE]
        validar(nrow(referencia)==1L && length(itr)==referencia$n_train &&
                length(iva)==referencia$n_validacion &&
                format(corte,"%Y-%m")==referencia$mes_validacion,
                "Validacion interna NO coincide con Etapa 5.")
        for (nom in names(propuestas)) {
          pred<-predecir(itr,iva,propuestas[[nom]])
          met<-metricas(yy[iva],pred)
          datos_k[[length(datos_k)+1L]]<-data.frame(
            fold_externo=k,fold_interno=j,mes_interno=format(corte,"%Y-%m"),
            especificacion=nom,n_train=length(itr),n_validacion=length(iva),
            rmse=unname(met["rmse"]),mae=unname(met["mae"]),r2=unname(met["r2"]))
        }
      }
      interna<-do.call(rbind,datos_k)
      resultados_internos[[k]]<-interna
      media<-aggregate(rmse~especificacion,interna,mean)
      media$n_variables<-candidatos$n_variables_originales[match(media$especificacion,candidatos$especificacion)]
      ord_sel<-order(media$rmse,media$n_variables,media$especificacion)
      elegido<-as.character(media$especificacion[ord_sel[1L]])
      ganador<-unname(media$rmse[ord_sel[1L]])
      decisiones[[k]]<-data.frame(fold_externo=k,mes_externo=f$mes_validacion,
        especificacion=elegido,n_variables=candidatos$n_variables_originales[
            match(elegido,candidatos$especificacion)],
        rmse_interno_promedio=ganador,
        rmse_interno_B=media$rmse[media$especificacion=="B_original_10"],
        n_train=length(it),n_validacion=length(iv))
      # Solo ganador interno frente a B y la media HISTORICA; nada de comparar
      # candidatos no elegidos en el fold externo (evita elegir mirando externos).
      pred_elegida<-predecir(it,iv,propuestas[[elegido]])
      comparados<-list(Seleccion_interna=pred_elegida,
                       B_original_archivado=hist$B,
                       Baseline_archivado=hist$media)
      for (nom in names(comparados)) {
        pred<-comparados[[nom]]; met<-metricas(yy[iv],pred)
        metricas_externas[[length(metricas_externas)+1L]]<-data.frame(
          fold=k,mes_validacion=f$mes_validacion,modelo=nom,
          especificacion=if(nom=="Seleccion_interna") elegido else
            if(nom=="B_original_archivado") "B_original_10" else "media_train",
          n_validacion=length(iv),rmse=unname(met["rmse"]),
          mae=unname(met["mae"]),r2=unname(met["r2"]),sesgo=unname(met["sesgo"]))
        preds_externas[[length(preds_externas)+1L]]<-data.frame(
          fold=k,order_id=b$order_id[iv],modelo=nom,
          especificacion=if(nom=="Seleccion_interna") elegido else
            if(nom=="B_original_archivado") "B_original_10" else "media_train",
          dias_reales=yy[iv],dias_predichos=pred)
      }
    }
    mostrar("[3/7] Consolidando decisiones y comparando predicciones por order_id...")
    tabla_interna<-do.call(rbind,resultados_internos)
    tabla_decisiones<-do.call(rbind,decisiones)
    tabla_externa<-do.call(rbind,metricas_externas)
    tabla_pred<-do.call(rbind,preds_externas)
    validar(nrow(tabla_interna)==4L*3L*length(propuestas) &&
            nrow(tabla_decisiones)==4L && nrow(tabla_externa)==12L &&
            nrow(tabla_pred)==3L*sum(folds$n_validacion),
            "Dimensiones de comparacion inesperadas.")
    validar(!anyDuplicated(paste(tabla_pred$fold,tabla_pred$modelo,
                                tabla_pred$order_id,sep="|")) &&
            !anyDuplicated(paste(tabla_externa$fold,tabla_externa$modelo,sep="|")),
            "Claves duplicadas modelo/fold/pedido.")
    for (k in 1:4) {
      refer<-b$order_id[obtener(k,"validacion")]
      for (nom in unique(tabla_externa$modelo)) {
        z<-tabla_pred[tabla_pred$fold==k & tabla_pred$modelo==nom,,drop=FALSE]
        pos<-match(refer,z$order_id)
        validar(nrow(z)==length(refer) && !anyNA(pos) &&
          max(abs(z$dias_reales[pos]-yy[obtener(k,"validacion")]))<1e-9,
          "IDs o respuestas diferentes entre modelos.")
        m<-metricas(z$dias_reales[pos],z$dias_predichos[pos])
        ref<-tabla_externa[tabla_externa$fold==k & tabla_externa$modelo==nom,,drop=FALSE]
        validar(nrow(ref)==1L && max(abs(as.numeric(ref[1,c("rmse","mae","r2","sesgo")])-m))<1e-10,
                "Metricas distintas de las predicciones guardadas.")
      }
    }
    resumen<-do.call(rbind,lapply(unique(tabla_externa$modelo),function(nom) {
      v<-tabla_externa[tabla_externa$modelo==nom,,drop=FALSE]
      data.frame(modelo=nom,rmse_promedio=mean(v$rmse),
        sd_rmse_folds=stats::sd(v$rmse),mae_promedio=mean(v$mae),
        r2_promedio=mean(v$r2),sesgo_promedio=mean(v$sesgo))
    }))
    resumen<-resumen[order(resumen$rmse_promedio),,drop=FALSE]
    mostrar("[4/7] Escribiendo SOLO nuevos resultados exploratorios...")
    guardar(candidatos,"01_candidatos_prespecificados.csv")
    guardar(tabla_interna,"02_metricas_internas_por_candidato.csv")
    guardar(tabla_decisiones,"03_seleccion_interna_por_fold.csv")
    guardar(tabla_externa,"04_metricas_externas_solo_seleccion.csv")
    guardar(tabla_pred,"05_predicciones_externas_verificadas.csv")
    guardar(resumen,"06_resumen_descriptivo.csv")
    guardar(data.frame(entrada=names(entradas),archivo=unname(entradas),
         md5=unname(tools::md5sum(entradas))),"07_huellas_entradas.csv")

    mostrar("[5/7] Dibujando el RMSE interno de los candidatos (R base)...")
    # Una figura de validacion interna aporta mas que repetir graficos de RMSE
    # externo casi identicos. Resultados externos: tabla por fold + resumen.
    interno_medio <- aggregate(rmse ~ especificacion, tabla_interna, mean)
    interno_medio <- interno_medio[order(interno_medio$rmse),,drop=FALSE]
    archivo_png <- file.path(figuras,"01_rmse_validacion_interna_por_candidato.png")
    grDevices::png(archivo_png,width=1750,height=920,res=155)
    tryCatch({
      op <- par(no.readonly=TRUE)
      par(mar=c(7,17,5,6),family="sans",xpd=NA)
      valores <- rev(interno_medio$rmse)
      etiquetas <- rev(interno_medio$especificacion)
      minimo <- floor(min(valores)*2)/2 - 0.1
      maximo <- ceiling(max(valores)*2)/2 + 0.5
      posicion <- seq_along(valores)
      plot(valores,posicion,type="n",xlim=c(minimo,maximo),
           ylim=c(0.5,length(valores)+0.5),yaxt="n",
           xlab="RMSE medio de validación interna (días)",ylab="",
           cex.lab=1.15,col.axis="#344b5d")
      axis(2,at=posicion,labels=etiquetas,las=1,tick=FALSE,
           cex.axis=1.02,col.axis="#253c52")
      abline(v=pretty(range(valores)),col="#e5ebf0",lty=3)
      ganador <- interno_medio$especificacion[which.min(interno_medio$rmse)]
      cols <- ifelse(etiquetas==ganador,"#157e80","#7695a8")
      segments(minimo,posicion,valores,posicion,col="#cbd7df",lwd=2)
      points(valores,posicion,pch=19,col=cols,cex=1.5)
      text(valores+0.055,posicion,labels=sprintf("%.3f",valores),
           pos=4,cex=0.92,col="#1d3650")
      title(main="Candidatos de variables: validación interna",adj=0,
            cex.main=1.45,col.main="#17334b",line=2.3)
      mtext("Promedios descriptivos: 3 validaciones internas por cada fold externo",
            side=3,line=0.5,adj=0,cex=0.88,col="#5e7282")
      mtext("Los meses internos pueden repetirse entre folds; no son 12 muestras independientes.",
            side=1,line=4.3,adj=0,cex=0.82,col="#536979")
      par(op)
    },finally=grDevices::dev.off())
    validar(file.exists(archivo_png) && file.info(archivo_png)$size>1500,
            "No se generó el PNG de RMSE interno.")
    firma_png <- as.integer(readBin(archivo_png,what="raw",n=8))
    validar(identical(firma_png,c(137L,80L,78L,71L,13L,10L,26L,10L)),
            "Formato PNG de salida inválido.")

    mostrar("[6/7] Certificando que los resultados anteriores siguen intactos...")
    md5_despues<-tools::md5sum(anteriores)
    validar(identical(unname(md5_antes),unname(md5_despues)),
            "Se modificó un archivo anterior: detener y revisar.")
    notas<-c(
      "ETAPA 4: ANALISIS EXPLORATORIO DE SELECCION DE VARIABLES - POST TEST",
      "No altera ni recalcula los resultados historicos de las Etapas 1 a 8.",
      "Mismos IDs y 4 folds temporales, regla de madurez 45 dias.",
      "Los 6 candidatos se declararon ANTES de mirar los nuevos resultados externos.",
      "Por cada fold externo, 3 validaciones internas coincidentes con Etapa 5.",
      "Las categorias y transformaciones se ajustaron SOLO con TRAIN interno.",
      "Los candidatos se ordenan por RMSE medio interno. Empate: menor complejidad.",
      "SOLO se evalua externamente el ganador de la seleccion interna.",
      "Las referencias OLS B y media historica provienen de archivos congelados.",
      "Se comprueba que recomputar OLS B da identicas predicciones al historico.",
      "La inspeccion previa de metricas externas introduce riesgo de meta-sobreajuste.",
      "Esta es una auditoria prospectiva dentro de desarrollo, pero POST-HOC al test previo.",
      "No constituye evidencia independiente nueva para generalizacion final.",
      "Se ha supuesto disponibilidad de precios, flete y geografia al aprobar; no demostrada fila a fila.",
      "Correlacion NO equivale a importancia condicional/causal ni prueba de seleccion.",
      "No se imputan etiquetas ni se eliminan observaciones extremas.",
      "Las figuras no sustituyen los resultados numericos por fold.",
      "El archivo 09_CONTROL_SELECCION_APROBADO.txt se crea solo al superar todas las verificaciones."
    )
    writeLines(notas,file.path(destino,"08_notas_limitaciones.txt"),useBytes=TRUE)
    writeLines(c("CONTROL APROBADO - SELECCION EXPLORATORIA (ETAPA 4)",
                 "Solo desarrollo: sin acceso a resultados del test historico.",
                 "Se reprodujeron 12 folds internos de la Etapa 5.",
                 "Se eligieron candidatos unicamente por RMSE interno.",
                 "Se evaluo fuera de los entrenamientos internos solo el ganador.",
                 "Se verificaron IDs, fechas, etiquetas, predicciones OLS B y metricas.",
                 "Se conservaron byte a byte los resultados previos de Etapas 1-8."),
               certificado,useBytes=TRUE)
    mostrar("[7/7] SELECCION EXPLORATORIA COMPLETADA, SIN REENTRENAR ETAPAS 5-8 NI TEST")
    print(tabla_decisiones,row.names=FALSE,digits=5)
    print(resumen,row.names=FALSE,digits=5)
    cat("Resultados:",destino,"\nGráficos:",figuras,"\n")
  })
} else {
# =============================================================================
# USACH | ESTADISTICA APLICADA | GRUPO 3 | PROYECTO OLIST
# ETAPA 4 - BASELINE Y REGRESION LINEAL (OLS) | R BASE
# Version 1.4 - mismos calculos/control v1.1; graficos en R base
# Se sobrescriben solo productos de la Etapa 4; las Etapas 1-3 quedan intactas.
# Requiere los archivos GENERADOS por los scripts 01, 02 y 03.
# Ejecutar desde la raiz del proyecto, idealmente en sesion limpia:
# source("scripts/04_baseline_y_ols_temporal.R", encoding = "UTF-8")
# =============================================================================

local({
  options(stringsAsFactors = FALSE)
  raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  origen <- file.path(raiz, "resultados")
  dir_1 <- file.path(origen, "01_revision_datos")
  dir_2 <- file.path(origen, "02_variables_candidatas")
  dir_3 <- file.path(origen, "03_protocolo_y_folds")
  dir_4 <- file.path(origen, "04_baseline_y_ols")
  dir_fig <- file.path(raiz, "graficos", "04_baseline_y_ols")
  archivos <- c(
    base = file.path(dir_2, "01_variables_candidatas_desarrollo.csv"),
    etiquetas = file.path(dir_1, "06_etiquetas_solo_desarrollo.csv"),
    folds = file.path(dir_3, "04_ids_train_validacion.csv"),
    resumen = file.path(dir_3, "03_resumen_folds_temporales.csv"),
    especificaciones = file.path(dir_3, "02_especificaciones_previas.csv"),
    zip = file.path(raiz, "datos_originales", "dataset.zip")
  )
  if (any(!file.exists(archivos))) {
    stop(paste("Faltan archivos:", paste(archivos[!file.exists(archivos)],
                                      collapse = " | ")), call. = FALSE)
  }
  # NO modifica los productos anteriores ni el dataset original.
  dir.create(dir_4, recursive = TRUE, showWarnings = FALSE)
  dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)
  # Si una nueva ejecucion falla, un certificado anterior NO sigue siendo valido.
  certificado <- file.path(dir_4, "11_CONTROL_COMPARABILIDAD_APROBADO.txt")
  if (file.exists(certificado)) file.remove(certificado)
  escribir <- function(obj, nombre) {
    utils::write.csv(obj, file.path(dir_4, nombre), row.names = FALSE, na = "")
  }
  leer <- function(ruta) utils::read.csv(ruta, stringsAsFactors = FALSE,
                                          check.names = FALSE, na.strings = c("", "NA"))
  imprimir <- function(txt) message(txt)
  falla <- function(cond, msg) if (!isTRUE(cond)) stop(msg, call. = FALSE)

  imprimir("[1/7] Leyendo salidas oficiales de etapas 1, 2 y 3...")
  b <- leer(archivos[["base"]])
  y <- leer(archivos[["etiquetas"]])
  ids <- leer(archivos[["folds"]])
  resumen <- leer(archivos[["resumen"]])
  specs <- leer(archivos[["especificaciones"]])
  esperado_abc <- c("A_temporal", "B_pedido_geografia", "C_catalogo_exploratoria")
  falla(nrow(b) == 86162L && nrow(y) == 81950L, "Bases historicas incompatibles.")
  falla(nrow(resumen) == 4L && nrow(specs) == 3L,
        "Faltan cuatro folds o tres especificaciones.")
  falla(setequal(specs$especificacion, esperado_abc), "Especificaciones desconocidas.")
  falla(!anyDuplicated(b$order_id) && !anyDuplicated(y$order_id),
        "Pedidos duplicados en datos historicos.")
  falla(!anyDuplicated(paste(ids$fold, ids$rol, ids$order_id, sep = "|")),
        "Duplicacion de pedido/rol/fold.")
  falla(all(ids$order_id %in% y$order_id) && all(ids$order_id %in% b$order_id),
        "Fold incluye pedido sin etiqueta o sin predictores.")
  falla(setequal(unique(ids$fold), 1:4) &&
          setequal(unique(ids$rol), c("train", "validacion")),
        "Folds o roles inesperados.")
  y_por_pedido <- y$dias_entrega[match(b$order_id, y$order_id)]
  fecha_ap <- as.POSIXct(b$fecha_aprobacion, tz = "UTC",
                          format = "%Y-%m-%d %H:%M:%S")
  falla(!anyNA(fecha_ap), "Hay fechas de aprobacion invalidas.")
  falla(all(fecha_ap < as.POSIXct("2018-07-01 00:00:00", tz = "UTC")),
        "Una fila de test o periodo posterior entro en las variables de desarrollo.")
  falla(all(!is.na(y_por_pedido[match(y$order_id, b$order_id)])),
        "Etiquetas y base de predictores no estan alineadas por order_id.")
  fecha_y <- as.POSIXct(y$fecha_aprobacion, tz = "UTC",
                         format = "%Y-%m-%d %H:%M:%S")
  falla(!anyNA(fecha_y) &&
          all(abs(as.numeric(difftime(fecha_y,
              fecha_ap[match(y$order_id, b$order_id)], units = "secs"))) < 0.5),
        "La fecha de aprobacion no coincide entre predictores y etiquetas.")

  # Auditoria de procedencia: recontrastar las etiquetas usadas con Orders
  # ORIGINAL, por identificador. No se calculan resultados del test historico.
  imprimir("     Contraste de order_id, respuesta y fechas con orders original...")
  con_orig <- unz(archivos[["zip"]], "olist_orders_dataset.csv", open = "r")
  ord <- tryCatch(utils::read.csv(con_orig, colClasses = "character",
                                  na.strings = c("", "NA"),
                                  stringsAsFactors = FALSE),
                  finally = close(con_orig))
  requeridas <- c("order_id", "order_approved_at",
                  "order_delivered_customer_date", "order_status")
  falla(all(requeridas %in% names(ord)) && !anyDuplicated(ord$order_id),
        "El CSV original de pedidos tiene IDs o columnas invalidos.")
  ix_ord <- match(b$order_id, ord$order_id)
  falla(!anyNA(ix_ord), "Faltan IDs de desarrollo en Orders original.")
  ord <- ord[ix_ord, requeridas, drop = FALSE]
  aprob_original <- as.POSIXct(ord$order_approved_at, tz = "UTC",
                               format = "%Y-%m-%d %H:%M:%S")
  entrega_original <- as.POSIXct(ord$order_delivered_customer_date,
                                 tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  falla(!anyNA(aprob_original) &&
          all(abs(as.numeric(difftime(aprob_original, fecha_ap,
                                       units = "secs"))) < 0.5),
        "La aprobacion no coincide con Orders original.")
  iy <- match(y$order_id, b$order_id)
  dias_recalculados <- as.numeric(difftime(entrega_original[iy],
                                           aprob_original[iy], units = "days"))
  falla(!anyNA(dias_recalculados) && all(is.finite(dias_recalculados)) &&
          all(ord$order_status[iy] == "delivered") &&
          all(entrega_original[iy] < as.POSIXct("2018-07-01", tz = "UTC")) &&
          max(abs(dias_recalculados - y$dias_entrega)) < 1e-08,
        "Etiquetas inconsistentes con la entrega real en Orders original.")
  rm(dias_recalculados)

  # Las columnas son variables observadas (originales), no coeficientes.
  A <- c("hora_aprobacion", "dia_semana", "mes_aprobacion", "horas_desde_compra")
  B <- c(A, "estado_cliente", "n_items", "n_vendedores", "precio_total_brl",
         "flete_total_brl", "todos_vendedores_mismo_estado")
  C <- c(B, "peso_total_g", "volumen_total_cm3")
  falla(all(C %in% names(b)), "Faltan variables de la etapa 2.")
  falla(all(vapply(seq_len(nrow(specs)), function(i) {
    s <- specs$especificacion[i]
    objetivo <- switch(s, A_temporal = A, B_pedido_geografia = B,
                       C_catalogo_exploratoria = C)
    setequal(trimws(strsplit(specs$variables[i], ";", fixed = TRUE)[[1]]), objetivo)
  }, logical(1))), "Las variables no coinciden con el protocolo de etapa 3.")

  # Entrenamiento y validacion usan EXACTAMENTE los IDs fijados en etapa 3.
  indices <- function(k, rol) {
    vec <- ids$order_id[ids$fold == k & ids$rol == rol]
    ix <- match(vec, b$order_id)
    falla(length(ix) > 0L && !anyNA(ix), "IDs del fold no encontrados.")
    ix
  }
  validar_fechas <- function(train, valid, fecha_inicio, mes_validacion) {
    ini <- as.POSIXct(fecha_inicio, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
    mes_esperado <- format(ini, "%Y-%m", tz = "UTC")
    fin <- seq(ini, by = "month", length.out = 2L)[2L]
    falla(!is.na(ini) && identical(mes_esperado, as.character(mes_validacion)),
          "Mes de validacion y fecha de corte inconsistentes.")
    falla(!length(intersect(train, valid)) &&
            all(fecha_ap[train] <= ini - as.difftime(45, units = "days")) &&
            all(entrega_original[train] < ini) &&
            all(fecha_ap[valid] >= ini) && all(fecha_ap[valid] < fin) &&
            all(entrega_original[valid] < as.POSIXct("2018-07-01", tz = "UTC")) &&
            all(ord$order_status[c(train, valid)] == "delivered") &&
            all(!is.na(y_por_pedido[c(train, valid)])) &&
            all(y_por_pedido[c(train, valid)] > 0),
          "Fuga temporal, mes incorrecto, etiqueta no observable o solapamiento.")
  }

  # Todas las transformaciones son FIJAS y no dependen de la respuesta.
  # La agrupacion de estados y las medias de imputacion dependen SOLO de train.
  variables_numericas <- c("horas_desde_compra", "n_items", "n_vendedores",
                           "precio_total_brl", "flete_total_brl", "peso_total_g",
                           "volumen_total_cm3")
  hacer_receta <- function(train, usar_catalogo = FALSE) {
    for (z in intersect(variables_numericas, names(train))) {
      dat <- train[[z]]
      falla(!any(!is.na(dat) & (!is.finite(dat) | dat < 0)),
            paste("Variable no numerica finita o negativa:", z))
    }
    cuenta_estado <- table(train$estado_cliente)
    frecuentes <- names(cuenta_estado)[cuenta_estado >= 100L]
    if (!length(frecuentes)) frecuentes <- names(cuenta_estado)[which.max(cuenta_estado)]
    grupo <- ifelse(is.na(train$estado_cliente) |
                      !(train$estado_cliente %in% frecuentes),
                    "OTROS", train$estado_cliente)
    niveles <- sort(unique(grupo))
    valor_fallback <- if ("OTROS" %in% niveles) "OTROS" else
      names(cuenta_estado)[which.max(cuenta_estado)]
    medias <- numeric(0)
    if (usar_catalogo) {
      medias <- vapply(c("peso_total_g", "volumen_total_cm3"), function(z) {
        m <- mean(train[[z]], na.rm = TRUE)
        falla(is.finite(m) && m >= 0, paste("Media invalida:", z))
        m
      }, numeric(1))
    }
    list(frecuentes = frecuentes, niveles = niveles,
         fallback = valor_fallback, medias = medias)
  }

  crear_matriz <- function(datos, tipo, receta) {
    n <- nrow(datos)
    num <- function(z) as.numeric(datos[[z]])
    # No tratar hora, dia y mes como distancia lineal arbitraria: ciclos.
    h <- num("hora_aprobacion")
    d <- num("dia_semana")
    m <- num("mes_aprobacion")
    hc <- num("horas_desde_compra")
    falla(all(is.finite(h) & h >= 0 & h <= 23) &&
            all(is.finite(d) & d >= 1 & d <= 7) &&
            all(is.finite(m) & m >= 1 & m <= 12) &&
            all(is.finite(hc) & hc >= 0), "Variables temporales invalidas.")
    X <- cbind(hora_sin = sin(2 * pi * h / 24),
               hora_cos = cos(2 * pi * h / 24),
               dia_sin = sin(2 * pi * (d - 1) / 7),
               dia_cos = cos(2 * pi * (d - 1) / 7),
               mes_sin = sin(2 * pi * (m - 1) / 12),
               mes_cos = cos(2 * pi * (m - 1) / 12),
               log_horas_compra = log1p(hc))
    if (tipo != "A_temporal") {
      for (z in c("n_items", "n_vendedores", "precio_total_brl", "flete_total_brl",
                  "todos_vendedores_mismo_estado")) {
        val <- num(z)
        falla(all(is.finite(val) & val >= 0),
              paste("Faltante/valor invalido en", z, ": revisar origen; no imputar arbitrariamente."))
      }
      geo <- num("todos_vendedores_mismo_estado")
      falla(all(geo %in% c(0, 1)), "La relacion entre estados debe ser 0/1.")
      raw_estado <- as.character(datos$estado_cliente)
      grupo <- ifelse(is.na(raw_estado) | !(raw_estado %in% receta$frecuentes),
                      receta$fallback, raw_estado)
      grupo <- factor(grupo, levels = receta$niveles)
      falla(!anyNA(grupo), "Estado nuevo sin grupo o fallback valido.")
      if (length(receta$niveles) > 1L) {
        D <- stats::model.matrix(~ grupo)[, -1, drop = FALSE]
        colnames(D) <- paste0("estado_", make.names(receta$niveles[-1]))
        X <- cbind(X, D)
      }
      X <- cbind(X,
                 log_items = log1p(num("n_items")),
                 log_vendedores = log1p(num("n_vendedores")),
                 log_precio = log1p(num("precio_total_brl")),
                 log_flete = log1p(num("flete_total_brl")),
                 vendedor_mismo_estado = geo)
    }
    if (tipo == "C_catalogo_exploratoria") {
      raw_p <- num("peso_total_g")
      raw_v <- num("volumen_total_cm3")
      raw_p[is.na(raw_p)] <- receta$medias[["peso_total_g"]]
      raw_v[is.na(raw_v)] <- receta$medias[["volumen_total_cm3"]]
      falla(all(is.finite(raw_p) & raw_p >= 0) &&
              all(is.finite(raw_v) & raw_v >= 0), "Imputacion de catalogo invalida.")
      X <- cbind(X, log_peso = log1p(raw_p), log_volumen = log1p(raw_v))
    }
    X <- cbind("(Intercept)" = rep(1, n), X)
    falla(is.matrix(X) && nrow(X) == n && !anyNA(X) && all(is.finite(X)),
          "Matriz de diseno contiene NA o infinitos.")
    storage.mode(X) <- "double"
    X
  }

  ajustar <- function(xtr, ytr, xva) {
    falla(identical(colnames(xtr), colnames(xva)) &&
            nrow(xtr) == length(ytr), "Matrices train/val inconsistentes.")
    modelo <- stats::lm.fit(xtr, ytr, tol = 1e-09)
    falla(modelo$rank == ncol(xtr),
          paste("Matriz singular: rango", modelo$rank, "vs columnas", ncol(xtr)))
    beta <- as.numeric(modelo$coefficients)
    falla(all(is.finite(beta)), "Coeficientes OLS invalidos.")
    pred <- as.vector(xva %*% beta)
    falla(all(is.finite(pred)), "Predicciones NA/infinitas.")
    list(pred = pred, coef = beta, nombres = colnames(xtr),
         rmse_train = sqrt(mean(modelo$residuals^2)), ncoef = ncol(xtr))
  }
  metricas <- function(real, pred) {
    falla(length(real) == length(pred) && length(real) >= 2L &&
            all(is.finite(real)) && all(is.finite(pred)), "Metricas invalidas.")
    error <- real - pred
    sst <- sum((real - mean(real))^2)
    falla(sst > 0, "R2 indefinido: respuesta sin variacion.")
    c(rmse = sqrt(mean(error^2)), mae = mean(abs(error)),
      r2 = 1 - sum(error^2) / sst, sesgo = mean(pred - real))
  }

  imprimir("[2/7] Comprobando folds, tiempos y variables observadas...")
  for (k in 1:4) {
    tr <- indices(k, "train")
    va <- indices(k, "validacion")
    sub <- resumen[resumen$fold == k, , drop = FALSE]
    falla(nrow(sub) == 1L && nrow(b[tr, , drop = FALSE]) == sub$n_train &&
            length(va) == sub$n_validacion, "Tamanos de folds no coinciden.")
    falla(all(ids$mes_validacion[ids$fold == k] == sub$mes_validacion),
          "Etiqueta de mes distinta entre lista de IDs y resumen de folds.")
    validar_fechas(tr, va, sub$fecha_corte_entrenamiento, sub$mes_validacion)
  }

  imprimir("[3/7] Ajustando baseline y OLS en las tres especificaciones...")
  nombres_modelo <- c("Baseline_media_train", esperado_abc)
  sal_metr <- list(); sal_preds <- list(); sal_coef <- list(); sal_imput <- list()
  sal_sub <- list(); sal_medias <- list(); sal_sens <- list()
  for (k in 1:4) {
    tr <- indices(k, "train")
    va <- indices(k, "validacion")
    dtr <- b[tr, , drop = FALSE]
    dva <- b[va, , drop = FALSE]
    ytr <- y_por_pedido[tr]
    yva <- y_por_pedido[va]
    predb <- rep(mean(ytr), length(va))
    lista_pred <- list(Baseline_media_train = predb)
    sal_medias[[length(sal_medias) + 1L]] <- data.frame(
      fold = k, media_y_train = mean(ytr), media_y_validacion = mean(yva),
      sd_y_validacion = stats::sd(yva), n_train = length(tr), n_val = length(va))
    for (tipo in esperado_abc) {
      isC <- identical(tipo, "C_catalogo_exploratoria")
      # Las recetas son distintas para cada fold y se calculan SOLO en train.
      receta <- hacer_receta(dtr, usar_catalogo = isC)
      Xtr <- crear_matriz(dtr, tipo, receta)
      Xva <- crear_matriz(dva, tipo, receta)
      fit <- ajustar(Xtr, ytr, Xva)
      lista_pred[[tipo]] <- fit$pred
      sal_coef[[length(sal_coef) + 1L]] <- data.frame(
        fold = k, modelo = tipo, termino = fit$nombres,
        coeficiente = fit$coef, stringsAsFactors = FALSE)
      if (isC) {
        sal_imput[[length(sal_imput) + 1L]] <- data.frame(
          fold = k, variable = names(receta$medias),
          media_train = as.numeric(receta$medias),
          n_faltantes_train = c(sum(is.na(dtr$peso_total_g)),
                               sum(is.na(dtr$volumen_total_cm3))),
          n_faltantes_validacion = c(sum(is.na(dva$peso_total_g)),
                                    sum(is.na(dva$volumen_total_cm3))),
          stringsAsFactors = FALSE)
        # SENSIBILIDAD: reentrenar C sin los casos incompletos.
        completos <- !is.na(dtr$peso_total_g) & !is.na(dtr$volumen_total_cm3)
        falla(sum(completos) > 100L && all(!is.na(dva$peso_total_g)) &&
                all(!is.na(dva$volumen_total_cm3)),
              "Casos completos insuficientes o validacion con faltantes.")
        dtrcc <- dtr[completos, , drop = FALSE]
        rec_cc <- hacer_receta(dtrcc, usar_catalogo = TRUE)
        fit_cc <- ajustar(crear_matriz(dtrcc, tipo, rec_cc), ytr[completos],
                          crear_matriz(dva, tipo, rec_cc))
        sal_sens[[length(sal_sens) + 1L]] <- data.frame(
          fold = k, contraste = "C_casos_completos_vs_imputacion",
          n_train_completo = sum(completos), n_train_imputado = length(tr),
          rmse_cc = unname(metricas(yva, fit_cc$pred)["rmse"]),
          rmse_imputado = unname(metricas(yva, fit$pred)["rmse"]),
          stringsAsFactors = FALSE)
      }
    }
    # Todos los modelos se miden sobre EXACTAMENTE los mismos pedidos.
    for (nombre in nombres_modelo) {
      pred <- lista_pred[[nombre]]
      met <- metricas(yva, pred)
      sal_metr[[length(sal_metr) + 1L]] <- data.frame(
        fold = k, mes_validacion = resumen$mes_validacion[resumen$fold == k], modelo = nombre,
        n_train = length(tr), n_validacion = length(va),
        rmse = unname(met["rmse"]), mae = unname(met["mae"]),
        r2 = unname(met["r2"]), sesgo = unname(met["sesgo"]),
        stringsAsFactors = FALSE)
      sal_preds[[length(sal_preds) + 1L]] <- data.frame(
        fold = k, order_id = b$order_id[va], modelo = nombre,
        dias_reales = yva, dias_predichos = pred, error_predicho_menos_real = pred-yva,
        stringsAsFactors = FALSE)
      # Subgrupo exigido por pauta: FUERA de la tabla principal.
      grupo <- dva$todos_vendedores_mismo_estado
      for (g in c(0, 1)) {
        idx <- which(!is.na(grupo) & grupo == g)
        if (length(idx) >= 2L) {
          ms <- metricas(yva[idx], pred[idx])
          sal_sub[[length(sal_sub) + 1L]] <- data.frame(
            fold = k, modelo = nombre,
            grupo = if (g == 0) "al_menos_un_vendedor_otro_estado" else
              "todos_vendedores_mismo_estado",
            n = length(idx), rmse = unname(ms["rmse"]),
            mae = unname(ms["mae"]), stringsAsFactors = FALSE)
        }
      }
    }
    imprimir(paste("  Fold", k, "completado: train", length(tr),
                  "| validacion", length(va)))
  }

  imprimir("[4/7] Consolidando metricas y coeficientes...")
  metr <- do.call(rbind, sal_metr)
  preds <- do.call(rbind, sal_preds)
  coefi <- do.call(rbind, sal_coef)
  imp <- do.call(rbind, sal_imput)
  sub <- do.call(rbind, sal_sub)
  medias <- do.call(rbind, sal_medias)
  sens <- do.call(rbind, sal_sens)
  # El resumen promedia folds, SIN mezclar media RMSE con RMSE total ponderado.
  # Cada resultado debe tener una clave UNICA (fold, modelo, order_id).
  falla(!anyDuplicated(paste(preds$fold, preds$modelo, preds$order_id, sep = "|")),
        "Predicciones duplicadas para mismo pedido/fold/modelo.")
  falla(!anyDuplicated(paste(metr$fold, metr$modelo, sep = "|")),
        "Se mezclaron filas de metricas entre modelos o folds.")
  for (k in 1:4) {
    referencia <- b$order_id[indices(k, "validacion")]
    respuesta <- y_por_pedido[indices(k, "validacion")]
    for (nom in nombres_modelo) {
      p <- preds[preds$fold == k & preds$modelo == nom, , drop = FALSE]
      pos <- match(referencia, p$order_id)
      falla(nrow(p) == length(referencia) && !anyNA(pos) &&
              !anyDuplicated(p$order_id) &&
              max(abs(p$dias_reales[pos] - respuesta)) < 1e-10,
            "Respuesta o IDs diferentes entre modelos del mismo fold.")
      er <- p$dias_reales[pos] - p$dias_predichos[pos]
      chequeo <- c(rmse = sqrt(mean(er^2)), mae = mean(abs(er)),
                   r2 = 1 - sum(er^2) / sum((p$dias_reales[pos] -
                         mean(p$dias_reales[pos]))^2),
                   sesgo = mean(-er))
      m <- metr[metr$fold == k & metr$modelo == nom, , drop = FALSE]
      falla(nrow(m) == 1L && m$n_validacion == length(referencia) &&
              m$n_train == length(indices(k, "train")) &&
              all(abs(as.numeric(m[1L, names(chequeo)]) - chequeo) < 1e-10),
            "Metricas resumidas NO coinciden con predicciones del pedido.")
      if (nom == "Baseline_media_train") {
        media_ref <- medias$media_y_train[medias$fold == k]
        falla(length(media_ref) == 1L &&
                max(abs(p$dias_predichos[pos] - media_ref)) < 1e-10,
              "El baseline utiliza una media ajena al entrenamiento.")
      }
    }
  }
  resumen_modelos <- do.call(rbind, lapply(nombres_modelo, function(nom) {
    z <- metr[metr$modelo == nom, , drop = FALSE]
    falla(nrow(z) == 4L, "Falta un fold por modelo.")
    data.frame(modelo = nom, folds = 4L, rmse_promedio = mean(z$rmse),
               sd_rmse_entre_folds = stats::sd(z$rmse),
               mae_promedio = mean(z$mae), r2_promedio = mean(z$r2),
               sesgo_promedio = mean(z$sesgo), stringsAsFactors = FALSE)
  }))
  resumen_modelos <- resumen_modelos[order(resumen_modelos$rmse_promedio), ]
  rownames(resumen_modelos) <- NULL
  escribir(metr, "01_metricas_por_fold.csv")
  escribir(resumen_modelos, "02_resumen_comparativo_principal.csv")
  escribir(preds, "03_predicciones_validacion.csv")
  escribir(coefi, "04_coeficientes_ols_por_fold.csv")
  escribir(imp, "05_medias_imputacion_train.csv")
  escribir(sub, "06_subgrupo_aparte.csv")
  escribir(medias, "07_medias_de_respuesta_por_fold.csv")
  escribir(sens, "08_sensibilidad_casos_completos.csv")
  # Huellas de los archivos de entrada: evitan mezclar versiones de etapas.
  manifest <- data.frame(entrada = names(archivos),
                         archivo = unname(archivos),
                         md5 = unname(tools::md5sum(archivos)),
                         stringsAsFactors = FALSE)
  escribir(manifest, "10_manifiesto_archivos_de_entrada.csv")

  imprimir("[5/7] Regenerando graficos portables con R base (v1.4)...")
  # Se parte de las metricas y las predicciones guardadas en CSV.
  # El script 04B verifica de nuevo que correspondan a los mismos IDs/folds.
  # No entrena ni evalua test; nunca modifica las etapas 1-3.
  script_graficos <- file.path(raiz, "scripts",
                                  "04B_regenerar_graficos_datacamp.R")
  falla(file.exists(script_graficos),
        "Falta scripts/04B_regenerar_graficos_datacamp.R")
  source(script_graficos, local = TRUE, encoding = "UTF-8")
  fichero_rmse <- file.path(dir_fig, "01_rmse_validacion_por_modelo.png")
  fichero_r2 <- file.path(dir_fig, "02_r2_validacion_por_fold.png")
  falla(file.exists(fichero_rmse) && file.exists(fichero_r2) &&
          all(file.info(c(fichero_rmse, fichero_r2))$size > 100),
        "No se generaron los PNG de Etapa 4 v1.4.")

  imprimir("[6/7] Validaciones finales del registro de resultados...")
  falla(nrow(metr) == 16L && nrow(resumen_modelos) == 4L &&
          nrow(preds) == 4L * sum(resumen$n_validacion),
        "Dimension inesperada de metricas/predicciones.")
  falla(all(is.finite(metr$rmse)) && all(metr$rmse >= 0) &&
          all(is.finite(metr$r2)) && all(is.finite(metr$mae)),
        "Metricas no validas.")
  # Cada prediccion debe estar asociada a un ID de validacion del fold.
  for (k in 1:4) {
    ref <- b$order_id[indices(k, "validacion")]
    for (nom in nombres_modelo) {
      idx <- preds$fold == k & preds$modelo == nom
      falla(sum(idx) == length(ref) && setequal(preds$order_id[idx], ref),
            "Comparaciones usan validaciones distintas.")
    }
  }

  imprimir("[7/7] Guardando notas metodologicas para la defensa...")
  notas <- c(
    "ETAPA 4: PRIMERA COMPARACION DE REGRESION LINEAL (OLS) EN DESARROLLO",
    "SOLO desarrollo y validacion; NO se reevalua el test historico 2018-07/08.",
    "El test original ya es conocido; cualquier nuevo modelo es exploratorio/post-test.",
    "Las tres especificaciones A/B/C fueron definidas por criterio previo en etapa 3.",
    "Mismo calendario y mismos IDs de validacion para todos los modelos.",
    "Baseline: media aritmetica de la respuesta en el entrenamiento de cada fold.",
    "OLS A: variables temporales; B: suma caracteristicas de pedido y geografia;",
    "C: agrega peso y volumen del catalogo; vigencia historica aun no probada.",
    "C: faltantes de peso/volumen imputados con la MEDIA ARITMETICA de TRAIN.",
    "Se verifica como sensibilidad un ajuste C con casos completos de entrenamiento.",
    "No se imputan etiquetas ni faltantes estructurales de items; no se eliminan outliers.",
    "log1p aplica a conteos, precios, fletes, peso y volumen; sin usar el target.",
    "Hora, dia y mes se codifican con seno/coseno por periodicidad.",
    "Estados poco frecuentes (<100 pedidos TRAIN) agrupados en OTROS por fold.",
    "RMSE y MAE: errores en dias. R2=1-SSE/SST del mes validado; puede ser negativo.",
    "La tabla principal NO mezcla un subgrupo con la comparacion global de modelos.",
    "El diagnostico geografico esta en archivo aparte por requisito de la pauta.",
    "Promedios de 4 folds: DESCRIPTIVOS; no equivalen a prueba de superioridad.",
    "NO se usa alpha=0.05 para seleccionar por RMSE ni forzar R2 alto.",
    "La inferencia con IC95/alpha=0.05 requiere supuestos y se analizara por separado.",
    "No probar la disponibilidad historica de vendedor/producto es una LIMITACION.",
    "Objetivo: comprobar cuanto aporta la informacion, no inventar mejora de precision.",
    "VALIDACION REFORZADA: IDs, fechas y dias de entrega comparados con CSV original.",
    "Se verifica en cada fold la misma respuesta por pedido y las metricas de prediccion.",
    "Graficos v1.4: R base, barras RMSE desde cero y matriz R2 por mes.",
    "El archivo 11_CONTROL_COMPARABILIDAD_APROBADO.txt certifica controles superados.",
    "Si falta dicho archivo, NO usar una tabla anterior como resultado de esta corrida."
  )
  writeLines(notas, file.path(dir_4, "09_notas_metodologicas.txt"), useBytes = TRUE)
  writeLines(c("CONTROL APROBADO - ETAPA 4 VERSION 1.4",
               "Se confrontaron las etiquetas con Orders original usando order_id.",
               "Se verificaron fechas de train y validacion por fold.",
               "Se comprobaron mismas validaciones y respuestas para los 4 modelos.",
               "Se recalcularon RMSE, MAE, R2 y sesgo desde predicciones guardadas.",
               "Se verifico el baseline con la media de train del fold.",
               "Se guardaron las huellas MD5 de los archivos de entrada.",
               "Los graficos de RMSE y R2 se crearon a partir de metricas verificadas.",
               "El test historico no fue reentrenado ni reevaluado."),
             certificado, useBytes = TRUE)
  cat("\n", strrep("=", 71), "\n", sep="")
  cat("ETAPA 4 COMPLETADA - BASELINE Y OLS; SOLO DESARROLLO\n")
  print(resumen_modelos[, c("modelo", "rmse_promedio", "mae_promedio", "r2_promedio")],
        row.names=FALSE, digits=5)
  cat("\nResultados:", dir_4, "\nGraficos:", dir_fig, "\n")
  cat(strrep("=", 71), "\n", sep="")
})
}
rm(modo_seleccion_04)
