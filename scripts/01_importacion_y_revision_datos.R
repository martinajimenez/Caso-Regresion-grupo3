# ============================================================================
# ESTADISTICA APLICADA | CASO 1 | GRUPO 3
# ETAPA 1: IMPORTACION Y REVISION INICIAL DEL DATASET OLIST
# VERSION: 1.0 (proyecto nuevo, independiente del codigo antiguo)
# ============================================================================
# OBJETIVO:
#   (1) Comprobar que la fuente contiene los nueve CSV originales.
#   (2) Revisar integridad de tablas y fechas, sin eliminar outliers.
#   (3) Definir explicitamente el universo de desarrollo y las etiquetas
#       observables en su fecha de corte historica.
#   (4) Generar resultados y figuras solo para desarrollo.
#   (5) Identificar posibles predictores; NO entrenar ni seleccionar modelos.
#
# EJECUCION EN RSTUDIO:
#   1. Abrir RStudio y reiniciar la sesion: Session > Restart R.
#   2. Definir como directorio de trabajo la carpeta Proyecto_Olist_Nuevo.
#   3. Ejecutar: source("scripts/01_importacion_y_revision_datos.R", encoding="UTF-8")
#
# REQUISITOS: R base (no necesita instalar paquetes).
# ENTRADA: datos_originales/dataset.zip (los nueve CSV originales de Olist).
# SALIDAS: resultados/01_revision_datos/ y graficos/01_revision_datos/.
#
# IMPORTANTE:
#   El test historico julio-agosto 2018 fue evaluado en un proyecto previo.
#   Este script no calcula resultados de test ni lo convierte en test nuevo.
#   La seleccion futura de variables y modelos exige validar dentro de folds.
#   El nivel alfa=0.05 se aplicara al hacer inferencia, no para excluir datos.
# ============================================================================

options(stringsAsFactors = FALSE)

# --- 1. VALIDAR ENTORNO Y ARCHIVO ORIGINAL ---------------------------------
raiz <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
ruta_zip <- file.path(raiz, "datos_originales", "dataset.zip")
if (!file.exists(ruta_zip)) {
  stop(paste0(
    "No se encontro el archivo original: ", ruta_zip, "\n",
    "Abre la carpeta Proyecto_Olist_Nuevo como directorio de trabajo, ",
    "y verifica que exista datos_originales/dataset.zip."
  ), call. = FALSE)
}

archivos_esperados <- c(
  "olist_customers_dataset.csv",
  "olist_geolocation_dataset.csv",
  "olist_order_items_dataset.csv",
  "olist_order_payments_dataset.csv",
  "olist_order_reviews_dataset.csv",
  "olist_orders_dataset.csv",
  "olist_products_dataset.csv",
  "olist_sellers_dataset.csv",
  "product_category_name_translation.csv"
)
info_zip <- utils::unzip(ruta_zip, list = TRUE)
archivos_zip <- info_zip$Name
faltan <- setdiff(archivos_esperados, archivos_zip)
if (length(faltan)) {
  stop(paste("ZIP incompleto; faltan:", paste(faltan, collapse = ", ")), call. = FALSE)
}
if (anyDuplicated(archivos_zip)) {
  stop("El ZIP contiene entradas con nombres duplicados.", call. = FALSE)
}

salida <- file.path(raiz, "resultados", "01_revision_datos")
figuras <- file.path(raiz, "graficos", "01_revision_datos")
dir.create(salida, recursive = TRUE, showWarnings = FALSE)
dir.create(figuras, recursive = TRUE, showWarnings = FALSE)

# Leer cadenas como texto protege codigos e identificadores con ceros iniciales.
leer_csv_original <- function(nombre) {
  conexion <- unz(ruta_zip, nombre, open = "r")
  on.exit(close(conexion), add = TRUE)
  tryCatch(
    utils::read.csv(
      conexion, header = TRUE, sep = ",", quote = '"',
      colClasses = "character", na.strings = c("", "NA"),
      check.names = FALSE, stringsAsFactors = FALSE,
      fileEncoding = ""
    ),
    error = function(e) stop(paste("Error al leer", nombre, ":", e$message), call. = FALSE)
  )
}

requerir_columnas <- function(tabla, columnas, nombre) {
  ausentes <- setdiff(columnas, names(tabla))
  if (length(ausentes)) {
    stop(paste("Faltan columnas en", nombre, ":", paste(ausentes, collapse = ", ")),
         call. = FALSE)
  }
}

# Cinco tablas prioritarias; los otros cuatro CSV quedan inventariados.
message("[1/7] Cargando cinco tablas esenciales desde el ZIP original...")
pedidos <- leer_csv_original("olist_orders_dataset.csv")
articulos <- leer_csv_original("olist_order_items_dataset.csv")
clientes <- leer_csv_original("olist_customers_dataset.csv")
vendedores <- leer_csv_original("olist_sellers_dataset.csv")
productos <- leer_csv_original("olist_products_dataset.csv")

requerir_columnas(pedidos,
  c("order_id", "customer_id", "order_status", "order_purchase_timestamp",
    "order_approved_at", "order_delivered_customer_date"), "orders")
requerir_columnas(articulos,
  c("order_id", "order_item_id", "product_id", "seller_id", "price", "freight_value"),
  "order_items")
requerir_columnas(clientes, c("customer_id", "customer_state"), "customers")
requerir_columnas(vendedores, c("seller_id", "seller_state"), "sellers")
requerir_columnas(productos,
  c("product_id", "product_weight_g", "product_length_cm", "product_height_cm",
    "product_width_cm"), "products")

if (anyNA(pedidos$order_id) || anyDuplicated(pedidos$order_id)) {
  stop("order_id de orders tiene faltantes o duplicados.", call. = FALSE)
}

manifiesto <- data.frame(
  archivo = info_zip$Name,
  tamano_bytes_original = info_zip$Length,
  cargado_en_etapa_1 = info_zip$Name %in% c(
    "olist_orders_dataset.csv", "olist_order_items_dataset.csv",
    "olist_customers_dataset.csv", "olist_sellers_dataset.csv",
    "olist_products_dataset.csv"),
  stringsAsFactors = FALSE
)
utils::write.csv(manifiesto, file.path(salida, "01_inventario_csv.csv"), row.names = FALSE,
                 na = "")

# --- 2. FECHAS Y DESARROLLO HISTORICO --------------------------------------
message("[2/7] Delimitando desarrollo por instante de aprobacion...")
# UTC se utiliza como reloj tecnico para no aplicar conversion automatica
# de huso horario de Chile a las cadenas de fecha locales de Olist.
convertir_fecha <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

fecha_aprobacion <- convertir_fecha(pedidos$order_approved_at)
fechas_mal_formadas <- sum(!is.na(pedidos$order_approved_at) & is.na(fecha_aprobacion))
if (fechas_mal_formadas) {
  stop(paste("Fechas de aprobacion imposibles de interpretar:", fechas_mal_formadas),
       call. = FALSE)
}

inicio_dev <- as.POSIXct("2017-01-01 00:00:00", tz = "UTC")
corte_dev <- as.POSIXct("2018-07-01 00:00:00", tz = "UTC")
en_desarrollo <- !is.na(fecha_aprobacion) &
  fecha_aprobacion >= inicio_dev & fecha_aprobacion < corte_dev
# No estudiar etiquetas ni distribuciones de pedidos ajenos a desarrollo.
dev <- pedidos[en_desarrollo, , drop = FALSE]
aprob_dev <- fecha_aprobacion[en_desarrollo]
compra_dev <- convertir_fecha(dev$order_purchase_timestamp)
entrega_dev <- convertir_fecha(dev$order_delivered_customer_date)
if (nrow(dev) == 0L) stop("No se encontraron pedidos de desarrollo.", call. = FALSE)

mal_formadas_compra <- sum(!is.na(dev$order_purchase_timestamp) & is.na(compra_dev))
mal_formadas_entrega <- sum(!is.na(dev$order_delivered_customer_date) & is.na(entrega_dev))
if (mal_formadas_compra + mal_formadas_entrega > 0L) {
  stop("Se encontraron fechas de compra o entrega no interpretables en desarrollo.",
       call. = FALSE)
}

# Una etiqueta solo existe al corte si la entrega ya ocurrio ANTES del corte.
# Se exige estado delivered y tiempo estrictamente positivo.
con_entrega <- !is.na(entrega_dev)
observada_al_corte <- con_entrega & entrega_dev < corte_dev
intervalo_positivo <- !is.na(entrega_dev) & entrega_dev > aprob_dev
estado_entregado <- !is.na(dev$order_status) & dev$order_status == "delivered"
etiqueta_valida <- estado_entregado & observada_al_corte & intervalo_positivo

etiquetas <- data.frame(
  order_id = dev$order_id[etiqueta_valida],
  fecha_aprobacion = dev$order_approved_at[etiqueta_valida],
  dias_entrega = as.numeric(difftime(
    entrega_dev[etiqueta_valida], aprob_dev[etiqueta_valida], units = "days")),
  stringsAsFactors = FALSE
)
stopifnot(nrow(etiquetas) > 1000L,
          !anyNA(etiquetas$dias_entrega),
          all(is.finite(etiquetas$dias_entrega)),
          all(etiquetas$dias_entrega > 0),
          !anyDuplicated(etiquetas$order_id),
          nrow(etiquetas) <= nrow(dev))

# --- 3. AUDITORIA DE REGLAS Y RELACIONES -----------------------------------
message("[3/7] Revisando integridad, exclusiones y relaciones...")
ids_desarrollo <- dev$order_id
art_dev <- articulos[!is.na(articulos$order_id) &
                       articulos$order_id %in% ids_desarrollo, , drop = FALSE]
cli_dev <- clientes[!is.na(clientes$customer_id) &
                     clientes$customer_id %in% dev$customer_id, , drop = FALSE]
ven_dev <- vendedores[!is.na(vendedores$seller_id) &
                        vendedores$seller_id %in% art_dev$seller_id, , drop = FALSE]
pro_dev <- productos[!is.na(productos$product_id) &
                       productos$product_id %in% art_dev$product_id, , drop = FALSE]

# Un producto/pedido puede aparecer en varias filas de articulos: no unir
# directamente tablas uno-a-muchos sin agregar antes por order_id.
clave_art <- paste(art_dev$order_id, art_dev$order_item_id, sep = "::")
conteos <- data.frame(
  concepto = c(
    "Pedidos originales: universo del archivo orders",
    "Aprobados en ventana de desarrollo (ene-2017 a jun-2018)",
    "Desarrollo con etiqueta observable y valida al corte",
    "Desarrollo sin etiqueta valida al corte (NO son test)",
    "Desarrollo: entrega faltante",
    "Desarrollo: entrega registrada pero no observable al corte",
    "Desarrollo: estado distinto de delivered",
    "Desarrollo: duracion no positiva con entrega observada",
    "Desarrollo: compra posterior a aprobacion",
    "Filas de articulos vinculadas a pedidos de desarrollo",
    "Pedidos de desarrollo sin fila de articulos",
    "Pedidos de desarrollo sin ficha de cliente",
    "Filas de articulos sin vendedor registrado en tabla sellers",
    "Filas de articulos sin producto registrado en tabla products",
    "Duplicados de (order_id, order_item_id) en desarrollo",
    "Duplicados customer_id vinculados al desarrollo",
    "Duplicados seller_id vinculados al desarrollo",
    "Duplicados product_id vinculados al desarrollo"
  ),
  n = c(
    nrow(pedidos), nrow(dev), nrow(etiquetas), nrow(dev) - nrow(etiquetas),
    sum(!con_entrega),
    sum(con_entrega & entrega_dev >= corte_dev, na.rm = TRUE),
    sum(!estado_entregado),
    sum(observada_al_corte & !intervalo_positivo, na.rm = TRUE),
    sum(!is.na(compra_dev) & compra_dev > aprob_dev),
    nrow(art_dev),
    sum(!ids_desarrollo %in% art_dev$order_id),
    sum(!dev$customer_id %in% cli_dev$customer_id),
    sum(!art_dev$seller_id %in% ven_dev$seller_id),
    sum(!art_dev$product_id %in% pro_dev$product_id),
    sum(duplicated(clave_art)),
    sum(duplicated(cli_dev$customer_id)),
    sum(duplicated(ven_dev$seller_id)),
    sum(duplicated(pro_dev$product_id))
  ), stringsAsFactors = FALSE
)
utils::write.csv(conteos, file.path(salida, "02_conteos_y_calidad.csv"), row.names = FALSE)

# Validaciones de integridad: errores estructurales deben detener el proyecto.
if (any(duplicated(clave_art)) || any(duplicated(cli_dev$customer_id)) ||
    any(duplicated(ven_dev$seller_id)) || any(duplicated(pro_dev$product_id))) {
  stop("Se detectaron claves duplicadas: revisar 02_conteos_y_calidad.csv.",
       call. = FALSE)
}
if (any(!dev$customer_id %in% cli_dev$customer_id)) {
  stop("Pedidos de desarrollo sin cliente: revisar fuentes.", call. = FALSE)
}

# --- 4. EXPLORACION DE LA VARIABLE RESPUESTA (SOLO DESARROLLO) ------------
message("[4/7] Describiendo el tiempo de entrega sin filtrar outliers...")
y <- etiquetas$dias_entrega
percentiles <- stats::quantile(y,
  probs = c(0, .01, .05, .25, .50, .75, .90, .95, .99, 1),
  na.rm = TRUE, names = FALSE)
resumen_y <- data.frame(
  estadistico = c("N", "Media", "Desviacion estandar", "P0", "P1",
                  "P5", "P25", "Mediana_P50", "P75", "P90", "P95", "P99",
                  "Maximo", "N_mayor_30_dias", "N_mayor_60_dias"),
  valor = c(length(y), mean(y), stats::sd(y), percentiles,
            sum(y > 30), sum(y > 60)),
  stringsAsFactors = FALSE
)
utils::write.csv(resumen_y, file.path(salida, "03_distribucion_dias_entrega.csv"),
                 row.names = FALSE)

# Resumen mensual informativo, NO scores ni datos de test.
mes_dev <- substr(etiquetas$fecha_aprobacion, 1L, 7L)
indices_mes <- split(seq_along(y), mes_dev)
resumen_mes <- do.call(rbind, lapply(names(indices_mes), function(m) {
  v <- y[indices_mes[[m]]]
  data.frame(mes_aprobacion = m, n = length(v),
             media_dias = mean(v), mediana_dias = stats::median(v),
             p90_dias = as.numeric(stats::quantile(v, .90)),
             stringsAsFactors = FALSE)
}))
rownames(resumen_mes) <- NULL
resumen_mes <- resumen_mes[order(resumen_mes$mes_aprobacion), , drop = FALSE]
utils::write.csv(resumen_mes, file.path(salida, "04_distribucion_mensual_desarrollo.csv"),
                 row.names = FALSE)

# --- 5. CANDIDATAS: SOLO INVENTARIO, AUN NO PREDICTORES -------------------
message("[5/7] Registrando disponibilidad hipotetica de variables...")
catalogo <- data.frame(
  elemento = c(
    "Momento de aprobacion", "Tiempo compra-aprobacion",
    "Cantidad de articulos", "Precio y flete pactados",
    "Estado/ubicacion del cliente", "Estado/ubicacion de vendedores",
    "Peso y dimensiones de productos", "Cantidad de vendedores",
    "Tipo/monto del pago", "Fecha prometida por Olist",
    "Fecha real de despacho", "Fecha real de entrega", "Reviews",
    "Estado final del pedido"
  ),
  origen = c(
    "orders", "orders", "order_items", "order_items", "customers",
    "sellers", "products", "order_items", "order_payments", "orders",
    "orders", "orders", "order_reviews", "orders"
  ),
  clasificacion_preliminar = c(
    rep("REVISAR DISPONIBILIDAD EN APROBACION", 10),
    rep("EXCLUIR: CONOCIDO DESPUES DE APROBACION", 4)
  ),
  observacion = c(
    "Conocido al aprobar; derivados temporales se crean por fold",
    "Fechas conocidas al aprobar, revisar consistencia",
    "Verificar que el detalle exista al aprobar; agregar por pedido",
    "Verificar valores y vigencia historica por pedido",
    "Ubicacion asociada a cliente; comprobar vigencia historica",
    "Puede haber varios vendedores por pedido: definir agregacion",
    "Metadatos de catalogo pueden no tener vigencia historica",
    "Agregar vendedor distinto por order_id antes de cruzar",
    "No cargado en etapa 1; verificar instante de registro",
    "No asumir que se guardo igual al instante de aprobacion",
    "Evento futuro: fuga de informacion",
    "Define respuesta; nunca incluir como predictor",
    "Escritas despues de la experiencia de compra",
    "Estado final puede codificar informacion futura"
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(catalogo, file.path(salida, "05_inventario_variables.csv"),
                 row.names = FALSE)

# --- 6. FIGURAS REPRODUCIBLES (ESTILO PROFESIONAL v2.1) -----------------
message("[6/7] Generando graficos descriptivos mejorados de desarrollo...")
source(file.path(raiz, "scripts", "01A_estilo_graficos_etapa1.R"),
       local = TRUE, encoding = "UTF-8")
crear_graficos_etapa1(y, resumen_mes, figuras)

# --- 7. RESULTADOS Y BITACORA ----------------------------------------------
message("[7/7] Guardando etiquetas y registro de verificacion...")
# Las etiquetas son exclusivamente de desarrollo. Son un artefacto de salida,
# no una base de predictores ni un conjunto de validacion definitivo.
utils::write.csv(etiquetas, file.path(salida, "06_etiquetas_solo_desarrollo.csv"),
                 row.names = FALSE, na = "")

metadatos <- c(
  "GRUPO 3 - PROYECTO OLIST NUEVO - ETAPA 1",
  paste("R:", R.version.string),
  paste("Directorio:", raiz),
  paste("MD5 del ZIP original:", as.character(tools::md5sum(ruta_zip))),
  "Ventana de desarrollo: [2017-01-01, 2018-07-01) por aprobacion",
  "Etiqueta observable: estado delivered; entrega antes de 2018-07-01 y posterior a aprobacion",
  "Fechas interpretadas con UTC tecnico; huso real de Brasil no verificado",
  "NO se exploraron ni evaluaron etiquetas de julio-agosto 2018",
  "NO se entrenaron modelos ni se eligieron variables mediante scores",
  "NO se eliminaron valores extremos por su magnitud",
  "NO se ejecutaron pruebas de hipotesis; alfa=0.05 se reserva para inferencia",
  paste("Pedidos aprobados de desarrollo:", nrow(dev)),
  paste("Desarrollo etiquetado valido:", nrow(etiquetas)),
  paste("Desarrollo no etiquetado valido:", nrow(dev) - nrow(etiquetas)),
  paste("Media dias:", round(mean(y), 4)),
  paste("Mediana dias:", round(stats::median(y), 4)),
  paste("P99 dias:", round(as.numeric(stats::quantile(y, .99)), 4)),
  paste("Maximo dias:", round(max(y), 4)),
  "Advertencia metodologica: el test historico fue visto en el proyecto anterior; no vuelve a ser independiente.",
  "Pendiente etapa 2: auditar origen/disponibilidad real de predictores y construir variables por fold."
)
writeLines(metadatos, file.path(salida, "07_resumen_y_limitaciones.txt"), useBytes = TRUE)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("ETAPA 1 COMPLETADA - SIN ENTRENAR MODELOS NI EVALUAR TEST\n")
cat("Pedidos originales:", nrow(pedidos), "\n")
cat("Pedidos de desarrollo:", nrow(dev), "\n")
cat("Etiquetas validas al corte:", nrow(etiquetas), "\n")
cat("Tiempo mediano de entrega:", round(stats::median(y), 2), "dias\n")
cat("Resultados:", salida, "\n")
cat("Graficos:", figuras, "\n")
cat(strrep("=", 70), "\n", sep = "")
