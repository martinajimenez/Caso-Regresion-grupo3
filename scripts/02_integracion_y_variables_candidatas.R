# =============================================================================
# ESTADISTICA APLICADA | GRUPO 3 | OLIST NUEVO
# ETAPA 2 - INTEGRACION POR PEDIDO Y VARIABLES CANDIDATAS (SIN MODELAMIENTO)
# VERSION 1.1 - FALTANTES Y TARDANZA DOCUMENTADOS
# =============================================================================
# USO (desde la raiz Proyecto_Olist_caso1, en sesion R limpia):
# source("scripts/02_integracion_y_variables_candidatas.R", encoding="UTF-8")
# REQUISITOS: R base; 01_importacion_y_revision_datos.R ya ejecutado una vez.
# ENTRADA: datos_originales/dataset.zip y etiqueta archivada en Etapa 1.
# SALIDA: resultados/02_variables_candidatas/*.csv y *.txt.
# LA TARDANZA SE REPORTA SOLO COMO DIAGNOSTICO, NO COMO PREDICTOR.
# LOS FALTANTES SE CONTABILIZAN, NO SE IMPUTAN EN ESTA ETAPA.
# NO SE ENTRENAN MODELOS, NO SE REVISAN PEDIDOS DE TEST NI SE ELIGEN
# VARIABLES SEGUN SU R2 O RMSE. LOS RESULTADOS SON DESCRIPTIVOS.
# ADVERTENCIA: ciertos atributos del catalogo, precios, vendedores y cliente
# podrian NO estar historicamente vigentes al aprobar. Son CANDIDATOS.
# =============================================================================

options(stringsAsFactors = FALSE)
raiz02 <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
zip02 <- file.path(raiz02, "datos_originales", "dataset.zip")
ref01 <- file.path(raiz02, "resultados", "01_revision_datos",
                   "06_etiquetas_solo_desarrollo.csv")
out02 <- file.path(raiz02, "resultados", "02_variables_candidatas")
if (!file.exists(zip02)) stop("Falta datos_originales/dataset.zip; revisa la carpeta activa.", call. = FALSE)
if (!file.exists(ref01)) stop("Falta la Etapa 1: ejecuta antes el script 01.", call. = FALSE)
dir.create(out02, recursive = TRUE, showWarnings = FALSE)

leer02 <- function(nombre) {
  con <- unz(zip02, nombre, open = "r")
  on.exit(close(con), add = TRUE)
  utils::read.csv(con, colClasses = "character", na.strings = c("", "NA"),
                  check.names = FALSE, stringsAsFactors = FALSE)
}
fecha02 <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
numero02 <- function(x, nombre) {
  valor <- suppressWarnings(as.numeric(x))
  mal <- !is.na(x) & is.na(valor)
  if (any(mal)) stop(paste("Datos numericos mal formados en", nombre,
                           ":", sum(mal)), call. = FALSE)
  valor
}
vector_por_id02 <- function(tabla, columna, ids) {
  # Extrae UNA fila agregada por ID; deja NA para pedidos sin articulos.
  if (anyDuplicated(tabla$order_id)) stop("Duplicados en tabla agregada.", call. = FALSE)
  tabla[[columna]][match(ids, tabla$order_id)]
}

message("[1/6] Cargando tablas originales y verificando etapa anterior...")
ord <- leer02("olist_orders_dataset.csv")
art <- leer02("olist_order_items_dataset.csv")
cli <- leer02("olist_customers_dataset.csv")
sel <- leer02("olist_sellers_dataset.csv")
pro <- leer02("olist_products_dataset.csv")
req <- list(
  orders = c("order_id", "customer_id", "order_purchase_timestamp",
             "order_approved_at", "order_delivered_customer_date", "order_estimated_delivery_date",
             "order_status"),
  items = c("order_id", "order_item_id", "seller_id", "product_id", "price", "freight_value"),
  customers = c("customer_id", "customer_state"),
  sellers = c("seller_id", "seller_state"),
  products = c("product_id", "product_weight_g", "product_length_cm",
               "product_height_cm", "product_width_cm"))
for (nm in names(req)) {
  orig <- switch(nm, orders = ord, items = art, customers = cli,
                 sellers = sel, products = pro)
  faltas <- setdiff(req[[nm]], names(orig))
  if (length(faltas)) stop(paste("Columnas ausentes en", nm,
                                 paste(faltas, collapse = ", ")), call. = FALSE)
}
if (anyDuplicated(ord$order_id) || anyDuplicated(cli$customer_id) ||
    anyDuplicated(sel$seller_id) || anyDuplicated(pro$product_id)) {
  stop("Hay IDs duplicados donde debe existir una unica fila.", call. = FALSE)
}
ini <- as.POSIXct("2017-01-01 00:00:00", tz = "UTC")
cut <- as.POSIXct("2018-07-01 00:00:00", tz = "UTC")
fecha_ap <- fecha02(ord$order_approved_at)
dev <- ord[!is.na(fecha_ap) & fecha_ap >= ini & fecha_ap < cut, , drop = FALSE]
ap <- fecha02(dev$order_approved_at)
if (nrow(dev) != 86162L) stop("La ventana de desarrollo no coincide con la Etapa 1.", call. = FALSE)
ref <- utils::read.csv(ref01, colClasses = c("character", "character", "numeric"),
                       stringsAsFactors = FALSE)
if (!all(c("order_id", "fecha_aprobacion", "dias_entrega") %in% names(ref)) ||
    anyDuplicated(ref$order_id)) stop("Archivo de etiquetas de Etapa 1 invalido.", call. = FALSE)
if (nrow(ref) != 81950L || any(!ref$order_id %in% dev$order_id)) {
  stop("Las etiquetas archivadas no coinciden con la Etapa 1.", call. = FALSE)
}
# Reproducir independientemente las etiquetas al corte: NO utilizar el futuro.
ent <- fecha02(dev$order_delivered_customer_date)
ok <- !is.na(dev$order_status) & dev$order_status == "delivered" &
      !is.na(ent) & ent < cut & ent > ap
ids_ok <- dev$order_id[ok]
dias_ok <- as.numeric(difftime(ent[ok], ap[ok], units = "days"))
ix_ref <- match(ids_ok, ref$order_id)
if (anyNA(ix_ref) || nrow(ref) != length(ids_ok) ||
    any(abs(ref$dias_entrega[ix_ref] - dias_ok) > 1e-8)) {
  stop("Inconsistencia entre etiquetas de Etapas 1 y 2.", call. = FALSE)
}

message("[2/6] Agregando articulos, productos y vendedores POR PEDIDO...")
art <- art[!is.na(art$order_id) & art$order_id %in% dev$order_id, , drop = FALSE]
if (!nrow(art)) stop("No hay articulos de desarrollo.", call. = FALSE)
if (anyDuplicated(paste(art$order_id, art$order_item_id, sep = "::"))) {
  stop("Filas de articulos duplicadas por pedido y posicion.", call. = FALSE)
}
precio <- numero02(art$price, "price")
flete <- numero02(art$freight_value, "freight_value")
pidx <- match(art$product_id, pro$product_id)
peso <- numero02(pro$product_weight_g[pidx], "product_weight_g")
largo <- numero02(pro$product_length_cm[pidx], "product_length_cm")
alto <- numero02(pro$product_height_cm[pidx], "product_height_cm")
ancho <- numero02(pro$product_width_cm[pidx], "product_width_cm")
# No corregir automaticamente medidas imposibles: reportar y marcar
# derivado volumetrico como NA si las dimensiones son no positivas.
vol <- largo * alto * ancho
vol[is.na(largo) | is.na(alto) | is.na(ancho) |
      (!is.na(largo) & largo <= 0) |
      (!is.na(alto) & alto <= 0) |
      (!is.na(ancho) & ancho <= 0)] <- NA_real_
if (any(!is.na(precio) & precio < 0) || any(!is.na(flete) & flete < 0)) {
  stop("Precios o fletes negativos; revisar la fuente antes de continuar.", call. = FALSE)
}
# rowsum suma filas del mismo order_id. Si alguna componente es NA, el
# TOTAL tambien queda NA. No se imputa informacion faltante como cero.
a <- rowsum(cbind(n_items = rep(1, nrow(art)),
                  precio_total_brl = precio, flete_total_brl = flete,
                  peso_total_g = peso, volumen_total_cm3 = vol),
            group = art$order_id, reorder = TRUE, na.rm = FALSE)
tot <- data.frame(order_id = rownames(a), a, check.names = FALSE,
                  stringsAsFactors = FALSE, row.names = NULL)

# Vendedores: agregacion sobre pares UNICOS (pedido, vendedor).
unicos <- unique(art[, c("order_id", "seller_id"), drop = FALSE])
unicos$seller_state <- sel$seller_state[match(unicos$seller_id, sel$seller_id)]
cliente_uf_por_pedido <- cli$customer_state[match(dev$customer_id, cli$customer_id)]
unicos$customer_state <- cliente_uf_por_pedido[match(unicos$order_id, dev$order_id)]
unicos$coincide <- ifelse(is.na(unicos$seller_state) |
                          is.na(unicos$customer_state), NA_integer_,
                          as.integer(unicos$seller_state == unicos$customer_state))
cuentas <- rowsum(cbind(n_vendedores = rep(1L, nrow(unicos)),
                         n_vendedores_mismo_estado = unicos$coincide),
                   group = unicos$order_id, reorder = TRUE, na.rm = FALSE)
vtab <- data.frame(order_id = rownames(cuentas), cuentas,
                   check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL)
vtab$todos_vendedores_mismo_estado <- ifelse(
  is.na(vtab$n_vendedores_mismo_estado), NA_integer_,
  as.integer(vtab$n_vendedores == vtab$n_vendedores_mismo_estado))
# Mantener una unica fila por pedido incluso con multiples vendedores.
if (anyDuplicated(tot$order_id) || anyDuplicated(vtab$order_id)) {
  stop("Error de agregacion: un pedido aparece mas de una vez.", call. = FALSE)
}

message("[3/6] Construyendo tabla de candidatas en desarrollo...")
compra <- fecha02(dev$order_purchase_timestamp)
horas <- as.numeric(difftime(ap, compra, units = "hours"))
if (any(!is.na(horas) & horas < 0)) {
  stop("Tiempo compra-aprobacion negativo; revisar fuente.", call. = FALSE)
}
mt <- as.POSIXlt(ap, tz = "UTC")
base <- data.frame(
  order_id = dev$order_id,
  fecha_aprobacion = dev$order_approved_at,
  hora_aprobacion = mt$hour,
  dia_semana = as.integer(format(ap, "%u", tz = "UTC")),
  mes_aprobacion = mt$mon + 1L,
  horas_desde_compra = horas,
  estado_cliente = cliente_uf_por_pedido,
  n_items = vector_por_id02(tot, "n_items", dev$order_id),
  n_vendedores = vector_por_id02(vtab, "n_vendedores", dev$order_id),
  precio_total_brl = vector_por_id02(tot, "precio_total_brl", dev$order_id),
  flete_total_brl = vector_por_id02(tot, "flete_total_brl", dev$order_id),
  peso_total_g = vector_por_id02(tot, "peso_total_g", dev$order_id),
  volumen_total_cm3 = vector_por_id02(tot, "volumen_total_cm3", dev$order_id),
  todos_vendedores_mismo_estado = vector_por_id02(
    vtab, "todos_vendedores_mismo_estado", dev$order_id),
  stringsAsFactors = FALSE, check.names = FALSE)
if (nrow(base) != nrow(dev) || anyDuplicated(base$order_id)) {
  stop("Se altero el numero de pedidos durante las uniones.", call. = FALSE)
}
if (any(!is.na(base$n_items) & base$n_items < 1)) stop("n_items invalido.", call. = FALSE)
if (any(!is.na(base$n_vendedores) & base$n_vendedores > base$n_items)) {
  stop("Se encontraron mas vendedores que articulos en algun pedido.", call. = FALSE)
}
# Importante: no anadir el estado final, despacho, entrega o fecha prometida
# a esta tabla. Ninguna variable de respuesta forma parte de las candidatas.
utils::write.csv(base, file.path(out02, "01_variables_candidatas_desarrollo.csv"),
                 row.names = FALSE, na = "")

message("[4/6] Comprobando madurez y censura temporal...")
maduro <- ap <= cut - 45 * 86400
marcas <- data.frame(
  periodo = c("Desarrollo aprobado", "Etiqueta valida al corte",
              "Aprobados con al menos 45 dias al corte",
              "Madurez 45 dias Y etiqueta valida", "Madurez 45 dias SIN etiqueta valida",
              "Etiqueta valida con menos de 45 dias"),
  n = c(nrow(dev), sum(ok), sum(maduro), sum(maduro & ok),
        sum(maduro & !ok), sum(!maduro & ok)),
  stringsAsFactors = FALSE)
utils::write.csv(marcas, file.path(out02, "02_madurez_y_etiquetas.csv"),
                 row.names = FALSE)
if (sum(maduro & ok) != 75172L) {
  stop("El conteo de 45 dias difiere del esperado; revisar limites de fecha.", call. = FALSE)
}
# Guardar solo IDs y etiquetas historicas para referencia; el entrenamiento
# FUTURO DEBE RECONSTRUIR LAS ETIQUETAS EN EL CORTE DE CADA FOLD.
y45 <- data.frame(order_id = dev$order_id[maduro & ok],
                  dias_entrega = as.numeric(difftime(
                    ent[maduro & ok], ap[maduro & ok], units = "days")),
                  stringsAsFactors = FALSE)
utils::write.csv(y45, file.path(out02, "03_etiquetas_maduras_45d_referencia.csv"),
                 row.names = FALSE)

message("[5/6] Resumiendo faltantes y relacion geografica (descriptivo)...")
nom <- setdiff(names(base), c("order_id", "fecha_aprobacion"))
calidad <- data.frame(
  variable = nom,
  n_faltante = vapply(base[nom], function(x) sum(is.na(x)), integer(1)),
  porcentaje_faltante = round(100 * vapply(base[nom],
                                          function(x) mean(is.na(x)), numeric(1)), 3),
  stringsAsFactors = FALSE)
calidad$tipo_variable <- vapply(base[nom], function(x) {
  if (is.numeric(x) || is.integer(x)) "numerica" else "categorica"
}, character(1))
calidad$nota_tratamiento <- vapply(nom, function(nombre) {
  if (nombre %in% c("n_items", "n_vendedores", "precio_total_brl",
                    "flete_total_brl")) {
    "Si faltan articulos, ausencia estructural: NO imputar media sin analizar origen"
  } else if (nombre %in% c("peso_total_g", "volumen_total_cm3")) {
    "Media aritmetica posible solo tras justificarla y calcularla en train de cada fold"
  } else if (nombre %in% c("estado_cliente", "todos_vendedores_mismo_estado")) {
    "Variable categorica o binaria: NO aplicar media como categoria"
  } else {
    "No requiere imputacion si esta completa; si falta, revisar origen"
  }
}, character(1))
utils::write.csv(calidad, file.path(out02, "04_faltantes_por_variable.csv"),
                 row.names = FALSE, na = "")
indices <- which(maduro & ok & !is.na(base$todos_vendedores_mismo_estado))
if (!length(indices)) stop("No hay muestra suficiente para resumen geografico.", call. = FALSE)
geo <- do.call(rbind, lapply(c(1L, 0L), function(g) {
  ix <- indices[base$todos_vendedores_mismo_estado[indices] == g]
  yy <- as.numeric(difftime(ent[ix], ap[ix], units = "days"))
  data.frame(
    grupo = if (g == 1L) "Todos los vendedores en estado del cliente" else
                         "Al menos un vendedor en otro estado",
    n = length(yy), media_dias = mean(yy), mediana_dias = stats::median(yy),
    p90_dias = as.numeric(stats::quantile(yy, 0.9, names = FALSE)),
    stringsAsFactors = FALSE)
}))
utils::write.csv(geo, file.path(out02, "05_resumen_geografico_descriptivo.csv"),
                 row.names = FALSE)

inventario <- data.frame(
  candidato = nom,
  condicion = ifelse(nom %in% c("hora_aprobacion", "dia_semana", "mes_aprobacion",
                               "horas_desde_compra"), "DERIVABLE AL APROBAR",
                     "DISPONIBILIDAD HISTORICA POR VERIFICAR"),
  observacion = ifelse(nom %in% c("peso_total_g", "volumen_total_cm3"),
                        "Metadatos del catalogo sin vigencia historica demostrada",
                ifelse(nom %in% c("todos_vendedores_mismo_estado", "estado_cliente"),
                       "Ubicacion del vendedor/cliente: vigencia historica no demostrada",
                ifelse(nom %in% c("precio_total_brl", "flete_total_brl", "n_items", "n_vendedores"),
                       "Detalle del pedido presumiblemente conocido al aprobar; verificar",
                       "Fecha de compra/aprobacion documentada"))),
  stringsAsFactors = FALSE)
utils::write.csv(inventario, file.path(out02, "06_disponibilidad_candidatas.csv"),
                 row.names = FALSE)

# TARDANZA: diagnostico descriptivo SEPARADO de la duracion de entrega.
# Promesa Olist es una FECHA CALENDARIO: llegar durante ese dia NO es tarde.
# Se usan solo entregas conocidas y maduras antes del corte de desarrollo.
# No se agrega esta medida a la tabla de variables candidatas ni se usa
# para entrenar, porque depende de la fecha real de entrega (dato futuro).
promesa02 <- fecha02(dev$order_estimated_delivery_date)
if (any(!is.na(dev$order_estimated_delivery_date) & is.na(promesa02))) {
  stop("Hay fechas prometidas mal formadas.", call. = FALSE)
}
ix_promesa <- which(maduro & ok & !is.na(promesa02))
if (!length(ix_promesa)) stop("No hay fechas prometidas validas para describir tardanza.", call. = FALSE)
fecha_llegada02 <- as.Date(ent[ix_promesa], tz = "UTC")
fecha_prometida02 <- as.Date(promesa02[ix_promesa], tz = "UTC")
exceso02 <- as.integer(fecha_llegada02 - fecha_prometida02)
tardanza02 <- pmax(0L, exceso02)
num_tardios02 <- sum(tardanza02 > 0L)
tab_tardanza02 <- data.frame(
  indicador = c("pedidos_maduros_entregados_con_promesa",
                "pedidos_que_llegaron_despues_de_fecha_prometida",
                "porcentaje_de_pedidos_con_tardanza",
                "tardanza_media_dias_todos_los_pedidos",
                "tardanza_media_dias_solo_pedidos_tardios"),
  valor = c(length(tardanza02), num_tardios02,
            round(100 * mean(tardanza02 > 0L), 3),
            round(mean(tardanza02), 4),
            if (num_tardios02) round(mean(tardanza02[tardanza02 > 0L]), 4) else NA_real_),
  unidad = c("pedidos", "pedidos", "porcentaje", "dias", "dias"),
  stringsAsFactors = FALSE)
utils::write.csv(tab_tardanza02,
                 file.path(out02, "08_tardanza_descriptiva_separada.csv"),
                 row.names = FALSE, na = "")

message("[6/6] Registrando limites y pruebas...")
nota <- c(
  "GRUPO 3 / ETAPA 2 - VARIABLES CANDIDATAS, NO MODELOS",
  paste("R:", R.version.string),
  paste("MD5 dataset.zip:", as.character(tools::md5sum(zip02))),
  paste("Pedidos de desarrollo:", nrow(dev)),
  paste("Articulos de desarrollo:", nrow(art)),
  paste("Sin articulos:", sum(is.na(base$n_items))),
  paste("Con etiqueta madura 45 dias:", sum(maduro & ok)),
  "Muestreo de desarrollo en junio esta censurado: NO interpretar su mediana como mejora operativa.",
  "Tiempo de respuesta (duracion) = dias entre aprobacion y entrega observada.",
  "Tardanza = max(0, fecha REAL calendario - fecha PROMETIDA calendario), en dias.",
  "Tardanza es diagnostico descriptivo SEPARADO, NO es variable predictora.",
  "Se cuantifican faltantes pero NO se imputan. Una media se calcula solo con el train del fold.",
  "No imputar fechas de entrega ni rellenar cero cuando faltan articulos sin justificarlo.",
  "Regla de 45 dias reduce censura por recencia, pero NO elimina la seleccion de entregados.",
  "Los metadatos de seller y products carecen de historial de vigencia por pedido.",
  "La coincidencia geografica se define para TODOS los vendedores por pedido; no se elige al primero.",
  "Ninguna imputacion, codificacion estadistica ni seleccion se aplico globalmente.",
  "No se evaluo test. No se entrenaron ni optimizaron modelos.",
  "Proximo paso: acordar variables admisibles y construir folds temporales con transformaciones solo en train.",
  "El test historico julio-agosto ya es conocido del proyecto antiguo; requiere nueva evidencia independiente."
)
writeLines(nota, file.path(out02, "07_notas_y_controles.txt"), useBytes = TRUE)
cat("\n", strrep("=", 65), "\n", sep = "")
cat("ETAPA 2 COMPLETADA - CANDIDATAS CONSTRUIDAS; SIN MODELOS NI TEST\n")
cat("Pedidos de desarrollo:", nrow(base), "\n")
cat("Etiquetas maduras 45 dias:", sum(maduro & ok), "\n")
cat("Variables candidatas:", length(nom), "\n")
cat("Pedidos sin articulos:", sum(is.na(base$n_items)), "\n")
cat("Promesas evaluadas solo en desarrollo maduro:", length(tardanza02), "\n")
cat("Porcentaje con tardanza:", round(100 * mean(tardanza02 > 0), 2), "%\n")
cat("Archivos:", out02, "\n")
cat(strrep("=", 65), "\n", sep = "")
