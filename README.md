# Predicción del tiempo de entrega de pedidos de Olist

**Estadística Aplicada · Ingeniería Industrial · Universidad de Santiago de Chile**
Caso de estudio 1: Regresión · Grupo 3 · Profesor: Luis Rojo-González, Ph.D. · 2026

Integrantes: Javier Reyes, Martina Jiménez, Francisco Droguett y Nicole Villarroel.

## Qué hace este proyecto

Predice, en el momento en que se **aprueba** un pedido de comercio electrónico, cuántos **días** tardará en llegar al cliente. El objetivo es apoyar la planificación y el seguimiento logístico. La respuesta es continua: días entre la aprobación y la entrega efectiva.

- **Datos:** *Brazilian E-Commerce Public Dataset by Olist* (Kaggle), 99.441 pedidos en 9 tablas.
- **Desarrollo:** pedidos aprobados entre enero de 2017 y junio de 2018. De ellos, 81.950 tienen una entrega observable al 1 de julio de 2018, y 75.172 cumplen además 45 días de madurez.
- **Predictores:** 10 variables conocidas al aprobar: calendario, demora compra–aprobación, estado del cliente, artículos, vendedores, precio, flete y si todos los vendedores están en el estado del cliente.
- **Validación:** temporal, con 4 meses validados (diciembre 2017 a marzo 2018, 26.279 pedidos). Los hiperparámetros se eligen con validación interna anidada. Todo el preprocesamiento se aprende solo con el entrenamiento.
- **Modelos comparados:** baseline (media), OLS, Ridge, Lasso, Elastic-net, splines naturales, árbol (CART), Random Forest, Gradient Boosting y un promedio 50/50 de Splines y Boosting.

## Resultado principal (validación temporal, promedio de 4 meses)

| Modelo | RMSE (días) | MAE (días) | R² | vs baseline |
|---|---|---|---|---|
| Baseline (media del entrenamiento) | 11,341 | 7,654 | −0,086 | — |
| OLS | 9,926 | 6,414 | 0,167 | −12,5 % |
| **Splines naturales (modelo elegido)** | **9,902** | **6,375** | **0,171** | **−12,7 %** |
| Gradient Boosting | 9,983 | 6,399 | 0,157 | −12,0 % |
| Random Forest | 10,213 | 6,500 | 0,120 | −9,9 % |

La tabla completa de las 10 alternativas está en `resultados/10_decision_modelo/08_comparacion_10_alternativas.csv`.

El R² es bajo porque la variable es muy asimétrica. La correlación de Spearman entre lo real y lo predicho es 0,60, es decir, el modelo ordena bien los pedidos. El modelo final (splines `ns_df5_geo1`) se ajustó una sola vez con 75.172 pedidos y está guardado en `resultados/11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds`.

**Limitaciones:**
- No hay un test independiente nuevo: el período julio–agosto de 2018 se examinó en una versión anterior del proyecto.
- Solo se observan pedidos entregados.
- El modelo subestima las entregas largas y falla más cuando hay vendedores en otro estado.

El detalle está en `documentacion/02_METODOLOGIA_Y_LIMITACIONES.md`.

## Estructura del repositorio

| Carpeta / archivo | Contenido |
|---|---|
| `scripts/` | Código R, por etapas `00` a `16` (más `01A` y `04B`, que generan gráficos) |
| `datos_originales/dataset.zip` | Los 9 CSV originales de Olist, sin modificar |
| `resultados/` | Tablas CSV, certificados de control y el modelo final (`.rds`) de cada etapa |
| `graficos/` | Figuras de cada etapa (PNG y PDF) |
| `documentacion/` | Guía de scripts, metodología, guía de evidencia, manifiesto SHA-256 y checklist |
| `evidencia_reproduccion/`, `evidencia_complementos/` | Registros de las reproducciones aisladas |
| `README_REPRODUCIBILIDAD.md` | Instrucciones detalladas de reproducción |

## Cómo reproducirlo

**Requisitos:**
- R 4.6.2 (probado en Windows), con `Rscript` disponible.
- Paquetes: `glmnet`, `rpart`, `ranger`, `gbm`, `lmtest` y `car`, además de `stats` y `splines`, que vienen con R.

```r
install.packages(c("glmnet", "rpart", "ranger", "gbm", "lmtest", "car"))
```

**Pasos**, con la carpeta del repositorio como directorio de trabajo, en una sesión limpia de R:

```r
# 1. (opcional) Verificar el entorno: paquetes, datos y Rscript
source("scripts/00_verificar_entorno.R", encoding = "UTF-8")

# 2. Reproducción completa: copia el proyecto a una carpeta hermana, ejecuta todas
#    las etapas con Rscript --vanilla y compara 45 tablas con las archivadas
source("scripts/12_reproduccion_integral_aislada.R", encoding = "UTF-8")
```

La última ejecución reprodujo **45 de 45 tablas**, abrió el modelo en una sesión nueva y confirmó que el proyecto original no cambió. El registro está en `evidencia_reproduccion/2026-11-04_integral_45_tablas_R4.6.2/`. Hay un modo rápido que reproduce solo las etapas 13 a 16, y está descrito en `README_REPRODUCIBILIDAD.md`.

**Usar el modelo final** con nuevos pedidos. El `data.frame` debe tener las 10 columnas de la especificación B:

```r
modelo <- readRDS("resultados/11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds")
predicciones <- modelo$predecir(nuevos_pedidos)
```

## Integridad de los archivos

`documentacion/04_MANIFIESTO_ARCHIVOS_SHA256.csv` lista la huella SHA-256 de los 281 archivos del proyecto. No incluye este `README.md`, `.gitignore` ni `.gitattributes`, que son archivos del repositorio. El archivo `.gitattributes` evita que Git cambie los finales de línea, para que las huellas coincidan después de clonar.

## Datos y licencia

Los datos son de Olist, *Brazilian E-Commerce Public Dataset by Olist*, publicados en Kaggle (<https://www.kaggle.com/datasets/olistbr/brazilian-ecommerce>) con licencia **CC BY-NC-SA 4.0**. Se redistribuyen sin modificar, con atribución y para un uso académico no comercial.
