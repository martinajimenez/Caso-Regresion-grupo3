# Grupo 3 · Estadística Aplicada · Caso 1 — Olist

## Instrucciones de reproducibilidad del laboratorio

**Estado del paquete:** se conservan los diez métodos, las predicciones, la tabla principal y el candidato originales. Se añaden las etapas **13 (diagnóstico)**, **14 (interpretación e intervalos de desarrollo)**, **15 (evaluación no lineal y supuestos)** y **16 (sensibilidad con `log(días)`)**; las dos últimas responden a la retroalimentación docente. **La cadena integral ampliada se reprodujo el 4 de noviembre de 2026 con R 4.6.2 en Windows: 45/45 tablas, RDS legible en sesión nueva y proyecto original intacto** (`evidencia_reproduccion/2026-11-04_integral_45_tablas_R4.6.2/`). La evidencia histórica de 15/15 tablas del 26 de octubre de 2026 se conserva en `evidencia_reproduccion/`. **No se obtuvo un test nuevo e independiente.**

### 0. Nuevos análisis: ejecución breve

Las tablas y figuras nuevas ya se incluyen. Para regenerarlas desde los resultados archivados, sin volver a entrenar los modelos de las etapas 01–11:

```r
source("scripts/13_diagnostico_complementario.R", encoding = "UTF-8")
source("scripts/14_interpretacion_e_incertidumbre.R", encoding = "UTF-8")
source("scripts/15_evaluacion_no_lineal_y_supuestos.R", encoding = "UTF-8")
source("scripts/16_sensibilidad_log_respuesta.R", encoding = "UTF-8")
```

**Etapa 15 (respuesta a la orientación docente sobre R² y supuestos, con herramientas del curso).** No reentrena los modelos de comparación: lee las predicciones de validación archivadas. Produce en `resultados/15_evaluacion_no_lineal_y_supuestos/`:
- R², Spearman (`cor(method = "spearman")`), RMSE, MAE y recta de calibración real ~ predicho, por fold y modelo (`01`, `02`).
- Diferencias pareadas por fold frente al baseline, con SE descriptivo (`03`), y error por tramo de predicción (`04`).
- Supuestos con la receta del curso (`supuestos1.R`): el OLS B y el Splines B se reajustan con `lm()` en el train del fold 4, comprobando que reproducen lo archivado, y se evalúan con `shapiro.test()` (submuestra fija de 5.000, límite de la función), `lmtest::bptest()`, `lmtest::dwtest()` y `car::vif()` con el estado del cliente como factor (GVIF) (`05`, `06`).
- `08_INTERPRETACION_PARA_INFORME.txt`: texto listo para el informe y la presentación.

Figuras en `graficos/15_evaluacion_no_lineal_y_supuestos/`: real vs predicho con error por tramo, R² frente a Spearman, y `plot(modelo, which = 1:4)` del OLS B y del Splines B.

**Etapa 16 (análisis de sensibilidad: `lm(log(días) ~ ...)`, como propone el curso ante residuos no normales).** Reajusta solo el OLS B sobre `log(días)` en los mismos 4 folds, con la misma receta aprendida solo en train, y vuelve a días con `exp()`, que estima la mediana condicional. Comprueba antes que el OLS en días reproduce exactamente lo archivado en la etapa 04. Repite el diagnóstico del curso antes y después. **No modifica el candidato congelado de la etapa 11.** Salidas en `resultados/16_sensibilidad_log_respuesta/` (métricas, diferencias por fold, supuestos y `06_INTERPRETACION_PARA_INFORME.txt`) y `graficos/16_sensibilidad_log_respuesta/`.

Las etapas 15 y 16 requieren además los paquetes `lmtest` y `car` (`install.packages(c("lmtest", "car"))`).

Para comprobar su reproducción en otro proceso y carpeta, con comparación de **30 CSV y 16 figuras regeneradas** (etapas 13 a 16):

```r
options(olist.solo_complementos = TRUE)
source("scripts/12_reproduccion_integral_aislada.R", encoding = "UTF-8")
options(olist.solo_complementos = FALSE)
```

Este modo requiere únicamente R y sus paquetes incluidos `stats` y `splines`; utiliza el RDS original. El script 14 reconstruye un Splines del fold 2 y lo mantiene fijo para un experimento retrospectivo: entrenamiento antes de enero de 2018, calibración enero con etiquetas conocidas antes de marzo, evaluación marzo. **Los intervalos de ese experimento no corresponden automáticamente al candidato final de la etapa 11.** Los PDP/ICE sí describen al candidato final, y se identifican como tales. Lea `documentacion/06_GUIA_COMPLEMENTOS_Y_RESULTADOS.md` antes de presentar estas figuras.

### 1. Objetivo y pregunta

Se busca predecir la **duración real de entrega**, medida en días entre la aprobación y la entrega efectiva de pedidos de comercio electrónico Olist. No debe confundirse con la **tardanza**, que es el exceso respecto de la fecha de entrega prometida. El análisis predictivo principal se refiere a pedidos efectivamente entregados, con etiqueta observable bajo cortes temporales de desarrollo, y no a todos los pedidos aprobados.

### 2. Requisitos antes de ejecutar

- R; versión utilizada en la última reproducción aprobada: **4.6.2 (Windows)**. La histórica de 15/15 usó 4.6.1.
- Paquetes `stats` y `splines`: incluidos normalmente con R.
- Paquetes adicionales: `glmnet`, `ranger` y `gbm`; comprobar también `rpart`.
- Versiones de la última ejecución aprobada: `glmnet` **5.2**, `rpart` **4.1.27**, `ranger` **0.18.0**, `gbm` **2.3.1** (la histórica usó `glmnet` 5.1, con los mismos resultados dentro de la tolerancia). `00_verificar_entorno.R` advierte si las versiones difieren de 4.6.1 / 5.1; no es un error.
- RStudio es útil, pero el procedimiento utiliza R base y `Rscript.exe`. No se necesita instalar `ggplot2` para estos gráficos.
- Espacio suficiente para una copia aislada del dataset y resultados. Es preferible extraer el ZIP en una ruta corta, fuera de carpetas sincronizadas o protegidas.

Instalar los paquetes faltantes **una sola vez y de forma explícita**, si el equipo tiene conexión a CRAN:

```r
install.packages(c("glmnet", "ranger", "gbm", "lmtest", "car"))
# Solo si la comprobación informa que falta:
install.packages("rpart")
```

La instalación puede requerir compatibilidad de versiones y herramientas de compilación según el sistema operativo. Este proyecto **no instala paquetes automáticamente**. Las versiones diferentes deben registrarse y sus resultados contrastarse; no se garantiza identidad binaria entre sistemas operativos.

### 3. Abrir el proyecto y comprobar el entorno

1. Descomprimir el paquete completo sin modificar su jerarquía.
2. Abrir RStudio y reiniciar R (**Session → Restart R**).
3. Establecer como directorio de trabajo la carpeta **`Grupo3_Olist_Laboratorio`** (no la subcarpeta `scripts`). En RStudio puede hacerse desde **Files → More → Set As Working Directory**.
4. Ejecutar el comprobador de solo lectura:

```r
source("scripts/00_verificar_entorno.R", encoding = "UTF-8")
```

Debe identificar las nueve fuentes CSV dentro de `datos_originales/dataset.zip`, los paquetes y un ejecutable `Rscript`.

### 4. Reproducción integral recomendada (una sola instrucción)

En la **raíz del proyecto**, ejecutar:

```r
source("scripts/12_reproduccion_integral_aislada.R", encoding = "UTF-8")
```

Con `olist.solo_complementos = FALSE` (valor predeterminado), este lanzador crea una **carpeta hermana** `Grupo3_Olist_Laboratorio__REPRODUCCION_AAAAMMDD_HHMMSS`, copia entradas originales y ejecuta las etapas 01–11 y después 13–16 en una sesión independiente (unos 18 minutos en el equipo de prueba). Los modelos se entrenan **solo sobre datos de desarrollo**, sin consultar como nuevo test el período julio–agosto de 2018, ya utilizado durante la versión anterior del estudio.

El proceso puede seguir trabajando mientras la consola principal muestra pocos mensajes: consultar dentro de la carpeta hermana `12_control/02_ejecucion_stdout.log` y `12_control/03_ejecucion_stderr.log`. **No interrumpir ni volver a lanzar simultáneamente el procedimiento.** Si el proceso termina con un error, consultar los archivos de registro; no dar la prueba por aprobada.

**Criterio de aprobación:** revisar en la carpeta nueva `12_control/09_RESULTADO_REPRODUCIBILIDAD.txt`:

```text
ESTADO: APROBADO
Comparaciones CSV: 45 / 45
Lectura RDS en segunda sesion: APROBADA
Archivos del proyecto original intactos (MD5): TRUE
```

Se verifica además la generación de 100 predicciones finitas en una segunda sesión R. La comparación numérica admite diferencias de redondeo controladas; no exige que el archivo `.rds` sea idéntico byte a byte en otros sistemas. **El resultado 45/45 ya fue certificado** el 4 de noviembre de 2026 (`evidencia_reproduccion/2026-11-04_integral_45_tablas_R4.6.2/09_RESULTADO_REPRODUCIBILIDAD.txt`). El acta anterior de 15/15 permanece en `evidencia_reproduccion/`; la reproducción de las etapas 13–16 (30/30 tablas y 16/16 figuras) está en `evidencia_complementos/2026-11-04_etapas13-16_30_tablas_R4.6.2/`.

### 5. Orden que utiliza internamente la Etapa 12

`01 → 02 → 03 → 03 (modo correlaciones) → 04 (OLS histórico) → 05 → 04 (modo selección de variables) → 06 → 07 (incluye Averaging 50/50) → 08 → 09 → 10 (10 alternativas) → 11 → 13 → 14 → 15 → 16`.

**Importante:** `04` tiene dos modos. El de selección necesita previamente los folds internos de `05`; por eso el orden no es estrictamente numérico. `01A` y `04B` **son dependencias activas** de los scripts `01` y `04`; **no deben eliminarse**. `00_verificar_entorno.R` es una utilidad adicional de solo lectura y no forma parte de los 12 cálculos cotejados.

No se recomienda ejecutar manualmente toda la cadena sobre este paquete ya certificado: ciertos scripts protegen resultados congelados e impiden sobrescribirlos. El método documentado para reconstruirlos es el lanzador aislado de la Etapa 12.

### 6. Resultados estadísticos que pueden citarse

Los cuatro folds de validación temporal de desarrollo compararon nueve alternativas originales bajo una especificación común B, con diez variables originales. Se añadió una décima combinación complementaria de Splines y Boosting mediante pesos fijos de 50 % y 50 %. Esta elección de componentes se documenta retrospectivamente y no representa evaluación independiente adicional. Datos de referencia, **no de test independiente nuevo**:

| Modelo | RMSE promedio (días) | R² promedio |
|---|---:|---:|
| Averaging 50 % Splines + 50 % Boosting (complementario) | 9,9009 | 0,1714 aprox. |
| Splines naturales | 9,9024 | 0,1709 |
| Ridge | 9,9219 | 0,1677 |
| Lasso | 9,9231 | 0,1675 |
| Elastic-Net | 9,9235 | 0,1674 |
| OLS B | 9,9257 | 0,1671 |
| Gradient Boosting | 9,9833 | 0,1574 |
| Árbol de regresión | 10,1407 | 0,1296 |
| Random Forest | 10,2133 | 0,1195 |
| Baseline media entrenamiento | 11,3410 | −0,0857 |

Splines tenía el menor RMSE entre las nueve alternativas originales; el averaging obtiene un RMSE descriptivo ligeramente menor (diferencia aproximada de 0,0016 días). No se modifica automáticamente el candidato Splines congelado porque la selección retrospectiva de componentes no establece superioridad independiente. La ventaja de Splines frente a OLS B es de solo **0,0232 días de RMSE** y no demuestra superioridad estadística. Su R² promedio es modesto. El candidato Splines almacenado fue ajustado con **75.172** pedidos maduros de desarrollo, configuración `ns_df5_geo1`. El error **8,5892 días** calculado sobre su entrenamiento final **no es un RMSE de test**.

### 7. Limitaciones que deben declararse en informe y exposición

- La fecha de evaluación de julio–agosto de 2018 fue conocida en el proyecto anterior: **no es un test virgen del nuevo candidato**.
- La selección y comparación de modelos se desarrollaron con folds de desarrollo observados reiteradamente: existe riesgo de sobreajuste del propio proceso de investigación.
- Algunas características logísticas extraídas de las tablas Olist requieren justificar si estaban realmente disponibles al aprobar cada pedido. El proyecto describe parcialmente ese riesgo de disponibilidad temporal.
- Las correlaciones no implican causalidad ni determinan por sí mismas qué variables deben utilizarse. La reducción de variables se estudió en validación interna, donde se conservó la especificación B de 10 variables en cuatro folds.
- Los pedidos entregados muy tarde concentran una fracción importante del error. No fueron eliminados arbitrariamente para mejorar las métricas.
- La inferencia OLS de la Etapa 09 usa intervalos robustos HC3, α = 0,05 y datos de entrenamiento. No debe equipararse con la evaluación predictiva de Splines.
- La reproducción integral original en un mismo PC no prueba portabilidad completa. Los complementos 13–14 se verifican por separado en R sobre Linux, leyendo el RDS original. La cadena integral ampliada (45 tablas) se comprobó el 4 de noviembre de 2026 en Windows con R 4.6.2. Una evaluación final verdaderamente independiente necesita un protocolo acordado con el profesor.
- Los residuos del OLS y del Splines no son normales ni de varianza constante (`shapiro.test` y `lmtest::bptest`, etapa 15). Con `lm(log(días) ~ ...)` la normalidad mejora mucho (W de Shapiro de 0,65 a 0,99), pero la heterocedasticidad no se corrige y, al volver a días con `exp()`, el RMSE sube (9,93 → 10,41) aunque baja la mediana del error absoluto (4,37 → 3,91) (etapa 16). Por eso se conserva el modelo en días y la versión en log se reporta como sensibilidad.
- El experimento de intervalos no alcanzó sus coberturas objetivo en marzo: Splines obtuvo aproximadamente 70,7 % para un objetivo 80 % y 79,4 % para un objetivo 90 %. Sus anchuras medias fueron 14,5 y 19,5 días. Es una limitación comprobada; no una promesa de cobertura futura.

### 8. Mapa de carpetas

- `datos_originales/dataset.zip`: nueve CSV Olist originales, sin transformaciones manuales.
- `scripts/`: códigos de etapas 01–16, scripts auxiliares `01A`/`04B`, y verificador `00`.
- `resultados/`: CSV, certificados, actas y `11_modelo_candidato/MODELO_CANDIDATO_SPLINES_B.rds`.
- `graficos/`: gráficos generados con R base, asociados a cada etapa.
- `documentacion/`: guía de scripts, límites metodológicos y manifiesto de archivos.
- `evidencia_reproduccion/`: certificado y registros de la reproducción aislada aprobada en el equipo de desarrollo.
- `resultados/13_diagnostico_complementario/` y `graficos/13_diagnostico_complementario/`: errores, correlaciones complementarias, calibración agrupada, distribuciones y sensibilidad interna RMSE/MAE.
- `resultados/14_interpretacion_e_incertidumbre/` y `graficos/14_interpretacion_e_incertidumbre/`: experimento temporal de intervalos, importancia por grupos y PDP/ICE.
- `evidencia_complementos/`: controles de las nuevas etapas, reconstrucción de 18 tablas en proceso limpio y validación independiente en Python.
- `resultados/15_evaluacion_no_lineal_y_supuestos/` y `graficos/15_evaluacion_no_lineal_y_supuestos/`: R² frente a Spearman, diferencias pareadas frente al baseline, error por tramo, supuestos con la receta del curso y texto sobre por qué el R² no basta.
- `resultados/16_sensibilidad_log_respuesta/` y `graficos/16_sensibilidad_log_respuesta/`: sensibilidad con respuesta `log(días)`.

**El contenido de `documentacion/` y `evidencia_reproduccion/` no es una fuente alternativa de métricas.** La fuente computacional es `resultados/`, y la reproducción se comprueba ejecutando `scripts/12_reproduccion_integral_aislada.R`.

### 9. Averaging: salidas incorporadas en esta versión final

Los scripts `07`, `08`, `10` y `12` incluidos aquí ya contienen Averaging; **no es necesario instalar parches**. Para reproducir desde cero utiliza `scripts/12_reproduccion_integral_aislada.R` con `olist.solo_complementos = FALSE`. Para regenerar y comprobar solo los nuevos análisis usa el modo breve de la sección 0. No ejecutes directamente el script 11 sobre la entrega, porque protege el modelo congelado.

Salidas adicionales: `resultados/07_ensembles/12_averaging_predicciones.csv`, `13_averaging_metricas_por_fold.csv`, `14_averaging_resumen.csv`, el certificado `16_CONTROL_AVERAGING_APROBADO.txt`, diagnóstico adicional de etapa 08 y tabla de diez modelos de etapa 10. La prueba integral histórica **aprobó 15 comparaciones CSV**, incluida la combinación. La ampliación incluye otras 30 tablas de las etapas 13–16 (45 en total para el modo integral). Las tablas históricas de nueve alternativas siguen disponibles como referencia.

**Límite:** el test de julio-agosto de 2018 ya se examinó y no debe usarse nuevamente para seleccionar modelos o afirmar desempeño independiente.

### 10. Archivos que se entregan

Se entrega **una sola raíz de proyecto**: `Grupo3_Olist_Laboratorio/`. Los ficheros `01A` y `04B` no son versiones antiguas para borrar: los scripts principales los necesitan. La carpeta `evidencia_reproduccion/` contiene el resultado histórico 15/15 y, en su subcarpeta fechada más reciente, el 45/45 actual con sus registros, no una segunda copia del modelo. El archivo `documentacion/04_MANIFIESTO_ARCHIVOS_SHA256.csv` lista ruta, tamaño y SHA256 de todos los archivos distribuidos (regenerado el 4 de noviembre de 2026, incluye las etapas 15–16). Excluye el propio manifiesto y los archivos de sesión de RStudio (`.Rhistory`, `.Rproj.user/`), que cambian al abrir el proyecto. Si se modifica cualquier archivo, el manifiesto debe regenerarse.

Se debe mencionar en el informe la atribución al dataset Olist Brazilian E-Commerce y no afirmar que el modelo final ha sido validado con una muestra nueva e independiente.
