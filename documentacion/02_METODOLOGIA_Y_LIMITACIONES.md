# Registro de metodología y limitaciones — Grupo 3

## Fuente y variable respuesta

Olist Brazilian E-Commerce, nueve archivos CSV en `datos_originales/dataset.zip`. Duración = días entre fecha de aprobación y fecha efectiva de entrega. Tardanza es el exceso por sobre la fecha prometida (no la respuesta principal).

## Diseño temporal

Corte de desarrollo: **2018-07-01**. En 2017-01 a 2018-06 hay **86.162** pedidos aprobados; **81.950** tienen etiquetas de entrega observables al corte. Para ajuste final se exige una madurez de **45 días** antes del corte: **75.172** observaciones maduras.

Los cuatro folds temporales externos validan diciembre 2017 y enero, febrero, marzo 2018; el entrenamiento anterior a cada fold respeta observabilidad de la etiqueta. Modelos flexibles seleccionan hiperparámetros con ventanas internas. Las nueve alternativas originales de la comparación común utilizan las mismas 10 variables B y los mismos pedidos externos.

## Correlaciones y selección de variables

Matrices de Pearson y Spearman de variables numéricas: cálculo restringido al entrenamiento de cada fold. Categóricas analizadas por separado. La correlación individual no determina causalidad, importancia condicional ni eliminación automática. Seis candidatos OLS fueron contrastados internamente; se mantuvo B completa de 10 variables en los cuatro folds.

## Modelos y decisión

Modelos originales: Baseline, OLS B, Ridge, Lasso, Elastic-Net, Splines naturales, CART, Random Forest y Gradient Boosting. Se incorpora además averaging fijo 50/50 de Splines y Gradient Boosting, calculado solo a partir de predicciones externas ya archivadas y alineadas por `fold` y `order_id`. Los resúmenes por fold, las predicciones y los certificados están en `resultados/`. Splines obtuvo **RMSE promedio 9,902439 días** y **R² promedio 0,170852** en el desarrollo examinado, con mejora relativa en RMSE de **12,6843 %** frente al baseline de media histórica. Ridge: RMSE **9,921887**; OLS B **9,925667**. No afirmamos significancia de diferencias entre algoritmos a partir de diferencias descriptivas pequeñas.

Averaging tiene RMSE medio descriptivo aproximado **9,900887** días; su ventaja frente a Splines es solo **0,001552** días de RMSE y no certifica superioridad estadística, ya que los componentes se consideraron después de explorar el desarrollo. El candidato principal preexistente sigue siendo Splines.

Candidato ajustado: `ns_df5_geo1`, 75.172 pedidos maduros; RDS con receta. RMSE de entrenamiento final **8,589203** no representa test independiente.

## Diagnóstico e inferencia

La distribución presenta cola derecha y entregas extremas. El sesgo medio de Splines es **−2,2791** días calculado como predicción menos resultado real y promedio entre folds. Los diagnósticos por duración son descriptivos retrospectivos. La Etapa 09 reconstruye OLS B del cuarto fold con intervalos robustos HC3; α=0,05. Detectó heterocedasticidad y colas no normales; no se extrapola directamente una prueba t a Splines.

## Transparencia temporal y restricciones

El test histórico julio–agosto 2018 **fue examinado en la versión previa del proyecto**. El estudio revisado no debe reclamar que volver a calcular o leer ese test sea una prueba nueva. Las comparaciones y decisiones posteriores son retrospectivas sobre desarrollo ya explorado. Una validación externa nueva, genuinamente independiente, requerirá otro conjunto de datos o un acuerdo de diseño con el profesor. Además, ciertas variables de pedidos/vendedor están registradas en las tablas originales, pero su disponibilidad exacta en el instante de aprobación no está plenamente garantizada.

## Reproducibilidad demostrada

El **26-10-2026** se ejecutó una reconstrucción aislada en R 4.6.1 del mismo equipo: **15/15 tablas comparadas, RDS cargado y usado para 100 predicciones, archivos originales intactos**, con Averaging incluido. La evidencia de esa ejecución se entrega sin alterar. Los archivos de evidencia se incluyen, y el procedimiento puede repetirse con el script 12. Esto **no prueba todavía** portabilidad entre computadores ni valida de nuevo el test.

## Control de la combinación

La combinación se computa con pesos constantes 0,5 y 0,5, sin ajuste a respuestas del fold externo. Los scripts 07, 08 y 10 verifican que cada predicción de Splines y Boosting se refiere al mismo pedido, fold y respuesta, y recomputan RMSE, MAE, R² y sesgo. Los registros `12_*` a `16_*` de la Etapa 07 y los complementos de las Etapas 08 y 10 conservan la comparación original como evidencia. La Etapa 12 incorpora las tablas complementarias en sus controles de reproducción (15 comparaciones).
