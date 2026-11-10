# Guía de scripts, entradas y salidas

| Archivo | Función principal | Resultado o control | Dependencias |
|---|---|---|---|
| `00_verificar_entorno.R` | Verifica paquetes, dataset, rutas y `Rscript` sin modificar archivos | Mensajes en consola | R base |
| `01_importacion_y_revision_datos.R` | Carga nueve CSV, define desarrollo, registra etiquetas y describe duraciones | `resultados/01_revision_datos/` | `01A` |
| `01A_estilo_graficos_etapa1.R` | Dibuja y da formato a figuras de Etapa 1 | `graficos/01_revision_datos/` | R base; requerido por `01` |
| `02_integracion_y_variables_candidatas.R` | Construye predictores de pedido y geografía, revisa faltantes y tardanza descriptiva | `resultados/02_variables_candidatas/` | Etapa 1 |
| `03_protocolo_variables_y_folds.R` | Define especificaciones A/B/C y cuatro particiones temporales; modo opcional de correlaciones | `resultados/03_protocolo_y_folds/`, `graficos/03_correlaciones/` | Etapas 1–2 |
| `04_baseline_y_ols_temporal.R` | Dos modos: Baseline/OLS, y selección interna de variables (este segundo después del script 05) | `resultados/04_baseline_y_ols/`, `resultados/04_seleccion_variables_exploratoria/` | Etapa 3, `04B` y, para selección, 05 |
| `04B_regenerar_graficos_datacamp.R` | Genera gráficos de Baseline/OLS con tablas archivadas | `graficos/04_baseline_y_ols/` | Requerido por `04` |
| `05_regularizacion_anidada.R` | Ridge, Lasso y Elastic-Net; hiperparámetros con validación temporal interna | `resultados/05_regularizacion/` | `glmnet`, Etapa 4 |
| `06_splines_validacion_anidada.R` | Splines naturales sobre las mismas 10 variables B | `resultados/06_splines/` | `splines`, Etapa 5 |
| `07_arbol_rf_boosting_anidados.R` | CART, Random Forest, Gradient Boosting y Averaging fijo (Splines + Boosting) | `resultados/07_ensembles/` | `rpart`, `ranger`, `gbm` |
| `08_diagnostico_errores_y_defensa.R` | Diagnósticos de sesgo, entregas extremas, medias reales/predichas y Averaging | `resultados/08_diagnostico_errores/` | Etapas 4–7 |
| `09_inferencia_y_diagnostico_ols.R` | OLS B fold 4; IC HC3 95 %, α=0,05, VIF, residuos, Q–Q | `resultados/09_inferencia_ols/` | Etapa 4 |
| `10_decision_modelo_y_trazabilidad.R` | Acta de selección de Splines entre 9 métodos originales y comparación retrospectiva de 10 alternativas con Averaging | `resultados/10_decision_modelo/` | Etapas 3–9 |
| `11_ajuste_candidato_splines_y_reproducibilidad.R` | Ajusta una vez Splines con datos maduros y guarda receta/RDS | `resultados/11_modelo_candidato/` | `splines`, Etapa 10 |
| `12_reproduccion_integral_aislada.R` | Modo integral: 01–11 + 13–14, 33 CSV, MD5 y RDS. Modo complementos: 13–14, 18 CSV y 10 figuras | carpeta hermana `__REPRODUCCION_.../12_control/` o `__COMPLEMENTOS_.../12_control_complementos/` | Modo integral: todos; complementos: resultados archivados y RDS |
| `13_diagnostico_complementario.R` | Real–predicho y residuos de diez alternativas, calibración agrupada, métricas complementarias, distribuciones y sensibilidad interna | `resultados/13_diagnostico_complementario/`, cinco figuras | R base, resultados 01–11 |
| `14_interpretacion_e_incertidumbre.R` | Experimento temporal de intervalos, importancia por grupos y PDP/ICE del candidato final | `resultados/14_interpretacion_e_incertidumbre/`, cinco figuras | `stats`, `splines`, RDS original y resultados archivados |

**Para una reproducción completa de un proyecto entregado:** ejecutar solo `00` (opcional) y `12` desde su carpeta raíz, no lanzar individualmente los scripts contra los archivos de resultados ya certificados.

Para regenerar únicamente los complementos 13/14 se permite ejecutarlos directamente: sus salidas propias son regenerables y verifican integridad de las entradas históricas. La reproducción original de 15/15 CSV es evidencia histórica; la nueva reproducción breve coteja 18/18 CSV. La ejecución integral ampliada de 33 tablas está pendiente de verificar en el equipo de entrega. Véase `06_GUIA_COMPLEMENTOS_Y_RESULTADOS.md`.

## Interpretación para la defensa

- **RMSE y MAE:** errores expresados en días; un número menor es mejor al comparar en la misma población.
- **R² negativo en validación:** posible cuando una predicción rinde peor que la media real retrospectiva de ese fold; no equivale a un error de fórmula.
- **Sesgo negativo (`predicho - real`):** indica subestimación *media*, no que cada pedido haya sido subestimado.
- **Correlaciones:** análisis descriptivo de relaciones; no son medidas automáticas de importancia condicional o causal.
- **Significancia OLS:** intervalos al 95 % y α=0,05 no sustituyen métricas de predicción fuera de muestra.
- **Splines:** menor RMSE medio entre los nueve métodos originales y candidato entrenado/congelado; Averaging obtuvo RMSE descriptivo marginalmente menor al añadirse después, sin probar superioridad general.
