# Complementos del mismo caso Olist

Esta ampliación conserva la respuesta (días desde aprobación hasta entrega), las diez alternativas, sus predicciones, sus métricas originales y el RDS del candidato. No cambia el informe ni la presentación. Añade evidencia para interpretar y defender el trabajo.

## Ejecución

Desde la raíz `Grupo3_Olist_Laboratorio`, con R y los paquetes incluidos `stats` y `splines`:

```r
source("scripts/13_diagnostico_complementario.R", encoding="UTF-8")
source("scripts/14_interpretacion_e_incertidumbre.R", encoding="UTF-8")
```

Los resultados de ambos scripts ya se incluyen. Para repetirlos en carpeta y proceso separados y cotejar 18 tablas:

```r
options(olist.solo_complementos=TRUE)
source("scripts/12_reproduccion_integral_aislada.R", encoding="UTF-8")
options(olist.solo_complementos=FALSE)
```

El modo integral predeterminado del script 12 también ejecuta 13/14 al final y espera 33 tablas comparables. La evidencia histórica de 15 tablas no certifica ese modo ampliado. Los resultados de verificación de esta actualización se registran en `evidencia_complementos/`.

## Etapa 13: qué añade

- Cinco figuras: real–predicho completo, acercamiento, residuos de diez alternativas, residuos OLS/Splines por fold y calibración por grupos definidos mediante la predicción.
- Mismos 26.279 pedidos por alternativa. Diagonal real=predicho; error firmado `predicho-real`, negativo=subestimación.
- Ocho tablas: métricas por fold; promedios entre folds; métricas combinadas aparte; calibración agrupada; errores por duración real; sensibilidad interna RMSE/MAE; distribuciones y frecuencias de train fold 4.
- Se conservan todos los extremos. El zoom no altera los cálculos.
- ±3 y ±5 días son tolerancias ilustrativas, no criterios operacionales aprobados.
- Pearson y Spearman complementan el diagnóstico; no reemplazan MAE/RMSE/R². Correlación no es precisión en días ni causalidad. Baseline constante: correlaciones no definidas dentro de cada fold.

### Hallazgos comprobados

La tabla principal sigue usando RMSE para selección. Splines mantiene MAE medio 6,3748, RMSE medio 9,9024 y R² medio 0,1709. El baseline mantiene 7,6542 de MAE y 11,3410 de RMSE. Las mejoras relativas son aproximadamente 16,7 % y 12,7 %.

Los 2.374 pedidos con duración real ≥30 días son 9,03 % de la validación y concentran 69,04 % del error cuadrático de Splines. Todos fueron subestimados. Esta división es retrospectiva: la duración real no se conoce al aprobar un pedido.

Seleccionar por MAE en las grillas internas archivadas cambiaría 16 de 28 configuraciones, pero la mayor mejora interna relativa observada es inferior al 1 %. Esto no demuestra una mejora externa: no se reentrenaron y evaluaron externamente esos candidatos con un nuevo protocolo MAE.

## Etapa 14: dos objetos que deben distinguirse

| Análisis | Objeto utilizado | Interpretación permitida |
|---|---|---|
| PDP/ICE de precio y flete | Candidato final Etapa 11 `ns_df5_geo1`, RDS original intacto | Comportamiento descriptivo de la función ajustada; sin afirmación causal |
| Importancia por permutación | Splines `ns_df3_geo0` del fold 2 reconstruido y verificado | Aporte predictivo por grupos en marzo dentro del experimento retrospectivo |
| Intervalos 80/90 % | Ese mismo Splines del fold 2, fijo en calibración y evaluación | Cobertura y anchura observadas del experimento; no cobertura futura garantizada |

Los intervalos del modelo fijo de enero NO se adjuntan automáticamente al candidato final entrenado con 75.172 pedidos. La tabla principal de selección sigue siendo la original, con el protocolo anidado original.

### Diseño temporal del experimento

1. **Entrenamiento:** 32.995 pedidos del fold 2, aprobados con al menos 45 días de antigüedad y entregados antes del 1 de enero de 2018. La receta y configuración son las originales de ese fold. Sus predicciones de enero se reconstruyen con diferencia máxima inferior a `1e-7`.
2. **Calibración:** pedidos aprobados en enero; solo se usan las 6.819 entregas conocidas antes del 1 de marzo. Se excluyen 368 pedidos de enero cuya entrega no era observable al corte. El archivo `02_auditoria_calibracion_enero.csv` registra esa decisión y los scores; las respuestas futuras no observables se dejan vacías.
3. **Evaluación posterior:** 7.083 pedidos aprobados en marzo, entregados y observables al corte global del 1 de julio. Los conjuntos son disjuntos y el modelo no se reajusta tras calibrar. Febrero deja tiempo para observar entregas de enero.
4. **Referencia:** media histórica del mismo entrenamiento, mantenida fija y calibrada con los mismos pedidos de enero.

Se usa el cuantil de orden `ceil((n+1)*nivel)` del error absoluto de calibración. El intervalo es `predicción ± cuantil`, intersectado con duraciones no negativas. Los niveles 80/90 % se fijan como objetivos de diseño. La exclusión de entregas no observadas, los cambios temporales y la dependencia impiden afirmar la garantía conformal estándar basada en intercambiabilidad. La familia y el experimento se eligieron después de examinar desarrollo: no es un nuevo test independiente.

### Resultado del experimento de intervalos

| Modelo fijo | Cobertura objetivo | Cobertura observada marzo | Anchura media |
|---|---:|---:|---:|
| Splines | 80 % | 70,69 % | 14,52 días |
| Splines | 90 % | 79,43 % | 19,54 días |
| Media histórica | 80 % | 68,32 % | 17,43 días |
| Media histórica | 90 % | 78,89 % | 22,99 días |

Los objetivos no se alcanzaron. Splines ofrece intervalos más estrechos y cobertura descriptiva algo mayor que la referencia, pero conserva cobertura insuficiente. No se corrigieron los cuantiles utilizando los errores de marzo para mejorar retrospectivamente esta cifra. La cobertura debe presentarse junto con anchura e interval score; un rango amplio puede tener poca utilidad práctica.

### Interpretación

La permutación se realizó en cinco grupos que reúnen las diez entradas originales y preservan pares internos relacionados. En este experimento, geografía produce el mayor aumento medio de RMSE al permutar (aproximadamente 1,20 días), seguida por precio/flete (0,40), composición del pedido (0,09), calendario (0,02) y demora de aprobación (cerca de cero). Son aumentos de error al alterar grupos, no porcentajes de varianza explicada. La desviación entre cinco repeticiones mide variación Monte Carlo, no incertidumbre temporal ni significancia. Permutar puede romper dependencias entre grupos.

Los PDP/ICE usan una muestra reproducible de 1.500 pedidos maduros, 40 curvas individuales y rangos P5–P95 de precio/flete. Describen el candidato final: no permiten afirmar que cambiar el flete o precio cause un cambio en la entrega. La asociación entre atributos puede hacer que algunas combinaciones simuladas sean poco realistas.

## Disponibilidad al aprobar: pendientes concretos

| Entradas | Estado de la evidencia | Qué debe justificarse |
|---|---|---|
| Hora, día y mes de aprobación; horas desde compra | Derivables de las marcas temporales disponibles al aprobar | Integridad y zona horaria de las marcas registradas |
| Estado del cliente | Supuesto de vigencia histórica | Que corresponda al domicilio registrado al aprobar, sin actualización posterior |
| Número de artículos y vendedores | Supuesto de vigencia histórica | Que el detalle corresponda a la composición conocida al aprobar |
| Precio y flete totales | Supuesto de vigencia histórica | Que no incluyan modificaciones posteriores a la aprobación |
| Coincidencia de estados cliente–vendedores | Supuesto de vigencia histórica | Disponibilidad y vigencia de la ubicación de cada vendedor al aprobar |

Estas verificaciones no se sustituyen por una correlación o una buena métrica. El dataset no permite certificar por sí solo todo el historial de modificaciones.

## Conclusiones defendibles y pendientes

- El proyecto construye una predicción continua y supera a la media histórica en los períodos de validación originales.
- Las mejores familias están muy próximas y la complejidad adicional aporta poco en lo ensayado.
- Las duraciones largas y los cambios entre períodos siguen limitando la precisión.
- La incertidumbre individual es relevante: el experimento no alcanzó las coberturas buscadas. MAE/RMSE no son intervalos de predicción.
- Los análisis complementarios fortalecen la interpretación; no mejoran por sí mismos las predicciones originales.
- La población sigue limitada a entregados observables. La evaluación independiente del sistema final sigue pendiente: julio–agosto de 2018 ya fue examinado. Debe acordarse con el profesor un protocolo válido; redividir el mismo período conocido no recupera independencia.
- Para la presentación, elegir pocas figuras con una conclusión visible. Las diez láminas no requieren mostrar las diez figuras nuevas.

## Fuentes metodológicas

- [Evaluación y métricas de regresión, scikit-learn](https://scikit-learn.org/stable/modules/model_evaluation.html).
- [PDP/ICE y límites de interpretación, scikit-learn](https://scikit-learn.org/stable/modules/partial_dependence.html).
- [Intercambiabilidad y sus límites, MAPIE](https://mapie.readthedocs.io/en/stable/content/exchangeability-testing/).

El contenido esperado de las láminas 7 y 8 se toma de la guía oficial de presentación del curso; los métodos añadidos son decisiones de implementación del grupo, no exigencias textuales de esa guía.
