# Evidencia de la actualización de código

Se conservó la raíz única del proyecto y no se creó ni modificó un informe o presentación.

## Verificaciones ejecutadas

- R 4.3.3 sobre Linux: scripts 13 y 14 ejecutados satisfactoriamente.
- Reconstrucción del Splines del fold 2 frente a sus predicciones archivadas: diferencia máxima aproximada 6,4 × 10⁻¹⁴ días.
- Reproducción de complementos en otra carpeta y proceso Rscript --vanilla: 18/18 CSV coincidentes, 10/10 figuras regeneradas y origen intacto por MD5.
- Verificación independiente en Python: 368 controles aprobados. Incluye integridad de 183 archivos originales protegidos, métricas, particiones, observabilidad, cuantiles, cobertura, amplitud e interval score.
- Revisión visual de los gráficos nuevos y comprobación de archivos PNG no vacíos.

## Alcance

La prueba breve reconstruye 13/14 usando los resultados y el RDS originales. No reentrena los ensembles ni sustituye la evidencia integral histórica 15/15. El nuevo modo integral de 33 tablas tiene sintaxis comprobada e incorpora las dos etapas ejecutadas; **no se certificó una nueva ejecución integral completa en esta actualización**.

Ninguna de estas verificaciones es un test predictivo nuevo. El experimento de intervalos permanece identificado como retrospectivo y no alcanzó sus coberturas objetivo.

## Archivos originales editados

- README_REPRODUCIBILIDAD.md: instrucciones y alcance actualizado.
- scripts/12_reproduccion_integral_aislada.R: integración 13/14 y modo de reproducción breve.
- documentacion/01_GUIA_SCRIPTS_Y_RESULTADOS.md: mapa de etapas actualizado.
- documentacion/05_CHECKLIST_ENTREGA.md: controles nuevos y pendientes.
- documentacion/04_MANIFIESTO_ARCHIVOS_SHA256.csv: inventario actualizado.

Se preservaron sin cambios los scripts 00–11 y auxiliares, los datos originales, las predicciones, las métricas, el candidato RDS y los registros históricos de reproducción. Las adiciones están en scripts 13/14, resultados y gráficos de esas etapas, la guía 06 y esta carpeta de evidencia.

Para repetir la verificación independiente, instalar `numpy` y `pandas` en un entorno Python y ejecutar:

```bash
python evidencia_complementos/verificacion_independiente.py
```

La ejecución de los scripts R no necesita Python ni instala dependencias automáticamente.
