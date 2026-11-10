# Evidencia de reproducción incluida en la entrega

Este paquete se construyó a partir de la carpeta de reproducción aislada aprobada **`Proyecto_Olist_caso1__REPRODUCCION_20261026_180102`** el 26-10-2026. Los archivos de `evidencia_reproduccion/` son copias sin modificar de sus registros.

- `09_RESULTADO_REPRODUCIBILIDAD.txt`: **ESTADO: APROBADO**, 15/15 tablas, RDS en segunda sesión, proyecto original sin modificaciones.
- `04_comparacion_con_resultados_originales.csv`: lista de las quince tablas numéricas reconstruidas.
- `01_dependencias.csv`: versiones instaladas en el entorno comprobado.
- `06_RDS_NUEVA_SESION_APROBADO.txt`: evidencia de lectura y predicciones del RDS en otra sesión.
- `02_ejecucion_stdout.log` y `03_ejecucion_stderr.log`: salida y mensajes del proceso Rscript aislado.
- `sessionInfo_Rscript.txt`: información de R y el sistema donde se reprodujo.

**Alcance:** reproducción certificada en un segundo proceso de R **del mismo computador**. No prueba portabilidad en otro computador ni produce una validación final independiente.

## Verificación por el evaluador

Desde la raíz `Grupo3_Olist_Laboratorio/` ejecutar `source("scripts/00_verificar_entorno.R", encoding="UTF-8")`; después `source("scripts/12_reproduccion_integral_aislada.R", encoding="UTF-8")`. La etapa 12 crea una carpeta hermana y **no sobrescribe la carpeta entregada**.
