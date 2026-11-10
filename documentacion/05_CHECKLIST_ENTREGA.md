# Lista de control de la entrega — Grupo 3

- [x] Dataset original Olist en ZIP, sin transformación manual.
- [x] Scripts actuales 01–12 y dependencias auxiliares 01A/04B.
- [x] Verificador de entorno (`00_verificar_entorno.R`), que no instala paquetes.
- [x] OLS, Ridge, Lasso, Elastic-Net, Splines, CART, Random Forest, Boosting, media histórica y averaging 50/50.
- [x] Mismas observaciones externas y protección del test histórico ya conocido.
- [x] Correlaciones de variables solo en train; selección de predictores mediante validación interna.
- [x] Inferencia OLS con α=0,05 e intervalos HC3 al 95 %.
- [x] Candidato Splines entrenado (75.172 pedidos) y RDS conservado.
- [x] Reproducción aislada en el mismo PC aprobada (15/15, RDS válido, MD5 originales intactos).
- [ ] Portabilidad ejecutada y aprobada en **otro computador** (si el evaluador o grupo puede realizarla).
- [ ] Informe definitivo y diapositivas separados, según pauta.

### Complementos añadidos al mismo caso

- [x] Script 13: gráficos real–predicho y residuos de diez alternativas sobre validación común.
- [x] Calibración por grupos de predicción; métricas por fold y combinadas claramente separadas.
- [x] Script 14: PDP/ICE del candidato final e importancia por grupos del modelo temporal fijo.
- [x] Intervalos exploratorios con calibración y evaluación posteriores separadas; cobertura y anchura informadas, aunque no se alcancen los objetivos.
- [x] Predicciones, métricas y RDS originales conservados.
- [x] Instrucciones de reproducción de complementos y límites en `06_GUIA_COMPLEMENTOS_Y_RESULTADOS.md`.
- [x] Reproducción nueva de complementos en proceso limpio: 18/18 CSV y 10/10 figuras; origen intacto.
- [ ] Nueva ejecución integral ampliada de 33 tablas en el equipo de entrega (distinta de la evidencia histórica de 15).
- [ ] Verificación histórica de disponibilidad de atributos al aprobar el pedido.
- [ ] Evaluación final verdaderamente independiente mediante un protocolo acordado con el profesor.

### Precauciones críticas

* NO reutilizar julio–agosto de 2018 como un test nuevo.
* No mostrar el RMSE de entrenamiento final como error fuera de muestra.
* Averaging fue agregado como comparación complementaria tras examinar los nueve modelos.
* No borrar `01A_estilo_graficos_etapa1.R` ni `04B_regenerar_graficos_datacamp.R`.
* No cargar ni modificar los resultados de una versión anterior para obtener una métrica diferente.
