"""Comprueba los resultados nuevos con Python; no entrena ni modifica modelos.

Requiere numpy y pandas. Desde cualquier carpeta:
python evidencia_complementos/verificacion_independiente.py
"""
from pathlib import Path
import hashlib
import json
import math
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RES = ROOT / "resultados"
OUT = Path(__file__).resolve().parent
checks = []


def require(condition, name):
    if not bool(condition):
        raise AssertionError(name)
    checks.append(name)


def close(a, b):
    return np.allclose(np.asarray(a, dtype=float), np.asarray(b, dtype=float),
                       rtol=1e-9, atol=1e-8, equal_nan=True)


def read(path):
    return pd.read_csv(RES / path)


original = pd.read_csv(OUT / "01_huellas_originales_sha256.csv")
protected = original[~original.editable_en_actualizacion]
for row in protected.itertuples():
    require(hashlib.sha256((ROOT / row.ruta_relativa).read_bytes()).hexdigest() == row.sha256_original,
            "Integridad: " + row.ruta_relativa)

models = read("10_decision_modelo/08_comparacion_10_alternativas.csv").modelo.tolist()
files = ["04_baseline_y_ols/03_predicciones_validacion.csv",
         "05_regularizacion/05_predicciones_validacion_regularizados.csv",
         "06_splines/05_predicciones_splines_por_pedido.csv",
         "07_ensembles/05_predicciones_validacion.csv",
         "07_ensembles/12_averaging_predicciones.csv"]
p = pd.concat([read(f)[["fold", "order_id", "modelo", "dias_reales", "dias_predichos"]]
               for f in files], ignore_index=True)
p = p[p.modelo.isin(models)]
require(not p.duplicated(["modelo", "fold", "order_id"]).any(), "Sin predicciones duplicadas")
reference = p[p.modelo == models[0]].set_index(["fold", "order_id"]).sort_index()
require(len(reference) == 26279, "26.279 pedidos de referencia")
new = read("13_diagnostico_complementario/01_metricas_por_fold.csv").set_index(["modelo", "fold"])
for model, group in p.groupby("modelo"):
    indexed = group.set_index(["fold", "order_id"]).sort_index()
    require(indexed.index.equals(reference.index) and close(indexed.dias_reales, reference.dias_reales),
            "Pedidos y respuestas comunes: " + model)
    for fold, part in group.groupby("fold"):
        error = part.dias_predichos.to_numpy() - part.dias_reales.to_numpy()
        real = part.dias_reales.to_numpy()
        values = [np.sqrt(np.mean(error ** 2)), np.mean(np.abs(error)),
                  1 - np.sum(error ** 2) / np.sum((real - real.mean()) ** 2), np.mean(error)]
        row = new.loc[(model, fold)]
        require(close(values, row[["rmse", "mae", "r2", "sesgo"]]),
                f"Metricas independientes {model} / fold {fold}")
        require(close([np.median(np.abs(error)), np.quantile(np.abs(error), .9)],
                      row[["mediana_error_absoluto", "p90_error_absoluto"]]),
                f"Mediana/P90 {model} / fold {fold}")
summary = read("13_diagnostico_complementario/02_promedios_entre_folds.csv").set_index("modelo")
historical = read("10_decision_modelo/08_comparacion_10_alternativas.csv").set_index("modelo")
for name in ["rmse", "mae", "r2", "sesgo"]:
    means = new[name].groupby(level="modelo").mean()
    require(close(means.loc[models], summary.loc[models, name + "_promedio"]), "Agregacion fold: " + name)
    require(close(means.loc[models], historical.loc[models, name + "_promedio"]), "Tabla historica intacta: " + name)
calgroup = read("13_diagnostico_complementario/04_calibracion_por_prediccion_y_fold.csv")
for (model, fold), g in calgroup.groupby(["modelo", "fold"]):
    src = p[(p.modelo == model) & (p.fold == fold)]
    require(g.n.sum() == len(src) and close(np.average(g.media_real, weights=g.n), src.dias_reales.mean()),
            f"Grupos de calibracion cubren todo: {model}/{fold}")

b = read("02_variables_candidatas/01_variables_candidatas_desarrollo.csv").set_index("order_id")
y = read("01_revision_datos/06_etiquetas_solo_desarrollo.csv").set_index("order_id").dias_entrega
b["y"] = y
b["aprobacion"] = pd.to_datetime(b.fecha_aprobacion)
b["entrega"] = b.aprobacion + pd.to_timedelta(b.y.fillna(0), unit="D")
ids = read("03_protocolo_y_folds/04_ids_train_validacion.csv")
train = set(ids.loc[(ids.fold == 2) & (ids.rol == "train"), "order_id"])
evaluation = set(ids.loc[(ids.fold == 4) & (ids.rol == "validacion"), "order_id"])
audit = read("14_interpretacion_e_incertidumbre/02_auditoria_calibracion_enero.csv")
jan = b[(b.aprobacion >= "2018-01-01") & (b.aprobacion < "2018-02-01")]
require(set(audit.order_id) == set(jan.index), "Auditoria incluye todos los aprobados en enero")
known = jan.y.notna() & (jan.entrega < pd.Timestamp("2018-03-01"))
calibration = set(audit.loc[audit.elegible_calibracion, "order_id"])
require(calibration == set(jan.index[known]), "Calibracion usa solo entregas conocidas antes de marzo")
require(not train & calibration and not train & evaluation and not calibration & evaluation,
        "Entrenamiento/calibracion/evaluacion disjuntos")
require(len(train) == 32995 and len(calibration) == 6819 and len(evaluation) == 7083,
        "Conteos del experimento temporal")
require((b.loc[list(train), "entrega"] < pd.Timestamp("2018-01-01")).all(), "Etiquetas de train disponibles")
require((b.loc[list(train), "aprobacion"] <= pd.Timestamp("2018-01-01") - pd.Timedelta(days=45)).all(),
        "Madurez de entrenamiento 45 dias")
require(audit.loc[~audit.elegible_calibracion, ["duracion_real_conocida_al_corte", "score_absoluto_splines",
        "score_absoluto_baseline"]].isna().all().all(), "Respuestas futuras excluidas de calibracion")
mean_train = b.loc[list(train), "y"].mean()
require(close(audit.loc[audit.elegible_calibracion, "prediccion_baseline"], mean_train), "Baseline solo de train")
quantiles = read("14_interpretacion_e_incertidumbre/03_cuantiles_calibrados.csv")
intervals = read("14_interpretacion_e_incertidumbre/04_intervalos_evaluacion_marzo_desarrollo.csv")
coverage = read("14_interpretacion_e_incertidumbre/05_cobertura_anchura_y_subgrupos.csv")
for row in quantiles.itertuples():
    score_name = "score_absoluto_splines" if row.modelo == "Splines_fijo_enero" else "score_absoluto_baseline"
    scores = np.sort(audit.loc[audit.elegible_calibracion, score_name].to_numpy())
    order = math.ceil((len(scores) + 1) * row.nivel_objetivo)
    require(order == row.orden_cuantil and close(scores[order - 1], row.q_error_absoluto),
            f"Cuantil de calibracion independiente {row.modelo}/{row.nivel_objetivo}")
    d = intervals[(intervals.modelo == row.modelo) & (intervals.nivel_objetivo == row.nivel_objetivo)]
    require(len(d) == 7083 and not d.order_id.duplicated().any() and set(d.order_id) == evaluation,
            f"Muestra evaluada comun {row.modelo}/{row.nivel_objetivo}")
    require(close(d.dias_reales, b.loc[d.order_id, "y"]), "Respuestas marzo coinciden con fuente")
    require(close(d.limite_inferior, np.maximum(0, d.dias_predichos - row.q_error_absoluto)) and
            close(d.limite_superior, np.maximum(d.limite_inferior, d.dias_predichos + row.q_error_absoluto)),
            "Intervalos derivados solo del cuantil de calibracion")
    masks = {"Todos": np.ones(len(d), dtype=bool),
             "Geografia_distinta": (d.todos_vendedores_mismo_estado == 0).to_numpy(),
             "Geografia_coincidente": (d.todos_vendedores_mismo_estado == 1).to_numpy(),
             "Duracion_real_30_o_mas_RETROSPECTIVO": (d.dias_reales >= 30).to_numpy()}
    for subgroup, mask in masks.items():
        s = d.loc[mask]
        c = coverage[(coverage.modelo == row.modelo) & (coverage.nivel_objetivo == row.nivel_objetivo) &
                     (coverage.subgrupo == subgroup)].iloc[0]
        covered = (s.dias_reales >= s.limite_inferior) & (s.dias_reales <= s.limite_superior)
        width = s.limite_superior - s.limite_inferior
        score = width + 2/(1-row.nivel_objetivo) * (np.maximum(s.limite_inferior-s.dias_reales, 0) +
                np.maximum(s.dias_reales-s.limite_superior, 0))
        require(len(s) == c.n and covered.sum() == c.n_cubiertos and
                close([covered.mean(), width.mean(), score.mean()],
                      [c.cobertura_observada, c.anchura_media_dias, c.interval_score_medio]),
                f"Cobertura/anchura/score {row.modelo}/{row.nivel_objetivo}/{subgroup}")
for model, d in intervals.groupby("modelo"):
    lo = d[d.nivel_objetivo == .8].set_index("order_id").sort_index()
    hi = d[d.nivel_objetivo == .9].set_index("order_id").sort_index()
    require((hi.limite_inferior <= lo.limite_inferior).all() and (hi.limite_superior >= lo.limite_superior).all(),
            "Intervalos 90 contienen intervalos 80: " + model)
ice = read("14_interpretacion_e_incertidumbre/09_ICE_candidato_final.csv")
require(set(ice.order_id).issubset(set(read("02_variables_candidatas/03_etiquetas_maduras_45d_referencia.csv").order_id)),
        "ICE usa pedidos del entrenamiento del candidato")
for folder in ["13_diagnostico_complementario", "14_interpretacion_e_incertidumbre"]:
    pngs = list((ROOT / "graficos" / folder).glob("*.png"))
    require(len(pngs) == 5 and all(f.stat().st_size > 500 for f in pngs), "Cinco figuras completas: " + folder)
result = {"estado": "APROBADO", "comprobaciones": len(checks), "controles": checks,
          "archivos_historicos_intactos": len(protected), "n_calibracion": len(calibration),
          "n_evaluacion": len(evaluation), "no_es_test_independiente": True}
(OUT / "VALIDACION_INDEPENDIENTE_PYTHON.json").write_text(json.dumps(result, indent=2, ensure_ascii=False) + "\n")
print(json.dumps({k: v for k, v in result.items() if k != "controles"}, ensure_ascii=False, indent=2))
