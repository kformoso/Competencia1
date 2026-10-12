# DM EyF 2026 – Primera Competencia · Karina Formoso

Predicción de churn (`BAJA+2`) para **202108** con LightGBM en R.

**Entrega final:** `KA_KF16_60semillas_12000.csv`, generada con `KF16.R`.

## Modelo

| Item | Valor |
|---|---|
| Algoritmo | LightGBM multiclase (CONTINUA / BAJA+1 / BAJA+2) |
| Ranking | P(BAJA+1) + P(BAJA+2) |
| Meses de entrenamiento | 202104, 202105, 202106 |
| Mes a predecir | 202108 |
| Undersampling | se conserva el 20% de CONTINUA |
| Hiperparámetros | learning_rate 0.027 · num_leaves 31 · min_data_in_leaf 200 · feature_fraction 0.5 · max_bin 255 · 500 rondas |
| Semillerío | 60 semillas (primos consecutivos desde 100003); se promedian las probabilidades |
| Corte | 12000 envíos |

## Feature engineering

- `ccajas_depositos` en NA en 202105 (variable rota).
- Lags y deltas de orden 1 y 2.
- Tendencia y promedio de los últimos 3 meses (`TendenciaYmuchomas`, código de la cátedra).
- Con préstamos personales, sin quitar intrusos.

## Archivos

- `KF16.R`: script completo; con `competencia_01.csv.gz` en la misma carpeta, se corre con `source("KF16.R")`.
- `KA_KF16_60semillas_12000.csv`: archivo entregado.
