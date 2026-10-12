# ============================================================
# DM EyF 2026 - Primera Competencia
#
# KF16 = KF11 + feature engineering histórico de la cátedra
#
# KF11 = KF10 sin intrusos: con préstamos personales, sin quitar
#        intrusos, undersampling 0.20, multiclase. Fue mi mejor
#        archivo único (promedio de 50 semillas).
#
# Qué cambia respecto de KF11:
#  - Tendencia y promedio de los últimos 3 meses de cada variable,
#    con la función TendenciaYmuchomas de la cátedra (era el único
#    tip del profe que me faltaba).
#  - 60 semillas (números primos consecutivos desde 100.003),
#    promediadas en UN solo archivo.
#  - Corte 12.000 en vez de 13.000: el backtest de junio de la
#    configuración KF11 (CORTE_KF11) dio la meseta de ganancia entre
#    10.816 y 12.263 envíos, así que 13.000 quedaba afuera.

# ============================================================

rm(list = ls(all.names = TRUE))
gc()

library(data.table)
library(lightgbm)
library(Rcpp)

# Más lugar para columnas nuevas (la tendencia agrega ~300)
options(datatable.alloccol = 4096L)


# ============================================================
# 1. PARÁMETROS
# ============================================================

ARCHIVO     <- "competencia_01.csv.gz"
EXPERIMENTO <- "KF16"

MESES_TRAIN <- c(202104L, 202105L, 202106L)
MES_ENTREGA <- 202108L

VARIABLES_ROTAS      <- c("ccajas_depositos")
MESES_ROTOS_FORZADOS <- c(202105L)   # lo que dijo el profe

VENTANA_HISTORIA <- 3L   # meses para tendencia y promedio

UNDERSAMPLING <- 0.20    # fracción de CONTINUA que se conserva

CORTE        <- 12000L
N_SEMILLAS   <- 60L
PRIMER_PRIMO <- 100003L

NROUNDS <- 500L

PARAM_DATASET <- list(
  max_bin            = 255L,
  feature_pre_filter = FALSE
)

PARAM_LGB <- list(
  objective         = "multiclass",
  num_class         = 3L,
  metric            = "multi_logloss",
  boosting          = "gbdt",
  learning_rate     = 0.027,
  num_leaves        = 31L,
  min_data_in_leaf  = 200L,
  feature_fraction  = 0.50,
  max_depth         = -1L,
  lambda_l1         = 0,
  lambda_l2         = 0,
  min_gain_to_split = 0,
  bagging_fraction  = 1,
  bagging_freq      = 0L,
  force_row_wise    = TRUE,
  deterministic     = TRUE,
  verbosity         = -1L
)


# ============================================================
# 2. SEMILLAS: los N_SEMILLAS primos a partir de PRIMER_PRIMO
# ============================================================

primos_desde <- function(desde, cuantos) {
  out <- integer(0)
  n <- as.integer(desde)
  while (length(out) < cuantos) {
    if (all(n %% 2:floor(sqrt(n)) != 0L)) out <- c(out, n)
    n <- n + 1L
  }
  out
}

SEMILLAS <- primos_desde(PRIMER_PRIMO, N_SEMILLAS)


# ============================================================
# 3. FEATURE ENGINEERING HISTÓRICO (código de la cátedra)
#
# fhistC: para cada fila, mira las filas del mismo cliente desde
# pdesde hasta la fila actual (sólo pasado y presente, nada del
# futuro) y calcula pendiente, mínimo, máximo, promedio y el valor
# del mes anterior.
# ============================================================

cppFunction("NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde )
{
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;

    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) )
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      {
        xsum  += x[h] ;
        ysum  += y[h] ;
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ;
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}")

# Agrega al dataset (ordenado por cliente y mes) las columnas pedidas
# para cada variable de cols: _tend, _min, _max, _avg, _ratioavg, _ratiomax.
TendenciaYmuchomas <- function(
    dataset, cols, ventana = 6, tendencia = TRUE,
    minimo = TRUE, maximo = TRUE, promedio = TRUE,
    ratioavg = FALSE, ratiomax = FALSE) {
  gc(verbose = FALSE)
  ventana_regresion <- ventana

  last <- nrow(dataset)

  vector_ids <- dataset[, numero_de_cliente]

  # primera fila de la ventana para cada fila
  vector_desde <- seq(
    -ventana_regresion + 2,
    nrow(dataset) - ventana_regresion + 1
  )

  vector_desde[1:ventana_regresion] <- 1

  # la ventana no puede empezar en otro cliente
  for (i in 2:last) {
    if (vector_ids[i - 1] != vector_ids[i]) {
      vector_desde[i] <- i
    }
  }
  for (i in 2:last) {
    if (vector_desde[i] < vector_desde[i - 1]) {
      vector_desde[i] <- vector_desde[i - 1]
    }
  }

  for (campo in cols) {
    nueva_col <- fhistC(dataset[, get(campo)], vector_desde)

    if (tendencia) {
      dataset[, paste0(campo, "_tend", ventana) :=
        nueva_col[(0 * last + 1):(1 * last)]]
    }

    if (minimo) {
      dataset[, paste0(campo, "_min", ventana) :=
        nueva_col[(1 * last + 1):(2 * last)]]
    }

    if (maximo) {
      dataset[, paste0(campo, "_max", ventana) :=
        nueva_col[(2 * last + 1):(3 * last)]]
    }

    if (promedio) {
      dataset[, paste0(campo, "_avg", ventana) :=
        nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratioavg) {
      dataset[, paste0(campo, "_ratioavg", ventana) :=
        get(campo) / nueva_col[(3 * last + 1):(4 * last)]]
    }

    if (ratiomax) {
      dataset[, paste0(campo, "_ratiomax", ventana) :=
        get(campo) / nueva_col[(2 * last + 1):(3 * last)]]
    }
  }
}


# ============================================================
# 4. CARPETAS, CONTROL DE PARÁMETROS Y LOG
#
#  KF16/
#    semillas/                 -> probabilidades de cada semilla (.rds)
#    submits/                  -> el archivo para subir al bot
#    prob_agosto_por_semilla.csv
#    parametros.txt, log.txt
# ============================================================

CARPETA_SEM <- file.path(EXPERIMENTO, "semillas")
CARPETA_SUB <- file.path(EXPERIMENTO, "submits")
dir.create(CARPETA_SEM, recursive = TRUE, showWarnings = FALSE)
dir.create(CARPETA_SUB, recursive = TRUE, showWarnings = FALSE)

arch_sem <- function(s) file.path(CARPETA_SEM, sprintf("seed_%d.rds", s))
ARCH_IDS <- file.path(EXPERIMENTO, "ids_agosto.csv")

# "Firma" de los parámetros: si la carpeta tiene semillas entrenadas
# con otros parámetros, se frena para no mezclar corridas distintas.
ARCH_FIRMA <- file.path(EXPERIMENTO, "firma.rds")
FIRMA <- list(MESES_TRAIN, MES_ENTREGA, VARIABLES_ROTAS, MESES_ROTOS_FORZADOS,
              VENTANA_HISTORIA, UNDERSAMPLING, NROUNDS, PARAM_DATASET, PARAM_LGB)
if (file.exists(ARCH_FIRMA) && !identical(readRDS(ARCH_FIRMA), FIRMA)) {
  stop("\n", EXPERIMENTO, " tiene semillas entrenadas con otros parametros. ",
       "Cambia EXPERIMENTO o borra la carpeta.")
}
saveRDS(FIRMA, ARCH_FIRMA)

while (sink.number() > 0) sink()
sink(file.path(EXPERIMENTO, "log.txt"), split = TRUE, append = TRUE)

cat("\n========================================\n")
cat(EXPERIMENTO, "- inicio:", format(Sys.time()), "\n")
cat("========================================\n")

writeLines(capture.output(print(list(
  EXPERIMENTO = EXPERIMENTO, MESES_TRAIN = MESES_TRAIN, MES_ENTREGA = MES_ENTREGA,
  VARIABLES_ROTAS = VARIABLES_ROTAS, MESES_ROTOS_FORZADOS = MESES_ROTOS_FORZADOS,
  VENTANA_HISTORIA = VENTANA_HISTORIA, UNDERSAMPLING = UNDERSAMPLING, CORTE = CORTE,
  SEMILLAS = SEMILLAS, NROUNDS = NROUNDS,
  PARAM_DATASET = PARAM_DATASET, PARAM_LGB = PARAM_LGB,
  sesion = sessionInfo()
))), file.path(EXPERIMENTO, "parametros.txt"))

faltan <- SEMILLAS[!file.exists(vapply(SEMILLAS, arch_sem, ""))]
cat("Semillas:", length(SEMILLAS), "| faltan entrenar:", length(faltan), "\n")


# ============================================================
# 5. DATOS, VARIABLES Y ENTRENAMIENTO (sólo si falta algo)
# ============================================================

if (length(faltan) > 0 || !file.exists(ARCH_IDS)) {

  if (!file.exists(ARCHIVO)) stop("\nNo encuentro ", ARCHIVO, "\nCarpeta actual: ", getwd())

  dataset <- fread(ARCHIVO)
  cat("\nFilas:", nrow(dataset), " Columnas:", ncol(dataset), "\n")

  # --- 5a. Data quality: variable rota -> NA (antes de los lags) ---
  for (v in intersect(VARIABLES_ROTAS, names(dataset))) {
    meses_todo_cero <- dataset[
      , .(todo_cero = all(.SD[[1]] == 0, na.rm = TRUE)),
      by = foto_mes, .SDcols = v
    ][todo_cero == TRUE, foto_mes]
    meses_rotos <- sort(unique(c(MESES_ROTOS_FORZADOS, meses_todo_cero)))
    dataset[foto_mes %in% meses_rotos, (v) := NA]
    cat(v, "-> NA en:", paste(meses_rotos, collapse = ", "), "\n")
  }

  # --- 5b. Lags y deltas de orden 1 y 2, invalidando huecos de meses ---
  setorder(dataset, numero_de_cliente, foto_mes)
  periodo <- (dataset$foto_mes %/% 100L) * 12L + dataset$foto_mes %% 100L

  campos_lag <- setdiff(
    names(dataset)[vapply(dataset, is.numeric, logical(1))],
    c("numero_de_cliente", "foto_mes")
  )

  for (k in 1:2) {
    cli_prev <- shift(dataset$numero_de_cliente, k)
    per_prev <- shift(periodo, k)
    invalidas <- which(
      is.na(cli_prev) |
        cli_prev != dataset$numero_de_cliente |
        (periodo - per_prev) != k
    )
    for (campo in campos_lag) {
      x  <- dataset[[campo]]
      xl <- shift(x, k)
      xl[invalidas] <- NA
      set(dataset, j = paste0(campo, "_lag", k),   value = xl)
      set(dataset, j = paste0(campo, "_delta", k), value = x - xl)
    }
  }
  rm(periodo, cli_prev, per_prev, invalidas, x, xl)
  gc()

  # --- 5c. Tendencia y promedio de los últimos 3 meses (lo nuevo de KF16) ---
  # Se calcula con todos los meses cargados, antes de filtrar,
  # para que agosto tenga su historia (junio, julio, agosto).
  TendenciaYmuchomas(dataset,
    cols      = campos_lag,
    ventana   = VENTANA_HISTORIA,
    tendencia = TRUE,
    minimo    = FALSE,
    maximo    = FALSE,
    promedio  = TRUE,
    ratioavg  = FALSE,
    ratiomax  = FALSE
  )
  gc()

  cat("Columnas con lags, deltas, tendencia y promedio:", ncol(dataset), "\n")

  # --- 5d. Sólo los meses que se usan (ahorra memoria) ---
  dataset <- dataset[foto_mes %in% c(MESES_TRAIN, MES_ENTREGA)]
  gc()

  # --- 5e. Clase multiclase: 0 = CONTINUA, 1 = BAJA+1, 2 = BAJA+2 ---
  dataset[, clase01 := fcase(
    clase_ternaria == "CONTINUA", 0L,
    clase_ternaria == "BAJA+1",   1L,
    clase_ternaria == "BAJA+2",   2L,
    default = NA_integer_
  )]

  print(dataset[, .N, by = .(foto_mes, clase_ternaria)][order(foto_mes, clase_ternaria)])

  campos_modelo <- setdiff(names(dataset), c("clase_ternaria", "clase01"))
  cat("\nVariables del modelo:", length(campos_modelo), "\n")

  idx_base   <- dataset[foto_mes %in% MESES_TRAIN & !is.na(clase01), which = TRUE]
  clase_base <- dataset$clase01[idx_base]

  X_pred <- data.matrix(dataset[foto_mes == MES_ENTREGA, ..campos_modelo])
  fwrite(data.table(numero_de_cliente = dataset[foto_mes == MES_ENTREGA, numero_de_cliente]), ARCH_IDS, scipen = 50L)

  # --- 5f. Semillerío: un modelo por semilla, cada uno con su
  #         undersampling de CONTINUA (se conservan todas las bajas) ---
  for (s in faltan) {

    t0 <- Sys.time()
    set.seed(s, kind = "L'Ecuyer-CMRG")

    conservar <- clase_base > 0L | runif(length(idx_base)) <= UNDERSAMPLING
    idx <- idx_base[conservar]

    dtrain <- lgb.Dataset(
      data   = data.matrix(dataset[idx, ..campos_modelo]),
      label  = dataset$clase01[idx],
      params = PARAM_DATASET
    )

    param      <- PARAM_LGB
    param$seed <- s

    modelo <- lgb.train(params = param, data = dtrain, nrounds = NROUNDS)

    # P(BAJA+1) + P(BAJA+2); lightgbm 3.x devuelve vector, 4.x matriz
    pr <- predict(modelo, X_pred)
    if (is.null(dim(pr))) pr <- matrix(pr, ncol = 3L, byrow = TRUE)

    # se guarda primero en .tmp y se renombra: si se corta a mitad
    # de la escritura no queda un archivo roto
    tmp <- paste0(arch_sem(s), ".tmp")
    saveRDS(pr[, 2] + pr[, 3], tmp)
    file.rename(tmp, arch_sem(s))

    cat(sprintf("  semilla %d | filas train %d | %.0f s | %s\n",
                s, length(idx), as.numeric(difftime(Sys.time(), t0, units = "secs")),
                format(Sys.time(), "%d/%m %H:%M")))

    rm(dtrain, modelo, pr)
    gc(verbose = FALSE)
  }

  rm(dataset, X_pred)
  gc()
}


# ============================================================
# 6. SUBMIT: promedio de las 60 semillas en UN archivo
# ============================================================

ids_agosto <- fread(ARCH_IDS)$numero_de_cliente

P <- sapply(SEMILLAS, function(s) readRDS(arch_sem(s)))
colnames(P) <- as.character(SEMILLAS)

prob <- rowMeans(P)
rk   <- frank(-prob, ties.method = "first")

# Una sola columna y SIN encabezado (el bot rechaza "numero_de_cliente")
archivo <- file.path(CARPETA_SUB, sprintf("KA_%s_%dsemillas_%d.csv", EXPERIMENTO, length(SEMILLAS), CORTE))
fwrite(data.table(ids_agosto[rk <= CORTE]), archivo, col.names = FALSE, scipen = 50L)

# Probabilidades por semilla y promedio: sirven para rearmar el
# submit con otro corte sin reentrenar.
out <- data.table(numero_de_cliente = ids_agosto)
for (s in colnames(P)) set(out, j = paste0("seed_", s), value = P[, s])
out[, prob_promedio := prob]
fwrite(out, file.path(EXPERIMENTO, "prob_agosto_por_semilla.csv"))

l <- readLines(archivo)
cat("\nCreado:", archivo, "|", length(l), "filas | primera:", l[1], "\n")
cat("md5:", tools::md5sum(archivo), "\n")

cat("\nFin:", format(Sys.time()), "\n")
sink()
