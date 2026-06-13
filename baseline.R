# ============================================================================
# MODELO BASELINE COMPLETO: ANÁLISIS DE MEDIDAS DE TENDENCIA CENTRAL
# ============================================================================
# Este script calcula todas las medidas de tendencia central para el modelo
# baseline, tanto para el dataset completo como separado por tipo de opción
# (CALL y PUT), siguiendo la estructura metodológica del documento.
# ============================================================================

# Limpiar entorno
rm(list = ls())

# ============================================================================
# [1] CONFIGURACIÓN Y CARGA DE DATOS
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("MODELO BASELINE - ANÁLISIS COMPLETO DE MEDIDAS DE TENDENCIA CENTRAL\n")
cat("=" , rep("=", 70), "\n\n", sep = "")

# Cargar librerías
library(readxl)
library(writexl)
library(moments)  # Para skewness y kurtosis

# Rutas de entrada
ruta_base <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1/sub_3-6/"

# Cargar datos de entrenamiento
train_CALL <- read_xlsx(paste0(ruta_base, "train_data_CALL.xlsx"))
train_PUT <- read_xlsx(paste0(ruta_base, "train_data_PUT.xlsx"))

# Cargar datos de prueba
test_CALL <- read_xlsx(paste0(ruta_base, "test_data_CALL.xlsx"))
test_PUT <- read_xlsx(paste0(ruta_base, "test_data_PUT.xlsx"))

# Combinar datasets
train_completo <- rbind(
  transform(train_CALL, Tipo = "CALL"),
  transform(train_PUT, Tipo = "PUT")
)

test_completo <- rbind(
  transform(test_CALL, Tipo = "CALL"),
  transform(test_PUT, Tipo = "PUT")
)

cat("[1] DATOS CARGADOS:\n")
cat("    Dataset Completo:\n")
cat("      - Train total:", nrow(train_completo), "observaciones\n")
cat("      - Test total:", nrow(test_completo), "observaciones\n")
cat("    Por tipo de opción (Train):\n")
cat("      - CALL:", nrow(train_CALL), "observaciones\n")
cat("      - PUT:", nrow(train_PUT), "observaciones\n")
cat("    Por tipo de opción (Test):\n")
cat("      - CALL:", nrow(test_CALL), "observaciones\n")
cat("      - PUT:", nrow(test_PUT), "observaciones\n\n")

# Ruta de salida
ruta_salida <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo_2/baseline/"
dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# [2] FUNCIÓN PARA CALCULAR TODAS LAS MEDIDAS DE TENDENCIA CENTRAL
# ============================================================================

calcular_medidas_tendencia <- function(datos, nombre_conjunto) {

  x <- datos$Diferencia
  n <- length(x)

  # --- MEDIA ---
  media <- mean(x, na.rm = TRUE)

  # --- MEDIANA ---
  mediana <- median(x, na.rm = TRUE)

  # --- MODA (estimación por densidad kernel) ---
  # Usamos density() para estimar la función de densidad y encontrar su máximo
  densidad <- density(x, na.rm = TRUE)
  moda_kernel <- densidad$x[which.max(densidad$y)]

  # Moda discreta aproximada (redondeando a 2 decimales)
  x_redondeado <- round(x, 2)
  tabla_freq <- table(x_redondeado)
  moda_discreta <- as.numeric(names(tabla_freq)[which.max(tabla_freq)])
  freq_moda <- max(tabla_freq)

  # --- PERCENTILES ---
  percentiles <- quantile(x, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)

  # --- MEDIDAS DE DISPERSIÓN ---
  desv_std <- sd(x, na.rm = TRUE)
  varianza <- var(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  rango <- range(x, na.rm = TRUE)
  rango_total <- diff(rango)

  # --- MEDIDAS DE FORMA ---
  asimetria <- skewness(x, na.rm = TRUE)
  curtosis <- kurtosis(x, na.rm = TRUE) - 3  # Exceso de curtosis

  # --- OTROS ---
  error_std_media <- desv_std / sqrt(n)
  coef_variacion <- abs(desv_std / media) * 100  # En porcentaje

  # Crear lista de resultados
  resultados <- list(
    conjunto = nombre_conjunto,
    n = n,
    # Tendencia central
    media = media,
    mediana = mediana,
    moda_kernel = moda_kernel,
    moda_discreta = moda_discreta,
    freq_moda_discreta = freq_moda,
    # Diferencias entre medidas
    diff_media_mediana = media - mediana,
    diff_media_moda = media - moda_kernel,
    # Percentiles
    P01 = as.numeric(percentiles["1%"]),
    P05 = as.numeric(percentiles["5%"]),
    P10 = as.numeric(percentiles["10%"]),
    Q1 = as.numeric(percentiles["25%"]),
    Q2 = as.numeric(percentiles["50%"]),  # Igual a mediana
    Q3 = as.numeric(percentiles["75%"]),
    P90 = as.numeric(percentiles["90%"]),
    P95 = as.numeric(percentiles["95%"]),
    P99 = as.numeric(percentiles["99%"]),
    # Dispersión
    desv_std = desv_std,
    varianza = varianza,
    iqr = iqr,
    minimo = rango[1],
    maximo = rango[2],
    rango_total = rango_total,
    error_std_media = error_std_media,
    coef_variacion = coef_variacion,
    # Forma
    skewness = asimetria,
    kurtosis_exceso = curtosis
  )

  return(resultados)
}

# ============================================================================
# [3] CALCULAR MEDIDAS PARA TODOS LOS CONJUNTOS
# ============================================================================

cat("[2] CALCULANDO MEDIDAS DE TENDENCIA CENTRAL...\n\n")

# Dataset completo
medidas_completo <- calcular_medidas_tendencia(train_completo, "Train_Completo")

# Por tipo de opción
medidas_CALL <- calcular_medidas_tendencia(train_CALL, "Train_CALL")
medidas_PUT <- calcular_medidas_tendencia(train_PUT, "Train_PUT")

# ============================================================================
# [4] MOSTRAR RESULTADOS - DATASET COMPLETO
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[3] RESULTADOS - DATASET COMPLETO (N =", medidas_completo$n, ")\n")
cat("=" , rep("=", 70), "\n\n")

cat(">>> MEDIDAS DE TENDENCIA CENTRAL:\n")
cat("    ┌─────────────────────────────────────────────────────────────┐\n")
cat(sprintf("    │ Media (C_media)           : $%12.4f                  │\n", medidas_completo$media))
cat(sprintf("    │ Mediana (C_mediana)       : $%12.4f                  │\n", medidas_completo$mediana))
cat(sprintf("    │ Moda (kernel density)     : $%12.4f                  │\n", medidas_completo$moda_kernel))
cat(sprintf("    │ Moda (discreta aprox.)    : $%12.4f (freq: %d)       │\n", medidas_completo$moda_discreta, medidas_completo$freq_moda_discreta))
cat("    └─────────────────────────────────────────────────────────────┘\n\n")

cat(">>> DIFERENCIAS ENTRE MEDIDAS:\n")
cat(sprintf("    Media - Mediana  = $%.4f\n", medidas_completo$diff_media_mediana))
cat(sprintf("    Media - Moda     = $%.4f\n", medidas_completo$diff_media_moda))
cat("\n")

cat(">>> PERCENTILES:\n")
cat("    ┌─────────────────────────────────────────────────────────────┐\n")
cat(sprintf("    │ P1  (1%%)   : $%12.4f                                 │\n", medidas_completo$P01))
cat(sprintf("    │ P5  (5%%)   : $%12.4f                                 │\n", medidas_completo$P05))
cat(sprintf("    │ P10 (10%%)  : $%12.4f                                 │\n", medidas_completo$P10))
cat(sprintf("    │ Q1  (25%%)  : $%12.4f                                 │\n", medidas_completo$Q1))
cat(sprintf("    │ Q2  (50%%)  : $%12.4f  [= Mediana]                    │\n", medidas_completo$Q2))
cat(sprintf("    │ Q3  (75%%)  : $%12.4f                                 │\n", medidas_completo$Q3))
cat(sprintf("    │ P90 (90%%)  : $%12.4f                                 │\n", medidas_completo$P90))
cat(sprintf("    │ P95 (95%%)  : $%12.4f                                 │\n", medidas_completo$P95))
cat(sprintf("    │ P99 (99%%)  : $%12.4f                                 │\n", medidas_completo$P99))
cat("    └─────────────────────────────────────────────────────────────┘\n\n")

cat(">>> MEDIDAS DE DISPERSIÓN:\n")
cat(sprintf("    Desviación Estándar  : $%.4f\n", medidas_completo$desv_std))
cat(sprintf("    Varianza             : $%.4f\n", medidas_completo$varianza))
cat(sprintf("    Rango Intercuartílico: $%.4f\n", medidas_completo$iqr))
cat(sprintf("    Mínimo               : $%.4f\n", medidas_completo$minimo))
cat(sprintf("    Máximo               : $%.4f\n", medidas_completo$maximo))
cat(sprintf("    Rango Total          : $%.4f\n", medidas_completo$rango_total))
cat(sprintf("    Error Estándar Media : $%.4f\n", medidas_completo$error_std_media))
cat("\n")

cat(">>> MEDIDAS DE FORMA:\n")
cat(sprintf("    Skewness (Asimetría)     : %.4f\n", medidas_completo$skewness))
cat(sprintf("    Kurtosis (Exceso)        : %.4f\n", medidas_completo$kurtosis_exceso))
cat("\n")

# Interpretación automática
cat(">>> INTERPRETACIÓN:\n")
if (medidas_completo$skewness > 0.5) {
  cat("    - Distribución con SESGO POSITIVO (cola derecha extendida)\n")
} else if (medidas_completo$skewness < -0.5) {
  cat("    - Distribución con SESGO NEGATIVO (cola izquierda extendida)\n")
} else {
  cat("    - Distribución aproximadamente SIMÉTRICA\n")
}

if (medidas_completo$kurtosis_exceso > 1) {
  cat("    - Distribución LEPTOCÚRTICA (colas pesadas, pico pronunciado)\n")
} else if (medidas_completo$kurtosis_exceso < -1) {
  cat("    - Distribución PLATICÚRTICA (colas ligeras, pico aplanado)\n")
} else {
  cat("    - Distribución aproximadamente MESOCÚRTICA (similar a normal)\n")
}
cat("\n")

# ============================================================================
# [5] MOSTRAR RESULTADOS - POR TIPO DE OPCIÓN
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[4] RESULTADOS - OPCIONES CALL (N =", medidas_CALL$n, ")\n")
cat("=" , rep("=", 70), "\n\n")

cat(">>> MEDIDAS DE TENDENCIA CENTRAL:\n")
cat(sprintf("    Media (C_media)           : $%.4f\n", medidas_CALL$media))
cat(sprintf("    Mediana (C_mediana)       : $%.4f\n", medidas_CALL$mediana))
cat(sprintf("    Moda (kernel density)     : $%.4f\n", medidas_CALL$moda_kernel))
cat(sprintf("    Diferencia Media-Mediana  : $%.4f\n", medidas_CALL$diff_media_mediana))
cat("\n")

cat(">>> PERCENTILES CLAVE:\n")
cat(sprintf("    Q1 (25%%): $%.4f | Q2 (50%%): $%.4f | Q3 (75%%): $%.4f\n",
            medidas_CALL$Q1, medidas_CALL$Q2, medidas_CALL$Q3))
cat(sprintf("    IQR: $%.4f\n", medidas_CALL$iqr))
cat("\n")

cat(">>> DISPERSIÓN Y FORMA:\n")
cat(sprintf("    Desv. Estándar: $%.4f | Rango: [$%.4f, $%.4f]\n",
            medidas_CALL$desv_std, medidas_CALL$minimo, medidas_CALL$maximo))
cat(sprintf("    Skewness: %.4f | Kurtosis (exc): %.4f\n",
            medidas_CALL$skewness, medidas_CALL$kurtosis_exceso))
cat("\n")

cat("=" , rep("=", 70), "\n", sep = "")
cat("[5] RESULTADOS - OPCIONES PUT (N =", medidas_PUT$n, ")\n")
cat("=" , rep("=", 70), "\n\n")

cat(">>> MEDIDAS DE TENDENCIA CENTRAL:\n")
cat(sprintf("    Media (C_media)           : $%.4f\n", medidas_PUT$media))
cat(sprintf("    Mediana (C_mediana)       : $%.4f\n", medidas_PUT$mediana))
cat(sprintf("    Moda (kernel density)     : $%.4f\n", medidas_PUT$moda_kernel))
cat(sprintf("    Diferencia Media-Mediana  : $%.4f\n", medidas_PUT$diff_media_mediana))
cat("\n")

cat(">>> PERCENTILES CLAVE:\n")
cat(sprintf("    Q1 (25%%): $%.4f | Q2 (50%%): $%.4f | Q3 (75%%): $%.4f\n",
            medidas_PUT$Q1, medidas_PUT$Q2, medidas_PUT$Q3))
cat(sprintf("    IQR: $%.4f\n", medidas_PUT$iqr))
cat("\n")

cat(">>> DISPERSIÓN Y FORMA:\n")
cat(sprintf("    Desv. Estándar: $%.4f | Rango: [$%.4f, $%.4f]\n",
            medidas_PUT$desv_std, medidas_PUT$minimo, medidas_PUT$maximo))
cat(sprintf("    Skewness: %.4f | Kurtosis (exc): %.4f\n",
            medidas_PUT$skewness, medidas_PUT$kurtosis_exceso))
cat("\n")

# ============================================================================
# [6] TABLA COMPARATIVA DE MEDIDAS
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[6] TABLA COMPARATIVA - TODAS LAS MEDIDAS\n")
cat("=" , rep("=", 70), "\n\n")

# Crear dataframe comparativo
tabla_comparativa <- data.frame(
  Medida = c("N observaciones",
             "--- TENDENCIA CENTRAL ---",
             "Media", "Mediana", "Moda (kernel)",
             "Diferencia Media-Mediana",
             "--- PERCENTILES ---",
             "P1 (1%)", "P5 (5%)", "P10 (10%)",
             "Q1 (25%)", "Q2 (50%)", "Q3 (75%)",
             "P90 (90%)", "P95 (95%)", "P99 (99%)",
             "--- DISPERSIÓN ---",
             "Desviación Estándar", "IQR", "Mínimo", "Máximo", "Rango Total",
             "--- FORMA ---",
             "Skewness", "Kurtosis (exceso)"),
  Completo = c(medidas_completo$n,
               NA,
               medidas_completo$media, medidas_completo$mediana, medidas_completo$moda_kernel,
               medidas_completo$diff_media_mediana,
               NA,
               medidas_completo$P01, medidas_completo$P05, medidas_completo$P10,
               medidas_completo$Q1, medidas_completo$Q2, medidas_completo$Q3,
               medidas_completo$P90, medidas_completo$P95, medidas_completo$P99,
               NA,
               medidas_completo$desv_std, medidas_completo$iqr, medidas_completo$minimo, medidas_completo$maximo, medidas_completo$rango_total,
               NA,
               medidas_completo$skewness, medidas_completo$kurtosis_exceso),
  CALL = c(medidas_CALL$n,
           NA,
           medidas_CALL$media, medidas_CALL$mediana, medidas_CALL$moda_kernel,
           medidas_CALL$diff_media_mediana,
           NA,
           medidas_CALL$P01, medidas_CALL$P05, medidas_CALL$P10,
           medidas_CALL$Q1, medidas_CALL$Q2, medidas_CALL$Q3,
           medidas_CALL$P90, medidas_CALL$P95, medidas_CALL$P99,
           NA,
           medidas_CALL$desv_std, medidas_CALL$iqr, medidas_CALL$minimo, medidas_CALL$maximo, medidas_CALL$rango_total,
           NA,
           medidas_CALL$skewness, medidas_CALL$kurtosis_exceso),
  PUT = c(medidas_PUT$n,
          NA,
          medidas_PUT$media, medidas_PUT$mediana, medidas_PUT$moda_kernel,
          medidas_PUT$diff_media_mediana,
          NA,
          medidas_PUT$P01, medidas_PUT$P05, medidas_PUT$P10,
          medidas_PUT$Q1, medidas_PUT$Q2, medidas_PUT$Q3,
          medidas_PUT$P90, medidas_PUT$P95, medidas_PUT$P99,
          NA,
          medidas_PUT$desv_std, medidas_PUT$iqr, medidas_PUT$minimo, medidas_PUT$maximo, medidas_PUT$rango_total,
          NA,
          medidas_PUT$skewness, medidas_PUT$kurtosis_exceso)
)

print(tabla_comparativa, row.names = FALSE)
cat("\n")

# ============================================================================
# [7] EVALUACIÓN: COMPARAR MEDIA vs MEDIANA COMO CONSTANTE C
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[7] EVALUACIÓN: ¿MEDIA O MEDIANA COMO CONSTANTE C?\n")
cat("=" , rep("=", 70), "\n\n")

# Función para calcular métricas de evaluación
calcular_metricas <- function(real, predicho) {
  error <- real - predicho
  rmse <- sqrt(mean(error^2, na.rm = TRUE))
  mae <- mean(abs(error), na.rm = TRUE)
  # R² no aplica para modelo constante (sería 0)
  return(list(RMSE = rmse, MAE = mae))
}

# --- DATASET COMPLETO ---
cat(">>> DATASET COMPLETO:\n\n")

# Constantes
C_media_completo <- medidas_completo$media
C_mediana_completo <- medidas_completo$mediana

# Test completo
y_test_completo <- test_completo$Diferencia

# Predicciones con media
pred_media_completo <- rep(C_media_completo, length(y_test_completo))
metricas_media_completo <- calcular_metricas(y_test_completo, pred_media_completo)

# Predicciones con mediana
pred_mediana_completo <- rep(C_mediana_completo, length(y_test_completo))
metricas_mediana_completo <- calcular_metricas(y_test_completo, pred_mediana_completo)

cat(sprintf("    Constante C (Media)   = $%.4f\n", C_media_completo))
cat(sprintf("    Constante C (Mediana) = $%.4f\n\n", C_mediana_completo))

cat("    Métricas en Test (N =", length(y_test_completo), "):\n")
cat("    ┌─────────────────┬─────────────────┬─────────────────┐\n")
cat("    │ Métrica         │ Usando MEDIA    │ Usando MEDIANA  │\n")
cat("    ├─────────────────┼─────────────────┼─────────────────┤\n")
cat(sprintf("    │ RMSE            │ $%13.4f │ $%13.4f │\n",
            metricas_media_completo$RMSE, metricas_mediana_completo$RMSE))
cat(sprintf("    │ MAE             │ $%13.4f │ $%13.4f │\n",
            metricas_media_completo$MAE, metricas_mediana_completo$MAE))
cat("    └─────────────────┴─────────────────┴─────────────────┘\n\n")

# --- OPCIONES CALL ---
cat(">>> OPCIONES CALL:\n\n")

C_media_CALL <- medidas_CALL$media
C_mediana_CALL <- medidas_CALL$mediana
y_test_CALL <- test_CALL$Diferencia

pred_media_CALL <- rep(C_media_CALL, length(y_test_CALL))
metricas_media_CALL <- calcular_metricas(y_test_CALL, pred_media_CALL)

pred_mediana_CALL <- rep(C_mediana_CALL, length(y_test_CALL))
metricas_mediana_CALL <- calcular_metricas(y_test_CALL, pred_mediana_CALL)

cat(sprintf("    Constante C (Media)   = $%.4f\n", C_media_CALL))
cat(sprintf("    Constante C (Mediana) = $%.4f\n\n", C_mediana_CALL))

cat("    Métricas en Test (N =", length(y_test_CALL), "):\n")
cat("    ┌─────────────────┬─────────────────┬─────────────────┐\n")
cat("    │ Métrica         │ Usando MEDIA    │ Usando MEDIANA  │\n")
cat("    ├─────────────────┼─────────────────┼─────────────────┤\n")
cat(sprintf("    │ RMSE            │ $%13.4f │ $%13.4f │\n",
            metricas_media_CALL$RMSE, metricas_mediana_CALL$RMSE))
cat(sprintf("    │ MAE             │ $%13.4f │ $%13.4f │\n",
            metricas_media_CALL$MAE, metricas_mediana_CALL$MAE))
cat("    └─────────────────┴─────────────────┴─────────────────┘\n\n")

# --- OPCIONES PUT ---
cat(">>> OPCIONES PUT:\n\n")

C_media_PUT <- medidas_PUT$media
C_mediana_PUT <- medidas_PUT$mediana
y_test_PUT <- test_PUT$Diferencia

pred_media_PUT <- rep(C_media_PUT, length(y_test_PUT))
metricas_media_PUT <- calcular_metricas(y_test_PUT, pred_media_PUT)

pred_mediana_PUT <- rep(C_mediana_PUT, length(y_test_PUT))
metricas_mediana_PUT <- calcular_metricas(y_test_PUT, pred_mediana_PUT)

cat(sprintf("    Constante C (Media)   = $%.4f\n", C_media_PUT))
cat(sprintf("    Constante C (Mediana) = $%.4f\n\n", C_mediana_PUT))

cat("    Métricas en Test (N =", length(y_test_PUT), "):\n")
cat("    ┌─────────────────┬─────────────────┬─────────────────┐\n")
cat("    │ Métrica         │ Usando MEDIA    │ Usando MEDIANA  │\n")
cat("    ├─────────────────┼─────────────────┼─────────────────┤\n")
cat(sprintf("    │ RMSE            │ $%13.4f │ $%13.4f │\n",
            metricas_media_PUT$RMSE, metricas_mediana_PUT$RMSE))
cat(sprintf("    │ MAE             │ $%13.4f │ $%13.4f │\n",
            metricas_media_PUT$MAE, metricas_mediana_PUT$MAE))
cat("    └─────────────────┴─────────────────┴─────────────────┘\n\n")

# ============================================================================
# [8] COMPARACIÓN BS ORIGINAL vs BS+C (MEDIA) vs BS+C (MEDIANA)
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[8] COMPARACIÓN: BS ORIGINAL vs BS+C(MEDIA) vs BS+C(MEDIANA)\n")
cat("=" , rep("=", 70), "\n\n")

# Función para evaluar ajuste de precios
evaluar_ajuste_precios <- function(test_data, C_media, C_mediana, nombre) {

  precio_real <- test_data$Last
  precio_BS <- test_data$Precio_BS

  # BS Original
  error_BS <- precio_real - precio_BS
  rmse_BS <- sqrt(mean(error_BS^2, na.rm = TRUE))
  mae_BS <- mean(abs(error_BS), na.rm = TRUE)

  # BS + C (Media)
  precio_ajustado_media <- precio_BS + C_media
  error_media <- precio_real - precio_ajustado_media
  rmse_media <- sqrt(mean(error_media^2, na.rm = TRUE))
  mae_media <- mean(abs(error_media), na.rm = TRUE)

  # BS + C (Mediana)
  precio_ajustado_mediana <- precio_BS + C_mediana
  error_mediana <- precio_real - precio_ajustado_mediana
  rmse_mediana <- sqrt(mean(error_mediana^2, na.rm = TRUE))
  mae_mediana <- mean(abs(error_mediana), na.rm = TRUE)

  # Mejoras porcentuales
  mejora_rmse_media <- (1 - rmse_media / rmse_BS) * 100
  mejora_mae_media <- (1 - mae_media / mae_BS) * 100
  mejora_rmse_mediana <- (1 - rmse_mediana / rmse_BS) * 100
  mejora_mae_mediana <- (1 - mae_mediana / mae_BS) * 100

  cat(sprintf(">>> %s (N = %d):\n\n", nombre, nrow(test_data)))
  cat("    ┌───────────────────┬─────────────┬─────────────┬─────────────┐\n")
  cat("    │ Modelo            │ RMSE ($)    │ MAE ($)     │ Mejora RMSE │\n")
  cat("    ├───────────────────┼─────────────┼─────────────┼─────────────┤\n")
  cat(sprintf("    │ BS Original       │ %11.4f │ %11.4f │      ---    │\n", rmse_BS, mae_BS))
  cat(sprintf("    │ BS + C (Media)    │ %11.4f │ %11.4f │ %+10.2f%% │\n", rmse_media, mae_media, mejora_rmse_media))
  cat(sprintf("    │ BS + C (Mediana)  │ %11.4f │ %11.4f │ %+10.2f%% │\n", rmse_mediana, mae_mediana, mejora_rmse_mediana))
  cat("    └───────────────────┴─────────────┴─────────────┴─────────────┘\n\n")

  return(list(
    rmse_BS = rmse_BS, mae_BS = mae_BS,
    rmse_media = rmse_media, mae_media = mae_media,
    rmse_mediana = rmse_mediana, mae_mediana = mae_mediana,
    mejora_rmse_media = mejora_rmse_media,
    mejora_rmse_mediana = mejora_rmse_mediana
  ))
}

# Evaluar para cada conjunto
resultados_completo <- evaluar_ajuste_precios(test_completo, C_media_completo, C_mediana_completo, "DATASET COMPLETO")
resultados_CALL <- evaluar_ajuste_precios(test_CALL, C_media_CALL, C_mediana_CALL, "OPCIONES CALL")
resultados_PUT <- evaluar_ajuste_precios(test_PUT, C_media_PUT, C_mediana_PUT, "OPCIONES PUT")

# ============================================================================
# [9] SELECCIÓN FINAL DE LA CONSTANTE C
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[9] SELECCIÓN FINAL DE LA CONSTANTE C\n")
cat("=" , rep("=", 70), "\n\n")

cat(">>> JUSTIFICACIÓN TEÓRICA:\n")
cat("    - La MEDIA minimiza el Error Cuadrático Medio (MSE/RMSE)\n")
cat("    - La MEDIANA minimiza el Error Absoluto Medio (MAE)\n")
cat("    - Dado que RMSE es la métrica principal de evaluación,\n")
cat("      se selecciona la MEDIA como estimador de C\n\n")

cat(">>> CONSTANTES SELECCIONADAS:\n")
cat("    ┌─────────────────────────────────────────────────────────────┐\n")
cat(sprintf("    │ C_completo = $%.4f (para análisis global)              │\n", C_media_completo))
cat(sprintf("    │ C_CALL     = $%.4f (para opciones de compra)           │\n", C_media_CALL))
cat(sprintf("    │ C_PUT      = $%.4f (para opciones de venta)            │\n", C_media_PUT))
cat("    └─────────────────────────────────────────────────────────────┘\n\n")

cat(">>> INTERPRETACIÓN FINANCIERA:\n")
if (C_media_CALL > 0) {
  cat(sprintf("    - CALL: C = +$%.4f → Black-Scholes SUBVALORA en promedio\n", C_media_CALL))
  cat("              (el mercado paga más de lo que predice el modelo)\n")
} else {
  cat(sprintf("    - CALL: C = $%.4f → Black-Scholes SOBREVALORA en promedio\n", C_media_CALL))
  cat("              (el mercado paga menos de lo que predice el modelo)\n")
}

if (C_media_PUT > 0) {
  cat(sprintf("    - PUT:  C = +$%.4f → Black-Scholes SUBVALORA en promedio\n", C_media_PUT))
} else {
  cat(sprintf("    - PUT:  C = $%.4f → Black-Scholes SOBREVALORA en promedio\n", C_media_PUT))
  cat("              (el mercado paga menos de lo que predice el modelo)\n")
}
cat("\n")

# ============================================================================
# [10] GUARDAR RESULTADOS
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[10] GUARDANDO RESULTADOS\n")
cat("=" , rep("=", 70), "\n\n")

# Tabla 1: Medidas de tendencia central
tabla_tendencia <- data.frame(
  Medida = c("N", "Media", "Mediana", "Moda (kernel)",
             "Dif. Media-Mediana", "Desv. Estándar", "IQR",
             "Q1", "Q3", "Mínimo", "Máximo",
             "Skewness", "Kurtosis (exc)"),
  Completo = round(c(medidas_completo$n, medidas_completo$media, medidas_completo$mediana,
                     medidas_completo$moda_kernel, medidas_completo$diff_media_mediana,
                     medidas_completo$desv_std, medidas_completo$iqr,
                     medidas_completo$Q1, medidas_completo$Q3,
                     medidas_completo$minimo, medidas_completo$maximo,
                     medidas_completo$skewness, medidas_completo$kurtosis_exceso), 4),
  CALL = round(c(medidas_CALL$n, medidas_CALL$media, medidas_CALL$mediana,
                 medidas_CALL$moda_kernel, medidas_CALL$diff_media_mediana,
                 medidas_CALL$desv_std, medidas_CALL$iqr,
                 medidas_CALL$Q1, medidas_CALL$Q3,
                 medidas_CALL$minimo, medidas_CALL$maximo,
                 medidas_CALL$skewness, medidas_CALL$kurtosis_exceso), 4),
  PUT = round(c(medidas_PUT$n, medidas_PUT$media, medidas_PUT$mediana,
                medidas_PUT$moda_kernel, medidas_PUT$diff_media_mediana,
                medidas_PUT$desv_std, medidas_PUT$iqr,
                medidas_PUT$Q1, medidas_PUT$Q3,
                medidas_PUT$minimo, medidas_PUT$maximo,
                medidas_PUT$skewness, medidas_PUT$kurtosis_exceso), 4)
)

# Tabla 2: Comparación Media vs Mediana
tabla_media_vs_mediana <- data.frame(
  Conjunto = c("Completo", "Completo", "CALL", "CALL", "PUT", "PUT"),
  Constante = c("Media", "Mediana", "Media", "Mediana", "Media", "Mediana"),
  Valor_C = round(c(C_media_completo, C_mediana_completo,
                    C_media_CALL, C_mediana_CALL,
                    C_media_PUT, C_mediana_PUT), 4),
  RMSE_Test = round(c(metricas_media_completo$RMSE, metricas_mediana_completo$RMSE,
                      metricas_media_CALL$RMSE, metricas_mediana_CALL$RMSE,
                      metricas_media_PUT$RMSE, metricas_mediana_PUT$RMSE), 4),
  MAE_Test = round(c(metricas_media_completo$MAE, metricas_mediana_completo$MAE,
                     metricas_media_CALL$MAE, metricas_mediana_CALL$MAE,
                     metricas_media_PUT$MAE, metricas_mediana_PUT$MAE), 4)
)

# Tabla 3: Comparación BS vs BS+C
tabla_comparacion_BS <- data.frame(
  Conjunto = c("Completo", "Completo", "Completo",
               "CALL", "CALL", "CALL",
               "PUT", "PUT", "PUT"),
  Modelo = rep(c("BS Original", "BS + C (Media)", "BS + C (Mediana)"), 3),
  RMSE = round(c(resultados_completo$rmse_BS, resultados_completo$rmse_media, resultados_completo$rmse_mediana,
                 resultados_CALL$rmse_BS, resultados_CALL$rmse_media, resultados_CALL$rmse_mediana,
                 resultados_PUT$rmse_BS, resultados_PUT$rmse_media, resultados_PUT$rmse_mediana), 4),
  MAE = round(c(resultados_completo$mae_BS, resultados_completo$mae_media, resultados_completo$mae_mediana,
                resultados_CALL$mae_BS, resultados_CALL$mae_media, resultados_CALL$mae_mediana,
                resultados_PUT$mae_BS, resultados_PUT$mae_media, resultados_PUT$mae_mediana), 4)
)

# Guardar archivos
write_xlsx(tabla_tendencia, paste0(ruta_salida, "1_medidas_tendencia_central.xlsx"))
write_xlsx(tabla_media_vs_mediana, paste0(ruta_salida, "2_comparacion_media_vs_mediana.xlsx"))
write_xlsx(tabla_comparacion_BS, paste0(ruta_salida, "3_comparacion_BS_vs_BSC.xlsx"))

cat("    Archivos guardados en:\n")
cat("    ", ruta_salida, "\n\n")
cat("    - 1_medidas_tendencia_central.xlsx\n")
cat("    - 2_comparacion_media_vs_mediana.xlsx\n")
cat("    - 3_comparacion_BS_vs_BSC.xlsx\n\n")

# ============================================================================
# [11] RESUMEN FINAL
# ============================================================================

cat("=" , rep("=", 70), "\n", sep = "")
cat("[11] RESUMEN FINAL - MODELO BASELINE\n")
cat("=" , rep("=", 70), "\n\n")

cat("┌─────────────────────────────────────────────────────────────────────┐\n")
cat("│                    CONSTANTES C SELECCIONADAS                      │\n")
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│  Dataset Completo:  C = $%8.4f                                 │\n", C_media_completo))
cat(sprintf("│  Opciones CALL:     C = $%8.4f (BS subvalora)                  │\n", C_media_CALL))
cat(sprintf("│  Opciones PUT:      C = $%8.4f (BS sobrevalora)                │\n", C_media_PUT))
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat("│                    MÉTRICAS BASELINE (TEST)                        │\n")
cat("├─────────────────────────────────────────────────────────────────────┤\n")
cat(sprintf("│  CALL: RMSE = $%8.4f  |  Mejora vs BS: %+.2f%%                  │\n",
            resultados_CALL$rmse_media, resultados_CALL$mejora_rmse_media))
cat(sprintf("│  PUT:  RMSE = $%8.4f  |  Mejora vs BS: %+.2f%%                  │\n",
            resultados_PUT$rmse_media, resultados_PUT$mejora_rmse_media))
cat("└─────────────────────────────────────────────────────────────────────┘\n\n")

cat(">>> CONCLUSIÓN:\n")
cat("    El modelo baseline establece el umbral mínimo de desempeño.\n")
cat("    Los modelos de ML en las siguientes secciones deben superar:\n")
cat(sprintf("    - RMSE CALL: $%.4f\n", resultados_CALL$rmse_media))
cat(sprintf("    - RMSE PUT:  $%.4f\n", resultados_PUT$rmse_media))
cat("\n")

cat("=" , rep("=", 70), "\n", sep = "")
cat("SCRIPT COMPLETADO EXITOSAMENTE\n")
cat("=" , rep("=", 70), "\n")
