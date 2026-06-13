# ============================================
# K-NEAREST NEIGHBORS (KNN) - OPCIONES CALL
# Capítulo 4 - Objetivo 2
# Versión con Ticker (one-hot encoding)
# ============================================

# Limpiar entorno
rm(list = ls())

# ============================================
# CONFIGURACIÓN Y LIBRERÍAS
# ============================================

library(readxl)
library(tidyverse)
library(class)        # Para knn()
library(caret)        # Para knnreg() y validación cruzada
library(writexl)
library(scales)

# Configuración de rutas (AJUSTA SEGÚN TU EQUIPO)
RUTA_ENTRADA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1/sub_3-6"
RUTA_SALIDA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 2/KNN"

# Crear carpeta de salida si no existe
if (!dir.exists(RUTA_SALIDA)) {
  dir.create(RUTA_SALIDA, recursive = TRUE)
}

# Crear subcarpeta para CALL
RUTA_CALL <- file.path(RUTA_SALIDA, "CALL")
if (!dir.exists(RUTA_CALL)) {
  dir.create(RUTA_CALL, recursive = TRUE)
}

# Crear subcarpetas para organizar outputs
carpeta_graficos <- file.path(RUTA_CALL, "graficos")
carpeta_tablas <- file.path(RUTA_CALL, "tablas")

if (!dir.exists(carpeta_graficos)) dir.create(carpeta_graficos, recursive = TRUE)
if (!dir.exists(carpeta_tablas)) dir.create(carpeta_tablas, recursive = TRUE)

set.seed(123)  # Reproducibilidad

cat("====================================\n")
cat("KNN - OPCIONES CALL (con Ticker)\n")
cat("====================================\n\n")

# ============================================
# TEMA PERSONALIZADO PARA GRÁFICOS
# ============================================

tema_tesis <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# ============================================
# CARGA DE DATOS
# ============================================

cat("Cargando datos de entrenamiento y prueba CALL...\n")

train_call <- read_xlsx(file.path(RUTA_ENTRADA, "train_data_CALL.xlsx"))
test_call <- read_xlsx(file.path(RUTA_ENTRADA, "test_data_CALL.xlsx"))

cat("  Train CALL:", nrow(train_call), "observaciones\n")
cat("  Test CALL:", nrow(test_call), "observaciones\n\n")

# ============================================
# PREPARACIÓN DE VARIABLES
# ============================================

# Definir las 12 variables predictoras NUMÉRICAS
variables_numericas <- c(
  # Inputs Black-Scholes (4)
  "Strike", "Precio_Actual", "Dias_Vencimiento", "Volatilidad_Historica",
  # Variables de mercado (3)
  "Vol", "OI", "IV",
  # Variables derivadas (5)
  "Moneyness", "Log_Moneyness", "Vol_Diff", "Strike_Normalizado", "In_The_Money"
)

# Variable objetivo
variable_objetivo <- "Diferencia"

# Verificar que todas las variables existen
vars_faltantes <- setdiff(variables_numericas, names(train_call))
if (length(vars_faltantes) > 0) {
  stop("Variables faltantes: ", paste(vars_faltantes, collapse = ", "))
}

cat("Variables numéricas (12):\n")
cat("  ", paste(variables_numericas, collapse = ", "), "\n\n")

# ============================================
# ONE-HOT ENCODING DE TICKER
# ============================================

cat("===========================================\n")
cat("ONE-HOT ENCODING DE TICKER\n")
cat("===========================================\n\n")

# Obtener niveles únicos de Ticker
tickers_unicos <- sort(unique(train_call$Ticker))
cat("Tickers encontrados:", paste(tickers_unicos, collapse = ", "), "\n")
cat("Número de tickers:", length(tickers_unicos), "\n\n")

# Crear variables dummy para Ticker (one-hot encoding)
# Usamos model.matrix para crear dummies, excluyendo la primera categoría (AAPL como referencia)
crear_dummies <- function(data, ticker_levels) {
  # Asegurar que Ticker sea factor con los mismos niveles
  data$Ticker <- factor(data$Ticker, levels = ticker_levels)

  # Crear matriz de dummies (sin intercepto, sin excluir ninguna categoría)
  dummy_matrix <- model.matrix(~ Ticker - 1, data = data)

  # Convertir a dataframe y limpiar nombres
  dummy_df <- as.data.frame(dummy_matrix)
  names(dummy_df) <- gsub("Ticker", "Ticker_", names(dummy_df))

  return(dummy_df)
}

# Crear dummies para train y test
dummies_train <- crear_dummies(train_call, tickers_unicos)
dummies_test <- crear_dummies(test_call, tickers_unicos)

cat("Variables dummy creadas:", paste(names(dummies_train), collapse = ", "), "\n\n")

# ============================================
# PREPARAR DATASETS COMPLETOS
# ============================================

# Preparar datasets para KNN (numéricas + dummies)
train_knn <- cbind(
  train_call[, c(variable_objetivo, variables_numericas)],
  dummies_train
)

test_knn <- cbind(
  test_call[, c(variable_objetivo, variables_numericas)],
  dummies_test
)

# Lista de todas las variables predictoras (numéricas + dummies)
variables_dummy <- names(dummies_train)
variables_predictoras <- c(variables_numericas, variables_dummy)

cat("Total de variables predictoras:", length(variables_predictoras), "\n")
cat("  - Numéricas:", length(variables_numericas), "\n")
cat("  - Dummies (Ticker):", length(variables_dummy), "\n\n")

# Verificar NAs
cat("NAs en train:", sum(is.na(train_knn)), "\n")
cat("NAs en test:", sum(is.na(test_knn)), "\n\n")

# Eliminar NAs si existen
train_knn <- na.omit(train_knn)
test_knn <- na.omit(test_knn)

# ============================================
# NORMALIZACIÓN Z-SCORE (solo variables numéricas)
# ============================================

cat("===========================================\n")
cat("NORMALIZACIÓN Z-SCORE\n")
cat("===========================================\n\n")

# Calcular media y desviación estándar del conjunto de entrenamiento
# SOLO para variables numéricas (las dummies ya son 0/1)
medias_train <- sapply(train_knn[, variables_numericas], mean)
sd_train <- sapply(train_knn[, variables_numericas], sd)

cat("Estadísticas de normalización (train) - Variables numéricas:\n")
stats_norm <- data.frame(
  Variable = variables_numericas,
  Media = round(medias_train, 4),
  SD = round(sd_train, 4)
)
print(stats_norm, row.names = FALSE)

# Aplicar normalización Z-score solo a variables numéricas
train_scaled <- train_knn
test_scaled <- test_knn

for (var in variables_numericas) {
  train_scaled[[var]] <- (train_knn[[var]] - medias_train[var]) / sd_train[var]
  test_scaled[[var]] <- (test_knn[[var]] - medias_train[var]) / sd_train[var]
}

# Las variables dummy se mantienen como 0/1 (no se normalizan)
cat("\nVariables dummy (Ticker) NO se normalizan (ya son 0/1)\n")

cat("\nVerificación de normalización (train):\n")
cat("  Media de variables numéricas normalizadas:", round(mean(colMeans(train_scaled[, variables_numericas])), 6), "\n")
cat("  SD promedio de variables numéricas normalizadas:", round(mean(apply(train_scaled[, variables_numericas], 2, sd)), 6), "\n\n")

# Guardar estadísticas de normalización
write_xlsx(stats_norm, file.path(carpeta_tablas, "normalizacion_stats_CALL.xlsx"))

# ============================================
# OPTIMIZACIÓN DE HIPERPARÁMETROS (k)
# ============================================

cat("===========================================\n")
cat("OPTIMIZACIÓN DE HIPERPARÁMETROS (k)\n")
cat("===========================================\n\n")

# Preparar matrices de características
X_train <- as.matrix(train_scaled[, variables_predictoras])
y_train <- train_scaled$Diferencia
X_test <- as.matrix(test_scaled[, variables_predictoras])
y_test <- test_scaled$Diferencia

# Valores de k a evaluar
n_train <- nrow(X_train)
k_sqrt <- round(sqrt(n_train))
cat("Número de observaciones (n):", n_train, "\n")
cat("Valor sugerido sqrt(n):", k_sqrt, "\n")
cat("Número de predictores:", length(variables_predictoras), "\n\n")

# Evaluar diferentes valores de k
k_valores <- c(1, 3, 5, 7, 10, 15, 20, 30, 50, 75, 100, 125, 150)
resultados_k <- data.frame(k = integer(), RMSE_CV = numeric())

cat("Evaluando k mediante validación cruzada 5-fold...\n")

# Configurar control de validación cruzada
ctrl <- trainControl(method = "cv", number = 5)

for (k_val in k_valores) {
  cat("  k =", k_val, "...")

  # Usar caret para validación cruzada
  modelo_cv <- train(
    x = X_train,
    y = y_train,
    method = "knn",
    tuneGrid = data.frame(k = k_val),
    trControl = ctrl
  )

  rmse_cv <- modelo_cv$results$RMSE
  resultados_k <- rbind(resultados_k, data.frame(k = k_val, RMSE_CV = rmse_cv))
  cat(" RMSE CV = $", round(rmse_cv, 2), "\n")
}

# Mejor k
mejor_k <- resultados_k$k[which.min(resultados_k$RMSE_CV)]
cat("\nMejor k:", mejor_k, "\n")
cat("RMSE CV:", round(min(resultados_k$RMSE_CV), 2), "\n\n")

# Guardar resultados de optimización
write_xlsx(resultados_k, file.path(carpeta_tablas, "optimizacion_k_CALL.xlsx"))

# ============================================
# ENTRENAMIENTO DEL MODELO FINAL
# ============================================

cat("===========================================\n")
cat("ENTRENAMIENTO DEL MODELO FINAL\n")
cat("===========================================\n\n")

cat("Entrenando modelo KNN con k =", mejor_k, "...\n")

# Entrenar modelo final con caret
modelo_knn_call <- train(
  x = X_train,
  y = y_train,
  method = "knn",
  tuneGrid = data.frame(k = mejor_k),
  trControl = trainControl(method = "none")
)

cat("Modelo KNN entrenado exitosamente\n")
cat("  - k (vecinos):", mejor_k, "\n")
cat("  - Predictores:", length(variables_predictoras), "\n")
cat("  - Observaciones de entrenamiento:", n_train, "\n\n")

# ============================================
# PREDICCIONES
# ============================================

cat("===========================================\n")
cat("PREDICCIONES\n")
cat("===========================================\n\n")

# Predicciones en train y test
pred_train <- predict(modelo_knn_call, X_train)
pred_test <- predict(modelo_knn_call, X_test)

# Agregar predicciones al test set original
test_call_results <- test_call[complete.cases(test_call[, c(variable_objetivo, variables_numericas)]), ]
test_call_results$Pred_KNN <- pred_test
test_call_results$Precio_KNN <- test_call_results$Precio_BS + pred_test
test_call_results$Error_KNN <- test_call_results$Diferencia - pred_test

cat("Predicciones generadas:\n")
cat("  - Train:", length(pred_train), "observaciones\n")
cat("  - Test:", length(pred_test), "observaciones\n\n")

# ============================================
# MÉTRICAS DE DESEMPEÑO
# ============================================

cat("===========================================\n")
cat("MÉTRICAS DE DESEMPEÑO\n")
cat("===========================================\n\n")

# Métricas en entrenamiento
rmse_train <- sqrt(mean((y_train - pred_train)^2))
mae_train <- mean(abs(y_train - pred_train))
ss_res_train <- sum((y_train - pred_train)^2)
ss_tot_train <- sum((y_train - mean(y_train))^2)
r2_train <- 1 - (ss_res_train / ss_tot_train)

# Métricas en prueba
rmse_test <- sqrt(mean((y_test - pred_test)^2))
mae_test <- mean(abs(y_test - pred_test))
ss_res_test <- sum((y_test - pred_test)^2)
ss_tot_test <- sum((y_test - mean(y_test))^2)
r2_test <- 1 - (ss_res_test / ss_tot_test)

cat("ENTRENAMIENTO:\n")
cat("  RMSE: $", round(rmse_train, 2), "\n")
cat("  MAE:  $", round(mae_train, 2), "\n")
cat("  R²:   ", round(r2_train, 4), "\n\n")

cat("PRUEBA:\n")
cat("  RMSE: $", round(rmse_test, 2), "\n")
cat("  MAE:  $", round(mae_test, 2), "\n")
cat("  R²:   ", round(r2_test, 4), "\n\n")

# Comparación con baseline
rmse_baseline <- 97.66  # Del modelo baseline CALL
mejora_vs_baseline <- (1 - rmse_test / rmse_baseline) * 100

cat("COMPARACIÓN CON BASELINE:\n")
cat("  RMSE Baseline: $", rmse_baseline, "\n")
cat("  RMSE KNN:      $", round(rmse_test, 2), "\n")
cat("  Mejora:        ", round(mejora_vs_baseline, 2), "%\n\n")

# Comparación con BS Original
rmse_bs_original <- sqrt(mean(test_call_results$Diferencia^2))
rmse_knn_precio <- sqrt(mean((test_call_results$Last - test_call_results$Precio_KNN)^2))
mejora_vs_bs <- (1 - rmse_knn_precio / rmse_bs_original) * 100

cat("MEJORA EN PRECIOS AJUSTADOS:\n")
cat("  RMSE BS Original:    $", round(rmse_bs_original, 2), "\n")
cat("  RMSE BS + KNN:       $", round(rmse_knn_precio, 2), "\n")
cat("  Mejora:              ", round(mejora_vs_bs, 2), "%\n\n")

# Proporción de opciones mejoradas
mejora_individual <- abs(test_call_results$Last - test_call_results$Precio_KNN) < abs(test_call_results$Diferencia)
pct_mejoradas <- mean(mejora_individual) * 100
cat("Opciones con error reducido:", round(pct_mejoradas, 1), "%\n\n")

# ============================================
# ANÁLISIS DE IMPORTANCIA DE VARIABLES
# ============================================

cat("===========================================\n")
cat("ANÁLISIS DE IMPORTANCIA DE VARIABLES\n")
cat("===========================================\n\n")

# Para KNN, usamos permutation importance
# Calcular RMSE base
rmse_base <- rmse_test

# Calcular importancia por permutación
importancia_perm <- data.frame(
  Variable = variables_predictoras,
  RMSE_Permutado = numeric(length(variables_predictoras)),
  Incremento_RMSE = numeric(length(variables_predictoras)),
  Importancia = numeric(length(variables_predictoras))
)

cat("Calculando importancia por permutación...\n")

for (i in seq_along(variables_predictoras)) {
  var <- variables_predictoras[i]
  cat("  Permutando:", var, "...")

  # Crear copia del test set y permutar la variable
  X_test_perm <- X_test
  X_test_perm[, var] <- sample(X_test_perm[, var])

  # Predecir con variable permutada
  pred_perm <- predict(modelo_knn_call, X_test_perm)
  rmse_perm <- sqrt(mean((y_test - pred_perm)^2))

  importancia_perm$RMSE_Permutado[i] <- rmse_perm
  importancia_perm$Incremento_RMSE[i] <- rmse_perm - rmse_base

  cat(" RMSE =", round(rmse_perm, 2), "\n")
}

# Calcular importancia relativa (normalizada)
importancia_perm <- importancia_perm %>%
  mutate(
    Importancia = pmax(Incremento_RMSE, 0),  # Evitar valores negativos
    Importancia_Normalizada = Importancia / max(Importancia) * 100
  ) %>%
  arrange(desc(Importancia_Normalizada)) %>%
  mutate(Ranking = row_number())

cat("\nTop 10 variables por incremento en RMSE:\n")
print(head(importancia_perm[, c("Ranking", "Variable", "Importancia_Normalizada")], 10), row.names = FALSE)

# Guardar importancia
write_xlsx(importancia_perm, file.path(carpeta_tablas, "importancia_variables_CALL.xlsx"))

# ============================================
# GUARDAR RESULTADOS (TABLAS)
# ============================================

cat("\n===========================================\n")
cat("GUARDANDO RESULTADOS\n")
cat("===========================================\n\n")

# Tabla de métricas
metricas <- data.frame(
  Conjunto = c("Entrenamiento", "Prueba"),
  RMSE = c(rmse_train, rmse_test),
  MAE = c(mae_train, mae_test),
  R2 = c(r2_train, r2_test)
)
write_xlsx(metricas, file.path(carpeta_tablas, "metricas_knn_CALL.xlsx"))

# Comparación con otros modelos
comparacion <- data.frame(
  Modelo = c("BS Original", "Baseline (C=+$10.28)", "Regresión Lineal", "Random Forest", "XGBoost", "KNN"),
  RMSE = c(98.42, 97.66, 73.15, 18.67, 17.20, rmse_test),
  MAE = c(25.96, 32.33, 30.71, 7.42, 6.79, mae_test)
) %>%
  mutate(Mejora_vs_Baseline = round((1 - RMSE / 97.66) * 100, 2))

write_xlsx(comparacion, file.path(carpeta_tablas, "comparacion_modelos_CALL.xlsx"))

# Resultados del test set
write_xlsx(test_call_results, file.path(carpeta_tablas, "resultados_test_knn_CALL.xlsx"))

# Guardar modelo
saveRDS(modelo_knn_call, file.path(RUTA_CALL, "modelo_knn_CALL.rds"))

# Guardar parámetros de normalización (necesarios para predicciones futuras)
params_norm <- list(
  medias = medias_train,
  sd = sd_train,
  variables_numericas = variables_numericas,
  variables_dummy = variables_dummy,
  tickers = tickers_unicos
)
saveRDS(params_norm, file.path(RUTA_CALL, "normalizacion_params_CALL.rds"))

# Resumen ejecutivo
resumen <- data.frame(
  Metrica = c(
    "Observaciones entrenamiento",
    "Observaciones prueba",
    "Número de predictores (total)",
    "Variables numéricas",
    "Variables dummy (Ticker)",
    "k (vecinos)",
    "R² Entrenamiento",
    "R² Prueba",
    "RMSE Entrenamiento ($)",
    "RMSE Prueba ($)",
    "MAE Prueba ($)",
    "RMSE Baseline ($)",
    "Mejora vs Baseline (%)",
    "Opciones mejoradas (%)"
  ),
  Valor = c(
    n_train,
    nrow(X_test),
    length(variables_predictoras),
    length(variables_numericas),
    length(variables_dummy),
    mejor_k,
    round(r2_train, 4),
    round(r2_test, 4),
    round(rmse_train, 2),
    round(rmse_test, 2),
    round(mae_test, 2),
    rmse_baseline,
    round(mejora_vs_baseline, 2),
    round(pct_mejoradas, 1)
  )
)
write_xlsx(resumen, file.path(carpeta_tablas, "resumen_ejecutivo_knn_CALL.xlsx"))

cat("Tablas guardadas en:", carpeta_tablas, "\n")

# ============================================
# GENERACIÓN DE GRÁFICOS (ggplot2)
# ============================================

cat("\n===========================================\n")
cat("GENERANDO GRÁFICOS\n")
cat("===========================================\n\n")

# Color para KNN CALL
color_call <- "#E74C3C"

# --- GRÁFICO 1: Optimización de k ---
cat("  Generando gráfico de optimización de k...\n")

p1_tuning_k <- ggplot(resultados_k, aes(x = k, y = RMSE_CV)) +
  geom_line(color = color_call, linewidth = 1) +
  geom_point(color = color_call, size = 3) +
  geom_point(data = resultados_k %>% filter(k == mejor_k),
             aes(x = k, y = RMSE_CV), color = "darkred", size = 5, shape = 18) +
  geom_vline(xintercept = mejor_k, linetype = "dashed", color = "darkred", alpha = 0.7) +
  annotate("text", x = mejor_k * 1.15, y = max(resultados_k$RMSE_CV) * 0.95,
           label = paste("k óptimo =", mejor_k), color = "darkred", size = 4, hjust = 0) +
  labs(
    title = "Optimización del Hiperparámetro k - KNN (CALL)",
    subtitle = paste("Validación cruzada 5-fold | k óptimo =", mejor_k, "| 22 predictores (12 + 10 dummies)"),
    x = "Número de Vecinos (k)",
    y = "RMSE CV ($)"
  ) +
  scale_x_continuous(breaks = k_valores) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-5-1_tuning_k_call.png"),
       p1_tuning_k, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 2: Importancia de Variables (Top 15) ---
cat("  Generando gráfico de importancia de variables...\n")

df_imp_plot <- importancia_perm %>%
  head(15) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

p2_importancia <- ggplot(df_imp_plot, aes(x = Variable, y = Importancia_Normalizada)) +
  geom_bar(stat = "identity", fill = color_call, alpha = 0.8) +
  geom_text(aes(label = paste0(round(Importancia_Normalizada, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 115)) +
  labs(
    title = "Importancia de Variables - KNN (CALL)",
    subtitle = "Medida: Incremento en RMSE al permutar la variable (Top 15)",
    x = NULL,
    y = "Importancia Relativa (%)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-5-1_importancia_variables_call.png"),
       p2_importancia, width = 10, height = 8, dpi = 300)

# --- GRÁFICO 3: Predicciones vs Valores Reales ---
cat("  Generando gráfico de predicciones vs reales...\n")

df_pred_plot <- data.frame(
  Real = y_test,
  Predicho = pred_test
)

# Calcular límites para el gráfico
lim_min <- min(c(df_pred_plot$Real, df_pred_plot$Predicho), na.rm = TRUE)
lim_max <- max(c(df_pred_plot$Real, df_pred_plot$Predicho), na.rm = TRUE)

p3_pred_vs_real <- ggplot(df_pred_plot, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = color_call, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", linewidth = 0.8) +
  coord_fixed(ratio = 1, xlim = c(lim_min, lim_max), ylim = c(lim_min, lim_max)) +
  labs(
    title = "Predicciones vs Valores Reales - KNN (CALL)",
    subtitle = paste("R² =", round(r2_test, 4), "| RMSE = $", round(rmse_test, 2), "| k =", mejor_k),
    x = "Error Real (Diferencia) [$]",
    y = "Error Predicho [$]"
  ) +
  annotate("text", x = lim_max * 0.7, y = lim_min + (lim_max - lim_min) * 0.15,
           label = "Línea roja = predicción perfecta", color = "red", size = 3.5) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-5-1_pred_vs_real_call.png"),
       p3_pred_vs_real, width = 9, height = 9, dpi = 300)

# --- GRÁFICO 4: Distribución de Residuos ---
cat("  Generando gráfico de distribución de residuos...\n")

residuos <- y_test - pred_test
df_residuos <- data.frame(Residuo = residuos)

p4_residuos <- ggplot(df_residuos, aes(x = Residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = color_call, alpha = 0.7, color = "white") +
  geom_density(color = "#2C3E50", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = mean(residuos), linetype = "dotted", color = "blue", linewidth = 1) +
  labs(
    title = "Distribución de Residuos - KNN (CALL)",
    subtitle = paste("Media =", round(mean(residuos), 2), "| SD =", round(sd(residuos), 2)),
    x = "Residuo (Error Real - Error Predicho) [$]",
    y = "Densidad"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-5-1_distribucion_residuos_call.png"),
       p4_residuos, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 5: Comparación de Modelos ---
cat("  Generando gráfico de comparación de modelos...\n")

df_comparacion <- comparacion %>%
  mutate(Modelo = factor(Modelo, levels = Modelo))

p5_comparacion <- ggplot(df_comparacion, aes(x = Modelo, y = RMSE)) +
  geom_bar(stat = "identity", fill = c("#95A5A6", "#95A5A6", "#3498DB", "#27AE60", "#9B59B6", color_call), alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(RMSE, 2))), vjust = -0.5, size = 3.5) +
  labs(
    title = "Comparación de Modelos - RMSE (CALL)",
    subtitle = "Evaluación en conjunto de prueba",
    x = NULL,
    y = "RMSE ($)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-5-1_comparacion_modelos_call.png"),
       p5_comparacion, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 6: Error por Ticker ---
cat("  Generando gráfico de error por Ticker...\n")

df_ticker <- test_call_results %>%
  group_by(Ticker) %>%
  summarise(
    RMSE = sqrt(mean(Error_KNN^2)),
    MAE = mean(abs(Error_KNN)),
    n = n()
  ) %>%
  arrange(RMSE)

p6_ticker <- ggplot(df_ticker, aes(x = reorder(Ticker, RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = color_call, alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(RMSE, 2))), vjust = -0.5, size = 3) +
  labs(
    title = "RMSE por Ticker - KNN (CALL)",
    subtitle = paste("k =", mejor_k, "vecinos | Con one-hot encoding de Ticker"),
    x = "Ticker",
    y = "RMSE ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-5-1_error_por_ticker_call.png"),
       p6_ticker, width = 10, height = 6, dpi = 300)

cat("\n  Todos los gráficos guardados en:", carpeta_graficos, "\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n===========================================\n")
cat("KNN CALL COMPLETADO (con Ticker)\n")
cat("===========================================\n\n")

cat("CONFIGURACIÓN:\n")
cat("  - k (vecinos):", mejor_k, "\n")
cat("  - Predictores totales:", length(variables_predictoras), "\n")
cat("  - Variables numéricas:", length(variables_numericas), "\n")
cat("  - Variables dummy (Ticker):", length(variables_dummy), "\n")
cat("  - Normalización: Z-score (solo numéricas)\n\n")

cat("MÉTRICAS (TEST):\n")
cat("  - RMSE: $", round(rmse_test, 2), "\n")
cat("  - MAE:  $", round(mae_test, 2), "\n")
cat("  - R²:   ", round(r2_test, 4), "\n\n")

cat("MEJORA VS BASELINE:", round(mejora_vs_baseline, 2), "%\n\n")

cat("TOP 5 VARIABLES:\n")
for (i in 1:5) {
  cat("  ", i, ". ", importancia_perm$Variable[i], " (",
      round(importancia_perm$Importancia_Normalizada[i], 1), "%)\n", sep = "")
}

cat("\n===========================================\n")

# ============================================================
# EXPORTAR MÉTRICAS A ARCHIVO PLANO PARA EL DOCUMENTO
# ============================================================
archivo_metricas <- "metricas_KNN_CALL.txt"
sink(archivo_metricas)
cat("=============================================================\n")
cat("MÉTRICAS - KNN - OPCIONES CALL\n")
cat("=============================================================\n\n")

cat("--- Configuración ---\n")
cat("Modelo: K-Nearest Neighbors\n")
cat("Tipo de opción: CALL\n")
cat("N observaciones entrenamiento:", n_train, "\n")
cat("N observaciones prueba:", nrow(test_call), "\n")
cat("k (vecinos):", mejor_k, "\n")
cat("Normalización: Z-score\n")
cat("Métrica de distancia: Euclidiana\n\n")

cat("--- Métricas de Entrenamiento ---\n")
cat("RMSE Train ($):", round(rmse_train, 2), "\n")
cat("MAE Train ($):", round(mae_train, 2), "\n")
cat("R2 Train:", round(r2_train, 4), "\n\n")

cat("--- Métricas de Prueba ---\n")
cat("RMSE Test ($):", round(rmse_test, 2), "\n")
cat("MAE Test ($):", round(mae_test, 2), "\n")
cat("R2 Test:", round(r2_test, 4), "\n\n")

cat("--- Comparación con Baseline ---\n")
cat("RMSE Baseline ($):", rmse_baseline, "\n")
cat("Mejora RMSE vs Baseline (%):", round(mejora_vs_baseline, 2), "\n\n")

cat("--- Efectividad en Ajuste de Precios ---\n")
cat("RMSE Precio BS Original ($):", round(rmse_bs_original, 2), "\n")
cat("RMSE Precio BS + KNN ($):", round(rmse_knn_precio, 2), "\n")
cat("Mejora en precio (%):", round(mejora_vs_bs, 2), "\n")
cat("Opciones mejoradas (%):", round(pct_mejoradas, 1), "\n\n")

cat("--- Top 5 Variables Importantes ---\n")
for (i in 1:min(5, nrow(importancia_perm))) {
  cat(i, ". ", importancia_perm$Variable[i], " (",
      round(importancia_perm$Importancia_Normalizada[i], 1), "%)\n", sep = "")
}
sink()
cat("\nMétricas exportadas a:", archivo_metricas, "\n")
