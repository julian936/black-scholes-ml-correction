# ============================================
# XGBOOST - OPCIONES PUT (con Ticker)
# Capítulo 4 - Objetivo 2
# Versión con gráficos ggplot2 profesionales
# ============================================

# Limpiar entorno
rm(list = ls())

# ============================================
# CONFIGURACIÓN Y LIBRERÍAS
# ============================================

library(readxl)
library(tidyverse)
library(xgboost)
library(writexl)
library(scales)
library(caret)

# Configuración de rutas
RUTA_ENTRADA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1/sub_3-6"
RUTA_SALIDA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 2/XGBoost/Put"

# Crear carpetas si no existen
if (!dir.exists(RUTA_SALIDA)) {
  dir.create(RUTA_SALIDA, recursive = TRUE)
}

carpeta_graficos <- file.path(RUTA_SALIDA, "graficos")
carpeta_tablas <- file.path(RUTA_SALIDA, "tablas")

if (!dir.exists(carpeta_graficos)) dir.create(carpeta_graficos, recursive = TRUE)
if (!dir.exists(carpeta_tablas)) dir.create(carpeta_tablas, recursive = TRUE)

set.seed(123)  # Reproducibilidad

cat("====================================\n")
cat("XGBOOST - OPCIONES PUT\n")
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

cat("Cargando datos de entrenamiento y prueba PUT...\n")

train_put <- read_xlsx(file.path(RUTA_ENTRADA, "train_data_PUT.xlsx"))
test_put <- read_xlsx(file.path(RUTA_ENTRADA, "test_data_PUT.xlsx"))

cat("  Train PUT:", nrow(train_put), "observaciones\n")
cat("  Test PUT:", nrow(test_put), "observaciones\n\n")

# ============================================
# PREPARACIÓN DE VARIABLES
# ============================================

# Definir las 13 variables predictoras (12 numéricas + 1 categórica)
variables_numericas <- c(
  "Strike", "Precio_Actual", "Dias_Vencimiento", "Volatilidad_Historica",
  "Vol", "OI", "IV",
  "Moneyness", "Log_Moneyness", "Vol_Diff", "Strike_Normalizado", "In_The_Money"
)

variable_categorica <- "Ticker"
variable_objetivo <- "Diferencia"

cat("Variables numéricas (12):\n")
cat("  ", paste(variables_numericas, collapse = ", "), "\n")
cat("Variable categórica (1): Ticker\n\n")

# Convertir Ticker a factor y luego a dummy variables
train_put$Ticker <- as.factor(train_put$Ticker)
test_put$Ticker <- as.factor(test_put$Ticker)

# Crear dummy variables para Ticker
dummy_train <- model.matrix(~ Ticker - 1, data = train_put)
dummy_test <- model.matrix(~ Ticker - 1, data = test_put)

# Preparar matrices para XGBoost
X_train <- as.matrix(cbind(train_put[, variables_numericas], dummy_train))
X_test <- as.matrix(cbind(test_put[, variables_numericas], dummy_test))
y_train <- train_put[[variable_objetivo]]
y_test <- test_put[[variable_objetivo]]

cat("Dimensiones X_train:", dim(X_train), "\n")
cat("Dimensiones X_test:", dim(X_test), "\n")
cat("Total de features:", ncol(X_train), "(12 numéricas + 10 dummies de Ticker)\n\n")

# Crear DMatrix para XGBoost
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest <- xgb.DMatrix(data = X_test, label = y_test)

# ============================================
# OPTIMIZACIÓN DE HIPERPARÁMETROS
# ============================================

cat("===========================================\n")
cat("OPTIMIZACIÓN DE HIPERPARÁMETROS\n")
cat("===========================================\n\n")

# Grid de hiperparámetros a evaluar
param_grid <- expand.grid(
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(4, 6, 8),
  subsample = c(0.8),
  colsample_bytree = c(0.8)
)

cat("Evaluando", nrow(param_grid), "combinaciones de hiperparámetros...\n\n")

resultados_cv <- data.frame()

for (i in 1:nrow(param_grid)) {
  params <- list(
    objective = "reg:squarederror",
    eta = param_grid$eta[i],
    max_depth = param_grid$max_depth[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  cv_result <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 500,
    nfold = 5,
    early_stopping_rounds = 20,
    verbose = 0,
    print_every_n = 100
  )
  
  best_rmse <- min(cv_result$evaluation_log$test_rmse_mean)
  best_iter <- which.min(cv_result$evaluation_log$test_rmse_mean)
  
  resultados_cv <- rbind(resultados_cv, data.frame(
    eta = param_grid$eta[i],
    max_depth = param_grid$max_depth[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i],
    best_rmse = best_rmse,
    best_iter = best_iter
  ))
  
  cat(sprintf("  eta=%.2f, max_depth=%d -> RMSE=$%.2f (iter=%d)\n",
              param_grid$eta[i], param_grid$max_depth[i], best_rmse, best_iter))
}

# Mejor combinación
mejor_idx <- which.min(resultados_cv$best_rmse)
mejor_params <- resultados_cv[mejor_idx, ]

cat("\n===========================================\n")
cat("MEJORES HIPERPARÁMETROS:\n")
cat("  eta:", mejor_params$eta, "\n")
cat("  max_depth:", mejor_params$max_depth, "\n")
cat("  subsample:", mejor_params$subsample, "\n")
cat("  colsample_bytree:", mejor_params$colsample_bytree, "\n")
cat("  Best RMSE CV: $", round(mejor_params$best_rmse, 2), "\n")
cat("  Best iteration:", mejor_params$best_iter, "\n")
cat("===========================================\n\n")

# Guardar resultados de optimización
write_xlsx(resultados_cv, file.path(carpeta_tablas, "optimizacion_hiperparametros_PUT.xlsx"))

# ============================================
# ENTRENAMIENTO DEL MODELO FINAL
# ============================================

cat("===========================================\n")
cat("ENTRENAMIENTO DEL MODELO FINAL\n")
cat("===========================================\n\n")

params_final <- list(
  objective = "reg:squarederror",
  eta = mejor_params$eta,
  max_depth = mejor_params$max_depth,
  subsample = mejor_params$subsample,
  colsample_bytree = mejor_params$colsample_bytree
)

# Entrenar modelo final
nrounds_final <- mejor_params$best_iter + 50  # Agregar margen

watchlist <- list(train = dtrain, test = dtest)

modelo_xgb_put <- xgb.train(
  params = params_final,
  data = dtrain,
  nrounds = nrounds_final,
  watchlist = watchlist,
  verbose = 0
)

cat("Modelo entrenado con", nrounds_final, "rondas\n\n")

# ============================================
# PREDICCIONES
# ============================================

cat("===========================================\n")
cat("PREDICCIONES\n")
cat("===========================================\n\n")

pred_train <- predict(modelo_xgb_put, dtrain)
pred_test <- predict(modelo_xgb_put, dtest)

# Agregar predicciones al test set
test_put$Pred_XGB <- pred_test
test_put$Precio_XGB <- test_put$Precio_BS + pred_test
test_put$Error_XGB <- test_put$Diferencia - pred_test

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

# Comparación con baseline y otros modelos
rmse_baseline <- 103.54
rmse_reglineal <- 31.43
rmse_rf <- 11.06

mejora_vs_baseline <- (1 - rmse_test / rmse_baseline) * 100
mejora_vs_reglineal <- (1 - rmse_test / rmse_reglineal) * 100
mejora_vs_rf <- (1 - rmse_test / rmse_rf) * 100

cat("COMPARACIÓN CON OTROS MODELOS:\n")
cat("  RMSE Baseline:        $", rmse_baseline, " -> Mejora:", round(mejora_vs_baseline, 2), "%\n")
cat("  RMSE Reg. Lineal:     $", rmse_reglineal, " -> Mejora:", round(mejora_vs_reglineal, 2), "%\n")
cat("  RMSE Random Forest:   $", rmse_rf, " -> Mejora:", round(mejora_vs_rf, 2), "%\n\n")

# Proporción de opciones mejoradas
mejora_individual <- abs(test_put$Last - test_put$Precio_XGB) < abs(test_put$Diferencia)
pct_mejoradas <- mean(mejora_individual) * 100
cat("Opciones con error reducido:", round(pct_mejoradas, 1), "%\n\n")

# ============================================
# IMPORTANCIA DE VARIABLES
# ============================================

cat("===========================================\n")
cat("IMPORTANCIA DE VARIABLES\n")
cat("===========================================\n\n")

importancia <- xgb.importance(model = modelo_xgb_put)

# Agregar importancia normalizada
importancia$Importancia_Normalizada <- importancia$Gain / max(importancia$Gain) * 100

cat("Top 10 variables por Gain:\n")
print(head(importancia, 10))

# Guardar importancia
write_xlsx(as.data.frame(importancia), file.path(carpeta_tablas, "importancia_variables_PUT.xlsx"))

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
write_xlsx(metricas, file.path(carpeta_tablas, "metricas_xgb_PUT.xlsx"))

# Comparación con otros modelos
comparacion <- data.frame(
  Modelo = c("BS Original", "Baseline (C=-$16.34)", "Regresión Lineal", "Random Forest", "XGBoost"),
  RMSE = c(105.02, 103.54, 31.43, 11.06, rmse_test),
  MAE = c(22.78, 34.06, 16.02, 2.64, mae_test)
) %>%
  mutate(Mejora_vs_Baseline = round((1 - RMSE / 103.54) * 100, 2))

write_xlsx(comparacion, file.path(carpeta_tablas, "comparacion_modelos_PUT.xlsx"))

# Resultados del test set
write_xlsx(test_put, file.path(carpeta_tablas, "resultados_test_xgb_PUT.xlsx"))

# Guardar modelo
xgb.save(modelo_xgb_put, file.path(RUTA_SALIDA, "modelo_xgb_PUT.model"))

# Resumen ejecutivo
resumen <- data.frame(
  Metrica = c(
    "Observaciones entrenamiento",
    "Observaciones prueba",
    "Número de features",
    "eta (learning rate)",
    "max_depth",
    "subsample",
    "colsample_bytree",
    "nrounds",
    "R² Entrenamiento",
    "R² Prueba",
    "RMSE Entrenamiento ($)",
    "RMSE Prueba ($)",
    "MAE Prueba ($)",
    "RMSE Baseline ($)",
    "Mejora vs Baseline (%)",
    "Mejora vs Random Forest (%)",
    "Opciones mejoradas (%)"
  ),
  Valor = c(
    nrow(X_train),
    nrow(X_test),
    ncol(X_train),
    mejor_params$eta,
    mejor_params$max_depth,
    mejor_params$subsample,
    mejor_params$colsample_bytree,
    nrounds_final,
    round(r2_train, 4),
    round(r2_test, 4),
    round(rmse_train, 2),
    round(rmse_test, 2),
    round(mae_test, 2),
    rmse_baseline,
    round(mejora_vs_baseline, 2),
    round(mejora_vs_rf, 2),
    round(pct_mejoradas, 1)
  )
)
write_xlsx(resumen, file.path(carpeta_tablas, "resumen_ejecutivo_xgb_PUT.xlsx"))

cat("Tablas guardadas en:", carpeta_tablas, "\n")

# ============================================
# GENERACIÓN DE GRÁFICOS (ggplot2)
# ============================================

cat("\n===========================================\n")
cat("GENERANDO GRÁFICOS\n")
cat("===========================================\n\n")

# --- GRÁFICO 1: Evolución del error durante entrenamiento ---
cat("  Generando gráfico de evolución del error...\n")

# Obtener log de evaluación
eval_log <- modelo_xgb_put$evaluation_log

df_evolucion <- data.frame(
  Iteracion = eval_log$iter,
  Train_RMSE = sqrt(eval_log$train_rmse),
  Test_RMSE = sqrt(eval_log$test_rmse)
) %>%
  pivot_longer(cols = c(Train_RMSE, Test_RMSE), 
               names_to = "Conjunto", 
               values_to = "RMSE")

p1_evolucion <- ggplot(df_evolucion, aes(x = Iteracion, y = RMSE, color = Conjunto)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Train_RMSE" = "#3498DB", "Test_RMSE" = "#E74C3C"),
                     labels = c("Entrenamiento", "Prueba")) +
  labs(
    title = "Evolución del Error durante Entrenamiento - XGBoost (PUT)",
    subtitle = paste("eta =", mejor_params$eta, "| max_depth =", mejor_params$max_depth, 
                     "| nrounds =", nrounds_final),
    x = "Número de Iteraciones (Boosting Rounds)",
    y = "RMSE ($)",
    color = "Conjunto"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_evolucion_error_PUT.png"),
       p1_evolucion, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 2: Importancia de Variables ---
cat("  Generando gráfico de importancia de variables...\n")

top_n_vars <- min(15, nrow(importancia))
df_imp_plot <- importancia %>%
  head(top_n_vars) %>%
  mutate(Feature = factor(Feature, levels = rev(Feature)))

p2_importancia <- ggplot(df_imp_plot, aes(x = Feature, y = Importancia_Normalizada)) +
  geom_bar(stat = "identity", fill = "#9B59B6", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Importancia_Normalizada, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 115)) +
  labs(
    title = "Importancia de Variables - XGBoost (PUT)",
    subtitle = "Medida: Gain (ganancia en reducción de pérdida)",
    x = NULL,
    y = "Importancia Relativa (%)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_importancia_variables_PUT.png"),
       p2_importancia, width = 10, height = 8, dpi = 300)

# --- GRÁFICO 3: Predicciones vs Valores Reales ---
cat("  Generando gráfico de predicciones vs reales...\n")

df_pred_plot <- data.frame(
  Real = y_test,
  Predicho = pred_test
)

lim_min <- min(c(df_pred_plot$Real, df_pred_plot$Predicho), na.rm = TRUE)
lim_max <- max(c(df_pred_plot$Real, df_pred_plot$Predicho), na.rm = TRUE)

p3_pred_vs_real <- ggplot(df_pred_plot, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = "#9B59B6", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", linewidth = 0.8) +
  coord_fixed(ratio = 1, xlim = c(lim_min, lim_max), ylim = c(lim_min, lim_max)) +
  labs(
    title = "Predicciones vs Valores Reales - XGBoost (PUT)",
    subtitle = paste("R² =", round(r2_test, 4), "| RMSE = $", round(rmse_test, 2)),
    x = "Error Real (Diferencia) [$]",
    y = "Error Predicho [$]"
  ) +
  annotate("text", x = lim_max * 0.7, y = lim_min + (lim_max - lim_min) * 0.15,
           label = "Línea roja = predicción perfecta", color = "red", size = 3.5) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_pred_vs_real_PUT.png"),
       p3_pred_vs_real, width = 9, height = 9, dpi = 300)

# --- GRÁFICO 4: Distribución de Residuos ---
cat("  Generando gráfico de distribución de residuos...\n")

residuos <- y_test - pred_test
df_residuos <- data.frame(Residuo = residuos)

p4_residuos <- ggplot(df_residuos, aes(x = Residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, 
                 fill = "#9B59B6", alpha = 0.7, color = "white") +
  geom_density(color = "#2C3E50", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = mean(residuos), linetype = "dotted", color = "blue", linewidth = 1) +
  labs(
    title = "Distribución de Residuos - XGBoost (PUT)",
    subtitle = paste("Media =", round(mean(residuos), 2), "| SD =", round(sd(residuos), 2)),
    x = "Residuo (Error Real - Error Predicho) [$]",
    y = "Densidad"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_distribucion_residuos_PUT.png"),
       p4_residuos, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 5: Comparación de Modelos ---
cat("  Generando gráfico de comparación de modelos...\n")

df_comparacion <- data.frame(
  Modelo = factor(c("BS Original", "Baseline", "Reg. Lineal", "Random Forest", "XGBoost"),
                  levels = c("BS Original", "Baseline", "Reg. Lineal", "Random Forest", "XGBoost")),
  RMSE = c(105.02, 103.54, 31.43, 11.06, rmse_test)
)

p5_comparacion <- ggplot(df_comparacion, aes(x = Modelo, y = RMSE, fill = Modelo)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(RMSE, 2))), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("#E74C3C", "#F39C12", "#3498DB", "#27AE60", "#9B59B6")) +
  scale_y_continuous(limits = c(0, 120)) +
  labs(
    title = "Comparación de RMSE entre Modelos - Opciones PUT",
    subtitle = "Evolución del error de predicción a través de los modelos",
    x = NULL,
    y = "RMSE ($)"
  ) +
  tema_tesis +
  theme(legend.position = "none")

ggsave(file.path(carpeta_graficos, "fig_comparacion_modelos_PUT.png"),
       p5_comparacion, width = 10, height = 6, dpi = 300)

cat("\n  Todos los gráficos guardados en:", carpeta_graficos, "\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n===========================================\n")
cat("XGBOOST PUT COMPLETADO\n")
cat("===========================================\n\n")

cat("CONFIGURACIÓN:\n")
cat("  - eta:", mejor_params$eta, "\n")
cat("  - max_depth:", mejor_params$max_depth, "\n")
cat("  - nrounds:", nrounds_final, "\n")
cat("  - Features:", ncol(X_train), "\n\n")

cat("MÉTRICAS (TEST):\n")
cat("  - RMSE: $", round(rmse_test, 2), "\n")
cat("  - MAE:  $", round(mae_test, 2), "\n")
cat("  - R²:   ", round(r2_test, 4), "\n\n")

cat("MEJORAS:\n")
cat("  - vs Baseline:      ", round(mejora_vs_baseline, 2), "%\n")
cat("  - vs Random Forest: ", round(mejora_vs_rf, 2), "%\n\n")

cat("TOP 5 VARIABLES:\n")
for (i in 1:min(5, nrow(importancia))) {
  cat("  ", i, ". ", importancia$Feature[i], " (", 
      round(importancia$Importancia_Normalizada[i], 1), "%)\n", sep = "")
}

cat("\n===========================================\n")

# ============================================================
# EXPORTAR MÉTRICAS A ARCHIVO PLANO PARA EL DOCUMENTO
# ============================================================
archivo_metricas <- "metricas_xgboost_PUT.txt"
sink(archivo_metricas)
cat("=============================================================\n")
cat("MÉTRICAS - XGBOOST - OPCIONES PUT\n")
cat("=============================================================\n\n")

cat("--- Configuración ---\n")
cat("Modelo: XGBoost\n")
cat("Tipo de opción: PUT\n")
cat("N observaciones entrenamiento:", nrow(train_put), "\n")
cat("N observaciones prueba:", nrow(test_put), "\n")
cat("eta:", mejor_params$eta, "\n")
cat("max_depth:", mejor_params$max_depth, "\n")
cat("nrounds:", nrounds_final, "\n")
cat("subsample:", mejor_params$subsample, "\n")
cat("colsample_bytree:", mejor_params$colsample_bytree, "\n\n")

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

cat("--- Opciones Mejoradas ---\n")
cat("Opciones mejoradas (%):", round(pct_mejoradas, 1), "\n\n")

cat("--- Top 5 Variables Importantes ---\n")
for (i in 1:min(5, nrow(importancia))) {
  cat(i, ". ", importancia$Feature[i], " (",
      round(importancia$Importancia_Normalizada[i], 1), "%)\n", sep = "")
}
sink()
cat("\nMétricas exportadas a:", archivo_metricas, "\n")
