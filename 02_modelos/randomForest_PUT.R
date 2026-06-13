# ============================================
# RANDOM FOREST - OPCIONES PUT (con Ticker)
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
library(randomForest)
library(writexl)
library(scales)

# Configuración de rutas (AJUSTA SEGÚN TU EQUIPO)
RUTA_ENTRADA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1/sub_3-6"
RUTA_SALIDA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 2/Random_Forest"

# Crear carpeta de salida si no existe
if (!dir.exists(RUTA_SALIDA)) {
  dir.create(RUTA_SALIDA, recursive = TRUE)
}

# Crear subcarpeta para PUT
RUTA_PUT <- file.path(RUTA_SALIDA, "PUT")
if (!dir.exists(RUTA_PUT)) {
  dir.create(RUTA_PUT, recursive = TRUE)
}

# Crear subcarpetas para organizar outputs
carpeta_graficos <- file.path(RUTA_PUT, "graficos")
carpeta_tablas <- file.path(RUTA_PUT, "tablas")

if (!dir.exists(carpeta_graficos)) dir.create(carpeta_graficos, recursive = TRUE)
if (!dir.exists(carpeta_tablas)) dir.create(carpeta_tablas, recursive = TRUE)

set.seed(123)  # Reproducibilidad

cat("====================================\n")
cat("RANDOM FOREST - OPCIONES PUT\n")
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
variables_predictoras <- c(
  # Inputs Black-Scholes (4)
  "Strike", "Precio_Actual", "Dias_Vencimiento", "Volatilidad_Historica",
  # Variables de mercado (3)
  "Vol", "OI", "IV",
  # Variables derivadas (5)
  "Moneyness", "Log_Moneyness", "Vol_Diff", "Strike_Normalizado", "In_The_Money",
  # Categórica (1)
  "Ticker"
)

# Variable objetivo
variable_objetivo <- "Diferencia"

# Verificar que todas las variables existen
vars_faltantes <- setdiff(variables_predictoras, names(train_put))
if (length(vars_faltantes) > 0) {
  stop("Variables faltantes: ", paste(vars_faltantes, collapse = ", "))
}

cat("Variables predictoras (13):\n")
cat("  ", paste(variables_predictoras, collapse = ", "), "\n\n")

# Convertir Ticker a factor
train_put$Ticker <- as.factor(train_put$Ticker)
test_put$Ticker <- as.factor(test_put$Ticker)

cat("Niveles de Ticker:", paste(levels(train_put$Ticker), collapse = ", "), "\n\n")

# Preparar datasets para Random Forest
train_rf <- train_put[, c(variable_objetivo, variables_predictoras)]
test_rf <- test_put[, c(variable_objetivo, variables_predictoras)]

# Verificar NAs
cat("NAs en train:", sum(is.na(train_rf)), "\n")
cat("NAs en test:", sum(is.na(test_rf)), "\n\n")

# ============================================
# OPTIMIZACIÓN DE HIPERPARÁMETROS
# ============================================

cat("===========================================\n")
cat("OPTIMIZACIÓN DE HIPERPARÁMETROS\n")
cat("===========================================\n\n")

# Número de predictores
p <- length(variables_predictoras)
cat("Número de predictores (p):", p, "\n")
cat("Valor por defecto mtry (p/3):", round(p/3), "\n\n")

# Probar diferentes valores de mtry
mtry_valores <- c(3, 4, 5, 6, 7, 8)
resultados_mtry <- data.frame(mtry = integer(), OOB_RMSE = numeric())

cat("Evaluando mtry con ntree=500...\n")

for (m in mtry_valores) {
  cat("  mtry =", m, "...")

  modelo_temp <- randomForest(
    Diferencia ~ .,
    data = train_rf,
    ntree = 500,
    mtry = m,
    importance = FALSE
  )

  oob_rmse <- sqrt(modelo_temp$mse[500])
  resultados_mtry <- rbind(resultados_mtry, data.frame(mtry = m, OOB_RMSE = oob_rmse))
  cat(" OOB RMSE = $", round(oob_rmse, 2), "\n")
}

# Mejor mtry
mejor_mtry <- resultados_mtry$mtry[which.min(resultados_mtry$OOB_RMSE)]
cat("\nMejor mtry:", mejor_mtry, "\n")
cat("OOB RMSE:", round(min(resultados_mtry$OOB_RMSE), 2), "\n\n")

# Guardar resultados de optimización
write_xlsx(resultados_mtry, file.path(carpeta_tablas, "optimizacion_mtry_PUT.xlsx"))

# ============================================
# ENTRENAMIENTO DEL MODELO FINAL
# ============================================

cat("===========================================\n")
cat("ENTRENAMIENTO DEL MODELO FINAL\n")
cat("===========================================\n\n")

# Evaluar convergencia con diferentes ntree
ntree_valores <- c(100, 200, 300, 500)
cat("Evaluando convergencia de ntree...\n")

for (nt in ntree_valores) {
  modelo_temp <- randomForest(
    Diferencia ~ .,
    data = train_rf,
    ntree = nt,
    mtry = mejor_mtry,
    importance = FALSE
  )
  oob_rmse <- sqrt(modelo_temp$mse[nt])
  cat("  ntree =", nt, " -> OOB RMSE = $", round(oob_rmse, 2), "\n")
}

# Modelo final (usar 500 árboles para PUT - convergencia más lenta)
mejor_ntree <- 500
cat("\nEntrenando modelo final con ntree=", mejor_ntree, ", mtry=", mejor_mtry, "...\n")

modelo_rf_put <- randomForest(
  Diferencia ~ .,
  data = train_rf,
  ntree = mejor_ntree,
  mtry = mejor_mtry,
  importance = TRUE,
  keep.forest = TRUE
)

print(modelo_rf_put)

# ============================================
# PREDICCIONES
# ============================================

cat("\n===========================================\n")
cat("PREDICCIONES\n")
cat("===========================================\n\n")

# Predicciones en train y test
pred_train <- predict(modelo_rf_put, train_rf)
pred_test <- predict(modelo_rf_put, test_rf)

# Agregar predicciones al test set
test_put$Pred_RF <- pred_test
test_put$Precio_RF <- test_put$Precio_BS + pred_test
test_put$Error_RF <- test_put$Diferencia - pred_test

# ============================================
# MÉTRICAS DE DESEMPEÑO
# ============================================

cat("===========================================\n")
cat("MÉTRICAS DE DESEMPEÑO\n")
cat("===========================================\n\n")

# Métricas en entrenamiento
rmse_train <- sqrt(mean((train_rf$Diferencia - pred_train)^2))
mae_train <- mean(abs(train_rf$Diferencia - pred_train))
ss_res_train <- sum((train_rf$Diferencia - pred_train)^2)
ss_tot_train <- sum((train_rf$Diferencia - mean(train_rf$Diferencia))^2)
r2_train <- 1 - (ss_res_train / ss_tot_train)

# Métricas en prueba
rmse_test <- sqrt(mean((test_rf$Diferencia - pred_test)^2))
mae_test <- mean(abs(test_rf$Diferencia - pred_test))
ss_res_test <- sum((test_rf$Diferencia - pred_test)^2)
ss_tot_test <- sum((test_rf$Diferencia - mean(test_rf$Diferencia))^2)
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
rmse_baseline <- 103.54  # Del modelo baseline PUT
mejora_vs_baseline <- (1 - rmse_test / rmse_baseline) * 100

cat("COMPARACIÓN CON BASELINE:\n")
cat("  RMSE Baseline: $", rmse_baseline, "\n")
cat("  RMSE RF:       $", round(rmse_test, 2), "\n")
cat("  Mejora:        ", round(mejora_vs_baseline, 2), "%\n\n")

# Comparación con BS Original
rmse_bs_original <- sqrt(mean(test_put$Diferencia^2))
rmse_rf_precio <- sqrt(mean((test_put$Last - test_put$Precio_RF)^2))
mejora_vs_bs <- (1 - rmse_rf_precio / rmse_bs_original) * 100

cat("MEJORA EN PRECIOS AJUSTADOS:\n")
cat("  RMSE BS Original:    $", round(rmse_bs_original, 2), "\n")
cat("  RMSE BS + RF:        $", round(rmse_rf_precio, 2), "\n")
cat("  Mejora:              ", round(mejora_vs_bs, 2), "%\n\n")

# Proporción de opciones mejoradas
mejora_individual <- abs(test_put$Last - test_put$Precio_RF) < abs(test_put$Diferencia)
pct_mejoradas <- mean(mejora_individual) * 100
cat("Opciones con error reducido:", round(pct_mejoradas, 1), "%\n\n")

# ============================================
# IMPORTANCIA DE VARIABLES
# ============================================

cat("===========================================\n")
cat("IMPORTANCIA DE VARIABLES\n")
cat("===========================================\n\n")

importancia <- importance(modelo_rf_put)
importancia_df <- data.frame(
  Variable = rownames(importancia),
  IncMSE = importancia[, "%IncMSE"],
  IncNodePurity = importancia[, "IncNodePurity"]
) %>%
  arrange(desc(IncMSE)) %>%
  mutate(
    Ranking = row_number(),
    Importancia_Normalizada = IncMSE / max(IncMSE) * 100
  )

cat("Top 10 variables por %IncMSE:\n")
print(head(importancia_df, 10), row.names = FALSE)

# Guardar importancia
write_xlsx(importancia_df, file.path(carpeta_tablas, "importancia_variables_PUT.xlsx"))

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
write_xlsx(metricas, file.path(carpeta_tablas, "metricas_rf_PUT.xlsx"))

# Comparación con otros modelos
comparacion <- data.frame(
  Modelo = c("BS Original", "Baseline (C=-$16.34)", "Regresión Lineal", "Random Forest"),
  RMSE = c(105.02, 103.54, 31.43, rmse_test),
  MAE = c(22.78, 34.06, 16.02, mae_test)
) %>%
  mutate(Mejora_vs_Baseline = round((1 - RMSE / 103.54) * 100, 2))

write_xlsx(comparacion, file.path(carpeta_tablas, "comparacion_modelos_PUT.xlsx"))

# Resultados del test set
write_xlsx(test_put, file.path(carpeta_tablas, "resultados_test_rf_PUT.xlsx"))

# Guardar modelo
saveRDS(modelo_rf_put, file.path(RUTA_PUT, "modelo_rf_PUT.rds"))

# Resumen ejecutivo
resumen <- data.frame(
  Metrica = c(
    "Observaciones entrenamiento",
    "Observaciones prueba",
    "Número de predictores",
    "ntree (árboles)",
    "mtry (variables/nodo)",
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
    nrow(train_rf),
    nrow(test_rf),
    p,
    mejor_ntree,
    mejor_mtry,
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
write_xlsx(resumen, file.path(carpeta_tablas, "resumen_ejecutivo_rf_PUT.xlsx"))

cat("Tablas guardadas en:", carpeta_tablas, "\n")

# ============================================
# GENERACIÓN DE GRÁFICOS (ggplot2)
# ============================================

cat("\n===========================================\n")
cat("GENERANDO GRÁFICOS\n")
cat("===========================================\n\n")

# --- GRÁFICO 1: Convergencia del error OOB ---
cat("  Generando gráfico de convergencia OOB...\n")

df_convergencia <- data.frame(
  Arboles = 1:mejor_ntree,
  RMSE_OOB = sqrt(modelo_rf_put$mse)
)

rmse_final <- df_convergencia$RMSE_OOB[mejor_ntree]

p1_convergencia <- ggplot(df_convergencia, aes(x = Arboles, y = RMSE_OOB)) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_hline(yintercept = rmse_final,
             linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = mejor_ntree * 0.75, y = rmse_final * 1.03,
           label = paste("RMSE final: $", round(rmse_final, 2)),
           color = "red", size = 3.5) +
  labs(
    title = "Convergencia del Error OOB - Random Forest (PUT)",
    subtitle = paste("ntree =", mejor_ntree, "| mtry =", mejor_mtry, "| 13 predictores"),
    x = "Número de Árboles",
    y = "RMSE OOB ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_convergencia_oob_PUT.png"),
       p1_convergencia, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 2: Importancia de Variables ---
cat("  Generando gráfico de importancia de variables...\n")

top_n_vars <- 13  # Todas las variables
df_imp_plot <- importancia_df %>%
  head(top_n_vars) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))

p2_importancia <- ggplot(df_imp_plot, aes(x = Variable, y = Importancia_Normalizada)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Importancia_Normalizada, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 115)) +
  labs(
    title = "Importancia de Variables - Random Forest (PUT)",
    subtitle = "Medida: %IncMSE (incremento porcentual en MSE al permutar la variable)",
    x = NULL,
    y = "Importancia Relativa (%)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_importancia_variables_PUT.png"),
       p2_importancia, width = 10, height = 7, dpi = 300)

# --- GRÁFICO 3: Predicciones vs Valores Reales ---
cat("  Generando gráfico de predicciones vs reales...\n")

df_pred_plot <- data.frame(
  Real = test_rf$Diferencia,
  Predicho = pred_test
)

# Calcular límites para el gráfico
lim_min <- min(c(df_pred_plot$Real, df_pred_plot$Predicho), na.rm = TRUE)
lim_max <- max(c(df_pred_plot$Real, df_pred_plot$Predicho), na.rm = TRUE)

p3_pred_vs_real <- ggplot(df_pred_plot, aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = "#3498DB", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "#2C3E50", linewidth = 0.8) +
  coord_fixed(ratio = 1, xlim = c(lim_min, lim_max), ylim = c(lim_min, lim_max)) +
  labs(
    title = "Predicciones vs Valores Reales - Random Forest (PUT)",
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

residuos <- test_rf$Diferencia - pred_test
df_residuos <- data.frame(Residuo = residuos)

p4_residuos <- ggplot(df_residuos, aes(x = Residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#3498DB", alpha = 0.7, color = "white") +
  geom_density(color = "#2C3E50", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = mean(residuos), linetype = "dotted", color = "blue", linewidth = 1) +
  labs(
    title = "Distribución de Residuos - Random Forest (PUT)",
    subtitle = paste("Media =", round(mean(residuos), 2), "| SD =", round(sd(residuos), 2)),
    x = "Residuo (Error Real - Error Predicho) [$]",
    y = "Densidad"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_distribucion_residuos_PUT.png"),
       p4_residuos, width = 10, height = 6, dpi = 300)

# --- GRÁFICO 5: Tuning de mtry ---
cat("  Generando gráfico de tuning mtry...\n")

p5_tuning_mtry <- ggplot(resultados_mtry, aes(x = factor(mtry), y = OOB_RMSE)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(OOB_RMSE, 2))), vjust = -0.5, size = 3.5) +
  geom_point(data = resultados_mtry %>% filter(mtry == mejor_mtry),
             aes(x = factor(mtry), y = OOB_RMSE), color = "red", size = 4) +
  labs(
    title = "Optimización de mtry - Random Forest (PUT)",
    subtitle = paste("Óptimo: mtry =", mejor_mtry, "(p = 13 predictores)"),
    x = "mtry (variables por split)",
    y = "RMSE OOB ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_tuning_mtry_PUT.png"),
       p5_tuning_mtry, width = 9, height = 6, dpi = 300)

cat("\n  Todos los gráficos guardados en:", carpeta_graficos, "\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n===========================================\n")
cat("RANDOM FOREST PUT COMPLETADO\n")
cat("===========================================\n\n")

cat("CONFIGURACIÓN:\n")
cat("  - ntree:", mejor_ntree, "\n")
cat("  - mtry:", mejor_mtry, "\n")
cat("  - Predictores:", p, "\n\n")

cat("MÉTRICAS (TEST):\n")
cat("  - RMSE: $", round(rmse_test, 2), "\n")
cat("  - MAE:  $", round(mae_test, 2), "\n")
cat("  - R²:   ", round(r2_test, 4), "\n\n")

cat("MEJORA VS BASELINE:", round(mejora_vs_baseline, 2), "%\n\n")

cat("TOP 5 VARIABLES:\n")
for (i in 1:5) {
  cat("  ", i, ". ", importancia_df$Variable[i], " (",
      round(importancia_df$Importancia_Normalizada[i], 1), "%)\n", sep = "")
}

cat("\n===========================================\n")

# ============================================================
# EXPORTAR MÉTRICAS A ARCHIVO PLANO PARA EL DOCUMENTO
# ============================================================
archivo_metricas <- "metricas_randomForest_PUT.txt"
sink(archivo_metricas)
cat("=============================================================\n")
cat("MÉTRICAS - RANDOM FOREST - OPCIONES PUT\n")
cat("=============================================================\n\n")

cat("--- Configuración ---\n")
cat("Modelo: Random Forest\n")
cat("Tipo de opción: PUT\n")
cat("N observaciones entrenamiento:", nrow(train_rf), "\n")
cat("N observaciones prueba:", nrow(test_rf), "\n")
cat("ntree:", mejor_ntree, "\n")
cat("mtry:", mejor_mtry, "\n\n")

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
cat("RMSE Precio BS + RF ($):", round(rmse_rf_precio, 2), "\n")
cat("Mejora en precio (%):", round(mejora_vs_bs, 2), "\n")
cat("Opciones mejoradas (%):", round(pct_mejoradas, 1), "\n\n")

cat("--- Top 5 Variables Importantes ---\n")
for (i in 1:min(5, nrow(importancia_df))) {
  cat(i, ". ", importancia_df$Variable[i], " (",
      round(importancia_df$Importancia_Normalizada[i], 1), "%)\n", sep = "")
}
sink()
cat("\nMétricas exportadas a:", archivo_metricas, "\n")
