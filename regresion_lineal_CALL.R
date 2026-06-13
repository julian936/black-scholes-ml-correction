# ============================================
# REGRESIÓN LINEAL MÚLTIPLE - OPCIONES CALL
# Capítulo 4 - Objetivo 2
# ============================================

# Limpiar entorno
rm(list = ls())

# ============================================
# CONFIGURACIÓN Y LIBRERÍAS
# ============================================

library(readxl)
library(tidyverse)
library(writexl)
library(ggplot2)
library(gridExtra)
library(car)        # Para VIF
library(lmtest)     # Para pruebas de diagnóstico
library(nortest)    # Para pruebas de normalidad

# Configuración de rutas
RUTA_ENTRADA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1/sub_3-6"
RUTA_SALIDA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 2/Regresion_lineal"

# Crear carpeta de salida si no existe
if (!dir.exists(RUTA_SALIDA)) {
  dir.create(RUTA_SALIDA, recursive = TRUE)
  cat("Carpeta de salida creada:", RUTA_SALIDA, "\n")
}

# Crear subcarpeta para CALL
RUTA_CALL <- file.path(RUTA_SALIDA, "CALL")
if (!dir.exists(RUTA_CALL)) {
  dir.create(RUTA_CALL, recursive = TRUE)
}

set.seed(123)  # Reproducibilidad

cat("====================================\n")
cat("REGRESIÓN LINEAL MÚLTIPLE - CALL\n")
cat("====================================\n\n")

# ============================================
# 4.2.2 CARGA DE DATOS
# ============================================

cat("Cargando datos de entrenamiento y prueba CALL...\n")

train_call <- read_xlsx(file.path(RUTA_ENTRADA, "train_data_CALL.xlsx"))
test_call <- read_xlsx(file.path(RUTA_ENTRADA, "test_data_CALL.xlsx"))

cat("  Train CALL:", nrow(train_call), "observaciones\n")
cat("  Test CALL:", nrow(test_call), "observaciones\n\n")

# Verificar columnas disponibles
cat("Columnas disponibles:\n")
print(names(train_call))
cat("\n")

# ============================================
# 4.2.3 CONSTRUCCIÓN Y ENTRENAMIENTO DEL MODELO
# ============================================

cat("===========================================\n")
cat("CONSTRUCCIÓN DEL MODELO DE REGRESIÓN LINEAL\n")
cat("===========================================\n\n")

# Definir la fórmula del modelo
# Excluimos: Ticker (ya que puede tener muchos niveles y usaremos dummies)
# Variable objetivo: Diferencia

# Verificar si las variables necesarias existen
variables_modelo <- c("Diferencia", "Strike", "Precio_Actual", "Dias_Vencimiento",
                      "Volatilidad_Historica", "Tasa_Libre_Riesgo", "Vol", "OI", "IV",
                      "Moneyness", "Log_Moneyness", "Vol_Diff", "Strike_Normalizado",
                      "In_The_Money", "Ticker")

# Verificar variables disponibles
vars_disponibles <- intersect(variables_modelo, names(train_call))
vars_faltantes <- setdiff(variables_modelo, names(train_call))

if (length(vars_faltantes) > 0) {
  cat("ADVERTENCIA: Variables faltantes:", paste(vars_faltantes, collapse = ", "), "\n")
}

cat("Variables a utilizar:", paste(vars_disponibles, collapse = ", "), "\n\n")

# Convertir Ticker a factor si existe
if ("Ticker" %in% names(train_call)) {
  train_call$Ticker <- as.factor(train_call$Ticker)
  test_call$Ticker <- as.factor(test_call$Ticker)
}

# Definir fórmula del modelo
formula_lm <- Diferencia ~ Strike + Precio_Actual + Dias_Vencimiento +
  Volatilidad_Historica + Tasa_Libre_Riesgo +
  Vol + OI + IV + Moneyness + Log_Moneyness +
  Vol_Diff + Strike_Normalizado + In_The_Money + Ticker

# Ajustar modelo de regresión lineal
cat("Ajustando modelo de regresión lineal...\n")
modelo_lm_call <- lm(formula_lm, data = train_call)

# Resumen del modelo
cat("\n--- RESUMEN DEL MODELO ---\n")
summary_modelo <- summary(modelo_lm_call)
print(summary_modelo)

# ============================================
# TABLA DE COEFICIENTES
# ============================================

cat("\n===========================================\n")
cat("TABLA DE COEFICIENTES ESTIMADOS\n")
cat("===========================================\n\n")

# Extraer coeficientes
coeficientes <- as.data.frame(summary_modelo$coefficients)
coeficientes$Variable <- rownames(coeficientes)
rownames(coeficientes) <- NULL

# Renombrar columnas
names(coeficientes) <- c("Coeficiente", "SE", "t_valor", "p_valor", "Variable")
coeficientes <- coeficientes[, c("Variable", "Coeficiente", "SE", "t_valor", "p_valor")]

# Agregar códigos de significancia
coeficientes$Significancia <- ifelse(coeficientes$p_valor < 0.001, "***",
                                     ifelse(coeficientes$p_valor < 0.01, "**",
                                            ifelse(coeficientes$p_valor < 0.05, "*",
                                                   ifelse(coeficientes$p_valor < 0.1, ".", ""))))

# Redondear valores
coeficientes$Coeficiente <- round(coeficientes$Coeficiente, 6)
coeficientes$SE <- round(coeficientes$SE, 6)
coeficientes$t_valor <- round(coeficientes$t_valor, 2)
coeficientes$p_valor <- ifelse(coeficientes$p_valor < 0.001, "<0.001",
                               round(coeficientes$p_valor, 4))

print(coeficientes, row.names = FALSE)

# Guardar tabla de coeficientes
write_xlsx(coeficientes, file.path(RUTA_CALL, "tabla_4-X_coeficientes_lm_CALL.xlsx"))
cat("\nTabla de coeficientes guardada.\n")

# ============================================
# 4.2.4 VALIDACIÓN DE SUPUESTOS
# ============================================

cat("\n===========================================\n")
cat("VALIDACIÓN DE SUPUESTOS\n")
cat("===========================================\n\n")

# Obtener residuos y valores ajustados
residuos <- residuals(modelo_lm_call)
valores_ajustados <- fitted(modelo_lm_call)
residuos_estandarizados <- rstandard(modelo_lm_call)

# --- LINEALIDAD ---
cat("1. LINEALIDAD\n")
cat("   Evaluación mediante gráfico de residuos vs valores ajustados\n")

# Gráfico de residuos vs valores ajustados
df_residuos <- data.frame(
  Ajustados = valores_ajustados,
  Residuos = residuos
)

p_linealidad <- ggplot(df_residuos, aes(x = Ajustados, y = Residuos)) +
  geom_point(alpha = 0.3, color = "#3498DB", size = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#E74C3C", linewidth = 1) +
  labs(
    title = "Residuos vs Valores Ajustados - CALL",
    subtitle = "Evaluación de linealidad",
    x = "Valores Ajustados ($)",
    y = "Residuos ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(RUTA_CALL, "fig_4-X_linealidad_CALL.png"),
       p_linealidad, width = 10, height = 7, dpi = 300)
cat("   Gráfico guardado: fig_4-X_linealidad_CALL.png\n\n")

# --- HOMOCEDASTICIDAD ---
cat("2. HOMOCEDASTICIDAD\n")

# Test de Breusch-Pagan
bp_test <- bptest(modelo_lm_call)
cat("   Test de Breusch-Pagan:\n")
cat("     Estadístico BP:", round(bp_test$statistic, 4), "\n")
cat("     p-valor:", format(bp_test$p.value, scientific = TRUE), "\n")
cat("     Conclusión:", ifelse(bp_test$p.value < 0.05,
                               "Se rechaza homocedasticidad (heterocedasticidad presente)",
                               "No se rechaza homocedasticidad"), "\n\n")

# Gráfico Scale-Location
df_scale <- data.frame(
  Ajustados = valores_ajustados,
  Residuos_Sqrt = sqrt(abs(residuos_estandarizados))
)

p_homocedasticidad <- ggplot(df_scale, aes(x = Ajustados, y = Residuos_Sqrt)) +
  geom_point(alpha = 0.3, color = "#27AE60", size = 1) +
  geom_smooth(method = "loess", se = TRUE, color = "#E74C3C", linewidth = 1) +
  labs(
    title = "Scale-Location - CALL",
    subtitle = "Evaluación de homocedasticidad",
    x = "Valores Ajustados ($)",
    y = expression(sqrt("|Residuos Estandarizados|"))
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(RUTA_CALL, "fig_4-X_homocedasticidad_CALL.png"),
       p_homocedasticidad, width = 10, height = 7, dpi = 300)
cat("   Gráfico guardado: fig_4-X_homocedasticidad_CALL.png\n\n")

# --- NORMALIDAD DE RESIDUOS ---
cat("3. NORMALIDAD DE RESIDUOS\n")

# Muestra para pruebas (máximo 5000 para Anderson-Darling)
muestra_residuos <- sample(residuos, min(5000, length(residuos)))

# Test de Anderson-Darling
ad_test <- ad.test(muestra_residuos)
cat("   Test de Anderson-Darling (muestra n=", length(muestra_residuos), "):\n", sep = "")
cat("     Estadístico A:", round(ad_test$statistic, 4), "\n")
cat("     p-valor:", format(ad_test$p.value, scientific = TRUE), "\n")

# Estadísticas de asimetría y curtosis
skewness_val <- moments::skewness(residuos)
kurtosis_val <- moments::kurtosis(residuos) - 3  # Exceso de curtosis

cat("   Asimetría (Skewness):", round(skewness_val, 4), "\n")
cat("   Curtosis (exceso):", round(kurtosis_val, 4), "\n\n")

# Q-Q Plot
df_qq <- data.frame(Residuos = residuos_estandarizados)

p_normalidad <- ggplot(df_qq, aes(sample = Residuos)) +
  stat_qq(alpha = 0.3, color = "#9B59B6", size = 1) +
  stat_qq_line(color = "red", linewidth = 1) +
  labs(
    title = "Q-Q Plot de Residuos Estandarizados - CALL",
    subtitle = "Evaluación de normalidad",
    x = "Cuantiles Teóricos",
    y = "Cuantiles Muestrales"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(RUTA_CALL, "fig_4-X_normalidad_CALL.png"),
       p_normalidad, width = 10, height = 7, dpi = 300)
cat("   Gráfico guardado: fig_4-X_normalidad_CALL.png\n\n")

# --- MULTICOLINEALIDAD ---
cat("4. MULTICOLINEALIDAD (VIF)\n")

# Calcular VIF (excluyendo variables categóricas con muchos niveles)
tryCatch({
  vif_valores <- vif(modelo_lm_call)

  # Si hay GVIF para factores, usar GVIF^(1/(2*Df))
  if (is.matrix(vif_valores)) {
    vif_df <- data.frame(
      Variable = rownames(vif_valores),
      GVIF = round(vif_valores[, "GVIF"], 4),
      Df = vif_valores[, "Df"],
      GVIF_adj = round(vif_valores[, "GVIF^(1/(2*Df))"], 4)
    )
    cat("   VIF Generalizado (GVIF):\n")
    print(vif_df, row.names = FALSE)

    # Guardar VIF
    write_xlsx(vif_df, file.path(RUTA_CALL, "tabla_4-X_vif_CALL.xlsx"))
  } else {
    vif_df <- data.frame(
      Variable = names(vif_valores),
      VIF = round(vif_valores, 4)
    )
    cat("   VIF:\n")
    print(vif_df, row.names = FALSE)

    # Variables con VIF > 5 (indicador de multicolinealidad)
    vars_multicolinealidad <- vif_df$Variable[vif_df$VIF > 5]
    if (length(vars_multicolinealidad) > 0) {
      cat("\n   ADVERTENCIA: Variables con VIF > 5:",
          paste(vars_multicolinealidad, collapse = ", "), "\n")
    }

    # Guardar VIF
    write_xlsx(vif_df, file.path(RUTA_CALL, "tabla_4-X_vif_CALL.xlsx"))
  }
}, error = function(e) {
  cat("   No se pudo calcular VIF:", conditionMessage(e), "\n")
})

# Guardar resumen de supuestos
supuestos_resumen <- data.frame(
  Supuesto = c("Linealidad", "Homocedasticidad", "Normalidad", "Multicolinealidad"),
  Test = c("Visual (Residuos vs Ajustados)",
           "Breusch-Pagan",
           "Anderson-Darling",
           "VIF"),
  Estadistico = c("Visual",
                  round(bp_test$statistic, 4),
                  round(ad_test$statistic, 4),
                  "Ver tabla VIF"),
  P_valor = c("N/A",
              format(bp_test$p.value, scientific = TRUE),
              format(ad_test$p.value, scientific = TRUE),
              "N/A"),
  Conclusion = c("Verificar gráfico",
                 ifelse(bp_test$p.value < 0.05, "Heterocedasticidad presente", "OK"),
                 ifelse(ad_test$p.value < 0.05, "No normalidad", "OK"),
                 "Verificar VIF > 5")
)

write_xlsx(supuestos_resumen, file.path(RUTA_CALL, "tabla_4-X_supuestos_CALL.xlsx"))
cat("\n   Tabla de supuestos guardada.\n\n")

# ============================================
# 4.2.5 MÉTRICAS DE DESEMPEÑO
# ============================================

cat("===========================================\n")
cat("MÉTRICAS DE DESEMPEÑO\n")
cat("===========================================\n\n")

# Predicciones en train y test
pred_train <- predict(modelo_lm_call, newdata = train_call)
pred_test <- predict(modelo_lm_call, newdata = test_call)

# Métricas de entrenamiento
rmse_train <- sqrt(mean((train_call$Diferencia - pred_train)^2, na.rm = TRUE))
mae_train <- mean(abs(train_call$Diferencia - pred_train), na.rm = TRUE)
r2_train <- summary_modelo$r.squared
r2_adj_train <- summary_modelo$adj.r.squared

# Métricas de prueba
rmse_test <- sqrt(mean((test_call$Diferencia - pred_test)^2, na.rm = TRUE))
mae_test <- mean(abs(test_call$Diferencia - pred_test), na.rm = TRUE)

# R² en test
ss_res_test <- sum((test_call$Diferencia - pred_test)^2, na.rm = TRUE)
ss_tot_test <- sum((test_call$Diferencia - mean(test_call$Diferencia, na.rm = TRUE))^2, na.rm = TRUE)
r2_test <- 1 - (ss_res_test / ss_tot_test)

# Calcular baseline (modelo constante C)
C_baseline <- mean(train_call$Diferencia, na.rm = TRUE)
pred_baseline_test <- rep(C_baseline, nrow(test_call))
rmse_baseline <- sqrt(mean((test_call$Diferencia - pred_baseline_test)^2, na.rm = TRUE))
mae_baseline <- mean(abs(test_call$Diferencia - pred_baseline_test), na.rm = TRUE)

# Mejora vs baseline
mejora_rmse <- (1 - rmse_test / rmse_baseline) * 100
mejora_mae <- (1 - mae_test / mae_baseline) * 100

cat("--- CONJUNTO DE ENTRENAMIENTO ---\n")
cat("  N observaciones:", nrow(train_call), "\n")
cat("  RMSE Train: $", round(rmse_train, 4), "\n")
cat("  MAE Train:  $", round(mae_train, 4), "\n")
cat("  R² Train:    ", round(r2_train, 4), "\n")
cat("  R² Ajustado: ", round(r2_adj_train, 4), "\n\n")

cat("--- CONJUNTO DE PRUEBA ---\n")
cat("  N observaciones:", nrow(test_call), "\n")
cat("  RMSE Test:  $", round(rmse_test, 4), "\n")
cat("  MAE Test:   $", round(mae_test, 4), "\n")
cat("  R² Test:     ", round(r2_test, 4), "\n\n")

cat("--- COMPARACIÓN CON BASELINE ---\n")
cat("  RMSE Baseline (Constante C):", round(rmse_baseline, 4), "\n")
cat("  RMSE Regresión Lineal:", round(rmse_test, 4), "\n")
cat("  Mejora RMSE:", round(mejora_rmse, 2), "%\n")
cat("  Mejora MAE:", round(mejora_mae, 2), "%\n\n")

# Crear tabla de métricas
tabla_metricas <- data.frame(
  Metrica = c("RMSE ($)", "MAE ($)", "R²", "R² Ajustado"),
  Train = c(round(rmse_train, 4), round(mae_train, 4),
            round(r2_train, 4), round(r2_adj_train, 4)),
  Test = c(round(rmse_test, 4), round(mae_test, 4),
           round(r2_test, 4), NA),
  Interpretacion = c("Error cuadrático medio", "Error absoluto medio",
                     "Varianza explicada", "Ajustado por número de variables")
)

# Agregar fila de mejora vs baseline
tabla_mejora <- data.frame(
  Modelo = c("Baseline (Constante C)", "Regresión Lineal"),
  RMSE_Test = c(round(rmse_baseline, 4), round(rmse_test, 4)),
  MAE_Test = c(round(mae_baseline, 4), round(mae_test, 4)),
  Mejora_RMSE_pct = c("---", paste0(round(mejora_rmse, 2), "%")),
  Mejora_MAE_pct = c("---", paste0(round(mejora_mae, 2), "%"))
)

print(tabla_metricas, row.names = FALSE)
cat("\n")
print(tabla_mejora, row.names = FALSE)

# Guardar tablas
write_xlsx(tabla_metricas, file.path(RUTA_CALL, "tabla_4-X_metricas_lm_CALL.xlsx"))
write_xlsx(tabla_mejora, file.path(RUTA_CALL, "tabla_4-X_mejora_vs_baseline_CALL.xlsx"))

# ============================================
# 4.2.6 CÁLCULO DE PRECIOS AJUSTADOS
# ============================================

cat("\n===========================================\n")
cat("CÁLCULO DE PRECIOS AJUSTADOS\n")
cat("===========================================\n\n")

# Agregar predicciones y precios ajustados al test set
test_call$Pred_LM <- pred_test
test_call$Precio_Ajustado_LM <- test_call$Precio_BS + pred_test
test_call$Error_Final_LM <- test_call$Last - test_call$Precio_Ajustado_LM

# Comparar errores
error_bs_original <- test_call$Last - test_call$Precio_BS  # = Diferencia
error_lm_ajustado <- test_call$Error_Final_LM

rmse_bs_original <- sqrt(mean(error_bs_original^2, na.rm = TRUE))
rmse_lm_ajustado <- sqrt(mean(error_lm_ajustado^2, na.rm = TRUE))

mae_bs_original <- mean(abs(error_bs_original), na.rm = TRUE)
mae_lm_ajustado <- mean(abs(error_lm_ajustado), na.rm = TRUE)

cat("--- COMPARACIÓN DE ERRORES EN PRECIOS ---\n")
cat("  RMSE BS Original: $", round(rmse_bs_original, 4), "\n")
cat("  RMSE BS + Reg. Lineal: $", round(rmse_lm_ajustado, 4), "\n")
cat("  Mejora:", round((1 - rmse_lm_ajustado/rmse_bs_original) * 100, 2), "%\n\n")

# Tabla de comparación de errores
tabla_errores_finales <- data.frame(
  Modelo = c("BS Original", "BS + Regresión Lineal"),
  RMSE_Test = c(round(rmse_bs_original, 4), round(rmse_lm_ajustado, 4)),
  MAE_Test = c(round(mae_bs_original, 4), round(mae_lm_ajustado, 4)),
  Mejora = c("---", paste0(round((1 - rmse_lm_ajustado/rmse_bs_original) * 100, 2), "%"))
)

print(tabla_errores_finales, row.names = FALSE)

write_xlsx(tabla_errores_finales, file.path(RUTA_CALL, "tabla_4-X_errores_finales_CALL.xlsx"))

# Ejemplos de ajustes
cat("\n--- EJEMPLOS DE AJUSTES ---\n")
set.seed(42)
ejemplos_idx <- sample(1:nrow(test_call), min(10, nrow(test_call)))
ejemplos <- test_call[ejemplos_idx, c("Ticker", "Strike", "Precio_Actual",
                                      "Dias_Vencimiento", "IV", "Moneyness",
                                      "Last", "Precio_BS", "Pred_LM",
                                      "Precio_Ajustado_LM", "Error_Final_LM")]
ejemplos <- ejemplos %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(ejemplos, row.names = FALSE)

write_xlsx(ejemplos, file.path(RUTA_CALL, "tabla_4-X_ejemplos_ajuste_CALL.xlsx"))

# ============================================
# GRÁFICOS ADICIONALES
# ============================================

cat("\n===========================================\n")
cat("GENERANDO GRÁFICOS\n")
cat("===========================================\n\n")

# Gráfico 1: Predicciones vs Valores Reales
p_pred_vs_real <- ggplot(data.frame(Real = test_call$Diferencia, Predicho = pred_test),
                         aes(x = Real, y = Predicho)) +
  geom_point(alpha = 0.4, color = "#3498DB", size = 1) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Predicciones vs Valores Reales - Regresión Lineal CALL",
    subtitle = paste0("R² = ", round(r2_test, 4), " | RMSE = $", round(rmse_test, 2)),
    x = "Error Real (Diferencia) ($)",
    y = "Error Predicho ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14)) +
  coord_fixed(ratio = 1)

ggsave(file.path(RUTA_CALL, "fig_4-X_pred_vs_real_CALL.png"),
       p_pred_vs_real, width = 10, height = 10, dpi = 300)
cat("  Gráfico guardado: fig_4-X_pred_vs_real_CALL.png\n")

# Gráfico 2: Distribución de errores (BS vs Ajustado)
df_errores_comp <- data.frame(
  Error = c(error_bs_original, error_lm_ajustado),
  Modelo = rep(c("BS Original", "BS + Reg. Lineal"), each = length(error_bs_original))
)

p_dist_errores <- ggplot(df_errores_comp, aes(x = Error, fill = Modelo)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
  scale_fill_manual(values = c("BS Original" = "#E74C3C", "BS + Reg. Lineal" = "#27AE60")) +
  labs(
    title = "Distribución de Errores: BS Original vs BS Ajustado - CALL",
    subtitle = "Línea negra = error cero (predicción perfecta)",
    x = "Error ($)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom") +
  xlim(-100, 100)  # Limitar para mejor visualización

ggsave(file.path(RUTA_CALL, "fig_4-X_dist_errores_CALL.png"),
       p_dist_errores, width = 12, height = 8, dpi = 300)
cat("  Gráfico guardado: fig_4-X_dist_errores_CALL.png\n")

# Gráfico 3: Histograma de residuos
p_hist_residuos <- ggplot(data.frame(Residuos = residuos), aes(x = Residuos)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50,
                 fill = "#3498DB", alpha = 0.7, color = "white") +
  geom_density(color = "#E74C3C", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = mean(residuos), sd = sd(residuos)),
                color = "black", linewidth = 1, linetype = "dashed") +
  labs(
    title = "Distribución de Residuos - Regresión Lineal CALL",
    subtitle = "Línea roja = densidad empírica | Línea negra = distribución normal teórica",
    x = "Residuos ($)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(RUTA_CALL, "fig_4-X_hist_residuos_CALL.png"),
       p_hist_residuos, width = 10, height = 7, dpi = 300)
cat("  Gráfico guardado: fig_4-X_hist_residuos_CALL.png\n")

# Gráfico 4: Importancia de coeficientes (top 10)
coef_plot <- coeficientes %>%
  filter(Variable != "(Intercept)") %>%
  mutate(Coef_abs = abs(as.numeric(Coeficiente))) %>%
  arrange(desc(Coef_abs)) %>%
  head(15)

# Para el gráfico, usar t-valor como medida de importancia
coef_plot_t <- coeficientes %>%
  filter(Variable != "(Intercept)") %>%
  mutate(t_abs = abs(as.numeric(t_valor))) %>%
  arrange(desc(t_abs)) %>%
  head(10)

p_coef <- ggplot(coef_plot_t, aes(x = reorder(Variable, t_abs), y = t_abs)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "red", linewidth = 0.8) +
  coord_flip() +
  labs(
    title = "Top 10 Variables por Significancia Estadística - CALL",
    subtitle = "Línea roja = umbral de significancia (|t| = 1.96, α = 0.05)",
    x = NULL,
    y = "|t-valor|"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(RUTA_CALL, "fig_4-X_importancia_coef_CALL.png"),
       p_coef, width = 10, height = 8, dpi = 300)
cat("  Gráfico guardado: fig_4-X_importancia_coef_CALL.png\n")

# ============================================
# GUARDAR RESULTADOS COMPLETOS
# ============================================

cat("\n===========================================\n")
cat("GUARDANDO RESULTADOS\n")
cat("===========================================\n\n")

# Guardar test set con predicciones
write_xlsx(test_call, file.path(RUTA_CALL, "resultados_test_lm_CALL.xlsx"))
cat("  resultados_test_lm_CALL.xlsx\n")

# Resumen ejecutivo
resumen_ejecutivo <- data.frame(
  Metrica = c(
    "Total observaciones entrenamiento",
    "Total observaciones prueba",
    "Número de variables explicativas",
    "R² Entrenamiento",
    "R² Prueba",
    "RMSE Entrenamiento ($)",
    "RMSE Prueba ($)",
    "MAE Prueba ($)",
    "RMSE Baseline ($)",
    "Mejora vs Baseline (%)",
    "RMSE BS Original ($)",
    "RMSE BS + Reg. Lineal ($)",
    "Mejora en valoración (%)"
  ),
  Valor = c(
    nrow(train_call),
    nrow(test_call),
    length(coef(modelo_lm_call)) - 1,  # Excluir intercepto
    round(r2_train, 4),
    round(r2_test, 4),
    round(rmse_train, 4),
    round(rmse_test, 4),
    round(mae_test, 4),
    round(rmse_baseline, 4),
    round(mejora_rmse, 2),
    round(rmse_bs_original, 4),
    round(rmse_lm_ajustado, 4),
    round((1 - rmse_lm_ajustado/rmse_bs_original) * 100, 2)
  )
)

write_xlsx(resumen_ejecutivo, file.path(RUTA_CALL, "resumen_ejecutivo_lm_CALL.xlsx"))
cat("  resumen_ejecutivo_lm_CALL.xlsx\n")

# Guardar modelo (para uso posterior)
saveRDS(modelo_lm_call, file.path(RUTA_CALL, "modelo_lm_CALL.rds"))
cat("  modelo_lm_CALL.rds\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n===========================================\n")
cat("RESUMEN FINAL - REGRESIÓN LINEAL CALL\n")
cat("===========================================\n\n")

cat("Observaciones de entrenamiento:", nrow(train_call), "\n")
cat("Observaciones de prueba:", nrow(test_call), "\n")
cat("Variables explicativas:", length(coef(modelo_lm_call)) - 1, "\n\n")

cat("MÉTRICAS DE DESEMPEÑO:\n")
cat("  R² (Train):", round(r2_train, 4), "\n")
cat("  R² (Test):", round(r2_test, 4), "\n")
cat("  RMSE (Test): $", round(rmse_test, 4), "\n")
cat("  MAE (Test): $", round(mae_test, 4), "\n\n")

cat("MEJORA VS BASELINE:\n")
cat("  RMSE Baseline:", round(rmse_baseline, 4), "\n")
cat("  RMSE Reg. Lineal:", round(rmse_test, 4), "\n")
cat("  Mejora:", round(mejora_rmse, 2), "%\n\n")

cat("ARCHIVOS GENERADOS EN:", RUTA_CALL, "\n")
cat("  - tabla_4-X_coeficientes_lm_CALL.xlsx\n")
cat("  - tabla_4-X_vif_CALL.xlsx\n")
cat("  - tabla_4-X_supuestos_CALL.xlsx\n")
cat("  - tabla_4-X_metricas_lm_CALL.xlsx\n")
cat("  - tabla_4-X_mejora_vs_baseline_CALL.xlsx\n")
cat("  - tabla_4-X_errores_finales_CALL.xlsx\n")
cat("  - tabla_4-X_ejemplos_ajuste_CALL.xlsx\n")
cat("  - resumen_ejecutivo_lm_CALL.xlsx\n")
cat("  - resultados_test_lm_CALL.xlsx\n")
cat("  - modelo_lm_CALL.rds\n")
cat("  - fig_4-X_linealidad_CALL.png\n")
cat("  - fig_4-X_homocedasticidad_CALL.png\n")
cat("  - fig_4-X_normalidad_CALL.png\n")
cat("  - fig_4-X_pred_vs_real_CALL.png\n")
cat("  - fig_4-X_dist_errores_CALL.png\n")
cat("  - fig_4-X_hist_residuos_CALL.png\n")
cat("  - fig_4-X_importancia_coef_CALL.png\n")

cat("\n===========================================\n")
cat("ANÁLISIS COMPLETADO EXITOSAMENTE\n")
cat("===========================================\n")

# ============================================================
# EXPORTAR MÉTRICAS A ARCHIVO PLANO PARA EL DOCUMENTO
# ============================================================
archivo_metricas <- "metricas_regresion_lineal_CALL.txt"
sink(archivo_metricas)
cat("=============================================================\n")
cat("MÉTRICAS - REGRESIÓN LINEAL - OPCIONES CALL\n")
cat("=============================================================\n\n")

cat("--- Configuración ---\n")
cat("Modelo: Regresión Lineal Múltiple\n")
cat("Tipo de opción: CALL\n")
cat("N observaciones entrenamiento:", nrow(train_call), "\n")
cat("N observaciones prueba:", nrow(test_call), "\n")
cat("N variables predictoras:", length(coef(modelo_lm_call)) - 1, "\n\n")

cat("--- Métricas de Entrenamiento ---\n")
cat("RMSE Train ($):", round(rmse_train, 2), "\n")
cat("MAE Train ($):", round(mae_train, 2), "\n")
cat("R2 Train:", round(r2_train, 4), "\n")
cat("R2 Ajustado Train:", round(r2_adj_train, 4), "\n")
cat("Error Estándar Residual ($):", round(summary_modelo$sigma, 2), "\n\n")

cat("--- Métricas de Prueba ---\n")
cat("RMSE Test ($):", round(rmse_test, 2), "\n")
cat("MAE Test ($):", round(mae_test, 2), "\n")
cat("R2 Test:", round(r2_test, 4), "\n\n")

cat("--- Comparación con Baseline ---\n")
cat("RMSE Baseline ($):", round(rmse_baseline, 2), "\n")
cat("Mejora RMSE vs Baseline (%):", round(mejora_rmse, 2), "\n")
cat("Mejora MAE vs Baseline (%):", round(mejora_mae, 2), "\n")
sink()

cat("\nMétricas exportadas a:", archivo_metricas, "\n")
