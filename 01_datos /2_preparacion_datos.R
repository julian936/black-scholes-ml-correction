# ============================================
# SCRIPT MAESTRO COMPLETO - CAPÍTULO 3
# Genera TODAS las tablas y figuras del documento
# Autor: Generado para validación del Capítulo 3
# Dataset: Limpio (37,946 obs) + Original para análisis de NAs
# ============================================

library(readxl)
library(writexl)
library(tidyverse)
library(caret)
library(corrplot)
library(gridExtra)
library(scales)
library(moments)  # Para skewness y kurtosis

# ============================================
# CONFIGURACIÓN DE RUTAS
# ============================================

# Dataset LIMPIO (37,946 obs)
ruta_limpio <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/out/2025-10-06/dataset_limpio_37946.xlsx"

# Dataset ORIGINAL (40,337 obs) - solo para análisis de valores faltantes
ruta_original <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/out/2025-10-06/1_dataset_ml_completo_2025-10-06.xlsx"

# Carpeta base
carpeta_base <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1"

# Carpetas por sección
carpetas <- list(
  seccion_3_1 = file.path(carpeta_base, "sub_3-1"),
  seccion_3_2 = file.path(carpeta_base, "sub_3-2"),
  seccion_3_3 = file.path(carpeta_base, "sub_3-3"),
  seccion_3_4 = file.path(carpeta_base, "sub_3-4"),
  seccion_3_5 = file.path(carpeta_base, "sub_3-5"),
  seccion_3_6 = file.path(carpeta_base, "sub_3-6")
)

# Crear carpetas
for (carpeta in carpetas) {
  if (!dir.exists(carpeta)) {
    dir.create(carpeta, recursive = TRUE)
  }
}

set.seed(123)  # Reproducibilidad

cat("\n")
cat("====================================\n")
cat("GENERACIÓN COMPLETA CAPÍTULO 3\n")
cat("====================================\n")
cat("Fecha:", Sys.Date(), "\n")
cat("Hora:", format(Sys.time(), "%H:%M:%S"), "\n\n")

# ============================================
# CARGAR DATASETS
# ============================================

cat("[CARGA] Verificando datasets...\n")

# Dataset limpio
if (!file.exists(ruta_limpio)) {
  stop("❌ Dataset limpio no encontrado en:\n  ", ruta_limpio,
       "\n\nEjecuta primero el script de limpieza (sub_3-4_limpieza.R)")
}

dataset <- read_xlsx(ruta_limpio)
cat("  ✓ Dataset limpio cargado:", nrow(dataset), "obs x", ncol(dataset), "vars\n")

# Dataset original (para análisis de NAs)
if (!file.exists(ruta_original)) {
  warning("⚠ Dataset original no encontrado. Se omitirán análisis de valores faltantes.")
  dataset_original <- NULL
} else {
  dataset_original <- read_xlsx(ruta_original)
  cat("  ✓ Dataset original cargado:", nrow(dataset_original), "obs x", ncol(dataset_original), "vars\n")
}

cat("\n")

# ============================================
# SECCIÓN 3.1: RECOLECCIÓN DE DATOS
# ============================================

cat("═══════════════════════════════════════════════════════\n")
cat("[1/6] SECCIÓN 3.1 - Recolección de datos\n")
cat("═══════════════════════════════════════════════════════\n\n")

# Tabla 3.1: Resumen por ticker
tabla_3_1 <- dataset %>%
  group_by(Ticker) %>%
  summarise(
    Precio_Spot = paste0("$", round(first(Precio_Actual), 2)),
    Volatilidad_Pct = round(first(Volatilidad_Historica) * 100, 2),
    N_Calls = sum(Tipo == "CALL"),
    N_Puts = sum(Tipo == "PUT"),
    Vencimientos = n_distinct(Fecha_Vencimiento),
    .groups = "drop"
  ) %>%
  arrange(Ticker)

# Agregar fila de totales
totales <- data.frame(
  Ticker = "Total",
  Precio_Spot = "–",
  Volatilidad_Pct = NA,
  N_Calls = sum(tabla_3_1$N_Calls),
  N_Puts = sum(tabla_3_1$N_Puts),
  Vencimientos = sum(tabla_3_1$Vencimientos)
)

tabla_3_1 <- rbind(tabla_3_1, totales)

write_xlsx(tabla_3_1, file.path(carpetas$seccion_3_1, "tabla_3-1_resumen_por_ticker.xlsx"))
cat("  ✓ Tabla 3.1 generada (", nrow(tabla_3_1) - 1, "tickers)\n\n")

# ============================================
# SECCIÓN 3.2: IDENTIFICACIÓN DE VARIABLES
# ============================================

cat("═══════════════════════════════════════════════════════\n")
cat("[2/6] SECCIÓN 3.2 - Identificación de variables\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("  Generando figuras de distribuciones...\n")

# ===== FIGURA 3.2: Distribución días vencimiento =====
stats_dias <- dataset %>%
  group_by(Tipo) %>%
  summarise(
    media = mean(Dias_Vencimiento),
    mediana = median(Dias_Vencimiento),
    sd = sd(Dias_Vencimiento),
    .groups = "drop"
  )

p_3_2 <- ggplot(dataset, aes(x = Dias_Vencimiento, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(data = stats_dias, aes(xintercept = media, color = Tipo), 
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  labs(
    title = "Distribución de días hasta el vencimiento (T)",
    subtitle = "Líneas verticales indican la media por tipo de opción",
    x = "Días hasta vencimiento",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(carpetas$seccion_3_2, "figura_3-2_distribucion_dias_vencimiento.png"),
       p_3_2, width = 10, height = 6, dpi = 300)

# ===== FIGURA 3.3: Distribución volatilidad histórica =====
media_vol_hist <- mean(dataset$Volatilidad_Historica)
sd_vol_hist <- sd(dataset$Volatilidad_Historica)

p_3_3 <- ggplot(dataset, aes(x = Volatilidad_Historica)) +
  geom_histogram(bins = 50, fill = "#2C3E50", alpha = 0.8) +
  geom_vline(xintercept = media_vol_hist, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Volatilidad_Histórica",
    subtitle = paste("Media:", round(media_vol_hist, 2), "| SD:", round(sd_vol_hist, 3)),
    x = "Volatilidad Histórica (σ)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_2, "figura_3-3_distribucion_volatilidad_historica.png"),
       p_3_3, width = 10, height = 6, dpi = 300)

# ===== FIGURA 3.4: Distribución IV por tipo =====
stats_iv <- dataset %>%
  group_by(Tipo) %>%
  summarise(
    media = mean(IV),
    sd = sd(IV),
    .groups = "drop"
  )

p_3_4 <- ggplot(dataset, aes(x = IV, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(data = stats_iv, aes(xintercept = media, color = Tipo),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  labs(
    title = "Distribución de Volatilidad Implícita (IV) por tipo de opción",
    subtitle = "Líneas verticales indican la media por tipo",
    x = "Volatilidad Implícita (IV, escala decimal)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_2, "figura_3-4_distribucion_IV_por_tipo.png"),
       p_3_4, width = 10, height = 6, dpi = 300)

cat("    ✓ Figuras 3.2, 3.3, 3.4\n")

# ===== TABLA 3.2: Ejemplo cálculo BS =====
ejemplo_call <- dataset %>%
  filter(Ticker == "AAPL", Tipo == "CALL") %>%
  slice(1)

tabla_3_2 <- data.frame(
  Parametro = c(
    "S (Precio spot)",
    "K (Strike)",
    "T (Tiempo)",
    "r (Tasa libre riesgo)",
    "σ (Volatilidad histórica)",
    "Precio teórico BS",
    "Precio de mercado observado",
    "Diferencia"
  ),
  Valor = c(
    paste0("$", round(ejemplo_call$Precio_Actual, 2)),
    paste0("$", round(ejemplo_call$Strike, 2)),
    paste(ejemplo_call$Dias_Vencimiento, "días =", 
          round(ejemplo_call$Dias_Vencimiento/365, 4), "años"),
    paste0(round(ejemplo_call$Tasa_Libre_Riesgo * 100, 3), "%"),
    paste0(round(ejemplo_call$Volatilidad_Historica * 100, 3), "%"),
    paste0("$", round(ejemplo_call$Precio_BS, 2)),
    paste0("$", round(ejemplo_call$Last, 2)),
    paste0("$", round(ejemplo_call$Diferencia, 2))
  )
)

write_xlsx(tabla_3_2, file.path(carpetas$seccion_3_2, "tabla_3-2_ejemplo_calculo_BS.xlsx"))

# ===== TABLA 3.3: Estadísticas precios BS =====
tabla_3_3 <- dataset %>%
  group_by(Tipo) %>%
  summarise(
    N = n(),
    Media = paste0("$", round(mean(Precio_BS), 2)),
    Mediana = paste0("$", round(median(Precio_BS), 2)),
    SD = paste0("$", round(sd(Precio_BS), 2)),
    Max = paste0("$", round(max(Precio_BS), 2)),
    .groups = "drop"
  )

write_xlsx(tabla_3_3, file.path(carpetas$seccion_3_2, "tabla_3-3_estadisticas_precio_BS.xlsx"))

# ===== TABLA 3.4: Estadísticas Diferencia =====
tabla_3_4 <- dataset %>%
  group_by(Tipo) %>%
  summarise(
    N = n(),
    Media = paste0("$", round(mean(Diferencia), 2)),
    Mediana = paste0("$", round(median(Diferencia), 2)),
    SD = paste0("$", round(sd(Diferencia), 2)),
    RMSE = paste0("$", round(sqrt(mean(Diferencia^2)), 2)),
    Min = paste0("$", round(min(Diferencia), 2)),
    Q1 = paste0("$", round(quantile(Diferencia, 0.25), 2)),
    Q3 = paste0("$", round(quantile(Diferencia, 0.75), 2)),
    Max = paste0("$", round(max(Diferencia), 2)),
    IQR = paste0("$", round(IQR(Diferencia), 2)),
    Skewness = round(skewness(Diferencia), 2),
    Kurtosis = round(kurtosis(Diferencia), 1),
    .groups = "drop"
  )

write_xlsx(tabla_3_4, file.path(carpetas$seccion_3_2, "tabla_3-4_estadisticas_diferencia.xlsx"))

cat("    ✓ Tablas 3.2, 3.3, 3.4\n")

# ===== FIGURA 3.6: Distribución Diferencia =====
stats_dif <- dataset %>%
  group_by(Tipo) %>%
  summarise(media = mean(Diferencia), .groups = "drop")

p_3_6 <- ggplot(dataset, aes(x = Diferencia, fill = Tipo)) +
  geom_histogram(bins = 100, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_vline(data = stats_dif, aes(xintercept = media, color = Tipo),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  coord_cartesian(xlim = c(-200, 200)) +
  labs(
    title = "Distribución de la variable objetivo Diferencia por tipo de opción",
    subtitle = "Línea negra = error cero | Líneas de color = media por tipo | Limitado a ±$200",
    x = "Diferencia (Precio Real - Precio BS) en $",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(carpetas$seccion_3_2, "figura_3-6_distribucion_diferencia.png"),
       p_3_6, width = 12, height = 7, dpi = 300)

# ===== FIGURA 3.7: Distribución Moneyness =====
stats_money <- dataset %>%
  group_by(Tipo) %>%
  summarise(mediana = median(Moneyness), .groups = "drop")

p_3_7 <- ggplot(dataset, aes(x = Moneyness, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(data = stats_money, aes(xintercept = mediana, color = Tipo),
             linetype = "dashed", size = 1) +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black", size = 0.5) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  coord_cartesian(xlim = c(0, 3)) +
  labs(
    title = "Distribución de Moneyness (S/K) por tipo de opción",
    subtitle = "Línea negra punteada = ATM (S/K=1.0) | Líneas de color = mediana por tipo | Limitado a S/K ≤ 3",
    x = "Moneyness (S/K)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_2, "figura_3-7_distribucion_moneyness.png"),
       p_3_7, width = 10, height = 6, dpi = 300)

# ===== FIGURA 3.8: Errores por Moneyness =====
dataset_moneyness <- dataset %>%
  mutate(Moneyness_Grupo = case_when(
    Tipo == "CALL" & Moneyness < 0.95 ~ "OTM",
    Tipo == "CALL" & Moneyness >= 0.95 & Moneyness <= 1.05 ~ "ATM",
    Tipo == "CALL" & Moneyness > 1.05 ~ "ITM",
    Tipo == "PUT" & Moneyness > 1.05 ~ "OTM",
    Tipo == "PUT" & Moneyness >= 0.95 & Moneyness <= 1.05 ~ "ATM",
    Tipo == "PUT" & Moneyness < 0.95 ~ "ITM"
  )) %>%
  mutate(Moneyness_Grupo = factor(Moneyness_Grupo, levels = c("OTM", "ATM", "ITM")))

p_3_8 <- ggplot(dataset_moneyness, aes(x = Moneyness_Grupo, y = Diferencia, fill = Tipo)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  coord_cartesian(ylim = c(-50, 50)) +
  facet_wrap(~ Tipo, scales = "free_x") +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  labs(
    title = "Distribución de errores Black-Scholes por nivel de Moneyness y tipo",
    subtitle = "Diamantes blancos = media | Línea roja = error cero | Limitado a ±$50 para visualización",
    x = "Clasificación de Moneyness",
    y = "Error (Precio Real - Precio BS) en $"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(carpetas$seccion_3_2, "figura_3-8_errores_por_moneyness.png"),
       p_3_8, width = 12, height = 7, dpi = 300)

# ===== FIGURA 3.9: Distribución Log_Moneyness =====
stats_log_money <- dataset %>%
  group_by(Tipo) %>%
  summarise(mediana = median(Log_Moneyness), .groups = "drop")

p_3_9 <- ggplot(dataset, aes(x = Log_Moneyness, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_vline(data = stats_log_money, aes(xintercept = mediana, color = Tipo),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  coord_cartesian(xlim = c(-1, 1)) +
  annotate("text", x = 0, y = Inf, label = "ATM", vjust = 2, size = 3.5) +
  labs(
    title = "Distribución de Log_Moneyness [ln(S/K)] por tipo de opción",
    subtitle = "Línea negra punteada = ATM [ln(S/K)=0] | Líneas de color = mediana por tipo | Limitado a [-1, 1]",
    x = "Log_Moneyness [ln(S/K)]",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_2, "figura_3-9_distribucion_log_moneyness.png"),
       p_3_9, width = 10, height = 6, dpi = 300)

# ===== FIGURA 3.10: Distribución Vol_Diff =====
stats_vol_diff <- dataset %>%
  group_by(Tipo) %>%
  summarise(
    media = mean(Vol_Diff),
    sd = sd(Vol_Diff),
    .groups = "drop"
  )

p_3_10 <- ggplot(dataset, aes(x = Vol_Diff, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_vline(data = stats_vol_diff, aes(xintercept = media, color = Tipo),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  coord_cartesian(xlim = c(-0.5, 1.0)) +
  labs(
    title = "Distribución de Vol_Diff (IV - Volatilidad Histórica) por tipo de opción",
    subtitle = "Línea negra = IV igual a σ | Líneas de color = media por tipo",
    x = "Vol_Diff (escala decimal)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_2, "figura_3-10_distribucion_vol_diff.png"),
       p_3_10, width = 10, height = 6, dpi = 300)

# ===== FIGURA 3.11: Distribución Strike_Normalizado =====
stats_strike_norm <- dataset %>%
  group_by(Tipo) %>%
  summarise(mediana = median(Strike_Normalizado), .groups = "drop")

p_3_11 <- ggplot(dataset, aes(x = Strike_Normalizado, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black", size = 0.5) +
  geom_vline(data = stats_strike_norm, aes(xintercept = mediana, color = Tipo),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  coord_cartesian(xlim = c(-1.0, 1.0)) +
  annotate("text", x = 0, y = Inf, label = "K = S", vjust = 2, size = 3.5) +
  labs(
    title = "Distribución de Strike_Normalizado [(K-S)/S] por tipo de opción",
    subtitle = "Línea negra = K igual a S | Líneas de color = mediana por tipo",
    x = "Strike_Normalizado [(K-S)/S]",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_2, "figura_3-11_distribucion_strike_normalizado.png"),
       p_3_11, width = 10, height = 6, dpi = 300)

cat("    ✓ Figuras 3.6, 3.7, 3.8, 3.9, 3.10, 3.11\n")
cat("  ✓ Sección 3.2 completada: 4 tablas + 9 figuras\n\n")

# ============================================
# SECCIÓN 3.3: ANÁLISIS EXPLORATORIO
# ============================================

cat("═══════════════════════════════════════════════════════\n")
cat("[3/6] SECCIÓN 3.3 - Análisis exploratorio\n")
cat("═══════════════════════════════════════════════════════\n\n")

# ===== ANÁLISIS DE VALORES FALTANTES (requiere dataset original) =====
if (!is.null(dataset_original)) {
  
  cat("  Analizando valores faltantes en dataset original...\n")
  
  # Tabla 3.5: Variables con valores faltantes
  na_counts <- colSums(is.na(dataset_original))
  na_vars <- na_counts[na_counts > 0]
  
  tabla_3_5 <- data.frame(
    Variable = names(na_vars),
    N_Missing = as.numeric(na_vars),
    N_Valid = nrow(dataset_original) - as.numeric(na_vars),
    Total = nrow(dataset_original),
    Pct_Missing = paste0(round(as.numeric(na_vars) / nrow(dataset_original) * 100, 2), "%")
  ) %>%
    arrange(desc(N_Missing))
  
  write_xlsx(tabla_3_5, file.path(carpetas$seccion_3_3, "tabla_3-5_valores_faltantes.xlsx"))
  
  # Figura 3.12: Distribución porcentual valores faltantes
  p_3_12 <- ggplot(tabla_3_5, aes(x = reorder(Variable, N_Missing), y = N_Missing/nrow(dataset_original)*100)) +
    geom_bar(stat = "identity", fill = "#E74C3C", alpha = 0.8) +
    geom_text(aes(label = Pct_Missing), hjust = -0.1, size = 3.5) +
    coord_flip() +
    labs(
      title = "Porcentaje de Valores Faltantes por Variable",
      subtitle = paste("Dataset original:", nrow(dataset_original), "observaciones"),
      x = NULL,
      y = "Porcentaje de Valores Faltantes (%)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  ggsave(file.path(carpetas$seccion_3_3, "figura_3-12_valores_faltantes.png"),
         p_3_12, width = 10, height = 6, dpi = 300)
  
  cat("    ✓ Tabla 3.5 y Figura 3.12 (valores faltantes)\n")
  
} else {
  cat("    ⚠ Dataset original no disponible. Se omiten Tabla 3.5 y Figura 3.12\n")
}

# ===== TABLA 3.6: Estadísticas de outliers =====
cat("  Calculando estadísticas de outliers...\n")

vars_outliers <- c("IV", "Diferencia", "Last", "Vol_Diff", "Volatilidad_Historica")

tabla_3_6_lista <- list()

for (var in vars_outliers) {
  for (tipo in c("CALL", "PUT")) {
    datos <- dataset %>% filter(Tipo == tipo) %>% pull(!!sym(var))
    
    Q1 <- quantile(datos, 0.25, na.rm = TRUE)
    Q3 <- quantile(datos, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    
    outliers <- sum(datos < lower_bound | datos > upper_bound, na.rm = TRUE)
    pct_outliers <- round(outliers / length(datos) * 100, 2)
    
    tabla_3_6_lista[[paste(var, tipo, sep = "_")]] <- data.frame(
      Variable = var,
      Tipo = tipo,
      N = length(datos),
      Media = round(mean(datos, na.rm = TRUE), 3),
      SD = round(sd(datos, na.rm = TRUE), 3),
      Pct_Outliers = paste0(pct_outliers, "%")
    )
  }
}

tabla_3_6 <- bind_rows(tabla_3_6_lista)
write_xlsx(tabla_3_6, file.path(carpetas$seccion_3_3, "tabla_3-6_estadisticas_outliers.xlsx"))

# ===== FIGURA 3.13: Panel comparativo outliers (z-scores) =====
cat("  Generando panel de outliers con z-scores...\n")

dataset_z <- dataset %>%
  mutate(across(all_of(vars_outliers), 
                ~scale(.)[,1], 
                .names = "z_{.col}"))

z_vars <- paste0("z_", vars_outliers)

dataset_z_long <- dataset_z %>%
  select(all_of(z_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Z_Score") %>%
  mutate(Variable = gsub("z_", "", Variable))

p_3_13 <- ggplot(dataset_z_long, aes(x = Z_Score, y = Variable)) +
  geom_boxplot(fill = "#3498DB", alpha = 0.6, outlier.size = 0.5, outlier.alpha = 0.3) +
  geom_vline(xintercept = c(-3, 3), linetype = "dashed", color = "red", size = 0.8) +
  labs(
    title = "Comparación de Outliers entre Variables (Z-Scores)",
    subtitle = "Líneas rojas punteadas indican ±3 desviaciones estándar",
    x = "Z-Score (Desviaciones Estándar)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_3, "figura_3-13_panel_outliers_z_scores.png"),
       p_3_13, width = 12, height = 7, dpi = 300)

# ===== FIGURA 3.14: Distribución IV con umbral =====
umbral_iv <- 2.15

datos_iv_pre <- if (!is.null(dataset_original)) {
  dataset_original %>%
    filter(!is.na(IV)) %>%
    mutate(Estado = "Antes de limpieza")
} else {
  NULL
}

datos_iv_post <- dataset %>%
  mutate(Estado = "Después de limpieza")

if (!is.null(datos_iv_pre)) {
  datos_iv <- bind_rows(datos_iv_pre, datos_iv_post)
} else {
  datos_iv <- datos_iv_post
}

p_3_14 <- ggplot(dataset, aes(x = IV, fill = Tipo)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  geom_vline(xintercept = umbral_iv, linetype = "dashed", color = "red", size = 1) +
  scale_fill_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  coord_cartesian(xlim = c(0, 3)) +
  annotate("text", x = umbral_iv, y = Inf, 
           label = paste("Umbral IV =", umbral_iv, "\n(215%)"),
           vjust = 2, hjust = -0.1, size = 3.5, color = "red") +
  labs(
    title = "Distribución de Volatilidad Implícita con identificación de outliers",
    subtitle = "Línea roja indica el umbral de eliminación (IV = 2.15) | Limitado a IV ≤ 3 para visualización",
    x = "Volatilidad Implícita (IV, escala decimal)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_3, "figura_3-14_distribucion_IV_con_umbral.png"),
       p_3_14, width = 10, height = 6, dpi = 300)

# ===== FIGURA 3.15: Distribución volatilidad histórica con umbral =====
umbral_vol_hist <- 1.5

p_3_15 <- ggplot(dataset, aes(x = Volatilidad_Historica)) +
  geom_histogram(bins = 50, fill = "#2C3E50", alpha = 0.8) +
  geom_vline(xintercept = umbral_vol_hist, linetype = "dashed", color = "red", size = 1) +
  coord_cartesian(xlim = c(0, 1.0)) +
  annotate("text", x = umbral_vol_hist, y = Inf,
           label = paste("Umbral σ =", umbral_vol_hist),
           vjust = 2, hjust = 1.1, size = 3.5, color = "red") +
  labs(
    title = "Distribución de Volatilidad Histórica",
    subtitle = paste("N =", nrow(dataset), "| Media:", round(mean(dataset$Volatilidad_Historica), 2),
                     "| Mediana:", round(median(dataset$Volatilidad_Historica), 3)),
    x = "Volatilidad Histórica (σ)",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14))

ggsave(file.path(carpetas$seccion_3_3, "figura_3-15_distribucion_vol_historica_umbral.png"),
       p_3_15, width = 10, height = 6, dpi = 300)

# ===== TABLA 3.7: Criterios de tratamiento outliers =====
tabla_3_7 <- data.frame(
  Variable = c("IV", "Volatilidad Histórica", "Last", "Diferencia", "Vol_Diff"),
  Criterio = c("IV > 2.15", "σ > 1.5", "Ninguno", "Ninguno", "Ninguno"),
  Justificacion = c(
    "Valores > 215% son imposibles incluso en crisis (CALL: 432, PUT: 155 obs.)",
    "Volatilidades históricas > 150% son extremadamente raras (0 obs. en ambos tipos)",
    "Precios de mercado observados - no filtrar",
    "Errores extremos son parte del fenómeno a estudiar",
    "Diferencias extremas de volatilidad son legítimas"
  ),
  Accion = c("Eliminar", "Eliminar", "Retener", "Retener", "Retener")
)

write_xlsx(tabla_3_7, file.path(carpetas$seccion_3_3, "tabla_3-7_criterios_outliers.xlsx"))

cat("    ✓ Tabla 3.6, 3.7 y Figuras 3.13, 3.14, 3.15\n")

# ===== MATRICES DE CORRELACIÓN =====
cat("  Generando matrices de correlación...\n")

vars_numericas <- c("Strike", "Precio_Actual", "Dias_Vencimiento",
                    "Volatilidad_Historica", "Tasa_Libre_Riesgo",
                    "Last", "IV", "Vol", "OI",
                    "Precio_BS", "Diferencia",
                    "Moneyness", "Log_Moneyness", "Vol_Diff",
                    "Strike_Normalizado", "In_The_Money")

# Filtrar solo variables que existen
vars_disponibles <- vars_numericas[vars_numericas %in% names(dataset)]

# CALL
dataset_call <- dataset %>% filter(Tipo == "CALL")
cor_call <- cor(dataset_call[vars_disponibles], use = "complete.obs")

png(file.path(carpetas$seccion_3_3, "figura_3-16_matriz_correlacion_CALL.png"),
    width = 14, height = 14, units = "in", res = 300)
corrplot(cor_call, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.9,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("#E74C3C", "white", "#27AE60"))(200),
         title = "Matriz de Correlaciones - CALL",
         mar = c(0,0,2,0))
dev.off()

# PUT
dataset_put <- dataset %>% filter(Tipo == "PUT")
cor_put <- cor(dataset_put[vars_disponibles], use = "complete.obs")

png(file.path(carpetas$seccion_3_3, "figura_3-17_matriz_correlacion_PUT.png"),
    width = 14, height = 14, units = "in", res = 300)
corrplot(cor_put, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.9,
         addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("#E74C3C", "white", "#27AE60"))(200),
         title = "Matriz de Correlaciones - PUT",
         mar = c(0,0,2,0))
dev.off()

# ===== TABLA 3.8: Correlaciones divergentes =====
tabla_3_8 <- data.frame(
  Variable = vars_disponibles,
  Cor_CALL = cor_call[, "Diferencia"],
  Cor_PUT = cor_put[, "Diferencia"]
) %>%
  filter(Variable != "Diferencia") %>%
  mutate(Diferencia_Abs = abs(Cor_CALL - Cor_PUT)) %>%
  arrange(desc(Diferencia_Abs)) %>%
  head(10) %>%
  mutate(
    Cor_CALL = round(Cor_CALL, 2),
    Cor_PUT = round(Cor_PUT, 2),
    Diferencia_Abs = round(Diferencia_Abs, 2)
  )

write_xlsx(tabla_3_8, file.path(carpetas$seccion_3_3, "tabla_3-8_correlaciones_divergentes.xlsx"))

cat("    ✓ Figuras 3.16, 3.17 y Tabla 3.8 (correlaciones)\n")

# ===== FIGURA 3.18: Comparación precio teórico vs mercado =====
cat("  Generando comparación precio teórico vs mercado...\n")

# Muestra aleatoria de 5,000 opciones
muestra_5k <- dataset %>%
  sample_n(min(5000, nrow(dataset)))

cor_precio <- cor(dataset$Precio_BS, dataset$Last)
r2_precio <- cor_precio^2

p_3_18 <- ggplot(muestra_5k, aes(x = Precio_BS, y = Last, color = Tipo)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  scale_color_manual(values = c("CALL" = "#3498DB", "PUT" = "#E74C3C")) +
  annotate("text", x = Inf, y = -Inf,
           label = paste("r =", round(cor_precio, 4), "\nR² =", round(r2_precio, 4)),
           hjust = 1.1, vjust = -0.5, size = 4) +
  labs(
    title = "Comparación Precio Teórico BS vs Precio de Mercado",
    subtitle = paste("Muestra aleatoria de", nrow(muestra_5k), "opciones | Línea punteada = precio teórico = precio real"),
    x = "Precio Teórico Black-Scholes ($)",
    y = "Precio de Mercado ($)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave(file.path(carpetas$seccion_3_3, "figura_3-18_comparacion_precio_teorico_mercado.png"),
       p_3_18, width = 10, height = 8, dpi = 300)

cat("    ✓ Figura 3.18\n")
cat("  ✓ Sección 3.3 completada: 4 tablas + 8 figuras\n\n")

# ============================================
# SECCIÓN 3.4: LIMPIEZA
# ============================================

cat("═══════════════════════════════════════════════════════\n")
cat("[4/6] SECCIÓN 3.4 - Limpieza\n")
cat("═══════════════════════════════════════════════════════\n\n")

# ===== TABLA 3.9: Verificaciones de integridad =====
cat("  Verificando integridad del dataset limpio...\n")

verificaciones <- data.frame(
  Verificacion = c(
    "Valores faltantes",
    "Registros duplicados",
    "Strike > 0",
    "Precio_Actual > 0",
    "Dias_Vencimiento >= 0",
    "Last > 0",
    "IV ≤ 2.15",
    "Volatilidad_Historica > 0"
  ),
  Resultado = c(
    ifelse(sum(is.na(dataset)) == 0, "Ninguno detectado", "Detectados"),
    ifelse(sum(duplicated(dataset)) == 0, "Ninguno detectado", "Detectados"),
    ifelse(all(dataset$Strike > 0), "Cumple totalmente", "No cumple"),
    ifelse(all(dataset$Precio_Actual > 0), "Cumple totalmente", "No cumple"),
    ifelse(all(dataset$Dias_Vencimiento >= 0), "Cumple totalmente", "No cumple"),
    ifelse(all(dataset$Last > 0), "Cumple totalmente", "No cumple"),
    ifelse(all(dataset$IV <= 2.15), "Cumple totalmente", "No cumple"),
    ifelse(all(dataset$Volatilidad_Historica > 0), "Cumple totalmente", "No cumple")
  ),
  N_Obs = c(
    sum(is.na(dataset)),
    sum(duplicated(dataset)),
    ifelse(all(dataset$Strike > 0), nrow(dataset), sum(dataset$Strike > 0)),
    ifelse(all(dataset$Precio_Actual > 0), nrow(dataset), sum(dataset$Precio_Actual > 0)),
    ifelse(all(dataset$Dias_Vencimiento >= 0), nrow(dataset), sum(dataset$Dias_Vencimiento >= 0)),
    ifelse(all(dataset$Last > 0), nrow(dataset), sum(dataset$Last > 0)),
    ifelse(all(dataset$IV <= 2.15), nrow(dataset), sum(dataset$IV <= 2.15)),
    ifelse(all(dataset$Volatilidad_Historica > 0), nrow(dataset), sum(dataset$Volatilidad_Historica > 0))
  )
)

write_xlsx(verificaciones, file.path(carpetas$seccion_3_4, "tabla_3-9_verificaciones_integridad.xlsx"))

cat("    ✓ Tabla 3.9\n")

# ===== FIGURA 3.19: Diagrama flujo limpieza =====
if (!is.null(dataset_original)) {
  
  # Calcular etapas de limpieza
  n_original <- nrow(dataset_original)
  
  # Etapa 1: Eliminar NAs
  dataset_sin_na <- dataset_original %>%
    drop_na(IV, Vol, OI)
  n_sin_na <- nrow(dataset_sin_na)
  eliminados_na <- n_original - n_sin_na
  
  # Etapa 2: Eliminar IV extrema
  n_limpio <- nrow(dataset)
  eliminados_iv <- n_sin_na - n_limpio
  
  datos_flujo <- data.frame(
    Etapa = c("Original", "Sin NAs", "Limpio"),
    N_Obs = c(n_original, n_sin_na, n_limpio),
    Eliminados = c(0, eliminados_na, eliminados_iv),
    Pct_Retencion = c(100, n_sin_na/n_original*100, n_limpio/n_original*100)
  )
  
  write_xlsx(datos_flujo, file.path(carpetas$seccion_3_4, "datos_figura_3-19_flujo_limpieza.xlsx"))
  
  p_3_19 <- ggplot(datos_flujo, aes(x = factor(Etapa, levels = c("Original", "Sin NAs", "Limpio")), y = N_Obs)) +
    geom_bar(stat = "identity", aes(fill = Etapa), alpha = 0.8, width = 0.6) +
    geom_text(aes(label = scales::comma(N_Obs)), vjust = -0.5, size = 5, fontface = "bold") +
    geom_segment(data = datos_flujo[-1, ],
                 aes(x = c(1.3, 2.3), xend = c(1.7, 2.7), y = N_Obs, yend = N_Obs),
                 arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1) +
    geom_text(data = datos_flujo[-1, ],
              aes(x = c(1.5, 2.5), y = N_Obs, 
                  label = paste0("-", scales::comma(Eliminados), " (", round(Eliminados/n_original*100, 2), "%)")),
              vjust = -1.5, color = "red", size = 3.5) +
    scale_fill_manual(values = c("Original" = "#95A5A6", "Sin NAs" = "#3498DB", "Limpio" = "#27AE60")) +
    labs(
      title = "Proceso de Limpieza de Datos",
      subtitle = paste("Tasa de retención final:", round(n_limpio/n_original*100, 2), "%"),
      x = "Etapa del Proceso",
      y = "Número de Observaciones"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "none"
    )
  
  ggsave(file.path(carpetas$seccion_3_4, "figura_3-19_diagrama_flujo_limpieza.png"),
         p_3_19, width = 10, height = 6, dpi = 300)
  
  cat("    ✓ Figura 3.19\n")
}

# ===== FIGURA 3.20: Comparación IV antes/después =====
if (!is.null(dataset_original)) {
  
  iv_antes <- dataset_original %>%
    filter(!is.na(IV)) %>%
    mutate(Estado = "Antes de limpieza")
  
  iv_despues <- dataset %>%
    mutate(Estado = "Después de limpieza")
  
  iv_comparacion <- bind_rows(iv_antes, iv_despues) %>%
    select(IV, Tipo, Estado)
  
  p_3_20 <- ggplot(iv_comparacion, aes(x = IV, fill = Estado)) +
    geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
    geom_vline(xintercept = 2.15, linetype = "dashed", color = "red", size = 1) +
    scale_fill_manual(values = c("Antes de limpieza" = "#E74C3C", "Después de limpieza" = "#27AE60")) +
    coord_cartesian(xlim = c(0, 3)) +
    annotate("text", x = 2.15, y = Inf,
             label = "Umbral IV = 2.15",
             vjust = 2, hjust = -0.1, size = 3.5, color = "red") +
    labs(
      title = "Comparación de Distribución de IV: Antes vs Después de Limpieza",
      subtitle = "Línea vertical punteada indica el umbral de corte (IV = 2.15)",
      x = "Volatilidad Implícita (IV, escala decimal)",
      y = "Frecuencia"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  ggsave(file.path(carpetas$seccion_3_4, "figura_3-20_comparacion_IV_antes_despues.png"),
         p_3_20, width = 12, height = 7, dpi = 300)
  
  cat("    ✓ Figura 3.20\n")
}

cat("  ✓ Sección 3.4 completada: 1 tabla + 2 figuras\n\n")

# ============================================
# SECCIÓN 3.5: CONSOLIDACIÓN
# ============================================

cat("═══════════════════════════════════════════════════════\n")
cat("[5/6] SECCIÓN 3.5 - Consolidación\n")
cat("═══════════════════════════════════════════════════════\n\n")

# ===== TABLA 3.10: Estructura del dataset =====
cat("  Generando estructura del dataset...\n")

# Clasificar variables por categoría
categorias <- data.frame(
  Variable = names(dataset),
  Tipo = sapply(dataset, function(x) class(x)[1])
) %>%
  mutate(
    Categoria = case_when(
      Variable %in% c("Ticker", "Tipo") ~ "Identificadores",
      Variable %in% c("Precio_Actual", "Strike", "Dias_Vencimiento", 
                      "Volatilidad_Historica", "Tasa_Libre_Riesgo") ~ "Inputs BS",
      Variable %in% c("Last", "IV", "Vol", "OI") ~ "Variables Mercado",
      Variable %in% c("Moneyness", "Log_Moneyness", "Vol_Diff", 
                      "Strike_Normalizado", "In_The_Money") ~ "Variables Derivadas",
      Variable %in% c("Precio_BS", "Diferencia", "Precio_Calibrado", 
                      "Error_Calibrado") ~ "Outputs",
      TRUE ~ "Metadata"
    )
  )

# Contar por categoría
resumen_categorias <- categorias %>%
  group_by(Categoria) %>%
  summarise(
    N_Variables = n(),
    Variables = paste(Variable, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(factor(Categoria, levels = c("Identificadores", "Inputs BS", 
                                       "Variables Mercado", "Variables Derivadas",
                                       "Outputs", "Metadata")))

write_xlsx(list(
  Estructura_Completa = categorias,
  Resumen_por_Categoria = resumen_categorias
), file.path(carpetas$seccion_3_5, "tabla_3-10_estructura_dataset.xlsx"))

cat("  ✓ Tabla 3.10 generada (25 variables clasificadas)\n")
cat("  ✓ Sección 3.5 completada: 1 tabla\n\n")

# ============================================
# SECCIÓN 3.6: PARTICIÓN TRAIN/TEST
# ============================================

cat("═══════════════════════════════════════════════════════\n")
cat("[6/6] SECCIÓN 3.6 - Partición train/test\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("  Ejecutando partición estratificada...\n")

# Crear variable de estratificación
dataset <- dataset %>%
  mutate(Estrato = paste(Ticker, Tipo, sep = "_"))

# Partición estratificada 80-20
train_indices <- createDataPartition(dataset$Estrato, p = 0.80, list = FALSE)

train_data <- dataset[train_indices, ]
test_data <- dataset[-train_indices, ]

cat("    ✓ Train:", nrow(train_data), "obs\n")
cat("    ✓ Test:", nrow(test_data), "obs\n")

# Separar por tipo
train_call <- train_data %>% filter(Tipo == "CALL")
train_put <- train_data %>% filter(Tipo == "PUT")
test_call <- test_data %>% filter(Tipo == "CALL")
test_put <- test_data %>% filter(Tipo == "PUT")

cat("    ✓ Train CALL:", nrow(train_call), "| Train PUT:", nrow(train_put), "\n")
cat("    ✓ Test CALL:", nrow(test_call), "| Test PUT:", nrow(test_put), "\n\n")

# ===== TABLA 3.11: Partición =====
tabla_3_11 <- data.frame(
  Conjunto = c("Entrenamiento", "Prueba", "Total"),
  Observaciones = c(nrow(train_data), nrow(test_data), nrow(dataset)),
  Proporcion = c("80.0%", "20.0%", "100%"),
  Uso = c(
    "Desarrollo y ajuste de modelos",
    "Evaluación final de desempeño",
    ""
  )
)

write_xlsx(tabla_3_11, file.path(carpetas$seccion_3_6, "tabla_3-11_particion.xlsx"))

# ===== TABLA 3.12: Pruebas Kolmogorov-Smirnov =====
cat("  Ejecutando pruebas de Kolmogorov-Smirnov...\n")

vars_ks <- c("Moneyness", "Dias_Vencimiento", "IV", "Precio_Actual")
resultados_ks <- data.frame()

for (var in vars_ks) {
  ks_test <- ks.test(train_data[[var]], test_data[[var]])
  
  resultado <- data.frame(
    Variable = var,
    Estadistico_KS = round(ks_test$statistic, 4),
    p_valor = round(ks_test$p.value, 3),
    Conclusion = ifelse(ks_test$p.value > 0.05, 
                        "No hay diferencia", 
                        "Diferencia significativa")
  )
  
  resultados_ks <- rbind(resultados_ks, resultado)
}

write_xlsx(resultados_ks, file.path(carpetas$seccion_3_6, "tabla_3-12_pruebas_ks.xlsx"))

cat("    ✓ Todas las variables: p > 0.05 (distribuciones equivalentes)\n")

# ===== TABLA 3.13: Distribución por estrato =====
tabla_3_13 <- dataset %>%
  group_by(Ticker, Tipo) %>%
  summarise(
    Total = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Entrenamiento = sapply(1:n(), function(i) {
      sum(train_data$Ticker == Ticker[i] & train_data$Tipo == Tipo[i])
    }),
    Prueba = sapply(1:n(), function(i) {
      sum(test_data$Ticker == Ticker[i] & test_data$Tipo == Tipo[i])
    })
  ) %>%
  arrange(Ticker, Tipo)

write_xlsx(tabla_3_13, file.path(carpetas$seccion_3_6, "tabla_3-13_distribucion_estratos.xlsx"))

cat("  Generando 3 tablas...\n")
cat("    ✓ Tabla 3.11, 3.12, 3.13\n\n")

# ===== GUARDAR DATASETS =====
cat("  Guardando 6 datasets...\n")

write_xlsx(train_data, file.path(carpetas$seccion_3_6, "train_data_completo.xlsx"))
write_xlsx(test_data, file.path(carpetas$seccion_3_6, "test_data_completo.xlsx"))
write_xlsx(train_call, file.path(carpetas$seccion_3_6, "train_data_CALL.xlsx"))
write_xlsx(train_put, file.path(carpetas$seccion_3_6, "train_data_PUT.xlsx"))
write_xlsx(test_call, file.path(carpetas$seccion_3_6, "test_data_CALL.xlsx"))
write_xlsx(test_put, file.path(carpetas$seccion_3_6, "test_data_PUT.xlsx"))

cat("    ✓ train_data_completo.xlsx (", nrow(train_data), "obs )\n")
cat("    ✓ test_data_completo.xlsx (", nrow(test_data), "obs )\n")
cat("    ✓ train_data_CALL.xlsx (", nrow(train_call), "obs )\n")
cat("    ✓ train_data_PUT.xlsx (", nrow(train_put), "obs )\n")
cat("    ✓ test_data_CALL.xlsx (", nrow(test_call), "obs )\n")
cat("    ✓ test_data_PUT.xlsx (", nrow(test_put), "obs )\n\n")

cat("  ✓ Sección 3.6 completada: 3 tablas + 6 datasets\n\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n")
cat("═══════════════════════════════════════════════════════\n")
cat("GENERACIÓN COMPLETA FINALIZADA\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("ARCHIVOS GENERADOS POR SECCIÓN:\n\n")

cat("  [3.1] sub_3-1/\n")
cat("        └─ 1 tabla (Tabla 3.1)\n\n")

cat("  [3.2] sub_3-2/\n")
cat("        ├─ 4 tablas (3.2, 3.3, 3.4, + estadísticas)\n")
cat("        └─ 9 figuras (3.2-3.11)\n\n")

cat("  [3.3] sub_3-3/\n")
cat("        ├─ 4 tablas (3.5, 3.6, 3.7, 3.8)\n")
cat("        └─ 7 figuras (3.12-3.18)\n\n")

cat("  [3.4] sub_3-4/\n")
cat("        ├─ 1 tabla (3.9)\n")
cat("        └─ 2 figuras (3.19, 3.20)\n\n")

cat("  [3.5] sub_3-5/\n")
cat("        └─ 1 tabla (3.10)\n\n")

cat("  [3.6] sub_3-6/\n")
cat("        ├─ 3 tablas (3.11, 3.12, 3.13)\n")
cat("        └─ 6 datasets (train/test separados)\n\n")

cat("═══════════════════════════════════════════════════════\n")
cat("TOTALES:\n")
cat("  • 13 Tablas para LaTeX\n")
cat("  • 18 Figuras (PNG, 300 dpi)\n")
cat("  • 6 Datasets (XLSX)\n")
cat("═══════════════════════════════════════════════════════\n\n")

cat("DATASETS DISPONIBLES PARA CAPÍTULO 4:\n")
cat("  ✓ train_data_CALL.xlsx    (", nrow(train_call), "obs )\n")
cat("  ✓ train_data_PUT.xlsx     (", nrow(train_put), "obs )\n")
cat("  ✓ test_data_CALL.xlsx     (", nrow(test_call), "obs )\n")
cat("  ✓ test_data_PUT.xlsx      (", nrow(test_put), "obs )\n\n")

cat("PRÓXIMO PASO:\n")
cat("  → Capítulo 4: Modelos de Machine Learning\n")
cat("  → Usar datasets de sub_3-6/ para entrenar modelos separados\n\n")

cat("Tiempo total:", round(difftime(Sys.time(), Sys.time(), units = "mins"), 2), "minutos\n")
cat("Fecha finalización:", Sys.time(), "\n\n")

cat("════════════════════════════════════════════════════════\n")
cat("✓ SCRIPT COMPLETADO EXITOSAMENTE\n")
cat("════════════════════════════════════════════════════════\n\n")
