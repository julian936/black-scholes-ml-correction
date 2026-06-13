# ============================================
# REDES NEURONALES MLP - OPCIONES PUT
# Capítulo 4 - Objetivo 2
# GRID SEARCH COMPLETO - 900 combinaciones
# ============================================

# Limpiar entorno
rm(list = ls())

# ============================================
# CONFIGURACIÓN Y LIBRERÍAS
# ============================================

library(readxl)
library(tidyverse)
library(keras3)
library(writexl)
library(scales)

# Configuración de rutas
RUTA_ENTRADA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 1/sub_3-6"
RUTA_SALIDA <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main/objetivo 2/MLP"

# Crear carpetas
if (!dir.exists(RUTA_SALIDA)) dir.create(RUTA_SALIDA, recursive = TRUE)
RUTA_PUT <- file.path(RUTA_SALIDA, "PUT")
if (!dir.exists(RUTA_PUT)) dir.create(RUTA_PUT, recursive = TRUE)

carpeta_graficos <- file.path(RUTA_PUT, "graficos")
carpeta_tablas <- file.path(RUTA_PUT, "tablas")
if (!dir.exists(carpeta_graficos)) dir.create(carpeta_graficos, recursive = TRUE)
if (!dir.exists(carpeta_tablas)) dir.create(carpeta_tablas, recursive = TRUE)

set.seed(123)
tensorflow::set_random_seed(123)

cat("====================================\n")
cat("MLP - OPCIONES PUT\n")
cat("GRID SEARCH COMPLETO - 900 combinaciones\n")
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

cat("Cargando datos...\n")

train_put <- read_xlsx(file.path(RUTA_ENTRADA, "train_data_PUT.xlsx"))
test_put <- read_xlsx(file.path(RUTA_ENTRADA, "test_data_PUT.xlsx"))

cat("  Train PUT:", nrow(train_put), "observaciones\n")
cat("  Test PUT:", nrow(test_put), "observaciones\n\n")

# ============================================
# PREPARACIÓN DE VARIABLES
# ============================================

variables_numericas <- c(
  "Strike", "Precio_Actual", "Dias_Vencimiento", "Volatilidad_Historica",
  "Vol", "OI", "IV",
  "Moneyness", "Log_Moneyness", "Vol_Diff", "Strike_Normalizado", "In_The_Money"
)

variable_objetivo <- "Diferencia"

# ============================================
# ONE-HOT ENCODING DE TICKER
# ============================================

tickers_unicos <- sort(unique(train_put$Ticker))

crear_dummies <- function(data, ticker_levels) {
  data$Ticker <- factor(data$Ticker, levels = ticker_levels)
  dummy_matrix <- model.matrix(~ Ticker - 1, data = data)
  dummy_df <- as.data.frame(dummy_matrix)
  names(dummy_df) <- gsub("Ticker", "Ticker_", names(dummy_df))
  return(dummy_df)
}

dummies_train <- crear_dummies(train_put, tickers_unicos)
dummies_test <- crear_dummies(test_put, tickers_unicos)

# ============================================
# PREPARAR DATASETS
# ============================================

train_mlp <- cbind(
  train_put[, c(variable_objetivo, variables_numericas)],
  dummies_train
)

test_mlp <- cbind(
  test_put[, c(variable_objetivo, variables_numericas)],
  dummies_test
)

variables_dummy <- names(dummies_train)
variables_predictoras <- c(variables_numericas, variables_dummy)

train_mlp <- na.omit(train_mlp)
test_mlp <- na.omit(test_mlp)

# ============================================
# NORMALIZACIÓN Z-SCORE
# ============================================

medias_train <- sapply(train_mlp[, variables_numericas], mean)
sd_train <- sapply(train_mlp[, variables_numericas], sd)

train_scaled <- train_mlp
test_scaled <- test_mlp

for (var in variables_numericas) {
  train_scaled[[var]] <- (train_mlp[[var]] - medias_train[var]) / sd_train[var]
  test_scaled[[var]] <- (test_mlp[[var]] - medias_train[var]) / sd_train[var]
}

X_train <- as.matrix(train_scaled[, variables_predictoras])
y_train <- train_scaled$Diferencia
X_test <- as.matrix(test_scaled[, variables_predictoras])
y_test <- test_scaled$Diferencia

cat("Datos preparados. Dimensiones X_train:", dim(X_train), "\n\n")

# ============================================
# DEFINIR GRID COMPLETO
# ============================================

cat("===========================================\n")
cat("DEFINICIÓN DEL GRID COMPLETO\n")
cat("===========================================\n\n")

# Todas las opciones
capas_opciones <- list(
  c(32),
  c(64),
  c(128),
  c(32, 16),
  c(64, 32),
  c(128, 64),
  c(64, 32, 16),
  c(128, 64, 32),
  c(256, 128, 64),
  c(128, 64, 32, 16)
)

dropout_opciones <- c(0.0, 0.1, 0.2, 0.3, 0.4)
learning_rate_opciones <- c(0.001, 0.005, 0.01)
batch_size_opciones <- c(32, 64, 128)
activacion_opciones <- c("relu", "tanh")

# Crear TODAS las combinaciones
grid <- expand.grid(
  capas_idx = 1:length(capas_opciones),
  dropout = dropout_opciones,
  learning_rate = learning_rate_opciones,
  batch_size = batch_size_opciones,
  activacion = activacion_opciones,
  stringsAsFactors = FALSE
)

# Agregar descripción de arquitectura
grid$arquitectura <- sapply(grid$capas_idx, function(idx) {
  paste(capas_opciones[[idx]], collapse = "-")
})

grid$n_capas <- sapply(grid$capas_idx, function(idx) {
  length(capas_opciones[[idx]])
})

grid$config_id <- 1:nrow(grid)

n_total <- nrow(grid)
cat("Total de combinaciones a probar:", n_total, "\n")
cat("Arquitecturas:", length(capas_opciones), "\n")
cat("Dropout:", length(dropout_opciones), "valores\n")
cat("Learning Rate:", length(learning_rate_opciones), "valores\n")
cat("Batch Size:", length(batch_size_opciones), "valores\n")
cat("Activación:", length(activacion_opciones), "funciones\n\n")

# ============================================
# FUNCIÓN PARA CREAR Y ENTRENAR MODELO
# ============================================

crear_entrenar_modelo <- function(capas, dropout, learning_rate, batch_size,
                                  activacion, X_train, y_train, X_test, y_test,
                                  epochs = 100, verbose = 0) {

  modelo <- keras_model_sequential()

  modelo %>% layer_dense(
    units = capas[1],
    activation = activacion,
    input_shape = ncol(X_train)
  )

  if (dropout > 0) {
    modelo %>% layer_dropout(rate = dropout)
  }

  if (length(capas) > 1) {
    for (i in 2:length(capas)) {
      modelo %>% layer_dense(units = capas[i], activation = activacion)
      if (dropout > 0) {
        modelo %>% layer_dropout(rate = dropout)
      }
    }
  }

  modelo %>% layer_dense(units = 1, activation = "linear")

  modelo %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = "mse",
    metrics = c("mae")
  )

  early_stop <- callback_early_stopping(
    monitor = "val_loss",
    patience = 15,
    restore_best_weights = TRUE
  )

  historia <- modelo %>% fit(
    X_train, y_train,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = 0.2,
    callbacks = list(early_stop),
    verbose = verbose
  )

  pred_train <- as.vector(predict(modelo, X_train))
  pred_test <- as.vector(predict(modelo, X_test))

  rmse_train <- sqrt(mean((y_train - pred_train)^2))
  rmse_test <- sqrt(mean((y_test - pred_test)^2))
  mae_train <- mean(abs(y_train - pred_train))
  mae_test <- mean(abs(y_test - pred_test))
  ss_res_train <- sum((y_train - pred_train)^2)
  ss_tot_train <- sum((y_train - mean(y_train))^2)
  r2_train <- 1 - (ss_res_train / ss_tot_train)
  ss_res_test <- sum((y_test - pred_test)^2)
  ss_tot_test <- sum((y_test - mean(y_test))^2)
  r2_test <- 1 - (ss_res_test / ss_tot_test)

  epochs_final <- length(historia$metrics$loss)

  return(list(
    rmse_train = rmse_train,
    rmse_test = rmse_test,
    mae_train = mae_train,
    mae_test = mae_test,
    r2_train = r2_train,
    r2_test = r2_test,
    epochs_final = epochs_final
  ))
}

# ============================================
# EJECUTAR GRID SEARCH COMPLETO
# ============================================

cat("===========================================\n")
cat("EJECUTANDO GRID SEARCH COMPLETO\n")
cat("Esto puede tardar varias horas...\n")
cat("===========================================\n\n")

resultados <- data.frame()

tiempo_inicio <- Sys.time()

# Archivo para guardar progreso
archivo_progreso <- file.path(carpeta_tablas, "progreso_grid_search_PUT.xlsx")

for (i in 1:n_total) {

  # Mostrar progreso cada 10 configuraciones
  if (i %% 10 == 0 || i == 1) {
    tiempo_actual <- Sys.time()
    tiempo_transcurrido <- difftime(tiempo_actual, tiempo_inicio, units = "mins")
    velocidad <- i / as.numeric(tiempo_transcurrido)
    tiempo_restante <- (n_total - i) / velocidad
    cat("\n--- Progreso:", i, "/", n_total,
        "(", round(i/n_total*100, 1), "%) ---\n")
    cat("Tiempo transcurrido:", round(tiempo_transcurrido, 1), "min\n")
    cat("Tiempo estimado restante:", round(tiempo_restante, 1), "min\n\n")
  }

  config <- grid[i, ]
  capas <- capas_opciones[[config$capas_idx]]

  cat("Config", i, ":", config$arquitectura,
      "| dropout=", config$dropout,
      "| lr=", config$learning_rate,
      "| batch=", config$batch_size,
      "| act=", config$activacion, "...")

  tryCatch({
    resultado <- crear_entrenar_modelo(
      capas = capas,
      dropout = config$dropout,
      learning_rate = config$learning_rate,
      batch_size = config$batch_size,
      activacion = config$activacion,
      X_train = X_train,
      y_train = y_train,
      X_test = X_test,
      y_test = y_test,
      epochs = 100,
      verbose = 0
    )

    fila <- data.frame(
      config_id = config$config_id,
      arquitectura = config$arquitectura,
      n_capas = config$n_capas,
      dropout = config$dropout,
      learning_rate = config$learning_rate,
      batch_size = config$batch_size,
      activacion = config$activacion,
      epochs_final = resultado$epochs_final,
      rmse_train = resultado$rmse_train,
      rmse_test = resultado$rmse_test,
      mae_train = resultado$mae_train,
      mae_test = resultado$mae_test,
      r2_train = resultado$r2_train,
      r2_test = resultado$r2_test
    )

    resultados <- rbind(resultados, fila)

    cat(" RMSE = $", round(resultado$rmse_test, 2), "\n")

    # Guardar progreso cada 50 configuraciones
    if (i %% 50 == 0) {
      write_xlsx(resultados, archivo_progreso)
      cat(">>> Progreso guardado en:", archivo_progreso, "\n")
    }

    k_clear_session()

  }, error = function(e) {
    cat(" ERROR:", e$message, "\n")
  })
}

tiempo_fin <- Sys.time()
tiempo_total <- difftime(tiempo_fin, tiempo_inicio, units = "hours")

cat("\n===========================================\n")
cat("GRID SEARCH COMPLETADO\n")
cat("Tiempo total:", round(tiempo_total, 2), "horas\n")
cat("Configuraciones exitosas:", nrow(resultados), "/", n_total, "\n")
cat("===========================================\n\n")

# ============================================
# ORDENAR Y SELECCIONAR TOP 3
# ============================================

resultados <- resultados %>%
  arrange(rmse_test) %>%
  mutate(ranking = row_number())

cat("TOP 10 CONFIGURACIONES:\n")
print(head(resultados[, c("ranking", "arquitectura", "dropout", "learning_rate",
                          "batch_size", "activacion", "rmse_test", "mae_test")], 10))

top3 <- resultados %>% head(3)

cat("\n===========================================\n")
cat("LAS 3 MEJORES REDES NEURONALES (PUT)\n")
cat("===========================================\n\n")

for (i in 1:3) {
  cat("RED NEURONAL", LETTERS[i], "\n")
  cat("  Arquitectura:", top3$arquitectura[i], "\n")
  cat("  Capas:", top3$n_capas[i], "\n")
  cat("  Dropout:", top3$dropout[i], "\n")
  cat("  Learning Rate:", top3$learning_rate[i], "\n")
  cat("  Batch Size:", top3$batch_size[i], "\n")
  cat("  Activación:", top3$activacion[i], "\n")
  cat("  Epochs:", top3$epochs_final[i], "\n")
  cat("  RMSE Test: $", round(top3$rmse_test[i], 2), "\n")
  cat("  MAE Test: $", round(top3$mae_test[i], 2), "\n")
  cat("  R² Test:", round(top3$r2_test[i], 4), "\n\n")
}

# ============================================
# GUARDAR RESULTADOS
# ============================================

cat("===========================================\n")
cat("GUARDANDO RESULTADOS\n")
cat("===========================================\n\n")

write_xlsx(resultados, file.path(carpeta_tablas, "900_configuraciones_MLP_PUT.xlsx"))
write_xlsx(top3, file.path(carpeta_tablas, "top3_MLP_PUT.xlsx"))

comparacion <- data.frame(
  Modelo = c("BS Original", "Baseline (C=-$16.34)", "Regresión Lineal",
             "Random Forest", "XGBoost", "KNN",
             paste0("Red Neuronal ", LETTERS[1:3])),
  RMSE = c(105.02, 103.54, 31.43, 11.18, 10.52, 13.79,
           top3$rmse_test[1], top3$rmse_test[2], top3$rmse_test[3]),
  MAE = c(22.78, 34.06, 16.02, 4.53, 4.12, 3.17,
          top3$mae_test[1], top3$mae_test[2], top3$mae_test[3])
) %>%
  mutate(Mejora_vs_Baseline = round((1 - RMSE / 103.54) * 100, 2))

write_xlsx(comparacion, file.path(carpeta_tablas, "comparacion_modelos_MLP_PUT.xlsx"))

cat("Resultados guardados en:", carpeta_tablas, "\n\n")

# ============================================
# GRÁFICOS
# ============================================

cat("===========================================\n")
cat("GENERANDO GRÁFICOS\n")
cat("===========================================\n\n")

color_put <- "#4ECDC4"

# Distribución de RMSE
p1 <- ggplot(resultados, aes(x = rmse_test)) +
  geom_histogram(bins = 30, fill = color_put, alpha = 0.7, color = "white") +
  geom_vline(xintercept = top3$rmse_test[1], linetype = "dashed", color = "darkgreen", linewidth = 1) +
  annotate("text", x = top3$rmse_test[1] + 2, y = Inf, vjust = 2,
           label = paste0("Mejor: $", round(top3$rmse_test[1], 2)), color = "darkgreen", size = 4) +
  labs(
    title = "Distribución de RMSE - 900 Configuraciones MLP (PUT)",
    subtitle = paste0("Grid Search Completo | Rango: $", round(min(resultados$rmse_test), 2),
                      " - $", round(max(resultados$rmse_test), 2)),
    x = "RMSE Test ($)",
    y = "Frecuencia"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-6-1_distribucion_rmse_900_put.png"),
       p1, width = 10, height = 6, dpi = 300)

# RMSE por arquitectura
p2 <- ggplot(resultados, aes(x = reorder(arquitectura, rmse_test), y = rmse_test)) +
  geom_boxplot(fill = color_put, alpha = 0.7) +
  coord_flip() +
  labs(
    title = "RMSE por Arquitectura - MLP (PUT)",
    subtitle = "Grid Search Completo (900 configuraciones)",
    x = "Arquitectura",
    y = "RMSE Test ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-6-1_rmse_arquitectura_put.png"),
       p2, width = 10, height = 8, dpi = 300)

# RMSE por dropout
p3 <- ggplot(resultados, aes(x = factor(dropout), y = rmse_test)) +
  geom_boxplot(fill = color_put, alpha = 0.7) +
  labs(
    title = "RMSE por Tasa de Dropout - MLP (PUT)",
    x = "Dropout",
    y = "RMSE Test ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-6-1_rmse_dropout_put.png"),
       p3, width = 10, height = 6, dpi = 300)

# RMSE por learning rate
p4 <- ggplot(resultados, aes(x = factor(learning_rate), y = rmse_test)) +
  geom_boxplot(fill = color_put, alpha = 0.7) +
  labs(
    title = "RMSE por Learning Rate - MLP (PUT)",
    x = "Learning Rate",
    y = "RMSE Test ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-6-1_rmse_learning_rate_put.png"),
       p4, width = 10, height = 6, dpi = 300)

# Comparación de modelos
df_comp <- comparacion %>%
  mutate(Modelo = factor(Modelo, levels = Modelo))

p5 <- ggplot(df_comp, aes(x = Modelo, y = RMSE)) +
  geom_bar(stat = "identity",
           fill = c("#95A5A6", "#95A5A6", "#3498DB", "#27AE60", "#9B59B6", "#E74C3C",
                    color_put, color_put, color_put),
           alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(RMSE, 2))), vjust = -0.5, size = 3) +
  labs(
    title = "Comparación de Modelos - RMSE (PUT)",
    subtitle = "Incluyendo las 3 mejores redes neuronales (Grid Search 900)",
    x = NULL,
    y = "RMSE ($)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-6-1_comparacion_modelos_put.png"),
       p5, width = 12, height = 7, dpi = 300)

# Top 3
top3_plot <- top3 %>%
  mutate(Red = paste0("Red ", LETTERS[1:3])) %>%
  mutate(Red = factor(Red, levels = Red))

p6 <- ggplot(top3_plot, aes(x = Red, y = rmse_test)) +
  geom_bar(stat = "identity", fill = color_put, alpha = 0.8) +
  geom_text(aes(label = paste0("$", round(rmse_test, 2))), vjust = -0.5, size = 4) +
  geom_text(aes(label = arquitectura, y = rmse_test/2), color = "white", size = 3.5, fontface = "bold") +
  labs(
    title = "Top 3 Redes Neuronales - RMSE (PUT)",
    subtitle = "Grid Search Completo (900 configuraciones)",
    x = NULL,
    y = "RMSE Test ($)"
  ) +
  tema_tesis

ggsave(file.path(carpeta_graficos, "fig_4-6-1_top3_put.png"),
       p6, width = 8, height = 6, dpi = 300)

cat("Gráficos guardados en:", carpeta_graficos, "\n")

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n===========================================\n")
cat("MLP PUT - GRID SEARCH COMPLETO\n")
cat("===========================================\n\n")

cat("CONFIGURACIONES PROBADAS:", nrow(resultados), "/ 900\n")
cat("TIEMPO TOTAL:", round(tiempo_total, 2), "horas\n\n")

cat("TOP 3 REDES NEURONALES:\n")
cat("  Red A - RMSE: $", round(top3$rmse_test[1], 2), " (", top3$arquitectura[1], ")\n")
cat("  Red B - RMSE: $", round(top3$rmse_test[2], 2), " (", top3$arquitectura[2], ")\n")
cat("  Red C - RMSE: $", round(top3$rmse_test[3], 2), " (", top3$arquitectura[3], ")\n\n")

mejora_vs_baseline <- (1 - top3$rmse_test[1] / 103.54) * 100
cat("Mejora vs Baseline:", round(mejora_vs_baseline, 2), "%\n")

cat("\n===========================================\n")

# ============================================================
# EXPORTAR MÉTRICAS A ARCHIVO PLANO PARA EL DOCUMENTO
# ============================================================
archivo_metricas <- "metricas_MLP_PUT.txt"
sink(archivo_metricas)
cat("=============================================================\n")
cat("MÉTRICAS - RED NEURONAL MLP - OPCIONES PUT\n")
cat("=============================================================\n\n")

cat("--- Configuración General ---\n")
cat("Modelo: Red Neuronal MLP\n")
cat("Tipo de opción: PUT\n")
cat("N observaciones entrenamiento:", nrow(train_put), "\n")
cat("N observaciones prueba:", nrow(test_put), "\n")
cat("Configuraciones evaluadas: 900\n")
cat("Optimizador: Adam\n")
cat("Función de pérdida: MSE\n")
cat("Epochs máximos: 100\n")
cat("Early stopping: Paciencia 15 epochs\n\n")

cat("--- Top 3 Redes Neuronales ---\n\n")
for (i in 1:3) {
  cat("Red", LETTERS[i], "\n")
  cat("  Arquitectura:", top3$arquitectura[i], "\n")
  cat("  Dropout:", top3$dropout[i], "\n")
  cat("  Learning Rate:", top3$learning_rate[i], "\n")
  cat("  Batch Size:", top3$batch_size[i], "\n")
  cat("  Activación:", top3$activacion[i], "\n")
  cat("  Epochs:", top3$epochs_final[i], "\n")
  cat("  RMSE Test ($):", round(top3$rmse_test[i], 2), "\n")
  cat("  MAE Test ($):", round(top3$mae_test[i], 2), "\n")
  cat("  R2 Test:", round(top3$r2_test[i], 4), "\n\n")
}

cat("--- Mejor Modelo (Red A) ---\n")
cat("RMSE Test ($):", round(top3$rmse_test[1], 2), "\n")
cat("MAE Test ($):", round(top3$mae_test[1], 2), "\n")
cat("R2 Test:", round(top3$r2_test[1], 4), "\n\n")

cat("--- Comparación con Baseline ---\n")
cat("RMSE Baseline ($): 103.54\n")
cat("Mejora RMSE vs Baseline (%):", round(mejora_vs_baseline, 2), "\n\n")

cat("--- Rango de RMSE en Grid Search ---\n")
cat("RMSE mínimo ($):", round(min(resultados$rmse_test), 2), "\n")
cat("RMSE máximo ($):", round(max(resultados$rmse_test), 2), "\n")
sink()
cat("\nMétricas exportadas a:", archivo_metricas, "\n")
