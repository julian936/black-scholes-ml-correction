# ============================================
# BLACK-SCHOLES: CALIBRACION MULTITICKER
# Script completo con multiples tickers y vencimientos
# Organizado por carpetas de fecha
# ============================================

library(quantmod)
library(tidyverse)
library(writexl)

# ============================================
# CONFIGURACION
# ============================================

# Lista de tickers a analizar
tickers <- c(
  "AAPL",   # Apple
  "MSFT",   # Microsoft
  "GOOGL",  # Google
  "TSLA",   # Tesla
  "AMZN",   # Amazon
  "META",   # Meta
  "NVDA",   # Nvidia
  "SPY",    # S&P 500 ETF
  "QQQ",    # Nasdaq ETF
  "AMD"     # AMD
)

# Generar graficos (toma mas tiempo)
generar_graficos <- FALSE

# Crear estructura de carpetas por fecha
fecha_descarga <- Sys.Date()
carpeta_base <- "out"
carpeta_fecha <- file.path(carpeta_base, as.character(fecha_descarga))

if (!dir.exists(carpeta_base)) {
  dir.create(carpeta_base)
  cat("Carpeta base 'out' creada\n")
}

if (!dir.exists(carpeta_fecha)) {
  dir.create(carpeta_fecha)
  cat("Carpeta de fecha creada:", carpeta_fecha, "\n")
} else {
  cat("Carpeta de fecha ya existe:", carpeta_fecha, "\n")
}

cat("\n====================================\n")
cat("CALIBRACION BLACK-SCHOLES CON C\n")
cat("====================================\n\n")
cat("Fecha de ejecucion:", fecha_descarga, "\n")
cat("Tickers a procesar:", length(tickers), "\n")
cat("Generar graficos:", ifelse(generar_graficos, "SI", "NO"), "\n")
cat("Archivos en:", carpeta_fecha, "\n\n")

# ============================================
# FUNCION BLACK-SCHOLES
# ============================================

black_scholes <- function(S, K, T, r, sigma, type = "call") {
  d1 <- (log(S/K) + (r + sigma^2/2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if (type == "call") {
    precio <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else {
    precio <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }

  return(precio)
}

# ============================================
# OBTENER TASA LIBRE DE RIESGO
# ============================================

cat("Obteniendo tasa libre de riesgo...\n")
getSymbols("^IRX", src = "yahoo", auto.assign = TRUE, warnings = FALSE)
r_global <- as.numeric(last(Cl(IRX))) / 100
cat("  Tasa libre de riesgo:", round(r_global * 100, 2), "%\n\n")

# ============================================
# DATASETS ACUMULADORES
# ============================================

dataset_calls_completo <- data.frame()
dataset_puts_completo <- data.frame()
resumen_por_ticker <- data.frame()

ticker_count <- 0

# ============================================
# LOOP PRINCIPAL
# ============================================

for(ticker in tickers) {

  ticker_count <- ticker_count + 1

  cat("\n")
  cat("===========================================\n")
  cat("TICKER", ticker_count, "de", length(tickers), ":", ticker, "\n")
  cat("===========================================\n\n")

  tryCatch({

    # Paso 1: Datos del activo
    cat("  [1/5] Descargando datos historicos...\n")
    getSymbols(ticker, src = "yahoo", from = Sys.Date() - 365,
               to = Sys.Date(), auto.assign = TRUE, warnings = FALSE)

    S <- as.numeric(last(Cl(get(ticker))))
    rendimientos <- dailyReturn(Cl(get(ticker)))
    sigma <- sd(rendimientos, na.rm = TRUE) * sqrt(252)
    r <- r_global

    cat("        Precio actual: $", round(S, 2), "\n")
    cat("        Volatilidad:", round(sigma * 100, 2), "%\n\n")

    # Paso 2: Cadenas de opciones
    cat("  [2/5] Obteniendo opciones...\n")
    opciones <- getOptionChain(ticker, NULL)
    fechas_vencimiento <- names(opciones)
    num_vencimientos <- length(fechas_vencimiento)

    cat("        Vencimientos:", num_vencimientos, "\n")
    cat("       ", paste(fechas_vencimiento[1:min(3, num_vencimientos)],
                         collapse = ", "))
    if(num_vencimientos > 3) cat(", ...")
    cat("\n\n")

    # Acumuladores del ticker
    calls_ticker <- data.frame()
    puts_ticker <- data.frame()

    # Paso 3: Procesar cada vencimiento
    cat("  [3/5] Procesando vencimientos...\n")

    for(fecha_venc_str in fechas_vencimiento) {

      calls <- opciones[[fecha_venc_str]]$calls
      puts <- opciones[[fecha_venc_str]]$puts

      # Calcular tiempo
      fecha_venc <- tryCatch({
        as.Date(fecha_venc_str)
      }, error = function(e) {
        tryCatch({
          as.Date(fecha_venc_str, format = "%b.%d.%Y")
        }, error = function(e2) {
          as.Date(fecha_venc_str, format = "%b %d %Y")
        })
      })

      T_dias <- as.numeric(difftime(fecha_venc, Sys.Date(), units = "days"))
      T <- T_dias / 365

      # Calcular precios BS
      calls$Precio_BS <- sapply(calls$Strike, function(K) {
        black_scholes(S, K, T, r, sigma, "call")
      })

      puts$Precio_BS <- sapply(puts$Strike, function(K) {
        black_scholes(S, K, T, r, sigma, "put")
      })

      # Calcular diferencias
      calls$Diferencia <- calls$Last - calls$Precio_BS
      puts$Diferencia <- puts$Last - puts$Precio_BS

      # Agregar metadatos
      calls$Ticker <- ticker
      calls$Fecha_Descarga <- fecha_descarga
      calls$Fecha_Vencimiento <- fecha_venc_str
      calls$Fecha_Vencimiento_Date <- fecha_venc
      calls$Dias_Vencimiento <- T_dias
      calls$Precio_Actual <- S
      calls$Volatilidad_Historica <- sigma
      calls$Tasa_Libre_Riesgo <- r
      calls$Tipo <- "CALL"

      puts$Ticker <- ticker
      puts$Fecha_Descarga <- fecha_descarga
      puts$Fecha_Vencimiento <- fecha_venc_str
      puts$Fecha_Vencimiento_Date <- fecha_venc
      puts$Dias_Vencimiento <- T_dias
      puts$Precio_Actual <- S
      puts$Volatilidad_Historica <- sigma
      puts$Tasa_Libre_Riesgo <- r
      puts$Tipo <- "PUT"

      # Acumular
      calls_ticker <- rbind(calls_ticker, calls)
      puts_ticker <- rbind(puts_ticker, puts)
    }

    cat("        CALL:", nrow(calls_ticker), "\n")
    cat("        PUT:", nrow(puts_ticker), "\n\n")

    # Paso 4: Calcular C
    cat("  [4/5] Calculando C...\n")

    C_calls <- mean(calls_ticker$Diferencia, na.rm = TRUE)
    C_puts <- mean(puts_ticker$Diferencia, na.rm = TRUE)

    cat("        C_calls: $", round(C_calls, 4), "\n")
    cat("        C_puts:  $", round(C_puts, 4), "\n\n")

    # Aplicar calibracion
    calls_ticker$Precio_Calibrado <- calls_ticker$Precio_BS + C_calls
    puts_ticker$Precio_Calibrado <- puts_ticker$Precio_BS + C_puts

    calls_ticker$Error_Calibrado <- calls_ticker$Last - calls_ticker$Precio_Calibrado
    puts_ticker$Error_Calibrado <- puts_ticker$Last - puts_ticker$Precio_Calibrado

    # Calcular RMSE
    rmse_calls_orig <- sqrt(mean(calls_ticker$Diferencia^2, na.rm = TRUE))
    rmse_calls_cal <- sqrt(mean(calls_ticker$Error_Calibrado^2, na.rm = TRUE))
    rmse_puts_orig <- sqrt(mean(puts_ticker$Diferencia^2, na.rm = TRUE))
    rmse_puts_cal <- sqrt(mean(puts_ticker$Error_Calibrado^2, na.rm = TRUE))

    mejora_calls <- (1 - rmse_calls_cal / rmse_calls_orig) * 100
    mejora_puts <- (1 - rmse_puts_cal / rmse_puts_orig) * 100

    cat("        RMSE Calls:", round(rmse_calls_orig, 2), "->",
        round(rmse_calls_cal, 2), "(", round(mejora_calls, 1), "%)\n")
    cat("        RMSE Puts:", round(rmse_puts_orig, 2), "->",
        round(rmse_puts_cal, 2), "(", round(mejora_puts, 1), "%)\n\n")

    # Guardar resumen
    resumen_ticker <- data.frame(
      Ticker = ticker,
      Fecha_Analisis = fecha_descarga,
      Precio_Actual = S,
      Volatilidad_Historica = sigma * 100,
      Num_Vencimientos = num_vencimientos,
      Num_Calls = nrow(calls_ticker),
      Num_Puts = nrow(puts_ticker),
      C_Calls = C_calls,
      C_Puts = C_puts,
      RMSE_Original_Calls = rmse_calls_orig,
      RMSE_Calibrado_Calls = rmse_calls_cal,
      Mejora_Calls_Pct = mejora_calls,
      RMSE_Original_Puts = rmse_puts_orig,
      RMSE_Calibrado_Puts = rmse_puts_cal,
      Mejora_Puts_Pct = mejora_puts
    )

    resumen_por_ticker <- rbind(resumen_por_ticker, resumen_ticker)

    # Paso 5: Features para ML
    cat("  [5/5] Agregando features ML...\n")

    calls_ticker <- calls_ticker %>%
      mutate(
        Moneyness = Precio_Actual / Strike,
        Log_Moneyness = log(Precio_Actual / Strike),
        Vol_Diff = ifelse(!is.na(IV), IV - Volatilidad_Historica, NA),
        Strike_Normalizado = (Strike - Precio_Actual) / Precio_Actual,
        In_The_Money = ifelse(Precio_Actual > Strike, 1, 0)
      )

    puts_ticker <- puts_ticker %>%
      mutate(
        Moneyness = Precio_Actual / Strike,
        Log_Moneyness = log(Precio_Actual / Strike),
        Vol_Diff = ifelse(!is.na(IV), IV - Volatilidad_Historica, NA),
        Strike_Normalizado = (Strike - Precio_Actual) / Precio_Actual,
        In_The_Money = ifelse(Precio_Actual < Strike, 1, 0)
      )

    # Acumular globalmente
    dataset_calls_completo <- rbind(dataset_calls_completo, calls_ticker)
    dataset_puts_completo <- rbind(dataset_puts_completo, puts_ticker)

    cat("        Ticker", ticker, "completado\n")

    # Pausa
    Sys.sleep(2)

  }, error = function(e) {
    cat("        ERROR en", ticker, ":", conditionMessage(e), "\n")
  })
}

# ============================================
# ESTADISTICAS GLOBALES
# ============================================

cat("\n\n")
cat("===========================================\n")
cat("ESTADISTICAS GLOBALES\n")
cat("===========================================\n\n")

total_calls <- nrow(dataset_calls_completo)
total_puts <- nrow(dataset_puts_completo)

C_calls_global <- mean(dataset_calls_completo$Diferencia, na.rm = TRUE)
C_puts_global <- mean(dataset_puts_completo$Diferencia, na.rm = TRUE)

rmse_calls_global <- sqrt(mean(dataset_calls_completo$Diferencia^2, na.rm = TRUE))
rmse_puts_global <- sqrt(mean(dataset_puts_completo$Diferencia^2, na.rm = TRUE))

cat("Total observaciones:\n")
cat("  CALLS:", total_calls, "\n")
cat("  PUTS:", total_puts, "\n")
cat("  TOTAL:", total_calls + total_puts, "\n\n")

cat("Constantes globales:\n")
cat("  C_calls: $", round(C_calls_global, 4), "\n")
cat("  C_puts:  $", round(C_puts_global, 4), "\n\n")

cat("RMSE global:\n")
cat("  CALLS: $", round(rmse_calls_global, 2), "\n")
cat("  PUTS:  $", round(rmse_puts_global, 2), "\n\n")

# ============================================
# GUARDAR ARCHIVOS (en carpeta por fecha)
# ============================================

cat("Guardando archivos en", carpeta_fecha, "...\n\n")

# Dataset completo
dataset_ml_completo <- rbind(dataset_calls_completo, dataset_puts_completo)

write_xlsx(dataset_ml_completo,
           file.path(carpeta_fecha, paste0("1_dataset_ml_completo_", fecha_descarga, ".xlsx")))
cat("  1_dataset_ml_completo_", fecha_descarga, ".xlsx\n", sep = "")

write_xlsx(dataset_calls_completo,
           file.path(carpeta_fecha, paste0("2_dataset_calls_ml_", fecha_descarga, ".xlsx")))
cat("  2_dataset_calls_ml_", fecha_descarga, ".xlsx\n", sep = "")

write_xlsx(dataset_puts_completo,
           file.path(carpeta_fecha, paste0("3_dataset_puts_ml_", fecha_descarga, ".xlsx")))
cat("  3_dataset_puts_ml_", fecha_descarga, ".xlsx\n", sep = "")

write_xlsx(resumen_por_ticker,
           file.path(carpeta_fecha, paste0("4_resumen_por_ticker_", fecha_descarga, ".xlsx")))
cat("  4_resumen_por_ticker_", fecha_descarga, ".xlsx\n", sep = "")

# Resumen global
resumen_global <- data.frame(
  Fecha_Analisis = fecha_descarga,
  Num_Tickers = length(unique(dataset_ml_completo$Ticker)),
  Total_Observaciones = nrow(dataset_ml_completo),
  Total_Calls = total_calls,
  Total_Puts = total_puts,
  C_Calls_Global = C_calls_global,
  C_Puts_Global = C_puts_global,
  RMSE_Calls_Global = rmse_calls_global,
  RMSE_Puts_Global = rmse_puts_global,
  Tasa_Libre_Riesgo = r_global * 100
)

write_xlsx(resumen_global,
           file.path(carpeta_fecha, paste0("5_resumen_global_", fecha_descarga, ".xlsx")))
cat("  5_resumen_global_", fecha_descarga, ".xlsx\n", sep = "")

# Archivo consolidado
if (require(openxlsx, quietly = TRUE)) {
  wb <- createWorkbook()

  addWorksheet(wb, "Resumen_Global")
  writeData(wb, "Resumen_Global", resumen_global)

  addWorksheet(wb, "Resumen_Por_Ticker")
  writeData(wb, "Resumen_Por_Ticker", resumen_por_ticker)

  addWorksheet(wb, "Dataset_Completo")
  writeData(wb, "Dataset_Completo", dataset_ml_completo)

  saveWorkbook(wb, file.path(carpeta_fecha, paste0("6_analisis_completo_", fecha_descarga, ".xlsx")), overwrite = TRUE)

  cat("  6_analisis_completo_", fecha_descarga, ".xlsx\n", sep = "")
}

# ============================================
# RESUMEN FINAL
# ============================================

cat("\n\n")
cat("===========================================\n")
cat("ANALISIS COMPLETADO\n")
cat("===========================================\n\n")

cat("Fecha de ejecucion:", fecha_descarga, "\n")
cat("Tickers procesados:", length(unique(dataset_ml_completo$Ticker)), "\n")
cat("Observaciones totales:", nrow(dataset_ml_completo), "\n")
cat("  CALLS:", total_calls, "\n")
cat("  PUTS:", total_puts, "\n\n")

cat("Archivos guardados en:\n")
cat(" ", carpeta_fecha, "\n\n")

cat("Variables ML:\n")
cat("  Features: Strike, Precio_Actual, Dias_Vencimiento,\n")
cat("            Vol, OI, IV, Volatilidad_Historica,\n")
cat("            Moneyness, Log_Moneyness, Vol_Diff,\n")
cat("            Strike_Normalizado, In_The_Money\n")
cat("  Target: Diferencia\n\n")

cat("Proximo paso: Machine Learning\n\n")

cat("Abrir carpeta de resultados:\n")
if (.Platform$OS.type == "windows") {
  cat("  shell.exec('", carpeta_fecha, "')\n", sep = "")
} else {
  cat("  system('open \"", carpeta_fecha, "\"')\n", sep = "")
}

cat("\n===========================================\n")
