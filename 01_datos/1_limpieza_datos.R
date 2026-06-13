# ============================================
# SCRIPT DE LIMPIEZA DE DATOS
# Genera el dataset limpio requerido
# ============================================

library(readxl)
library(tidyverse)
library(writexl)

# ============================================
# CONFIGURACIÓN
# ============================================

# Configuración de rutas
CARPETA_BASE <- "C:/Users/julia/OneDrive/Documentos/bookdown-demo-main"
CARPETA_DATOS <- file.path(CARPETA_BASE, "out/2025-10-06")
CARPETA_SALIDA <- file.path(CARPETA_BASE, "objetivo 1/sub_3-4")  # Sección 3.4

fecha_archivo <- as.Date("2025-10-06")

cat("====================================\n")
cat("LIMPIEZA DE DATOS\n")
cat("====================================\n\n")
cat("Carpeta base:", CARPETA_BASE, "\n")
cat("Carpeta datos (entrada):", CARPETA_DATOS, "\n")
cat("Carpeta salida (Sección 3.4):", CARPETA_SALIDA, "\n")
cat("Fecha de análisis:", as.character(fecha_archivo), "\n\n")

# ============================================
# VERIFICAR Y CREAR CARPETAS
# ============================================

cat("Verificando estructura de carpetas...\n")

if (!dir.exists(CARPETA_DATOS)) {
  stop("ERROR: No existe la carpeta de datos: ", CARPETA_DATOS)
}

if (!dir.exists(CARPETA_SALIDA)) {
  cat("  Creando carpeta de salida sub_3-4...\n")
  dir.create(CARPETA_SALIDA, recursive = TRUE)
  cat("  ✓ Carpeta creada exitosamente\n")
}

# Listar archivos disponibles
cat("\nArchivos disponibles en carpeta de entrada:\n")
archivos_disponibles <- list.files(CARPETA_DATOS, pattern = "\\.xlsx$")
if (length(archivos_disponibles) > 0) {
  for (arch in archivos_disponibles) {
    cat("  -", arch, "\n")
  }
} else {
  stop("ERROR: No hay archivos Excel en la carpeta de datos")
}
cat("\n")

# ============================================
# CARGAR DATOS ORIGINALES
# ============================================

cat("Cargando dataset original...\n")
archivo_original <- file.path(CARPETA_DATOS, 
                              paste0("1_dataset_ml_completo_", fecha_archivo, ".xlsx"))

cat("  Buscando archivo:", archivo_original, "\n")

if (!file.exists(archivo_original)) {
  stop("ERROR: No se encuentra el archivo original.\n",
       "  Ruta esperada: ", archivo_original, "\n",
       "  Verifica que el archivo existe en la carpeta.")
}

dataset_original <- read_xlsx(archivo_original)
cat("  ✓ Datos cargados:", format(nrow(dataset_original), big.mark = ","), "observaciones\n")
cat("  ✓ Variables:", ncol(dataset_original), "\n\n")

# Mostrar primeras columnas
cat("Variables disponibles:\n")
nombres_vars <- names(dataset_original)
if (length(nombres_vars) > 10) {
  cat(" ", paste(nombres_vars[1:10], collapse = ", "), "...\n\n")
} else {
  cat(" ", paste(nombres_vars, collapse = ", "), "\n\n")
}

# ============================================
# PROCESO DE LIMPIEZA
# ============================================

cat("====================================\n")
cat("INICIANDO PROCESO DE LIMPIEZA\n")
cat("====================================\n\n")

# Etapa 1: Eliminar valores faltantes en variables críticas
cat("ETAPA 1: Eliminando valores faltantes\n")
cat("--------------------------------------\n")

# Verificar que las variables existen
variables_criticas <- c("IV", "Vol", "OI")
variables_faltantes <- setdiff(variables_criticas, names(dataset_original))

if (length(variables_faltantes) > 0) {
  stop("ERROR: Variables no encontradas en el dataset: ", 
       paste(variables_faltantes, collapse = ", "))
}

# Contar NAs antes
cat("Valores faltantes antes de limpieza:\n")
cat("  IV:", sum(is.na(dataset_original$IV)), "\n")
cat("  Vol:", sum(is.na(dataset_original$Vol)), "\n")
cat("  OI:", sum(is.na(dataset_original$OI)), "\n\n")

dataset_sin_na <- dataset_original %>%
  drop_na(IV, Vol, OI)

eliminados_na <- nrow(dataset_original) - nrow(dataset_sin_na)
cat("Resultado:\n")
cat("  Observaciones eliminadas:", format(eliminados_na, big.mark = ","), 
    sprintf("(%.2f%%)", 100 * eliminados_na / nrow(dataset_original)), "\n")
cat("  Observaciones restantes:", format(nrow(dataset_sin_na), big.mark = ","), "\n\n")

# Etapa 2: Eliminar volatilidades implícitas extremas
cat("ETAPA 2: Eliminando volatilidades extremas\n")
cat("-------------------------------------------\n")

# Mostrar estadísticas de IV antes del filtro
cat("Estadísticas de IV antes del filtro:\n")
cat("  Mínimo:", round(min(dataset_sin_na$IV, na.rm = TRUE), 3), "\n")
cat("  Máximo:", round(max(dataset_sin_na$IV, na.rm = TRUE), 3), "\n")
cat("  Media:", round(mean(dataset_sin_na$IV, na.rm = TRUE), 3), "\n")
cat("  Mediana:", round(median(dataset_sin_na$IV, na.rm = TRUE), 3), "\n")
cat("  Umbral de corte: 2.15 (215%)\n\n")

# Contar cuántas observaciones tienen IV > 2.15
n_extremos <- sum(dataset_sin_na$IV > 2.15, na.rm = TRUE)
cat("Observaciones con IV > 2.15:", n_extremos, "\n\n")

dataset_limpio <- dataset_sin_na %>%
  filter(IV <= 2.15)

eliminados_iv <- nrow(dataset_sin_na) - nrow(dataset_limpio)
cat("Resultado:\n")
cat("  Observaciones eliminadas:", format(eliminados_iv, big.mark = ","),
    sprintf("(%.2f%%)", 100 * eliminados_iv / nrow(dataset_sin_na)), "\n")
cat("  Observaciones finales:", format(nrow(dataset_limpio), big.mark = ","), "\n\n")

# ============================================
# VERIFICACIONES DE INTEGRIDAD
# ============================================

cat("====================================\n")
cat("VERIFICACIONES DE INTEGRIDAD\n")
cat("====================================\n\n")

# Verificar ausencia de NAs
na_count <- sum(is.na(dataset_limpio))
cat("1. Verificación de valores faltantes:\n")
cat("   Total de NAs en dataset limpio:", na_count, "\n")

if (na_count == 0) {
  cat("   ✓ No hay valores faltantes\n\n")
} else {
  cat("   ⚠ Aún hay valores faltantes:\n")
  na_por_columna <- colSums(is.na(dataset_limpio))
  print(na_por_columna[na_por_columna > 0])
  cat("\n")
}

# Verificar restricciones de dominio
cat("2. Verificación de restricciones de dominio:\n\n")

verificaciones <- data.frame(
  Verificacion = c(
    "Strike > 0",
    "Precio_Actual > 0",
    "Dias_Vencimiento >= 0",
    "Last > 0",
    "IV <= 2.15",
    "Volatilidad_Historica > 0"
  ),
  N_Validos = c(
    sum(dataset_limpio$Strike > 0),
    sum(dataset_limpio$Precio_Actual > 0),
    sum(dataset_limpio$Dias_Vencimiento >= 0),
    sum(dataset_limpio$Last > 0),
    sum(dataset_limpio$IV <= 2.15),
    sum(dataset_limpio$Volatilidad_Historica > 0)
  ),
  Total = nrow(dataset_limpio),
  Cumple = c(
    all(dataset_limpio$Strike > 0),
    all(dataset_limpio$Precio_Actual > 0),
    all(dataset_limpio$Dias_Vencimiento >= 0),
    all(dataset_limpio$Last > 0),
    all(dataset_limpio$IV <= 2.15),
    all(dataset_limpio$Volatilidad_Historica > 0)
  )
)

print(verificaciones)

if (all(verificaciones$Cumple)) {
  cat("\n✓ Todas las verificaciones pasaron exitosamente\n\n")
} else {
  cat("\n⚠ ADVERTENCIA: Algunas verificaciones fallaron\n\n")
  print(verificaciones[!verificaciones$Cumple, ])
  cat("\n")
}

# ============================================
# GUARDAR DATASET LIMPIO
# ============================================

cat("====================================\n")
cat("GUARDANDO DATASET LIMPIO\n")
cat("====================================\n\n")

nombre_archivo <- paste0("dataset_limpio_", nrow(dataset_limpio), ".xlsx")
archivo_limpio <- file.path(CARPETA_SALIDA, nombre_archivo)

cat("Guardando archivo...\n")
cat("  Nombre:", nombre_archivo, "\n")
cat("  Ubicación:", CARPETA_SALIDA, "\n")
cat("  Ruta completa:", archivo_limpio, "\n\n")

write_xlsx(dataset_limpio, archivo_limpio)

# Verificar que se guardó correctamente
if (file.exists(archivo_limpio)) {
  cat("✓ Archivo guardado exitosamente\n")
  tamanio_mb <- round(file.size(archivo_limpio) / 1024^2, 2)
  cat("  Tamaño:", tamanio_mb, "MB\n\n")
} else {
  stop("ERROR: No se pudo guardar el archivo")
}

# ============================================
# RESUMEN FINAL
# ============================================

cat("====================================\n")
cat("RESUMEN FINAL\n")
cat("====================================\n\n")

cat("ESTADÍSTICAS DEL PROCESO:\n")
cat("-------------------------\n")
cat("Observaciones originales:", format(nrow(dataset_original), big.mark = ","), "\n")
cat("Eliminadas por NAs:", format(eliminados_na, big.mark = ","), 
    sprintf("(%.2f%%)", 100 * eliminados_na / nrow(dataset_original)), "\n")
cat("Eliminadas por IV extremo:", format(eliminados_iv, big.mark = ","),
    sprintf("(%.2f%%)", 100 * eliminados_iv / nrow(dataset_original)), "\n")
cat("Total eliminadas:", format(nrow(dataset_original) - nrow(dataset_limpio), big.mark = ","),
    sprintf("(%.2f%%)", 100 * (nrow(dataset_original) - nrow(dataset_limpio)) / nrow(dataset_original)), "\n")
cat("Observaciones finales:", format(nrow(dataset_limpio), big.mark = ","), "\n")
cat("Tasa de retención:", 
    sprintf("%.2f%%", 100 * nrow(dataset_limpio) / nrow(dataset_original)), "\n\n")

cat("DISTRIBUCIÓN POR TIPO DE OPCIÓN:\n")
cat("--------------------------------\n")
tabla_tipo <- table(dataset_limpio$Tipo)
for (i in seq_along(tabla_tipo)) {
  cat(sprintf("  %s: %s (%.1f%%)\n", 
              names(tabla_tipo)[i], 
              format(tabla_tipo[i], big.mark = ","),
              100 * tabla_tipo[i] / nrow(dataset_limpio)))
}
cat("\n")

cat("DISTRIBUCIÓN POR TICKER:\n")
cat("------------------------\n")
tabla_ticker <- sort(table(dataset_limpio$Ticker), decreasing = TRUE)
for (i in seq_along(tabla_ticker)) {
  cat(sprintf("  %s: %s (%.1f%%)\n", 
              names(tabla_ticker)[i], 
              format(tabla_ticker[i], big.mark = ","),
              100 * tabla_ticker[i] / nrow(dataset_limpio)))
}
cat("\n")

cat("ARCHIVO GENERADO:\n")
cat("-----------------\n")
cat("Sección: 3.4 - Limpieza y preparación de bases de datos\n")
cat("Archivo:", nombre_archivo, "\n")
cat("Ubicación:", CARPETA_SALIDA, "\n\n")

cat("====================================\n")
cat("✓ PROCESO COMPLETADO EXITOSAMENTE\n")
cat("====================================\n\n")

cat("El dataset limpio está listo para:\n")
cat("→ Validación (script_3-4_validacion.R)\n")
cat("→ Sección 3.5: Consolidación y estructura del dataset final\n")
cat("→ Sección 3.6: Partición en conjuntos de entrenamiento y prueba\n\n")
