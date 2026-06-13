# Predicción de Errores del Modelo Black-Scholes mediante Modelos Clásicos de Ciencia de Datos

Repositorio de código del trabajo de grado de la **Maestría en Ciencia de Datos**
(Pontificia Universidad Javeriana, Sede Cali).

El proyecto implementa un enfoque híbrido **Black-Scholes + Machine Learning**: en lugar de
reemplazar el modelo de Black-Scholes, se predice su error sistemático de valoración mediante
modelos de aprendizaje automático y se utiliza esa predicción para corregir el precio teórico,
acercándolo al precio observado en el mercado.

Las opciones **CALL** y **PUT** se modelan por separado, dado que sus errores de valoración
presentan distribuciones y mecanismos generadores distintos.

---

## Estructura del repositorio

```
black-scholes-ml-correction/
├── 01_datos/
│   ├── 0_descarga_datos.R          # Descarga Yahoo Finance + cálculo Black-Scholes
│   ├── 1_limpieza_datos.R          # Limpieza del dataset
│   ├── 2_preparacion_datos.R       # Variables, exploratorio, consolidación y partición train/test
│   └── 1_dataset_ml_completo_2025-10-06.xlsx   # Dataset descargado (datos congelados)
├── 02_modelos/
│   ├── baseline.R                  # Modelo baseline (corrección por constante C)
│   ├── regresion_lineal_CALL.R / regresion_lineal_PUT.R
│   ├── randomForest_CALL.R / randomForest_PUT.R
│   ├── xgboost_CALL.R / xgboost_PUT.R
│   ├── KNN_CALL.R / KNN_PUT.R
│   └── MLP_CALL.R / MLP_PUT.R
└── README.md
```

---

## Orden de ejecución (pipeline)

Los scripts deben ejecutarse en este orden, ya que cada etapa genera los insumos de la siguiente:

1. `01_datos/0_descarga_datos.R` — descarga las cadenas de opciones de Yahoo Finance, obtiene la
   tasa libre de riesgo (`^IRX`) y calcula el precio teórico de Black-Scholes. Produce
   `1_dataset_ml_completo_2025-10-06.xlsx`.
2. `01_datos/1_limpieza_datos.R` — elimina valores faltantes y filtra valores atípicos de
   volatilidad implícita. Produce el dataset limpio.
3. `01_datos/2_preparacion_datos.R` — genera las variables derivadas, el análisis exploratorio y
   la partición estratificada 80/20, produciendo los conjuntos de entrenamiento y prueba para CALL
   y PUT.
4. `02_modelos/*.R` — cada script de modelo (baseline, regresión lineal, Random Forest, XGBoost,
   KNN y MLP) se entrena y evalúa de forma independiente para CALL y PUT.

> **Nota sobre reproducibilidad:** `0_descarga_datos.R` descarga datos en vivo desde Yahoo Finance,
> por lo que ejecutarlo en una fecha distinta producirá un conjunto de datos diferente. Para
> reproducir exactamente los resultados de la tesis se incluye el dataset descargado el
> **6 de octubre de 2025** (`1_dataset_ml_completo_2025-10-06.xlsx`); basta con partir del paso 2.

---

## Requisitos

- **R** (versión 4.x o superior)
- Para el modelo MLP: **Python** con **TensorFlow**, ya que `keras3` lo usa como backend.

### Paquetes de R

```r
install.packages(c(
  "readxl", "tidyverse", "writexl", "scales", "ggplot2", "gridExtra",
  "quantmod", "caret", "corrplot", "moments",
  "randomForest", "xgboost", "class",
  "car", "lmtest", "nortest", "keras3"
))
```

Dependencias principales por etapa:

- **Descarga (`0_descarga_datos.R`):** `quantmod`, `tidyverse`, `writexl`
- **Preparación (`1_` y `2_`):** `readxl`, `tidyverse`, `writexl`, `caret`, `corrplot`, `gridExtra`, `moments`, `scales`
- **Regresión Lineal:** `ggplot2`, `gridExtra`, `car` (VIF), `lmtest` (Breusch-Pagan), `nortest` (Anderson-Darling)
- **Random Forest:** `randomForest`
- **XGBoost:** `xgboost`, `caret`
- **KNN:** `class`, `caret`
- **MLP:** `keras3` (requiere backend de TensorFlow en Python)

---

## Datos

Los datos provienen de **Yahoo Finance**, descargados el **6 de octubre de 2025**, cubriendo diez
activos del mercado estadounidense: AAPL, AMD, AMZN, GOOGL, META, MSFT, NVDA, QQQ, SPY y TSLA.
La tasa libre de riesgo se obtiene del ticker `^IRX`.

El dataset original contiene 40,337 contratos; tras la limpieza quedan 37,947, particionados en
30,365 de entrenamiento y 7,582 de prueba.

---

## Configuración de rutas

Cada script define al inicio las rutas de entrada y salida, ajustadas al equipo original:

```r
RUTA_ENTRADA <- "..."
RUTA_SALIDA  <- "..."
```

Antes de ejecutar en otro equipo, **estas rutas deben modificarse** para apuntar a las carpetas
locales correspondientes.

---

## Salidas

Cada script de modelo genera:

- Tablas de resultados en formato `.xlsx`
- Gráficos en `.png` (convergencia, importancia de variables, predicciones vs. reales, etc.)
- Un archivo de métricas en `.txt` con la configuración y el desempeño del modelo

---

## Resultados principales

| Tipo de opción | Mejor modelo | RMSE (prueba) | Mejora vs. baseline |
|---|---|---|---|
| CALL | Random Forest | $17.38 | 82.20 % |
| PUT  | XGBoost | $10.83 | 89.54 % |

---

## Prototipo web interactivo

Como cierre de la fase de despliegue (CRISP-DM), se desarrolló un prototipo web que permite
explorar, contrato por contrato, la corrección aplicada por los modelos ganadores sobre el
conjunto de prueba:

**https://julian936.github.io/Tesis---Prediccion-BS/**

---

## Autores

- Natalia María Tangarife Acevedo
- Julián Rojas Ramírez

**Director:** Cristian Alejandro Torres Valencia

**Maestría en Ciencia de Datos — Pontificia Universidad Javeriana, Sede Cali**
