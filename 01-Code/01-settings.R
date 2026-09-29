# ==============================================================================
# 01-settings.R
# Configuración general: paquetes y rutas del proyecto
# Proyecto: Manuscrito 4 - Violencia Escolar y Calor
# ==============================================================================

# Paquetes ---------------------------------------------------------------------

library(tidyverse)   # readr, dplyr, purrr, stringr, tibble
library(lubridate)   # manejo de fechas
library(rio)        # import/export de datos
library(lubridate) # manejo de fechas
library(ggpubr) # compilado de gráficos

# Rutas ------------------------------------------------------------------------

# Se asume que el working directory es la raíz del proyecto
# (Manuscrito4_Violencia_Escolar_Calor/)
DIR_RAW   <- "00-Datos-raw"
DIR_CODE  <- "01-Code"
DIR_INPUT <- "02-Input"
DIR_OUT <- "03-Output"

# Patrón de nombres de archivos de denuncias (versión pública)
# Nomenclatura según ER_DENUNCIAS_PUBL.pdf: AAAAMMDD_DENUNCIAS_AAAA_aaaammdd_PUBL.csv
PATTERN_DENUNCIAS <- "^\\d{8}_DENUNCIAS_\\d{4}_\\d{8}_PUBL\\.csv$"

# Fecha de elaboración del documento ER_DENUNCIAS_PUBL.pdf disponible:
# sus cifras de la sección B corresponden al corte 31-03-2026
PDF_ELABORACION <- "20260331"

# Número esperado de denuncias por año (sección B del esquema de registro,
# ER_DENUNCIAS_PUBL.pdf). Se usa para validar la extracción.
N_DENUNCIAS_ESPERADAS <- tibble::tribble(
  ~AGNO, ~n_esperado,
  2014,  11425,
  2015,  10792,
  2016,  11502,
  2017,  12672,
  2018,  15017,
  2019,  12016,
  2020,   3379,
  2021,   3961,
  2022,  16161,
  2023,  17323,
  2024,  19114,
  2025,  22519,
  2026,   2175
)
