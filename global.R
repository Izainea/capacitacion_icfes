# ==============================================================================
# ARCHIVO GLOBAL: PREPARACIÓN DEL ENTORNO
# ==============================================================================
# Este script se ejecuta una sola vez al iniciar la aplicación.
# Su misión es cargar librerías, datos y definir objetos globales.
# ------------------------------------------------------------------------------

# 1. Carga de Librerías
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)

# 2. Carga de Datos Crudos
# Leemos y preparamos los datos una sola vez al iniciar la app
datos_saber <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE) %>%
  na.omit() # Asegurarnos de quitar NAs para los cálculos

# 3. Creación de Objetos Globales
# Creamos objetos que la UI o el Server puedan necesitar.
puntajes_choices <- datos_saber %>%
  select(starts_with("punt_")) %>%
  names()

# 4. Cálculos Estáticos para las Tarjetas de KPI
total_estudiantes <- format(nrow(datos_saber), big.mark = ",")
puntaje_global_promedio <- round(mean(datos_saber$punt_global, na.rm = TRUE), 1)
colegios_oficiales <- format(sum(datos_saber$cole_naturaleza == "OFICIAL"), big.mark = ",")
colegios_no_oficiales <- format(sum(datos_saber$cole_naturaleza == "NO OFFICIAL"), big.mark = ",")

message("==> Entorno global cargado exitosamente.")

