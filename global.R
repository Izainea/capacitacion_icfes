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

# Paso data_processing 1. Cargar el artefacto de procesamiento de datos
source("R/data_processing.R")

# Paso data_processing  2. Cargar los datos crudos
datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)

#Paso data_processing  3. Ejecutar el pipeline para crear el dataframe limpio que usará la app
datos_saber <- prepare_saber_data(datos_crudos)



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

# 4. Cargar el artefacto del modelo pre-entrenado
modelo_matematicas <- readRDS("models/modelo_puntaje_mat.rds")

message("==> Modelo de predicción cargado en memoria.")

message("==> Entorno global cargado exitosamente.")

