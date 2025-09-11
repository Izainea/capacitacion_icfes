#############################################################
# ARTEFACTO: Model Training
#############################################################

# Contenido: R/model_training.R

# Cargamos las librerías necesarias para este script
library(readr)
library(dplyr)

# 1. Cargamos los datos crudos (simulando un entorno de entrenamiento)
datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE) %>% na.omit()

# 2. Entrenamos un modelo lineal simple
# El objetivo es predecir el puntaje de matemáticas basándose en los otros puntajes
modelo_puntaje <- lm(
  punt_matematicas ~ punt_lectura_critica + punt_sociales_ciudadanas + punt_c_naturales,
  data = datos_crudos
)

# 3. Guardamos el modelo entrenado como un artefacto en la carpeta /models
# Este es el único resultado que nos importa de este script
saveRDS(modelo_puntaje, "models/modelo_puntaje_mat.rds")

message("==> Modelo entrenado y guardado exitosamente en /models/modelo_puntaje_mat.rds")