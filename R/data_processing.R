#############################################################
# ARTEFACTO: Data Processing
# Descripción: Funciones para procesar los datos de SABER 11
#############################################################


prepare_saber_data <- function(raw_data) {
  
  message("Ejecutando pipeline de preparación de datos...")
  
  # Pipeline de dplyr para procesar los datos
  processed_data <- raw_data %>%
    na.omit() %>%
    # Creación de una nueva característica (feature engineering)
    mutate(
      rendimiento_global = case_when(
        punt_global >= 350 ~ "Alto",
        punt_global >= 250 ~ "Medio",
        TRUE ~ "Bajo"
      )
    ) %>%
    # Seleccionamos solo las columnas que nos interesan para la app
    select(
      starts_with("punt_"),
      cole_naturaleza,
      rendimiento_global
    )
    
  message("==> Pipeline de datos ejecutado. Datos listos para la app.")
  return(processed_data)
}
