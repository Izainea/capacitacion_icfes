modelo_matematicas <- readRDS("models/modelo_puntaje_mat.rds")

message("==> Modelo de predicción cargado en memoria.")

predict_math_score <- function(new_data) {
    required_cols <- c("punt_lectura_critica", "punt_sociales_ciudadanas", "punt_c_naturales")
    if (!all(required_cols %in% names(new_data))) {
        stop(paste("El dataframe debe contener las siguientes columnas:", paste(required_cols, collapse = ", ")))
    }
    prediction <- predict(modelo_matematicas, newdata = new_data)
    return(round(prediction))
}
