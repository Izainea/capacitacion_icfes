library(httr2)
library(dplyr)


api_base_url <- "https://www.datos.gov.co/resource/kgxf-xxbe.json"

req_api <- request(api_base_url) %>%
  req_url_query(
    `$limit` = 10000,
    `cole_mcpio_ubicacion` = "BOGOTÁ D.C.",
    `$select` = "punt_global"
  )

resp_api <- req_perform(req_api)
datos_api <- resp_body_json(resp_api, simplifyVector = TRUE)


tryCatch({
  req_api <- request(api_base_url) %>%
    req_url_query(
      `$limit` = 10000,
      `cole_mcpio_ubicacion` = "BOGOTÁ D.C.",
      `$select` = "punt_global",
      `$where` = "punt_global IS NOT NULL"
      
    )
  resp_api <- req_perform(req_api)
  datos_api <- resp_body_json(resp_api, simplifyVector = TRUE)
  
  if (is.data.frame(datos_api) && nrow(datos_api) > 0 && "punt_global" %in% names(datos_api)) {
    datos_api <- datos_api %>%
      mutate(punt_global = as.numeric(punt_global))
    resultado_api <- mean(datos_api$punt_global, na.rm = TRUE)
  } else {
    cat("Benchmark API: La consulta no devolvió los datos esperados.", type = "warning")
    resultado_api <- NA
  }
}, error = function(e){
  cat(paste("Error en Benchmark API:", e$message), type = "error")
  resultado_api <- NA
})



tryCatch({
  # --- Construcción de consulta dinámica (Usando nombres correctos) ---
  req <- request(api_base_url) %>%
    req_url_query(
      `$limit` = 100,
      `cole_depto_ubicacion` = toupper("BOGOTÁ D.C."),
      `periodo` = "20224",
      `$select` = "punt_global, punt_matematicas, cole_naturaleza, cole_nombre_sede",
      `$order` = "punt_global DESC"
    )
  
  resp <- req_perform(req)
  
  resultados <- as_tibble(resp_body_json(resp, simplifyVector = TRUE))
  
}, error = function(e) {
  showNotification(paste("Error en la consulta a la API:", e$message), type = "error", duration = 10)
  return(NULL) # Devuelve NULL en caso de error
})


req <- request(api_base_url) %>%
  req_url_query(
    `$limit` = 100,
    `cole_depto_ubicacion` = toupper("BOGOTÁ"),
    `$select` = "punt_global, punt_matematicas, cole_naturaleza, cole_nombre_sede",
    
  )

resp <- req_perform(req)

resultados<- resp_body_json(resp, simplifyVector = TRUE)
