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
