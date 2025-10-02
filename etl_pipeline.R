# ==============================================================================
# PIPELINE ETL (EXTRACCIÓN, TRANSFORMACIÓN Y CARGA)
# ==============================================================================
# Objetivo: Leer el archivo de datos crudos, transformarlo a un esquema
#           en estrella y cargarlo en una base de datos SQLite.
#
# Autor: [Tu Nombre]
# Fecha: 2024-10-01
# ------------------------------------------------------------------------------

# 1. CONFIGURACIÓN DEL ENTORNO
# ==============================================================================
# Carga de librerías necesarias para el proceso
library(dplyr)
library(readr)
library(DBI)
library(RSQLite)

cat("-> Pipeline ETL iniciado.\n")

# 2. EXTRACCIÓN (Extract)
# ==============================================================================
# Se carga el archivo de texto plano con los resultados de las pruebas.

cat("-> (E) Extrayendo datos desde el archivo .txt...\n")
ruta_archivo_crudo <- "data/raw/Examen_Saber_11_20242.txt"

if (!file.exists(ruta_archivo_crudo)) {
  stop("¡Error! El archivo de datos crudos no se encontró en: ", ruta_archivo_crudo)
}

datos_crudos <- read_delim(ruta_archivo_crudo, delim = ";", show_col_types = FALSE)
cat("-> [✓] Datos crudos cargados exitosamente.\n")

cat("-> Columnas disponibles: ", paste(names(datos_crudos), collapse = ", "), "\n")
# 3. TRANSFORMACIÓN (Transform)
# ==============================================================================
# Se procesan los datos crudos para crear las 4 tablas del esquema estrella.

cat("-> (T) Transformando datos al esquema en estrella...\n")

# --- Limpieza Inicial ---
datos_limpios <- datos_crudos %>%
  # Seleccionamos solo las columnas que necesitamos para la base de datos
  select(
    estu_consecutivo, fami_estratovivienda,
    cole_cod_dane_sede, cole_nombre_sede, cole_naturaleza,
    cole_mcpio_ubicacion, cole_depto_ubicacion,
    punt_global, punt_matematicas, punt_lectura_critica,
    punt_sociales_ciudadanas, punt_c_naturales, punt_ingles,
    periodo
  ) %>%

  # Renombramos las columnas para que sean más manejables
  rename(
    estudiante_id = estu_consecutivo,
    estrato = fami_estratovivienda,
    cole_dane_sede = cole_cod_dane_sede,
    cole_nombre_sede = cole_nombre_sede,
    cole_naturaleza = cole_naturaleza,
    cole_municipio = cole_mcpio_ubicacion,
    cole_departamento = cole_depto_ubicacion,
    periodo = periodo
  )

# --- Creación de la Dimensión: Colegios ---
dim_colegios <- datos_limpios %>%
  select(cole_dane_sede, cole_nombre_sede, cole_naturaleza, cole_municipio, cole_departamento) %>%
  distinct(cole_dane_sede, .keep_all = TRUE) # Nos aseguramos de que cada colegio sea único

cat("   - Creada tabla de dimensión: Colegios (", nrow(dim_colegios), " registros únicos)\n")

# --- Creación de la Dimensión: Estudiantes ---
# En este set de datos, no tenemos nombres, así que usaremos el ID y estrato.
dim_estudiantes <- datos_limpios %>%
  select(estudiante_id, estrato) %>%
  distinct(estudiante_id, .keep_all = TRUE)

cat("   - Creada tabla de dimensión: Estudiantes (", nrow(dim_estudiantes), " registros únicos)\n")

# --- Creación de la Dimensión: Examenes ---
# Creamos una tabla que define el periodo del examen.
dim_examenes <- datos_limpios %>%
  distinct(periodo) %>%
  rename(periodo = periodo) %>%
  mutate(examen_id = row_number()) # Creamos un ID numérico para el examen

cat("   - Creada tabla de dimensión: Examenes (", nrow(dim_examenes), " registros únicos)\n")

# --- Creación de la Tabla de Hechos: Resultados ---
# Esta tabla contiene las métricas y las claves foráneas (FK) a las dimensiones.
fact_resultados <- datos_limpios %>%
  # Unimos con dim_examenes para obtener el examen_id
  left_join(dim_examenes, by = c("periodo" = "periodo")) %>%
  select(
    estudiante_id,              # FK a dim_estudiantes
    cole_dane_sede,             # FK a dim_colegios
    examen_id,                  # FK a dim_examenes
    punt_global,
    punt_matematicas,
    punt_lectura_critica,
    punt_sociales_ciudadanas,
    punt_c_naturales,
    punt_ingles
  )

cat("   - Creada tabla de hechos: Resultados (", nrow(fact_resultados), " registros)\n")
cat("-> [✓] Transformación completada.\n")


# 4. CARGA (Load)
# ==============================================================================
# Se crea la base de datos SQLite y se cargan las tablas transformadas.

cat("-> (L) Cargando tablas en la base de datos SQLite...\n")

# --- Conexión a la Base de Datos ---
# Nos aseguramos de que el directorio exista
if (!dir.exists("database")) {
  dir.create("database")
}
ruta_db <- "database/icfes_data.db"
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_db)

# --- Carga de Tablas ---
tryCatch({
  dbWriteTable(con, "Colegios", dim_colegios, overwrite = TRUE)
  cat("   - Tabla 'Colegios' cargada.\n")
  
  dbWriteTable(con, "Estudiantes", dim_estudiantes, overwrite = TRUE)
  cat("   - Tabla 'Estudiantes' cargada.\n")
  
  dbWriteTable(con, "Examenes", dim_examenes, overwrite = TRUE)
  cat("   - Tabla 'Examenes' cargada.\n")
  
  dbWriteTable(con, "Resultados", fact_resultados, overwrite = TRUE)
  cat("   - Tabla 'Resultados' cargada.\n")
  
  cat("-> [✓] Carga a la base de datos completada.\n")
  
}, error = function(e) {
  cat("-> ¡Error durante la carga a la base de datos!: ", e$message, "\n")
}, finally = {
  dbDisconnect(con)
  cat("-> Conexión a la base de datos cerrada.\n")
})


# 5. VERIFICACIÓN
# ==============================================================================
# Nos reconectamos y ejecutamos una consulta de prueba para verificar la integridad.

cat("-> Verificando la integridad de la base de datos...\n")
con <- dbConnect(RSQLite::SQLite(), dbname = ruta_db)

tryCatch({
  cat("   - Tablas en la base de datos: ", paste(dbListTables(con), collapse = ", "), "\n")
  
  # Consulta de prueba: Puntaje global promedio para colegios oficiales
  query_resultado <- dbGetQuery(con, "
    SELECT
      c.cole_naturaleza,
      AVG(r.punt_global) AS puntaje_promedio
    FROM Resultados r
    JOIN Colegios c ON r.cole_dane_sede = c.cole_dane_sede
    WHERE c.cole_naturaleza = 'OFICIAL'
  ")
  
  cat("   - Consulta de prueba exitosa. Puntaje promedio para colegios OFICIALES: ", round(query_resultado$puntaje_promedio, 2), "\n")
  cat("-> [✓] ¡Pipeline ETL finalizado con éxito! La base de datos 'icfes_data.db' está lista.\n")
  
}, error = function(e) {
  cat("-> ¡Error durante la verificación!: ", e$message, "\n")
}, finally = {
  dbDisconnect(con)
})
