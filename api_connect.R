# ==============================================================================
# APLICACIÓN SHINY: MONITOREO 3-VÍAS Y ANÁLISIS DESDE API (VERSIÓN CORREGIDA)
# ==============================================================================
# Este script crea un dashboard que:
# 1. Compara el rendimiento de consultas contra archivo plano, DB local y API.
# 2. Ofrece un explorador de datos que consume en vivo desde la API de
#    Datos Abiertos de Colombia (Socrata).
# ------------------------------------------------------------------------------

# 1. ENTORNO GLOBAL
# ==============================================================================
library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(dplyr)
library(readr)
library(plotly)
library(httr2)
library(jsonlite)
library(DT)

# --- Definición de Rutas y Endpoints ---
db_path <- "database/icfes_data.db"
flat_file_path <- "data/raw/Examen_Saber_11_20242.txt"
api_base_url <- "https://www.datos.gov.co/resource/kgxf-xxbe.json"

# --- Verificación de Archivos Locales ---
if (!file.exists(db_path)) {
  stop("ERROR: No se encuentra la base de datos 'icfes_data.db'. Por favor, ejecute el 'etl_pipeline.R' primero.")
}
if (!file.exists(flat_file_path)) {
  stop("ERROR: No se encuentra el archivo de datos crudos 'Examen_Saber_11_20242.txt'.")
}


# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "ICFES: Datos Vivos"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Monitoreo de Rendimiento", tabName = "monitoring", icon = icon("tachometer-alt")),
            menuItem("Análisis desde API", tabName = "analysis_api", icon = icon("cloud-download-alt"))
        )
    ),
    dashboardBody(
        tags$head(
            # Se asume que el archivo www/custom.css existe para el branding
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # --- Pestaña 1: Monitoreo de Rendimiento ---
            tabItem(tabName = "monitoring",
                h2("Comparativa de Rendimiento: Archivo Plano vs. Base de Datos vs. API"),
                p("Esta herramienta ejecuta una consulta para obtener los primeros 10,000 registros de BOGOTA D.C. y calcular su puntaje promedio, midiendo el tiempo de cada fuente."),
                fluidRow(
                    box(
                        title = "Control", status = "primary", solidHeader = TRUE, width = 12,
                        actionButton("run_benchmark", "Ejecutar Comparativa de Rendimiento", icon = icon("play-circle"), width = "100%", class = "btn-warning")
                    )
                ),
                fluidRow(
                    valueBoxOutput("time_flat_file", width = 4),
                    valueBoxOutput("time_db", width = 4),
                    valueBoxOutput("time_api", width = 4)
                ),
                fluidRow(
                    box(
                        title = "Visualización de Tiempos", status = "primary", solidHeader = TRUE, width = 12,
                        plotlyOutput("plot_benchmark")
                    )
                )
            ),
            
            # --- Pestaña 2: Análisis desde API ---
            tabItem(tabName = "analysis_api",
                h2("Análisis Exploratorio con Datos en Vivo desde la API de Socrata"),
                fluidRow(
                    box(
                        title = "Filtros Dinámicos", status = "primary", solidHeader = TRUE, width = 4,
                        p("Estos filtros se llenan con datos de la API."),
                        selectInput("periodo_select", "Seleccione Periodo:", choices = NULL),
                        selectInput("depto_select", "Seleccione Departamento:", choices = NULL),
                        actionButton("run_api_query", "Consultar API", icon = icon("search"), width = "100%")
                    ),
                    box(
                        title = "Top 100 Puntajes Globales", status = "primary", solidHeader = TRUE, width = 8,
                        p("Resultados obtenidos de la API según los filtros seleccionados."),
                        DTOutput("table_api_results")
                    )
                )
            )
        )
    )
)

# 3. LÓGICA DEL SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
    
    # --- Lógica de Monitoreo ---
    benchmark_results <- reactiveValues(time_flat = NULL, time_db = NULL, time_api = NULL)

    observeEvent(input$run_benchmark, {
        
        # --- Método 1: Archivo Plano ---
        withProgress(message = 'Consultando Archivo Plano...', value = 0.1, {
            start_time_flat <- Sys.time()
            datos_crudos <- read_delim(flat_file_path, delim = ";", show_col_types = FALSE, progress = FALSE)
            names(datos_crudos) <- tolower(names(datos_crudos))
            resultado_flat <- datos_crudos %>%
                filter(cole_mcpio_ubicacion == 'BOGOTA D.C.') %>%
                slice_head(n = 10000) %>%
                summarise(avg_score = mean(punt_global, na.rm = TRUE))
            end_time_flat <- Sys.time()
            benchmark_results$time_flat <- as.numeric(difftime(end_time_flat, start_time_flat, units = "secs"))
            incProgress(0.3)
        })
        
        # --- Método 2: Base de Datos SQLite ---
        withProgress(message = 'Consultando Base de Datos...', value = 0.4, {
            start_time_db <- Sys.time()
            con <- dbConnect(RSQLite::SQLite(), db_path)
            query_db <- "
                SELECT r.punt_global
                FROM Resultados r
                JOIN Colegios c ON r.cole_dane_sede = c.cole_dane_sede
                WHERE c.cole_municipio = 'BOGOTA D.C.'
                LIMIT 10000
            "
            datos_db <- dbGetQuery(con, query_db)
            resultado_db <- mean(datos_db$punt_global, na.rm = TRUE)
            dbDisconnect(con)
            end_time_db <- Sys.time()
            benchmark_results$time_db <- as.numeric(difftime(end_time_db, start_time_db, units = "secs"))
            incProgress(0.3)
        })

        # --- Método 3: API de Datos Abiertos (SoQL) (CORREGIDO Y ROBUSTO) ---
        withProgress(message = 'Consultando API Socrata...', value = 0.7, {
            start_time_api <- Sys.time()
            tryCatch({
                req_api <- request(api_base_url) %>%
                    req_url_query(
                        `$limit` = 10000,
                        `cole_mcpio_ubicacion` = "BOGOTA D.C.",
                        `$select` = "punt_global"
                    )
                resp_api <- req_perform(req_api)
                datos_api <- resp_body_json(resp_api, simplifyVector = TRUE)
                
                # CHEQUEO DEFENSIVO: Asegurarse que la respuesta es un dataframe con la columna esperada
                if (is.data.frame(datos_api) && nrow(datos_api) > 0 && "punt_global" %in% names(datos_api)) {
                    datos_api <- datos_api %>%
                        mutate(punt_global = as.numeric(punt_global))
                    resultado_api <- mean(datos_api$punt_global, na.rm = TRUE)
                } else {
                    showNotification("Benchmark API: La consulta no devolvió los datos esperados.", type = "warning")
                    resultado_api <- NA
                }
            }, error = function(e){
                showNotification(paste("Error en Benchmark API:", e$message), type = "error")
                resultado_api <- NA
            })
            
            end_time_api <- Sys.time()
            benchmark_results$time_api <- as.numeric(difftime(end_time_api, start_time_api, units = "secs"))
            incProgress(0.3)
        })
    })

    output$time_flat_file <- renderValueBox({ valueBox( if (is.null(benchmark_results$time_flat)) "N/A" else paste(round(benchmark_results$time_flat, 4), "s"), "Tiempo (Archivo Plano)", icon = icon("file-alt"), color = "red") })
    output$time_db <- renderValueBox({ valueBox( if (is.null(benchmark_results$time_db)) "N/A" else paste(round(benchmark_results$time_db, 4), "s"), "Tiempo (Base de Datos)", icon = icon("database"), color = "yellow") })
    output$time_api <- renderValueBox({ valueBox( if (is.null(benchmark_results$time_api)) "N/A" else paste(round(benchmark_results$time_api, 4), "s"), "Tiempo (API)", icon = icon("cloud"), color = "green") })

    output$plot_benchmark <- renderPlotly({
        req(benchmark_results$time_flat, benchmark_results$time_db, benchmark_results$time_api)
        data_plot <- data.frame( Metodo = c("Archivo Plano", "Base de Datos", "API"), Tiempo = c(benchmark_results$time_flat, benchmark_results$time_db, benchmark_results$time_api) )
        plot_ly(data_plot, x = ~Metodo, y = ~Tiempo, type = 'bar', marker = list(color = c('rgba(220, 53, 69, 0.7)', 'rgba(249, 194, 46, 0.7)', 'rgba(40, 167, 69, 0.7)'))) %>%
        layout(title = "Comparación de Tiempos de Ejecución", yaxis = list(title = "Tiempo (segundos)"), xaxis = list(title = "Fuente de Datos"), font = list(color = '#e0e0e0'), plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)')
    })

    # --- Lógica de Análisis desde API ---
    
    # Poblar filtros al iniciar la app
    observe({
        showNotification("Cargando filtros desde la API...", type = "message", duration = 3)
        tryCatch({
            # --- Obtener periodos distintos (Usando nombres correctos) ---
            query_periodos <- "SELECT DISTINCT periodo ORDER BY periodo DESC"
            req_p <- request(api_base_url) %>% req_url_query(`$query` = query_periodos)
            resp_p <- req_perform(req_p)
            periodos <- resp_body_json(resp_p, simplifyVector = TRUE)$periodo
            updateSelectInput(session, "periodo_select", choices = periodos)
            
            # --- Obtener departamentos distintos (Usando nombres correctos) ---
            query_deptos <- "SELECT DISTINCT cole_depto_ubicacion ORDER BY cole_depto_ubicacion"
            req_d <- request(api_base_url) %>% req_url_query(`$query` = query_deptos)
            resp_d <- req_perform(req_d)
            deptos <- resp_body_json(resp_d, simplifyVector = TRUE)$cole_depto_ubicacion
            deptos <- deptos[!is.na(deptos) & deptos != ""] # Limpiar valores vacíos
            updateSelectInput(session, "depto_select", choices = deptos)
            
        }, error = function(e) {
            showNotification(paste("Error al cargar filtros:", e$message), type = "error", duration = 10)
        })
    })

    # Conductor reactivo que se activa al presionar el botón
    api_data <- eventReactive(input$run_api_query, {
        req(input$periodo_select, input$depto_select)
        
        withProgress(message = 'Consultando API de Datos Abiertos...', value = 0.3, {
            tryCatch({
                # --- Construcción de consulta dinámica (Usando nombres correctos) ---
                req <- request(api_base_url) %>%
                    req_url_query(
                        `$limit` = 100,
                        `cole_depto_ubicacion` = toupper(input$depto_select),
                        `periodo` = input$periodo_select,
                        `$select` = "punt_global, punt_matematicas, cole_naturaleza, cole_nombre_sede",
                        `$order` = "punt_global DESC"
                    )
                
                resp <- req_perform(req)
                
                resultados <- as_tibble(resp_body_json(resp, simplifyVector = TRUE))
                
                validate(need(nrow(resultados) > 0, "La API no devolvió resultados para esta selección. Intente con otros filtros."))
                
                # Mutate con los nombres de columna correctos (punt_ en lugar de puntaje_)
                resultados %>% mutate(across(starts_with("punt_"), as.numeric))
                
            }, error = function(e) {
                showNotification(paste("Error en la consulta a la API:", e$message), type = "error", duration = 10)
                return(NULL) # Devuelve NULL en caso de error
            })
        })
    })
    
    # Renderizar la tabla con los resultados de la API
    output$table_api_results <- renderDT({
        datatable(api_data(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE, class = 'cell-border stripe')
    })
}

# 4. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
shinyApp(ui, server)

