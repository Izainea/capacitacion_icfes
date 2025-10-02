# ==============================================================================
# APLICACIÓN SHINY: CONEXIÓN A DB Y MONITOREO DE RENDIMIENTO
# ==============================================================================
# Este script crea un dashboard que:
# 1. Se conecta a la base de datos SQLite 'icfes_data.db'.
# 2. Proporciona una herramienta para comparar el rendimiento de consultas
#    contra el archivo plano vs. la base de datos.
# 3. Muestra un análisis exploratorio consumiendo datos desde la DB.
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

# --- Definición de Rutas ---
db_path <- "database/icfes_data.db"
flat_file_path <- "data/raw/Examen_Saber_11_20242.txt"

# --- Verificación de Archivos ---
if (!file.exists(db_path)) {
  stop("ERROR: No se encuentra la base de datos 'icfes_data.db'. Por favor, ejecute el 'etl_pipeline.R' primero.")
}
if (!file.exists(flat_file_path)) {
  stop("ERROR: No se encuentra el archivo de datos crudos 'Examen_Saber_11_20242.txt'.")
}


# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "ICFES: Conectividad DB"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Monitoreo de Rendimiento", tabName = "monitoring", icon = icon("tachometer-alt")),
            menuItem("Análisis desde DB", tabName = "analysis", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            # --- Pestaña 1: Monitoreo de Rendimiento ---
            tabItem(tabName = "monitoring",
                h2("Comparativa de Rendimiento: Archivo Plano vs. Base de Datos"),
                p("Esta herramienta ejecuta la misma consulta en ambas fuentes de datos y mide el tiempo de ejecución. La consulta calcula el puntaje global promedio para los colegios OFICIALES de BOGOTA D.C."),
                fluidRow(
                    box(
                        title = "Control", status = "primary", solidHeader = TRUE, width = 12,
                        actionButton("run_benchmark", "Ejecutar Comparativa", icon = icon("play-circle"), width = "100%", class = "btn-warning")
                    )
                ),
                fluidRow(
                    valueBoxOutput("time_flat_file", width = 6),
                    valueBoxOutput("time_db", width = 6)
                ),
                fluidRow(
                    box(
                        title = "Visualización de Tiempos", status = "primary", solidHeader = TRUE, width = 12,
                        plotlyOutput("plot_benchmark")
                    )
                )
            ),
            
            # --- Pestaña 2: Análisis Exploratorio desde DB ---
            tabItem(tabName = "analysis",
                h2("Análisis Exploratorio de Datos Leídos desde la Base de Datos"),
                fluidRow(
                    box(
                        title = "Filtros", status = "primary", solidHeader = TRUE, width = 4,
                        selectInput("depto_select", "Seleccione Departamento:", choices = NULL),
                        selectInput("naturaleza_select", "Seleccione Naturaleza:", choices = c("TODAS", "OFICIAL", "NO OFICIAL"))
                    ),
                    box(
                        title = "Puntaje Global Promedio", status = "primary", solidHeader = TRUE, width = 8,
                        h1(textOutput("avg_score_db"))
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
    benchmark_results <- reactiveValues(time_flat = NULL, time_db = NULL)

    observeEvent(input$run_benchmark, {
        # --- Método 1: Archivo Plano ---
        start_time_flat <- Sys.time()
        
        datos_crudos <- read_delim(flat_file_path, delim = ";", show_col_types = FALSE)
        
        # CORRECCIÓN: Estandarizar los nombres de columnas a minúsculas
        names(datos_crudos) <- tolower(names(datos_crudos))
        
        resultado_flat <- datos_crudos %>%
            # CORRECCIÓN: Usar nombres de columna en minúsculas
            filter(cole_naturaleza == 'OFICIAL' & cole_mcpio_ubicacion == 'BOGOTA D.C.') %>%
            summarise(avg_score = mean(punt_global, na.rm = TRUE))
        
        end_time_flat <- Sys.time()
        benchmark_results$time_flat <- as.numeric(difftime(end_time_flat, start_time_flat, units = "secs"))

        # --- Método 2: Base de Datos ---
        start_time_db <- Sys.time()
        
        con <- dbConnect(RSQLite::SQLite(), db_path)
        query <- "
            SELECT AVG(r.punt_global) as avg_score
            FROM Resultados r
            JOIN Colegios c ON r.cole_dane_sede = c.cole_dane_sede
            WHERE c.cole_naturaleza = 'OFICIAL' AND c.cole_municipio = 'BOGOTA D.C.'
        "
        resultado_db <- dbGetQuery(con, query)
        dbDisconnect(con)
        
        end_time_db <- Sys.time()
        benchmark_results$time_db <- as.numeric(difftime(end_time_db, start_time_db, units = "secs"))
    })

    output$time_flat_file <- renderValueBox({
        valueBox(
            value = if (is.null(benchmark_results$time_flat)) "N/A" else paste(round(benchmark_results$time_flat, 4), "s"),
            subtitle = "Tiempo de Consulta (Archivo Plano)",
            icon = icon("file-alt"),
            color = "red"
        )
    })

    output$time_db <- renderValueBox({
        valueBox(
            value = if (is.null(benchmark_results$time_db)) "N/A" else paste(round(benchmark_results$time_db, 4), "s"),
            subtitle = "Tiempo de Consulta (Base de Datos)",
            icon = icon("database"),
            color = "green"
        )
    })

    output$plot_benchmark <- renderPlotly({
        req(benchmark_results$time_flat, benchmark_results$time_db)
        
        data_plot <- data.frame(
            Metodo = c("Archivo Plano", "Base de Datos"),
            Tiempo = c(benchmark_results$time_flat, benchmark_results$time_db)
        )
        
        plot_ly(data_plot, x = ~Metodo, y = ~Tiempo, type = 'bar',
                marker = list(color = c('rgba(220, 53, 69, 0.7)', 'rgba(40, 167, 69, 0.7)'))) %>%
        layout(
            title = "Comparación de Tiempos de Ejecución",
            yaxis = list(title = "Tiempo (segundos)"),
            xaxis = list(title = "Fuente de Datos"),
            font = list(color = '#e0e0e0'),
            plot_bgcolor = 'rgba(0,0,0,0)', 
            paper_bgcolor = 'rgba(0,0,0,0)'
        )
    })

    # --- Lógica de Análisis desde DB ---
    
    # Poblar el selector de departamentos desde la DB
    observe({
        con <- dbConnect(RSQLite::SQLite(), db_path)
        deptos <- dbGetQuery(con, "SELECT DISTINCT cole_departamento FROM Colegios ORDER BY cole_departamento")
        dbDisconnect(con)
        updateSelectInput(session, "depto_select", choices = deptos$cole_departamento)
    })

    # Calcular el puntaje promedio reactivamente
    output$avg_score_db <- renderText({
        
        # Construcción de la consulta base
        base_query <- "
            SELECT AVG(r.punt_global)
            FROM Resultados r
            JOIN Colegios c ON r.cole_dane_sede = c.cole_dane_sede
        "
        
        # Añadir filtros si es necesario
        conditions <- list()
        if (!is.null(input$depto_select) && input$depto_select != "") {
            conditions <- c(conditions, paste0("c.cole_departamento = '", input$depto_select, "'"))
        }
        if (input$naturaleza_select != "TODAS") {
            conditions <- c(conditions, paste0("c.cole_naturaleza = '", input$naturaleza_select, "'"))
        }
        
        if (length(conditions) > 0) {
            where_clause <- paste(conditions, collapse = " AND ")
            final_query <- paste(base_query, "WHERE", where_clause)
        } else {
            final_query <- base_query
        }
        
        # Ejecutar la consulta
        con <- dbConnect(RSQLite::SQLite(), db_path)
        result <- dbGetQuery(con, final_query)
        dbDisconnect(con)
        
        round(result[1, 1], 2)
    })
}

# 4. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
shinyApp(ui, server)

