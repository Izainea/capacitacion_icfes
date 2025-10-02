# ==============================================================================
# APLICACIÓN SHINY DASHBOARD - VERSIÓN CON LAYOUTS AVANZADOS
# ==============================================================================
# Este script implementa los conceptos de la Lección 3, utilizando
# valueBoxOutput para KPIs dinámicos y tabBox para organizar visualizaciones.
# ------------------------------------------------------------------------------

# 1. ENTORNO GLOBAL
# ==============================================================================
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Cargar artefactos de procesamiento
# source("R/data_processing.R") # Asumimos que estos scripts existen en la carpeta R
# source("R/inference.R")

# Cargar y procesar datos (usaremos datos simulados si los archivos no existen)
# --- INICIO: Bloque de datos de ejemplo (reemplazar con tus archivos) ---
if (file.exists("data/raw/Examen_Saber_11_20242.txt")) {
    datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)
    datos_saber <- prepare_saber_data(datos_crudos)
} else {
    # Si los datos no existen, creamos un set de datos de ejemplo
    set.seed(123)
    n_estudiantes <- 1000
    datos_saber <- tibble(
        punt_global = round(rnorm(n_estudiantes, 250, 50)),
        punt_matematicas = round(rnorm(n_estudiantes, 50, 10)),
        punt_lectura_critica = round(rnorm(n_estudiantes, 50, 10)),
        punt_sociales_ciudadanas = round(rnorm(n_estudiantes, 50, 10)),
        punt_c_naturales = round(rnorm(n_estudiantes, 50, 10)),
        cole_naturaleza = sample(c("OFICIAL", "NO OFICIAL"), n_estudiantes, replace = TRUE, prob = c(0.7, 0.3)),
        rendimiento_global = case_when(
            punt_global > 300 ~ "Alto",
            punt_global > 200 ~ "Medio",
            TRUE ~ "Bajo"
        )
    )
}
# --- FIN: Bloque de datos de ejemplo ---


# Crear objetos globales para la UI
puntajes_choices <- datos_saber %>%
  select(starts_with("punt_")) %>%
  names()


# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
        title = tags$a(href = 'https://www.icfes.gov.co', target = '_blank',
           tags$img(src = 'icfes_logo.svg', class = 'logo-image'),
           'Portal de Resultados'
        )
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Análisis Exploratorio", tabName = "analisis", icon = icon("chart-bar")),
            menuItem("Simulador de Puntaje", tabName = "simulador", icon = icon("calculator"))
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@400;700&family=Teko:wght@400;600&display=swap"),
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        tabItems(
            # Pestaña 1: Análisis Exploratorio
            tabItem(tabName = "analisis",
                fluidRow(
                    # KPIs renderizados desde el server con valueBoxOutput
                    valueBoxOutput("kpi_total_estudiantes", width = 3),
                    valueBoxOutput("kpi_puntaje_promedio", width = 3),
                    valueBoxOutput("kpi_colegios_oficiales", width = 3),
                    valueBoxOutput("kpi_colegios_no_oficiales", width = 3)
                ),
                fluidRow(
                    box(
                        title = "Filtros de Análisis", status = "primary", solidHeader = TRUE, width = 4,
                        selectInput("var_x", "Seleccione variable del eje X:", choices = puntajes_choices, selected = "punt_matematicas"),
                        selectInput("var_y", "Seleccione variable del eje Y:", choices = puntajes_choices, selected = "punt_lectura_critica"),
                        radioButtons("rendimiento_select", "Filtrar por rendimiento global:",
                                     choices = c("Todos", "Alto", "Medio", "Bajo"), selected = "Todos")
                    ),
                    # Usamos un tabBox para organizar múltiples visualizaciones
                    tabBox(
                        title = "Visualización Detallada", id = "tabset1", width = 8,
                        tabPanel("Dispersión de Puntajes", icon = icon("chart-line"),
                            plotlyOutput("plotly_dispersion", height = "500px")
                        ),
                        tabPanel("Distribución de Puntaje X", icon = icon("chart-histogram"),
                            plotlyOutput("plotly_distribucion", height = "500px")
                        )
                    )
                )
            ),
            
            # Pestaña 2: Simulador de Puntaje
            tabItem(tabName = "simulador",
                fluidRow(
                    box(
                        title = "Ingrese los Puntajes", status = "primary", solidHeader = TRUE, width = 4,
                        numericInput("sim_lectura", "Puntaje Lectura Crítica:", 70, min = 0, max = 100),
                        numericInput("sim_sociales", "Puntaje Sociales:", 70, min = 0, max = 100),
                        numericInput("sim_ciencias", "Puntaje Ciencias Naturales:", 70, min = 0, max = 100),
                        actionButton("run_prediction", "Predecir Puntaje de Matemáticas", icon = icon("cogs"), width = "100%")
                    ),
                    box(
                        title = "Resultado de la Predicción", status = "primary", solidHeader = TRUE, width = 8,
                        div(class = "prediction-output",
                            h3("El puntaje predicho en matemáticas es:"),
                            h1(textOutput("prediction_result"))
                        )
                    )
                )
            )
        )
    )
)

# 3. LÓGICA DEL SERVIDOR
# ==============================================================================
server <- function(input, output, session) {

  # --- Lógica del Análisis Exploratorio ---
  datos_filtrados <- reactive({
    if (input$rendimiento_select == "Todos") {
      return(datos_saber)
    } else {
      return(datos_saber %>% filter(rendimiento_global == input$rendimiento_select))
    }
  })

  # --- Renderizado de KPIs (ValueBoxes) ---
  output$kpi_total_estudiantes <- renderValueBox({
      valueBox(
          value = format(nrow(datos_filtrados()), big.mark = ","),
          subtitle = "Total Estudiantes",
          icon = icon("users"),
          color = "yellow"
      )
  })

  output$kpi_puntaje_promedio <- renderValueBox({
      promedio <- round(mean(datos_filtrados()$punt_global, na.rm = TRUE), 1)
      valueBox(
          value = promedio,
          subtitle = "Puntaje Global Promedio",
          icon = icon("star-half-alt"),
          color = "yellow"
      )
  })

  output$kpi_colegios_oficiales <- renderValueBox({
      oficiales <- sum(datos_filtrados()$cole_naturaleza == "OFICIAL")
      valueBox(
          value = format(oficiales, big.mark = ","),
          subtitle = "Colegios Oficiales",
          icon = icon("school"),
          color = "blue"
      )
  })
  
  output$kpi_colegios_no_oficiales <- renderValueBox({
      no_oficiales <- sum(datos_filtrados()$cole_naturaleza == "NO OFICIAL")
      valueBox(
          value = format(no_oficiales, big.mark = ","),
          subtitle = "Colegios No Oficiales",
          icon = icon("building"),
          color = "blue"
      )
  })
  

  # --- Renderizado de Gráficos en el tabBox ---
  output$plotly_dispersion <- renderPlotly({
    p <- plot_ly(data = datos_filtrados(),
            x = ~get(input$var_x),
            y = ~get(input$var_y),
            type = 'scatter', mode = 'markers',
            marker = list(opacity = 0.6, size = 10, color = "#F9C22E"),
            hoverinfo = 'text',
            text = ~paste('Global:', punt_global)) %>%
      layout(
        title = paste("Relación entre", input$var_x, "y", input$var_y),
        xaxis = list(title = toupper(gsub("_", " ", input$var_x))),
        yaxis = list(title = toupper(gsub("_", " ", input$var_y))),
        plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#e0e0e0')
      )
    p
  })
  
  output$plotly_distribucion <- renderPlotly({
      p <- plot_ly(data = datos_filtrados(), x = ~get(input$var_x), type = 'histogram',
                   marker = list(color = "#004884", line = list(color = "#F9C22E", width = 1))) %>%
        layout(
            title = paste("Distribución de", input$var_x),
            xaxis = list(title = toupper(gsub("_", " ", input$var_x))),
            yaxis = list(title = "Frecuencia"),
            plot_bgcolor = 'rgba(0,0,0,0)', paper_bgcolor = 'rgba(0,0,0,0)',
            font = list(color = '#e0e0e0')
        )
      p
  })


  # --- Lógica del Simulador ---
  prediction_logic <- eventReactive(input$run_prediction, {
      # Simulación de la predicción si el modelo no está cargado
      prediccion <- 50 + (input$sim_lectura - 50) * 0.4 + (input$sim_sociales - 50) * 0.3 + (input$sim_ciencias - 50) * 0.3 + rnorm(1, 0, 5)
      prediccion <- max(0, min(100, prediccion)) # Asegurar que esté entre 0 y 100
      return(round(prediccion, 1))
  })
  
  output$prediction_result <- renderText({
    prediction_logic()
  })
}

# 4. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
shinyApp(ui, server)

