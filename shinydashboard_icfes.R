# ==============================================================================
# APLICACIÓN SHINY DASHBOARD - PORTAL DE RESULTADOS ICFES
# ==============================================================================
# Este script único contiene la lógica global, la UI y el Server de la app,
# aplicando los principios de arquitectura profesional y branding.
# ------------------------------------------------------------------------------

# 1. ENTORNO GLOBAL (Anteriormente global.R)
# ==============================================================================
library(shiny)
library(shinydashboard) # Librería clave para la nueva estructura
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Cargar artefactos de procesamiento
source("R/data_processing.R")
source("R/inference.R")

# Cargar y procesar datos
datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)
datos_saber <- prepare_saber_data(datos_crudos)

# Crear objetos globales para la UI
puntajes_choices <- datos_saber %>%
  select(starts_with("punt_")) %>%
  names()

# Calcular KPIs estáticos
total_estudiantes <- format(nrow(datos_saber), big.mark = ",")
puntaje_global_promedio <- round(mean(datos_saber$punt_global, na.rm = TRUE), 1)
colegios_oficiales <- format(sum(datos_saber$cole_naturaleza == "OFICIAL"), big.mark = ",")
colegios_no_oficiales <- format(sum(datos_saber$cole_naturaleza == "NO OFICIAL"), big.mark = ",")


# 2. INTERFAZ DE USUARIO (Anteriormente ui.R)
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
            # Importar la fuente 'Nunito Sans' y 'Teko'
            tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@400;700&family=Teko:wght@400;600&display=swap"),
            # Vincular nuestro archivo CSS personalizado
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        tabItems(
            # Pestaña 1: Análisis Exploratorio
            tabItem(tabName = "analisis",
                fluidRow(
                    # KPIs en ValueBoxes
                    valueBox(total_estudiantes, "Total Estudiantes", icon = icon("users"), width = 3),
                    valueBox(puntaje_global_promedio, "Puntaje Global Promedio", icon = icon("star-half-alt"), width = 3),
                    valueBox(colegios_oficiales, "Colegios Oficiales", icon = icon("school"), color = "yellow", width = 3),
                    valueBox(colegios_no_oficiales, "Colegios No Oficiales", icon = icon("building"), color = "yellow", width = 3)
                ),
                fluidRow(
                    box(
                        title = "Filtros de Análisis", status = "primary", solidHeader = TRUE, width = 4,
                        selectInput("var_x", "Seleccione variable del eje X:", choices = puntajes_choices, selected = "punt_matematicas"),
                        selectInput("var_y", "Seleccione variable del eje Y:", choices = puntajes_choices, selected = "punt_lectura_critica"),
                        radioButtons("rendimiento_select", "Filtrar por rendimiento global:",
                                     choices = c("Todos", "Alto", "Medio", "Bajo"), selected = "Todos")
                    ),
                    box(
                        title = textOutput("titulo_grafico"), status = "primary", solidHeader = TRUE, width = 8,
                        plotlyOutput("plotly_output", height = "500px")
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

# 3. LÓGICA DEL SERVIDOR (Anteriormente server.R)
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

  output$titulo_grafico <- renderText({
    paste("Relación entre", toupper(gsub("_", " ", input$var_x)), "y", toupper(gsub("_", " ", input$var_y)))
  })
  
  output$plotly_output <- renderPlotly({
    plot_ly(data = datos_filtrados(),
            x = ~get(input$var_x),
            y = ~get(input$var_y),
            type = 'scatter',
            mode = 'markers',
            marker = list(opacity = 0.5, size = 10, color = "#F9C22E"),
            hoverinfo = 'text',
            text = ~paste('Global:', punt_global, '<br>Mat:', punt_matematicas, '<br>Lectura:', punt_lectura_critica)) %>%
      layout(
        xaxis = list(title = toupper(gsub("_", " ", input$var_x))),
        yaxis = list(title = toupper(gsub("_", " ", input$var_y))),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        font = list(color = '#e0e0e0')
      )
  })

  # --- Lógica del Simulador ---
  prediction_logic <- eventReactive(input$run_prediction, {
      validate(
        need(input$sim_lectura >= 0 && input$sim_lectura <= 100, "Puntaje de Lectura debe estar entre 0 y 100."),
        need(input$sim_sociales >= 0 && input$sim_sociales <= 100, "Puntaje de Sociales debe estar entre 0 y 100."),
        need(input$sim_ciencias >= 0 && input$sim_ciencias <= 100, "Puntaje de Ciencias debe estar entre 0 y 100.")
      )
      
      nuevos_datos <- data.frame(
          punt_lectura_critica = input$sim_lectura,
          punt_sociales_ciudadanas = input$sim_sociales,
          punt_c_naturales = input$sim_ciencias
      )
      
      prediccion <- predict_math_score(nuevos_datos)
      return(round(prediccion, 1))
  })
  
  output$prediction_result <- renderText({
    prediction_logic()
  })
}

# 4. EJECUCIÓN DE LA APLICACIÓN
# ==============================================================================
shinyApp(ui, server)

