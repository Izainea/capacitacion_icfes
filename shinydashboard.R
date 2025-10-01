# ==============================================================================
# APLICACIÓN SHINY DASHBOARD: ANÁLISIS PRUEBAS SABER 11
# ==============================================================================
# Este script consolida la lógica de global.R, ui.R y server.R en una
# única aplicación profesional utilizando el paquete {shinydashboard}.
# ------------------------------------------------------------------------------


# ==============================================================================
# 1. PREPARACIÓN DEL ENTORNO (Lógica de global.R)
# ==============================================================================

# Carga de Librerías
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Cargar funciones personalizadas de procesamiento e inferencia
# (Asegúrate de que estos archivos estén en una carpeta 'R')
source("R/data_processing.R")
source("R/inference.R")

# Cargar y procesar los datos
# (Asegúrate de que los datos estén en una carpeta 'data/raw')
datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)
datos_saber <- prepare_saber_data(datos_crudos)

# Crear objetos globales para la UI
puntajes_choices <- datos_saber %>%
  select(starts_with("punt_")) %>%
  names()

# Calcular KPIs estáticos para las tarjetas
total_estudiantes <- format(nrow(datos_saber), big.mark = ",")
puntaje_global_promedio <- round(mean(datos_saber$punt_global, na.rm = TRUE), 1)
colegios_oficiales <- format(sum(datos_saber$cole_naturaleza == "OFICIAL"), big.mark = ",")
colegios_no_oficiales <- format(sum(datos_saber$cole_naturaleza == "NO OFICIAL"), big.mark = ",")


# ==============================================================================
# 2. INTERFAZ DE USUARIO (UI) con shinydashboard
# ==============================================================================

ui <- dashboardPage(
  skin = "blue", # Tema de color base
  
  # --- Cabecera ---
  dashboardHeader(title = "Dashboard ICFES"),
  
  # --- Barra Lateral (Menú de Navegación) ---
  dashboardSidebar(
    sidebarMenu(
      menuItem("Análisis Exploratorio", tabName = "analisis", icon = icon("chart-bar")),
      menuItem("Simulador de Puntajes", tabName = "simulador", icon = icon("calculator"))
    )
  ),
  
  # --- Cuerpo del Dashboard (Contenido Principal) ---
  dashboardBody(
    # Inyectar CSS para Branding Institucional ICFES
    tags$head(
      tags$style(HTML("
        /* Usar la fuente Raleway definida en la lección */
        body, .main-header .logo, .main-sidebar {
            font-family: 'Raleway', sans-serif;
        }

        /* Paleta de colores ICFES */
        .skin-blue .main-header .navbar, .skin-blue .main-header .logo {
            background-color: #1c3055 !important; /* Azul ICFES */
        }
        .skin-blue .sidebar-menu > .active > a {
            background-color: #f18415 !important; /* Naranja ICFES */
        }
        .box.box-solid.box-primary > .box-header {
            background-color: #1c3055 !important; /* Azul ICFES */
        }
        .box.box-solid.box-success {
            border-left-color: #28a745 !important;
        }
        .box.box-solid.box-success > .box-header {
            background-color: #28a745 !important;
        }
        #prediction_result {
            font-size: 4rem;
            font-weight: bold;
            color: #28a745; /* Verde éxito */
        }
      "))
    ),
    
    tabItems(
      # --- Pestaña 1: Análisis Exploratorio ---
      tabItem(tabName = "analisis",
        # Fila para los ValueBoxes (KPIs)
        fluidRow(
          valueBoxOutput("total_estudiantes_kpi", width = 3),
          valueBoxOutput("puntaje_global_kpi", width = 3),
          valueBoxOutput("colegios_oficiales_kpi", width = 3),
          valueBoxOutput("colegios_no_oficiales_kpi", width = 3)
        ),
        
        # Fila para el gráfico y los controles
        fluidRow(
          box(
            title = "Controles de Visualización", status = "warning", solidHeader = TRUE, width = 4,
            selectInput("var_x", "Seleccione el Eje X:", choices = puntajes_choices, selected = "punt_matematicas"),
            selectInput("var_y", "Seleccione el Eje Y:", choices = puntajes_choices, selected = "punt_lectura_critica"),
            selectInput("rendimiento_select", "Filtrar por Rendimiento Global:", choices = c("Todos", "Bajo", "Medio", "Alto"), selected = "Todos")
          ),
          box(
            title = textOutput("titulo_grafico"), status = "primary", solidHeader = TRUE, width = 8,
            plotlyOutput("grafico_distribucion", height = "500px")
          )
        )
      ),
      
      # --- Pestaña 2: Simulador de Puntajes ---
      tabItem(tabName = "simulador",
        fluidRow(
          box(
            title = "Variables de Entrada", status = "warning", solidHeader = TRUE, width = 5,
            h4("Ingrese los puntajes para predecir el resultado en Matemáticas:"),
            numericInput("sim_lectura", "Puntaje Lectura Crítica (0-100):", 70, min = 0, max = 100),
            numericInput("sim_sociales", "Puntaje Sociales (0-100):", 70, min = 0, max = 100),
            numericInput("sim_ciencias", "Puntaje Ciencias Naturales (0-100):", 70, min = 0, max = 100),
            actionButton("run_prediction", "Predecir Puntaje", icon = icon("rocket"), class = "btn-primary btn-lg")
          ),
          box(
            title = "Resultado de la Predicción", status = "success", solidHeader = TRUE, width = 7,
            div(style = "text-align: center; padding: 20px;",
              h4("El puntaje predicho en matemáticas es:"),
              textOutput("prediction_result")
            )
          )
        )
      )
    )
  )
)


# ==============================================================================
# 3. LÓGICA DEL SERVIDOR
# ==============================================================================

server <- function(input, output, session) {

  # --- Lógica del Análisis Exploratorio ---
  
  datos_filtrados <- reactive({
    req(input$rendimiento_select) # Asegura que el input no sea nulo
    if (input$rendimiento_select == "Todos") {
      datos_saber
    } else {
      datos_saber %>% filter(rendimiento_global == input$rendimiento_select)
    }
  })
  
  # Renderizar los ValueBoxes
  output$total_estudiantes_kpi <- renderValueBox({
    valueBox(total_estudiantes, "Total Estudiantes", icon = icon("users"), color = "blue")
  })
  output$puntaje_global_kpi <- renderValueBox({
    valueBox(puntaje_global_promedio, "Puntaje Global Promedio", icon = icon("graduation-cap"), color = "orange")
  })
  output$colegios_oficiales_kpi <- renderValueBox({
    valueBox(colegios_oficiales, "Colegios Oficiales", icon = icon("building"), color = "purple")
  })
  output$colegios_no_oficiales_kpi <- renderValueBox({
    valueBox(colegios_no_oficiales, "Colegios No Oficiales", icon = icon("building-user"), color = "maroon")
  })

  output$titulo_grafico <- renderText({
    paste("Relación entre", toupper(gsub("punt_", "", input$var_x)), "y", toupper(gsub("punt_", "", input$var_y)))
  })
  
  output$grafico_distribucion <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes_string(x = input$var_x, y = input$var_y, color = "rendimiento_global")) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("Bajo" = "#f18415", "Medio" = "#1c3055", "Alto" = "#28a745")) +
      theme_minimal() +
      labs(color = "Rendimiento Global")
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })

  # --- Lógica del Simulador ---
  
  observeEvent(input$run_prediction, {
    
    # Validaciones de entrada
    validate(
      need(input$sim_lectura >= 0 && input$sim_lectura <= 100, "El puntaje de Lectura Crítica debe estar entre 0 y 100."),
      need(input$sim_sociales >= 0 && input$sim_sociales <= 100, "El puntaje de Sociales debe estar entre 0 y 100."),
      need(input$sim_ciencias >= 0 && input$sim_ciencias <= 100, "El puntaje de Ciencias Naturales debe estar entre 0 y 100.")
    )
    
    # Crear dataframe para la predicción
    nuevos_datos <- data.frame(
        punt_lectura_critica = input$sim_lectura,
        punt_sociales_ciudadanas = input$sim_sociales,
        punt_c_naturales = input$sim_ciencias
    )
    
    # Realizar la predicción
    prediccion <- predict_math_score(nuevos_datos)
    
    # Mostrar el resultado
    output$prediction_result <- renderText({
      round(prediccion, 1)
    })
  })
}


# ==============================================================================
# 4. EJECUTAR LA APLICACIÓN
# ==============================================================================

shinyApp(ui, server)
