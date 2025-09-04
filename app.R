# Cargar todas las librerías necesarias
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)

#  ---  Lógica de Datos (fuera del Server)  --- 
# Leemos y preparamos los datos una sola vez al iniciar la app
datos_saber <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE) %>%
  na.omit() # Asegurarnos de quitar NAs para los cálculos

puntajes_choices <- datos_saber %>%
  select(starts_with("punt_")) %>%
  names()

# Cálculos para las tarjetas de KPI
total_estudiantes <- format(nrow(datos_saber), big.mark = ",")
puntaje_global_promedio <- round(mean(datos_saber$punt_global, na.rm = TRUE), 1)
colegios_oficiales <- format(sum(datos_saber$cole_naturaleza == "OFICIAL"), big.mark = ",")
colegios_no_oficiales <- format(sum(datos_saber$cole_naturaleza == "NO OFFICIAL"), big.mark = ",")


#  ---  UI (User Interface)  --- 
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("Dashboard Exploratorio Pruebas Saber 11"),

  # Fila para las tarjetas de KPIs
  fluidRow(
    # Tarjeta 1: Total Estudiantes
    column(3,
           div(class = "kpi-card",
               h3(textOutput("total_estudiantes_kpi")),
               p("Total Estudiantes en la Muestra")
           )
    ),
    # Tarjeta 2: Puntaje Global Promedio
    column(3,
           div(class = "kpi-card",
               h3(textOutput("puntaje_global_kpi")),
               p("Puntaje Global Promedio")
           )
    ),
    # Tarjeta 3: Colegios Oficiales
    column(3,
           div(class = "kpi-card",
               h3(textOutput("colegios_oficiales_kpi")),
               p("Colegios Oficiales")
           )
    ),
    # Tarjeta 4: Colegios No Oficiales
    column(3,
           div(class = "kpi-card",
               h3(textOutput("colegios_no_oficiales_kpi")),
               p("Colegios No Oficiales")
           )
    )
  ),

  hr(), # Una línea horizontal para separar

  sidebarLayout(
    sidebarPanel(
      h3("Filtros del Gráfico"),
      selectInput(
        inputId = "var_x",
        label = "Seleccione Puntaje Eje X:",
        choices = puntajes_choices,
        selected = "punt_matematicas"
      ),
      selectInput(
        inputId = "var_y",
        label = "Seleccione Puntaje Eje Y:",
        choices = puntajes_choices,
        selected = "punt_lectura_critica"
      )
    ),
    mainPanel(
      # Usaremos pestañas para organizar mejor el contenido
      tabsetPanel(
        type = "tabs",
        tabPanel("Gráfico Interactivo",
                 h3(textOutput("titulo_grafico")),
                 plotlyOutput("scatter_plot")
        ),
        tabPanel("Tabla de Datos",
                 h3("Datos Completos"),
                 p("Explora los datos crudos en la siguiente tabla interactiva."),
                 dataTableOutput("tabla_completa")
        )
      )
    )
  ),

  # Estilos CSS para las tarjetas de KPI
  tags$style(HTML("
    .kpi-card {
      background-color: #1e1e1e;
      padding: 20px;
      border-radius: 10px;
      text-align: center;
      border-top: 4px solid #ff9900;
      margin-bottom: 20px;
    }
    .kpi-card h3 {
      font-family: 'Teko', sans-serif;
      font-size: 3rem;
      margin-top: 0;
      color: #ff9900;
    }
    .kpi-card p {
      text-align: center;
      margin-bottom: 0;
      color: #a0a0a0;
      font-size: 1rem;
    }
  "))
)

#  ---  SERVER (Lógica del Servidor)  --- 
server <- function(input, output) {

  # KPIs para las tarjetas
  output$total_estudiantes_kpi <- renderText({ total_estudiantes })
  output$puntaje_global_kpi <- renderText({ puntaje_global_promedio })
  output$colegios_oficiales_kpi <- renderText({ colegios_oficiales })
  output$colegios_no_oficiales_kpi <- renderText({ colegios_no_oficiales })

  # Título dinámico para el gráfico
  output$titulo_grafico <- renderText({
    paste("Relación entre", toupper(gsub("_", " ", input$var_x)), "y", toupper(gsub("_", " ", input$var_y)))
  })

  # Renderizar el gráfico de dispersión interactivo
  output$scatter_plot <- renderPlotly({
    p <- ggplot(datos_saber, aes_string(x = input$var_x, y = input$var_y)) +
      geom_point(aes(color = cole_naturaleza), alpha = 0.6) +
      labs(
        x = "", # Los quitamos porque el título dinámico ya es suficiente
        y = "",
        color = "Naturaleza Colegio"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom" # Mover leyenda abajo
      )
    ggplotly(p)
  })

  # Renderizar la tabla de datos completa
  output$tabla_completa <- renderDataTable({
    datos_saber
  })
}

#  ---  Ejecutar la Aplicación  --- 
shinyApp(ui = ui, server = server)