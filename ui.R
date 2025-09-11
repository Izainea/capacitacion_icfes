# ==============================================================================
# ARCHIVO UI: EL ARQUITECTO VISUAL
# ==============================================================================
# Este script contiene únicamente la definición de la Interfaz de Usuario.
# No debe contener ninguna lógica de cálculo o procesamiento de datos.
# ------------------------------------------------------------------------------

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
      h3("Filtros del Análisis"),
      
      # FILTRO PARA LA REACTIVIDAD
      selectInput(
        inputId = "naturaleza_select",
        label = "Seleccione Naturaleza del Colegio:",
        choices = c("Ambas", "OFICIAL", "NO OFICIAL"),
        selected = "Ambas"
      ),
      
      hr(),
      
      h3("Variables del Gráfico"),
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

