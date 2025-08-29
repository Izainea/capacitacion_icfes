# ==============================================================================
# Aplicación Shiny de Ejemplo
# Muestra un histograma interactivo del dataset 'faithful'.
# NOTA: Para aplicaciones más complejas, se puede dividir este archivo en
# ui.R (interfaz de usuario) y server.R (lógica del servidor).
# ==============================================================================

# Cargar la librería Shiny
library(shiny)

# --- Interfaz de Usuario (UI) ---
# Define cómo se verá la aplicación.
ui <- fluidPage(
  
  # Título de la aplicación
  titlePanel("Mi Primera Aplicación de Datos"),
  
  # Layout con una barra lateral y un panel principal
  sidebarLayout(
    
    # Panel de la barra lateral con los controles de entrada
    sidebarPanel(
      sliderInput("bins",
                  "Número de intervalos (bins):",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Panel principal donde se mostrarán las salidas (el gráfico)
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# --- Lógica del Servidor (Server) ---
# Define cómo funciona la aplicación.
server <- function(input, output) {
  
  # Crea el gráfico del histograma.
  # La expresión se re-ejecuta cada vez que el input 'bins' cambia.
  output$distPlot <- renderPlot({
    
    # Extraer los datos y definir los intervalos
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # Dibujar el histograma
    hist(x, breaks = bins, col = '#ff9900', border = 'white',
         xlab = 'Tiempo de espera entre erupciones (en mins)',
         main = 'Histograma de tiempos de espera')
  })
}

# --- Ejecución de la Aplicación ---
# Combina la UI y el Server para crear y lanzar la aplicación Shiny.
shinyApp(ui = ui, server = server)
