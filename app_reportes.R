# ==============================================================================
# APP_REPORTES.R - APLICACIÓN DEDICADA A LA GENERACIÓN DE REPORTES
# Versión Corregida:
# 1. Pasa el dataframe filtrado como parámetro al Rmd para evitar errores de ruta.
# 2. Usa .data[[...]] en ggplot() para evitar la advertencia de 'aes_string()'.
# 3. Mejora el manejo de notificaciones y errores.
# ==============================================================================

# --- Carga de Librerías ---
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(rmarkdown)
library(ggplot2)

# --- Carga y Procesamiento de Datos ---
# Los datos se cargan UNA SOLA VEZ al iniciar la app.
datos_saber <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE) %>%
  na.omit()

# --- Interfaz de Usuario (UI) ---
# (La UI no necesita cambios, se mantiene idéntica)
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("Generador de Reportes ICFES"),
  sidebarLayout(
    sidebarPanel(
      h3("Parámetros del Reporte"),
      p("Selecciona los filtros para generar tu reporte personalizado."),
      selectInput("naturaleza_select", "Naturaleza del Colegio:",
                  choices = c("OFICIAL", "NO OFICIAL"), selected = "OFICIAL"),
      selectInput("var_x", "Seleccione la Variable del Eje X:",
                  choices = names(datos_saber |> select(starts_with("punt_"))), selected = "punt_matematicas"),
      selectInput("var_y", "Seleccione la Variable del Eje Y:",
                  choices = names(datos_saber |> select(starts_with("punt_"))), selected = "punt_lectura_critica"),
      hr(),
      downloadButton("descargar_reporte", "Generar y Descargar Reporte (PDF)", icon = icon("file-pdf"), class = "btn-primary btn-lg")
    ),
    mainPanel(
      h3("Bienvenido al Generador de Reportes"),
      p("Usa los controles del panel izquierdo para configurar el contenido de tu reporte. Una vez que hayas hecho tus selecciones, haz clic en el botón 'Generar y Descargar'."),
      p("El sistema tomará tus selecciones, las pasará a una plantilla de R Markdown y compilará un documento PDF listo para ser compartido."),
      h4("Vista Previa del Gráfico del Reporte"),
      plotOutput("preview_plot")
    )
  )
)

# --- Lógica del Servidor (SERVER) ---
server <- function(input, output, session) {

  datos_filtrados_preview <- reactive({
    req(input$naturaleza_select)
    filter(datos_saber, cole_naturaleza == input$naturaleza_select)
  })

  output$preview_plot <- renderPlot({
    # Advertencia solucionada: cambiamos aes_string() por aes() con .data
    ggplot(datos_filtrados_preview(), aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
      geom_point(aes(color = cole_naturaleza), alpha = 0.6) +
      labs(title = paste("Vista Previa: Relación entre", input$var_x, "y", input$var_y),
           x = input$var_x, y = input$var_y, color = "Naturaleza") +
      theme_minimal()
  }, res = 96)

  output$descargar_reporte <- downloadHandler(
    filename = function() {
      paste0("reporte-icfes-", Sys.Date(), "-", input$naturaleza_select, ".pdf")
    },
    content = function(file) {
      # Usamos tryCatch para manejar errores de forma elegante
      tryCatch({
        # Mejora: La notificación persiste hasta que termina el proceso
        id <- showNotification("Generando reporte en PDF...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)

        # 1. Filtramos los datos DENTRO de la app.
        datos_para_el_reporte <- datos_saber %>%
          filter(cole_naturaleza == input$naturaleza_select)

        # 2. Creamos la lista de parámetros, incluyendo el NUEVO dataframe.
        params_list <- list(
          naturaleza_filtro = input$naturaleza_select,
          variable_x = input$var_x,
          variable_y = input$var_y,
          datos_reporte = datos_para_el_reporte
        )

        # Copiamos el reporte a un directorio temporal para evitar problemas de permisos/rutas
        tempReport <- file.path(tempdir(), "reporte.Rmd")
        file.copy("reporte.Rmd", tempReport, overwrite = TRUE)

        # Renderizamos el Rmd a PDF
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params_list,
          envir = new.env(parent = globalenv())
        )
      }, error = function(e) {
        # Si ocurre un error, muéstralo en una notificación clara
        showNotification(paste("Error al generar el reporte:", e$message), duration = NULL, type = "error")
        # Devolvemos NULL para que no intente descargar un archivo fallido
        return(NULL)
      })
    }
  )
}

# --- Ejecución ---
shinyApp(ui, server)

