# ==============================================================================
# ARCHIVO SERVER: EL CEREBRO LÓGICO
# ==============================================================================
# Este script contiene únicamente la función del servidor, que define
# cómo reacciona la aplicación a las interacciones del usuario.
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Lógica del Análisis Exploratorio ---
  
  # Conductor Reactivo para filtrar los datos según el input del usuario
  datos_filtrados <- reactive({
    message("-> Reactivo 'datos_filtrados' ejecutado.")
    
    # Si la selección es "Ambas", no se filtra.
    if (input$rendimiento_select == "Todos") {
      return(datos_saber)
    } else {
      return(datos_saber %>% filter(rendimiento_global == input$rendimiento_select))
    }
  })

  # KPIs para las tarjetas (leen los objetos estáticos creados en global.R)
  output$total_estudiantes_kpi <- renderText({ total_estudiantes })
  output$puntaje_global_kpi <- renderText({ puntaje_global_promedio })
  output$colegios_oficiales_kpi <- renderText({ colegios_oficiales })
  output$colegios_no_oficiales_kpi <- renderText({ colegios_no_oficiales })

  # Título dinámico para el gráfico
  output$titulo_grafico <- renderText({
    paste("Relación entre", toupper(gsub("_", " ", input$var_x)), "y", toupper(gsub("_", " ", input$var_y)))
  })

  # Renderizar el gráfico de dispersión interactivo
  # Usa el conductor reactivo 'datos_filtrados()'
  output$scatter_plot <- renderPlotly({
    p <- ggplot(datos_filtrados(), aes_string(x = input$var_x, y = input$var_y)) +
      geom_point(aes(color = cole_naturaleza), alpha = 0.6) +
      labs(
        x = "",
        y = "",
        color = "Naturaleza Colegio"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom"
      )
    ggplotly(p)
  })

  # Renderizar la tabla de datos completa
  # Usa el conductor reactivo 'datos_filtrados()'
  output$tabla_completa <- renderDataTable({
    datos_filtrados()
  })
  
  message("==> Lógica del servidor definida.")
}

