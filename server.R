# ==============================================================================
# ARCHIVO SERVER: EL CEREBRO LÓGICO
# ==============================================================================
# Este script contiene toda la lógica reactiva de la aplicación.
# Define CÓMO funciona la app en respuesta a las interacciones del usuario.
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

  # KPIs para las tarjetas (leen los objetos creados en global.R)
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
  
  message("==> Lógica del servidor definida.")
}

