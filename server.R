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

    # Lógica para la predicción del puntaje
  puntaje_predicho <- eventReactive(input$run_prediction, {
      validate(
        ## Condicion 1: Validar que los inputs sean numéricos y estén en el rango esperado
        need(input$sim_lectura >= 0 && input$sim_lectura <= 100, "El puntaje de Lectura Crítica debe estar entre 0 y 100."),
        need(input$sim_sociales >= 0 && input$sim_sociales <= 100, "El puntaje de Sociales debe estar entre 0 y 100."),
        need(input$sim_ciencias >= 0 && input$sim_ciencias <= 100, "El puntaje de Ciencias Naturales debe estar entre 0 y 100."),

        ## Condicion 2: Validar que los inputs no sean NA
        need(!is.na(input$sim_lectura), "El puntaje de Lectura Crítica es obligatorio."),
        need(!is.na(input$sim_sociales), "El puntaje de Sociales es obligatorio."),
        need(!is.na(input$sim_ciencias), "El puntaje de Ciencias Naturales es obligatorio.")
      )

      message("-> Evento 'run_prediction' activado.")

      # 1. Crear un dataframe con los datos de entrada del usuario
      #    Los nombres de las columnas DEBEN COINCIDIR EXACTAMENTE con los del modelo.
      nuevos_datos <- data.frame(
          punt_lectura_critica = input$sim_lectura,
          punt_sociales_ciudadanas = input$sim_sociales,
          punt_c_naturales = input$sim_ciencias
      )

      
      # 2. Usar la función predict() con el modelo cargado
      prediccion <- predict_math_score(nuevos_datos)
      
      # 3. Devolver el resultado redondeado
      return(round(prediccion))
  })

  # 4. Renderizar el resultado en la UI
  output$prediction_result <- renderText({
      puntaje_predicho()
  })
  
  message("==> Lógica del servidor definida.")
}

