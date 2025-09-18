# ==============================================================================
# APP.R - GALERÍA AVANZADA DE VISUALIZACIONES PLOTLY
# Objetivo: Demostrar las capacidades nativas de Plotly para crear
#           visualizaciones interactivas complejas y funcionalidades avanzadas.
# ==============================================================================

# ------------------------------------------------------------------------------
# SECCIÓN 0: GESTIÓN DE PAQUETES
# ------------------------------------------------------------------------------
# Este bloque de código verifica si los paquetes necesarios están instalados
# y, si no, los instala. Esto asegura que la aplicación siempre tenga lo que necesita.
required_packages <- c("shiny", "bslib", "readr", "dplyr", "plotly", "igraph", "crosstalk", "DT")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)


# ------------------------------------------------------------------------------
# SECCIÓN 1: CARGA DE LIBRERÍAS Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(plotly)
library(igraph)      # Para el layout del grafo de red
library(crosstalk)   # Para la interactividad entre widgets
library(DT)          # Para tablas interactivas

# --- Carga y Procesamiento de Datos ---
datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)

prepare_saber_data <- function(raw_data) {
  raw_data %>%
    na.omit() %>%
    mutate(
      rendimiento_global = case_when(
        punt_global >= 350 ~ "Alto",
        punt_global >= 250 ~ "Medio",
        TRUE ~ "Bajo"
      ),
      rendimiento_global = factor(rendimiento_global, levels = c("Bajo", "Medio", "Alto"))
    ) %>%
    select(
      starts_with("punt_"),
      cole_naturaleza,
      rendimiento_global
    )
}

datos_saber <- prepare_saber_data(datos_crudos)

# --- Datos Pre-calculados para Gráficos Específicos ---
datos_promedio_naturaleza <- datos_saber %>%
  group_by(cole_naturaleza) %>%
  summarise(across(starts_with("punt_"), mean, na.rm = TRUE), .groups = 'drop')

# --- Preparación de Datos para Sankey ---
sankey_data <- datos_saber %>%
  count(cole_naturaleza, rendimiento_global, name = "value") %>%
  mutate(
    source_label = cole_naturaleza,
    target_label = rendimiento_global
  )

nodes <- data.frame(name = unique(c(sankey_data$source_label, sankey_data$target_label)))
sankey_data$source <- match(sankey_data$source_label, nodes$name) - 1
sankey_data$target <- match(sankey_data$target_label, nodes$name) - 1

# --- Preparación de Datos para Grafo de Red ---
corr_matrix <- cor(datos_saber %>% select(starts_with("punt_")))
corr_matrix[lower.tri(corr_matrix, diag = TRUE)] <- NA
corr_links <- as.data.frame(as.table(corr_matrix)) %>%
  na.omit() %>%
  filter(Freq > 0.6) # Filtramos para ver solo correlaciones fuertes
names(corr_links) <- c("source", "target", "weight")

net <- graph_from_data_frame(d = corr_links, directed = FALSE)
layout <- layout_with_fr(net)
net_nodes <- data.frame(id = V(net)$name, x = layout[,1], y = layout[,2])
net_edges <- as_data_frame(net, what = "edges")

# --- Datos para Crosstalk ---
# Para esta interacción, creamos un objeto de datos compartidos sin una clave específica.
# Crosstalk usará los identificadores de fila por defecto.
shared_data <- SharedData$new(datos_saber, group = "subset")


# ------------------------------------------------------------------------------
# SECCIÓN 2: DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI)
# ------------------------------------------------------------------------------
ui <- navbarPage(
  title = "Galería Avanzada Plotly",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  # --- Pestaña 1: Gráficos Fundamentales ---
  tabPanel(
    "1. Gráficos Fundamentales",
    icon = icon("chart-bar"),
    fluidPage(
      h3("Gráficos Esenciales con `plot_ly()`"),
      p("Aquí construimos gráficos comunes usando la sintaxis nativa de Plotly. Esto nos da un control más granular sobre la interactividad desde el inicio."),
      fluidRow(
        column(6,
               h4("Scatter Plot Interactivo"),
               p("Un gráfico de dispersión con tooltips personalizados que muestran múltiples variables al pasar el cursor."),
               plotlyOutput("fund_scatter"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("fund_scatter_code"))
        ),
        column(6,
               h4("Gráfico de Barras con Ordenamiento"),
               p("Un gráfico de barras que muestra el puntaje promedio. El eje X está ordenado de forma descendente para facilitar la comparación."),
               plotlyOutput("fund_bar"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("fund_bar_code"))
        )
      )
    )
  ),
  
  # --- Pestaña 2: Funcionalidades Interactivas ---
  tabPanel(
    "2. Laboratorio de Interactividad",
    icon = icon("vials"),
    fluidPage(
      h3("Explorando las Herramientas de Interacción"),
      p("Esta sección es un laboratorio para demostrar funcionalidades específicas de Plotly que potencian el análisis exploratorio."),
      
      # --- INICIO DE CORRECCIÓN: Estructura de Crosstalk con bscols y filter_select ---
      h4("Filtro Cruzado (Crosstalk)"),
      p("Selecciona una categoría en el menú desplegable. Verás cómo tanto el gráfico como la tabla se filtran automáticamente para mostrar solo los datos de ese grupo."),
      
      bscols(
        widths = c(3, 9),
        list(
          filter_select("cole_naturaleza_filter", "Filtrar por Naturaleza", shared_data, ~cole_naturaleza)
        ),
        plotlyOutput("int_brush_plot")
      ),
      DTOutput("int_brush_table"),
      tags$details(tags$summary("Ver Código"), verbatimTextOutput("int_brush_plot_code")),
      # --- FIN DE CORRECCIÓN ---
      
      hr(),
      h4("Animaciones y Controles"),
      p("Plotly puede crear animaciones basadas en una variable. Usa el control deslizante (slider) para ver cómo ha evolucionado (simuladamente) la relación entre puntajes a lo largo de los años."),
      plotlyOutput("int_animation_plot"),
      tags$details(tags$summary("Ver Código"), verbatimTextOutput("int_animation_plot_code"))
    )
  ),
  
  # --- Pestaña 3: Diagramas de Flujo y Red ---
  tabPanel(
    "3. Diagramas de Flujo y Red",
    icon = icon("sitemap"),
    fluidPage(
      h3("Visualizaciones para Relaciones Complejas"),
      p("Estos diagramas son excelentes para mostrar flujos, conexiones y estructuras en los datos."),
      
      fluidRow(
        column(6,
               h4("Diagrama de Sankey"),
               p("Muestra el flujo de estudiantes desde la naturaleza de su colegio (izquierda) hacia su nivel de rendimiento (derecha). El grosor de las bandas es proporcional al número de estudiantes."),
               plotlyOutput("diag_sankey"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("diag_sankey_code"))
        ),
        column(6,
               h4("Grafo de Red de Correlaciones"),
               p("Visualiza las materias como nodos y las correlaciones fuertes ( > 0.6) entre ellas como enlaces. El grosor del enlace representa la fuerza de la correlación."),
               plotlyOutput("diag_network"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("diag_network_code"))
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SECCIÓN 3: DEFINICIÓN DEL SERVIDOR (SERVER)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Lógica Pestaña 1: Gráficos Fundamentales ---
  output$fund_scatter <- renderPlotly({
    plot_ly(
      data = datos_saber,
      x = ~punt_matematicas, 
      y = ~punt_lectura_critica,
      color = ~cole_naturaleza,
      type = 'scatter',
      mode = 'markers',
      hoverinfo = 'text',
      text = ~paste(
        "<b>Matemáticas:</b>", punt_matematicas,
        "<br><b>Lectura:</b>", punt_lectura_critica,
        "<br><b>Global:</b>", punt_global
      )
    ) %>% layout(
      title = "Dispersión de Puntajes con Tooltips Personalizados",
      xaxis = list(title = "Puntaje Matemáticas"),
      yaxis = list(title = "Puntaje Lectura Crítica")
    )
  })
  output$fund_scatter_code <- renderPrint({ cat("plot_ly(\n  data = datos_saber,\n  x = ~punt_matematicas, y = ~punt_lectura_critica,\n  color = ~cole_naturaleza, type = 'scatter', mode = 'markers',\n  hoverinfo = 'text',\n  text = ~paste(\n    '<b>Matemáticas:</b>', punt_matematicas,\n    '<br><b>Lectura:</b>', punt_lectura_critica,\n    '<br><b>Global:</b>', punt_global\n  )\n) %>% layout(...)") })
  
  output$fund_bar <- renderPlotly({
    plot_ly(
      data = datos_promedio_naturaleza,
      x = ~reorder(cole_naturaleza, -punt_global),
      y = ~punt_global,
      color = ~cole_naturaleza,
      type = 'bar',
      hoverinfo = 'y',
      showlegend = FALSE
    ) %>% layout(
      title = "Puntaje Global Promedio por Naturaleza de Colegio",
      xaxis = list(title = "Naturaleza del Colegio"),
      yaxis = list(title = "Puntaje Global Promedio")
    )
  })
  output$fund_bar_code <- renderPrint({ cat("# El ordenamiento se hace en la preparación de datos\ndatos_promedio_naturaleza <- datos_saber %>% ... \n\nplot_ly(\n  data = datos_promedio_naturaleza,\n  x = ~reorder(cole_naturaleza, -punt_global),\n  y = ~punt_global,\n  color = ~cole_naturaleza, type = 'bar'\n) %>% layout(...)") })
  
  # --- Lógica Pestaña 2: Interactividad ---
  output$int_brush_plot <- renderPlotly({
    plot_ly(shared_data, x = ~punt_global, y = ~punt_matematicas, color = ~rendimiento_global)
  })
  output$int_brush_plot_code <- renderPrint({ cat("# 1. Crear el objeto de datos compartidos\nshared_data <- SharedData$new(datos_saber, group = 'subset')\n\n# 2. Usar el objeto en el filtro (UI) y en el gráfico (Server)\n# UI:\n# filter_select('id', 'Label', shared_data, ~variable_a_filtrar)\n# plotlyOutput('plot_id')\n\n# Server:\n# output$plot_id <- renderPlotly({\n#   plot_ly(shared_data, ...)\n# })") })

  output$int_brush_table <- renderDT({
    datatable(shared_data, extensions = "Scroller", style = "bootstrap",
              options = list(deferRender = TRUE, scrollY = 250, scroller = TRUE))
  }, server = FALSE)
  
  output$int_animation_plot <- renderPlotly({
    sim_data <- do.call(rbind, lapply(1:5, function(year) {
      datos_saber %>%
        sample_n(200) %>%
        mutate(
          punt_matematicas = punt_matematicas + (year * runif(n(), -2, 2)),
          year = 2020 + year
        )
    }))
    
    plot_ly(
      data = sim_data,
      x = ~punt_matematicas,
      y = ~punt_lectura_critica,
      size = ~punt_global,
      color = ~cole_naturaleza,
      frame = ~year,
      type = 'scatter',
      mode = 'markers',
      hoverinfo = 'text',
      text = ~paste("Año:", year)
    ) %>% layout(
      title = "Evolución Simulada de Puntajes"
    ) %>% animation_opts(
      frame = 1000, 
      transition = 500,
      easing = "linear-in-out"
    )
  })
  output$int_animation_plot_code <- renderPrint({ cat("# 1. Simular datos a través del tiempo\nsim_data <- ...\n\n# 2. Mapear la variable de tiempo al argumento `frame`\nplot_ly(\n  data = sim_data,\n  x = ~punt_matematicas,\n  y = ~punt_lectura_critica,\n  frame = ~year, # <-- La clave de la animación\n  ...\n) %>% animation_opts(...)") })
  
  # --- Lógica Pestaña 3: Diagramas ---
  output$diag_sankey <- renderPlotly({
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = nodes$name,
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = sankey_data$source,
        target = sankey_data$target,
        value =  sankey_data$value
      )
    ) %>% layout(
      title = "Flujo de Estudiantes por Naturaleza y Rendimiento"
    )
  })
  output$diag_sankey_code <- renderPrint({ cat("# 1. Preparar datos: crear nodos y enlaces con índices numéricos\nsankey_data <- datos_saber %>% count(...) ...\nnodes <- data.frame(name = ...)\nsankey_data$source <- match(...) - 1\nsankey_data$target <- match(...) - 1\n\n# 2. Construir el gráfico\nplot_ly(\n  type = 'sankey',\n  node = list(label = nodes$name, ...),\n  link = list(source = sankey_data$source, target = sankey_data$target, value = sankey_data$value)\n)") })
  
  output$diag_network <- renderPlotly({
    p <- plot_ly(
      type = "scatter",
      mode = "markers+text",
      x = net_nodes$x,
      y = net_nodes$y,
      text = net_nodes$id,
      textposition = "bottom center",
      marker = list(size = 20, color = "#ff9900"),
      hoverinfo = "none"
    )
    
    for(i in 1:nrow(net_edges)) {
      edge <- net_edges[i,]
      node1 <- net_nodes[net_nodes$id == edge$from,]
      node2 <- net_nodes[net_nodes$id == edge$to,]
      
      p <- p %>% add_trace(
        type = "scatter",
        mode = "lines",
        x = c(node1$x, node2$x),
        y = c(node1$y, node2$y),
        line = list(width = edge$weight * 5, color = "#a0a0a0"),
        hoverinfo = "text",
        text = paste("Correlación:", round(edge$weight, 2))
      )
    }
    
    p %>% layout(
      title = "Red de Correlaciones Fuertes entre Puntajes",
      showlegend = FALSE,
      xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  })
  output$diag_network_code <- renderPrint({ cat("# 1. Calcular matriz de correlación y filtrar enlaces fuertes\ncorr_links <- ...\n\n# 2. Calcular layout con `igraph`\nnet <- graph_from_data_frame(d = corr_links, ...)\nlayout <- layout_with_fr(net)\nnet_nodes <- data.frame(id = V(net)$name, x = layout[,1], y = layout[,2])\n\n# 3. Dibujar nodos y luego iterar para dibujar cada enlace\np <- plot_ly(..., type = 'scatter', mode = 'markers+text')\nfor(i in 1:nrow(net_edges)) {\n  p <- p %>% add_trace(..., type = 'scatter', mode = 'lines')\n}") })
}

# ------------------------------------------------------------------------------
# SECCIÓN 4: EJECUCIÓN DE LA APLICACIÓN
# ------------------------------------------------------------------------------
shinyApp(ui, server)

