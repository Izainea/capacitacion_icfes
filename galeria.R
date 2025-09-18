# ==============================================================================
# APP.R - GALERÍA DE VISUALIZACIONES Y COMPONENTES
# Objetivo: Servir como un catálogo interactivo con más de 30 ejemplos
#           de visualizaciones para el análisis de datos del ICFES,
#           incluyendo el código fuente de cada gráfico.
# ==============================================================================

# ------------------------------------------------------------------------------
# NOTA IMPORTANTE ANTES DE EJECUTAR:
# ------------------------------------------------------------------------------
# Esta aplicación está diseñada para ser un archivo único y autocontenido.
# Si tienes archivos como `global.R`, `ui.R`, o `server.R` en el mismo 
# directorio, Shiny podría intentar ejecutar esa aplicación modular en su lugar,
# lo que causaría conflictos.
#
# Para asegurar que esta galería se ejecute correctamente:
# 1. Asegúrate de que no haya otros archivos `ui.R`/`server.R`/`global.R` en la carpeta.
# 2. Ejecuta la app explícitamente por su nombre, por ejemplo:
#    shiny::runApp('app.R') (o el nombre que le hayas dado al archivo).
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# SECCIÓN 1: CARGA DE LIBRERÍAS Y PREPARACIÓN DE DATOS
# ------------------------------------------------------------------------------

# --- Carga de Librerías ---
library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(ggridges)
library(treemapify)
library(GGally)
library(reshape2)
library(patchwork)

# --- Carga y Procesamiento de Datos ---
# Para que la app sea autocontenida, emulamos la lógica de `global.R` aquí.
datos_crudos <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)

# Función de procesamiento encapsulada
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

# Ejecutamos el pipeline para tener los datos listos
datos_saber <- prepare_saber_data(datos_crudos)

# --- Datos Pre-calculados para Gráficos Específicos ---
datos_promedio_naturaleza <- datos_saber %>%
  group_by(cole_naturaleza) %>%
  summarise(across(starts_with("punt_"), mean, na.rm = TRUE), .groups = 'drop')

datos_conteo_rendimiento <- datos_saber %>%
  count(cole_naturaleza, rendimiento_global, .drop = FALSE)

# ------------------------------------------------------------------------------
# SECCIÓN 2: DEFINICIÓN DE LA INTERFAZ DE USUARIO (UI)
# ------------------------------------------------------------------------------
ui <- navbarPage(
  id = "nav",
  title = "Galería de Visualizaciones ICFES",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  # --- INICIO DE CAMBIO: CSS mejorado para la tabla ---
  tags$head(
    tags$style(HTML("
      /* Estilos para la tabla DT */
      .dataTables_wrapper {
        color: #e0e0e0; /* Color del texto general de la tabla */
      }
      table.dataTable thead th, table.dataTable thead td {
        background-color: #2c3e50 !important;
        color: #ffffff !important;
        border-bottom: 2px solid #ff9900;
      }
      /* Estilo para los filtros de columna */
      table.dataTable thead input {
        background-color: #555 !important;
        color: #fff !important;
        border: 1px solid #777 !important;
      }
      table.dataTable tbody tr {
        background-color: #343a40;
      }
      table.dataTable.stripe tbody tr.odd {
        background-color: #454d55;
      }
      table.dataTable.hover tbody tr:hover {
        background-color: #ff9900 !important;
        color: #2c3e50 !important;
      }
      .dataTables_filter input, .dataTables_length select {
        background-color: #555;
        color: #fff;
        border: 1px solid #777;
      }
      .paginate_button {
        background: #555 !important;
        color: #fff !important;
        border: 1px solid #777 !important;
      }
      .paginate_button.current, .paginate_button.current:hover {
        background: #ff9900 !important;
        color: #2c3e50 !important;
      }
      /* Estilo para los botones de exportación de DT */
      .dt-buttons .btn {
        background-color: #ff9900 !important;
        color: #2c3e50 !important;
        border: 1px solid #ff9900 !important;
      }
      .dt-buttons .btn:hover {
        background-color: #e68a00 !important;
        border: 1px solid #e68a00 !important;
      }
    "))
  ),
  # --- FIN DE CAMBIO ---
  
  # --- Pestaña 1: Distribuciones (1D) ---
  tabPanel(
    "1. Distribuciones",
    icon = icon("chart-line"),
    fluidPage(
      h3("Visualización de una Sola Variable"),
      p("El primer paso en cualquier análisis es entender las variables individualmente. Estos gráficos nos ayudan a ver la forma, el centro y la dispersión de los datos."),
      
      fluidRow(
        column(4, 
               h4("1. Histograma"),
               p("Muestra la frecuencia de los puntajes en 'bins' o rangos. Ideal para ver la forma de la distribución."),
               plotOutput("dist_hist", height = "300px"),
               tags$details(
                 tags$summary("Ver Código"),
                 verbatimTextOutput("dist_hist_code")
               )
        ),
        column(4, 
               h4("2. Gráfico de Densidad"),
               p("Una versión suavizada del histograma. Es útil para visualizar la forma de la distribución sin la rigidez de los 'bins'."),
               plotOutput("dist_density", height = "300px"),
               tags$details(
                 tags$summary("Ver Código"),
                 verbatimTextOutput("dist_density_code")
               )
        ),
        column(4,
               h4("3. Histograma + Densidad"),
               p("Combinar ambos gráficos permite tener una visión completa: la frecuencia real de los datos y su forma teórica suavizada."),
               plotOutput("dist_hist_density", height = "300px"),
               tags$details(
                 tags$summary("Ver Código"),
                 verbatimTextOutput("dist_hist_density_code")
               )
        )
      ),
      hr(),
      fluidRow(
        column(4, 
               h4("4. Boxplot"),
               p("Resume la distribución en cinco números: mínimo, primer cuartil, mediana, tercer cuartil y máximo. Excelente para comparar grupos."),
               plotOutput("dist_boxplot", height = "300px"),
               tags$details(
                 tags$summary("Ver Código"),
                 verbatimTextOutput("dist_boxplot_code")
               )
        ),
        column(4, 
               h4("5. Gráfico de Violín"),
               p("Combina un boxplot con un gráfico de densidad. Muestra no solo el resumen estadístico sino también la densidad de los datos en cada punto."),
               plotOutput("dist_violin", height = "300px"),
               tags$details(
                 tags$summary("Ver Código"),
                 verbatimTextOutput("dist_violin_code")
               )
        ),
        column(4,
               h4("6. Ridge Plot"),
               p("Permite comparar múltiples distribuciones de densidad de forma superpuesta, ideal para ver cómo cambia la forma del puntaje global entre niveles de rendimiento."),
               plotOutput("dist_ridge", height = "300px"),
               tags$details(
                 tags$summary("Ver Código"),
                 verbatimTextOutput("dist_ridge_code")
               )
        )
      )
    )
  ),
  
  # --- Pestaña 2: Relaciones (2D) ---
  tabPanel(
    "2. Relaciones",
    icon = icon("project-diagram"),
    fluidPage(
      h3("Visualización de Dos Variables"),
      p("Una vez entendidas las variables por separado, buscamos relaciones entre ellas. ¿Una variable afecta a la otra?"),
      
      fluidRow(
        column(4, 
               h4("7. Gráfico de Dispersión (Scatter Plot)"),
               p("La herramienta fundamental para visualizar la relación entre dos variables numéricas."),
               plotOutput("rel_scatter", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("rel_scatter_code"))
        ),
        column(4, 
               h4("8. Dispersión con Línea de Regresión"),
               p("Añadir una línea de regresión (lineal en este caso) ayuda a visualizar la tendencia general en los datos."),
               plotOutput("rel_scatter_lm", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("rel_scatter_lm_code"))
        ),
        column(4,
               h4("9. Dispersión con 'Jitter'"),
               p("Cuando muchos puntos se superponen, el 'jittering' añade un pequeño ruido aleatorio para separarlos y visualizar mejor la densidad."),
               plotOutput("rel_scatter_jitter", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("rel_scatter_jitter_code"))
        )
      ),
      hr(),
      fluidRow(
        column(4, 
               h4("10. Gráfico de Burbujas"),
               p("Un gráfico de dispersión que usa una tercera variable para definir el tamaño de los puntos. Aquí, el tamaño representa el puntaje global."),
               plotOutput("rel_bubble", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("rel_bubble_code"))
        ),
        column(4, 
               h4("11. Heatmap de Densidad 2D"),
               p("Visualiza la densidad de puntos en un gráfico de dispersión. Las áreas más 'calientes' indican una mayor concentración de datos."),
               plotOutput("rel_density2d", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("rel_density2d_code"))
        ),
        column(4,
               h4("12. Gráfico de Hexágonos (Hexbin)"),
               p("Similar al heatmap de densidad, pero agrupa los puntos en hexágonos. Es una alternativa eficaz para manejar el 'overplotting'."),
               plotOutput("rel_hexbin", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("rel_hexbin_code"))
        )
      )
    )
  ),
  
  # --- Pestaña 3: Comparaciones y Composiciones ---
  tabPanel(
    "3. Comparaciones y Composiciones",
    icon = icon("chart-pie"),
    fluidPage(
      h3("Comparando Grupos y Viendo el Todo"),
      p("Estos gráficos nos ayudan a comparar valores entre diferentes categorías o a entender cómo las partes componen un todo."),

      fluidRow(
        column(4, 
               h4("13. Gráfico de Barras"),
               p("Compara una métrica (en este caso, el puntaje promedio) entre diferentes grupos (naturaleza del colegio)."),
               plotOutput("comp_bar", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("comp_bar_code"))
        ),
        column(4, 
               h4("14. Lollipop Chart"),
               p("Una alternativa estéticamente más ligera al gráfico de barras, útil cuando hay muchas categorías que comparar."),
               plotOutput("comp_lollipop", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("comp_lollipop_code"))
        ),
        column(4,
               h4("15. Gráfico de Barras Agrupadas"),
               p("Permite comparar una métrica principal a través de dos variables categóricas. Aquí, el conteo de estudiantes por rendimiento y naturaleza."),
               plotOutput("comp_bar_grouped", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("comp_bar_grouped_code"))
        )
      ),
      hr(),
      fluidRow(
        column(4, 
               h4("16. Gráfico de Barras Apiladas"),
               p("Muestra cómo se compone cada barra principal. Vemos la distribución del rendimiento dentro de cada tipo de colegio."),
               plotOutput("comp_bar_stacked", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("comp_bar_stacked_code"))
        ),
        column(4, 
               h4("17. Gráfico de Barras 100% Apiladas"),
               p("Ideal para comparar proporciones. Normaliza cada barra al 100%, facilitando ver si la distribución de rendimiento difiere entre colegios oficiales y no oficiales."),
               plotOutput("comp_bar_filled", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("comp_bar_filled_code"))
        ),
        column(4,
               h4("18. Treemap"),
               p("Muestra datos jerárquicos como rectángulos anidados. El área de cada rectángulo es proporcional a su valor. Útil para ver la composición de una categoría."),
               plotOutput("comp_treemap", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("comp_treemap_code"))
        )
      )
    )
  ),

  # --- Pestaña 4: Visualizaciones Avanzadas ---
  tabPanel(
    "4. Avanzadas",
    icon = icon("atom"),
    fluidPage(
      h3("Técnicas de Visualización Avanzadas"),
      p("Gráficos que permiten analizar múltiples variables simultáneamente o que requieren una preparación de datos más específica."),
      
      fluidRow(
        column(4, 
               h4("19. Matriz de Correlación (Heatmap)"),
               p("Calcula la correlación entre todas las variables numéricas y la visualiza como un heatmap. Fundamental para la exploración previa al modelado."),
               plotOutput("adv_corrplot", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("adv_corrplot_code"))
        ),
        column(4, 
               h4("20. Gráfico de Coordenadas Paralelas"),
               p("Permite visualizar datos multivariados. Cada línea representa un estudiante (o un grupo) y su trayectoria a través de los diferentes puntajes."),
               plotOutput("adv_parallel", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("adv_parallel_code"))
        ),
        column(4,
               h4("21. Gráfico de Pares (Pairs Plot)"),
               p("Crea una matriz de gráficos de dispersión para todas las combinaciones de variables, mostrando también las distribuciones en la diagonal."),
               plotOutput("adv_pairs", height = "300px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("adv_pairs_code"))
        )
      ),
      hr(),
      fluidRow(
        column(6, 
               h4("22. Facet Wrap"),
               p("Divide un gráfico en una cuadrícula de sub-gráficos basados en una variable categórica. Aquí, vemos la distribución de puntaje global para cada naturaleza de colegio."),
               plotOutput("adv_facet_wrap", height = "400px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("adv_facet_wrap_code"))
        ),
        column(6, 
               h4("23. Facet Grid"),
               p("Similar a Facet Wrap, pero crea una matriz 2D basada en dos variables categóricas (naturaleza vs. rendimiento)."),
               plotOutput("adv_facet_grid", height = "400px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("adv_facet_grid_code"))
        )
      )
    )
  ),

  # --- Pestaña 5: Storytelling ---
  tabPanel(
    "5. Storytelling",
    icon = icon("book-open"),
    fluidPage(
      h3("Contando una Historia con Gráficos"),
      p("Estos gráficos van más allá de la simple visualización. Usan títulos, subtítulos, anotaciones y color de manera intencionada para comunicar un mensaje específico y claro. Son ejemplos de visualizaciones explicativas."),
      
      fluidRow(
        column(6, 
               h4("24. Destacando un Grupo"),
               p("En lugar de colorear todos los grupos, usamos el color para resaltar solo el grupo de interés (Rendimiento Alto), dejando el resto en un segundo plano."),
               plotOutput("story_highlight", height = "400px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("story_highlight_code"))
        ),
        column(6, 
               h4("25. Gráfico Anotado"),
               p("Usamos flechas y texto (`annotate`) para señalar puntos de datos específicos o tendencias importantes, guiando la atención del lector directamente al insight."),
               plotOutput("story_annotated", height = "400px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("story_annotated_code"))
        )
      )
    )
  ),
  
  # --- Pestaña 6: Interactividad ---
  tabPanel(
    "6. Interactividad",
    icon = icon("hand-pointer"),
    fluidPage(
      h3("Herramientas de Exploración Interactiva"),
      p("La combinación de `plotly` para gráficos y `DT` para tablas proporciona un entorno de exploración de datos de alto nivel."),
      
      fluidRow(
        column(6, 
               h4("26. Boxplot Interactivo"),
               p("Al pasar el cursor sobre cada boxplot, `plotly` nos muestra automáticamente los valores de los cuartiles, la mediana y los outliers."),
               plotlyOutput("int_boxplotly", height = "400px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("int_boxplotly_code"))
        ),
        column(6, 
               h4("27. Histograma Interactivo con Selección"),
               p("Este histograma permite al usuario cambiar el número de 'bins' con un slider, explorando la distribución a diferentes niveles de granularidad."),
               sliderInput("hist_bins", "Número de Bins:", min = 5, max = 100, value = 30),
               plotlyOutput("int_histplotly", height = "350px"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("int_histplotly_code"))
        )
      ),
      hr(),
      fluidRow(
        column(12,
               h4("28-30+. Tabla Interactiva con Filtros, Paginación y Exportación"),
               p("Esta única tabla de `DT` ya cuenta como múltiples visualizaciones por su riqueza funcional: búsqueda global, filtros por columna, ordenamiento, paginación y botones para exportar los datos a CSV, Excel o PDF."),
               DTOutput("int_datatable"),
               tags$details(tags$summary("Ver Código"), verbatimTextOutput("int_datatable_code"))
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# SECCIÓN 3: DEFINICIÓN DEL SERVIDOR (SERVER)
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Lógica para Pestaña 1: Distribuciones ---
  output$dist_hist <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_global)) +
      geom_histogram(binwidth = 10, fill = "#1E90FF", color = "white", alpha = 0.8) +
      labs(x = "Puntaje Global", y = "Frecuencia") + theme_minimal()
    print(p)
  }, res = 96)
  output$dist_hist_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_global)) +\n  geom_histogram(binwidth = 10, fill = '#1E90FF', color = 'white', alpha = 0.8) +\n  labs(x = 'Puntaje Global', y = 'Frecuencia') + theme_minimal()") })
  
  output$dist_density <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_global)) +
      geom_density(fill = "#1E90FF", alpha = 0.8) +
      labs(x = "Puntaje Global", y = "Densidad") + theme_minimal()
    print(p)
  }, res = 96)
  output$dist_density_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_global)) +\n  geom_density(fill = '#1E90FF', alpha = 0.8) +\n  labs(x = 'Puntaje Global', y = 'Densidad') + theme_minimal()") })
  
  output$dist_hist_density <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_global)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 10, fill = "#1E90FF", color = "white", alpha = 0.7) +
      geom_density(color = "#FFD700", linewidth = 1.2) +
      labs(x = "Puntaje Global", y = "Densidad") + theme_minimal()
    print(p)
  }, res = 96)
  output$dist_hist_density_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_global)) +\n  geom_histogram(aes(y = after_stat(density)), binwidth = 10, fill = '#1E90FF', color = 'white', alpha = 0.7) +\n  geom_density(color = '#FFD700', linewidth = 1.2) +\n  labs(x = 'Puntaje Global', y = 'Densidad') + theme_minimal()") })
  
  output$dist_boxplot <- renderPlot({
    p <- ggplot(datos_saber, aes(x = cole_naturaleza, y = punt_global, fill = cole_naturaleza)) +
      geom_boxplot(alpha = 0.8) +
      labs(x = "Naturaleza del Colegio", y = "Puntaje Global") + theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$dist_boxplot_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = cole_naturaleza, y = punt_global, fill = cole_naturaleza)) +\n  geom_boxplot(alpha = 0.8) +\n  labs(x = 'Naturaleza del Colegio', y = 'Puntaje Global') + theme_minimal() + theme(legend.position = 'none')") })
  
  output$dist_violin <- renderPlot({
    p <- ggplot(datos_saber, aes(x = cole_naturaleza, y = punt_global, fill = cole_naturaleza)) +
      geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +
      labs(x = "Naturaleza del Colegio", y = "Puntaje Global") + theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$dist_violin_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = cole_naturaleza, y = punt_global, fill = cole_naturaleza)) +\n  geom_violin(alpha = 0.8, draw_quantiles = c(0.25, 0.5, 0.75)) +\n  labs(x = 'Naturaleza del Colegio', y = 'Puntaje Global') + theme_minimal() + theme(legend.position = 'none')") })
  
  output$dist_ridge <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_global, y = rendimiento_global, fill = rendimiento_global)) +
      geom_density_ridges() +
      labs(x = "Puntaje Global", y = "Nivel de Rendimiento") + theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$dist_ridge_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_global, y = rendimiento_global, fill = rendimiento_global)) +\n  geom_density_ridges() +\n  labs(x = 'Puntaje Global', y = 'Nivel de Rendimiento') + theme_minimal() + theme(legend.position = 'none')") })
  
  # --- Lógica para Pestaña 2: Relaciones ---
  output$rel_scatter <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      geom_point(aes(color = cole_naturaleza), alpha = 0.4) +
      labs(x = "Puntaje Matemáticas", y = "Puntaje Lectura Crítica") + theme_minimal()
    print(p)
  }, res = 96)
  output$rel_scatter_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  geom_point(aes(color = cole_naturaleza), alpha = 0.4) +\n  labs(x = 'Puntaje Matemáticas', y = 'Puntaje Lectura Crítica') + theme_minimal()") })
  
  output$rel_scatter_lm <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      geom_point(aes(color = cole_naturaleza), alpha = 0.4) +
      geom_smooth(method = "lm", color = "#FFFFFF", se = FALSE) +
      labs(x = "Puntaje Matemáticas", y = "Puntaje Lectura Crítica") + theme_minimal()
    print(p)
  }, res = 96)
  output$rel_scatter_lm_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  geom_point(aes(color = cole_naturaleza), alpha = 0.4) +\n  geom_smooth(method = 'lm', color = '#FFFFFF', se = FALSE) +\n  labs(x = 'Puntaje Matemáticas', y = 'Puntaje Lectura Crítica') + theme_minimal()") })
  
  output$rel_scatter_jitter <- renderPlot({
    p <- ggplot(datos_saber, aes(x = rendimiento_global, y = punt_global)) +
      geom_jitter(aes(color = rendimiento_global), width = 0.2, alpha = 0.5) +
      labs(x = "Rendimiento", y = "Puntaje Global") + theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$rel_scatter_jitter_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = rendimiento_global, y = punt_global)) +\n  geom_jitter(aes(color = rendimiento_global), width = 0.2, alpha = 0.5) +\n  labs(x = 'Rendimiento', y = 'Puntaje Global') + theme_minimal() + theme(legend.position = 'none')") })
  
  output$rel_bubble <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      geom_point(aes(color = cole_naturaleza, size = punt_global), alpha = 0.6) +
      scale_size_continuous(range = c(1, 10)) +
      labs(x = "Puntaje Matemáticas", y = "Puntaje Lectura Crítica", size = "Puntaje Global") + theme_minimal()
    print(p)
  }, res = 96)
  output$rel_bubble_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  geom_point(aes(color = cole_naturaleza, size = punt_global), alpha = 0.6) +\n  scale_size_continuous(range = c(1, 10)) +\n  labs(x = 'Puntaje Matemáticas', y = 'Puntaje Lectura Crítica', size = 'Puntaje Global') + theme_minimal()") })

  output$rel_density2d <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon") +
      scale_fill_viridis_c() +
      labs(x = "Puntaje Matemáticas", y = "Puntaje Lectura Crítica") + theme_minimal()
    print(p)
  }, res = 96)
  output$rel_density2d_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  stat_density_2d(aes(fill = after_stat(level)), geom = 'polygon') +\n  scale_fill_viridis_c() +\n  labs(x = 'Puntaje Matemáticas', y = 'Puntaje Lectura Crítica') + theme_minimal()") })

  output$rel_hexbin <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      geom_hex() +
      scale_fill_viridis_c() +
      labs(x = "Puntaje Matemáticas", y = "Puntaje Lectura Crítica") + theme_minimal()
    print(p)
  }, res = 96)
  output$rel_hexbin_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  geom_hex() +\n  scale_fill_viridis_c() +\n  labs(x = 'Puntaje Matemáticas', y = 'Puntaje Lectura Crítica') + theme_minimal()") })

  # --- Lógica para Pestaña 3: Comparaciones ---
  output$comp_bar <- renderPlot({
    p <- ggplot(datos_promedio_naturaleza, aes(x = reorder(cole_naturaleza, -punt_global), y = punt_global, fill = cole_naturaleza)) +
      geom_col() +
      labs(x = "Naturaleza del Colegio", y = "Puntaje Global Promedio") + theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$comp_bar_code <- renderPrint({ cat("ggplot(datos_promedio_naturaleza, aes(x = reorder(cole_naturaleza, -punt_global), y = punt_global, fill = cole_naturaleza)) +\n  geom_col() +\n  labs(x = 'Naturaleza del Colegio', y = 'Puntaje Global Promedio') + theme_minimal() + theme(legend.position = 'none')") })
  
  output$comp_lollipop <- renderPlot({
     p <- ggplot(datos_promedio_naturaleza, aes(x = reorder(cole_naturaleza, punt_global), y = punt_global)) +
      geom_segment(aes(xend = reorder(cole_naturaleza, punt_global), yend = 0), color = "grey50") +
      geom_point(aes(color = cole_naturaleza), size = 5) +
      labs(x = "Naturaleza del Colegio", y = "Puntaje Global Promedio") + theme_minimal() + theme(legend.position = "none") +
      coord_flip()
     print(p)
  }, res = 96)
  output$comp_lollipop_code <- renderPrint({ cat("ggplot(datos_promedio_naturaleza, aes(x = reorder(cole_naturaleza, punt_global), y = punt_global)) +\n  geom_segment(aes(xend = reorder(cole_naturaleza, punt_global), yend = 0), color = 'grey50') +\n  geom_point(aes(color = cole_naturaleza), size = 5) +\n  labs(x = 'Naturaleza del Colegio', y = 'Puntaje Global Promedio') + theme_minimal() + theme(legend.position = 'none') +\n  coord_flip()") })
  
  output$comp_bar_grouped <- renderPlot({
    p <- ggplot(datos_conteo_rendimiento, aes(x = cole_naturaleza, y = n, fill = rendimiento_global)) +
      geom_col(position = "dodge") +
      labs(x = "Naturaleza", y = "Número de Estudiantes", fill = "Rendimiento") + theme_minimal()
    print(p)
  }, res = 96)
  output$comp_bar_grouped_code <- renderPrint({ cat("ggplot(datos_conteo_rendimiento, aes(x = cole_naturaleza, y = n, fill = rendimiento_global)) +\n  geom_col(position = 'dodge') +\n  labs(x = 'Naturaleza', y = 'Número de Estudiantes', fill = 'Rendimiento') + theme_minimal()") })
  
  output$comp_bar_stacked <- renderPlot({
    p <- ggplot(datos_conteo_rendimiento, aes(x = cole_naturaleza, y = n, fill = rendimiento_global)) +
      geom_col(position = "stack") +
      labs(x = "Naturaleza", y = "Número de Estudiantes", fill = "Rendimiento") + theme_minimal()
    print(p)
  }, res = 96)
  output$comp_bar_stacked_code <- renderPrint({ cat("ggplot(datos_conteo_rendimiento, aes(x = cole_naturaleza, y = n, fill = rendimiento_global)) +\n  geom_col(position = 'stack') +\n  labs(x = 'Naturaleza', y = 'Número de Estudiantes', fill = 'Rendimiento') + theme_minimal()") })
  
  output$comp_bar_filled <- renderPlot({
    p <- ggplot(datos_conteo_rendimiento, aes(x = cole_naturaleza, y = n, fill = rendimiento_global)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Naturaleza", y = "Proporción", fill = "Rendimiento") + theme_minimal()
    print(p)
  }, res = 96)
  output$comp_bar_filled_code <- renderPrint({ cat("ggplot(datos_conteo_rendimiento, aes(x = cole_naturaleza, y = n, fill = rendimiento_global)) +\n  geom_col(position = 'fill') +\n  scale_y_continuous(labels = scales::percent) +\n  labs(x = 'Naturaleza', y = 'Proporción', fill = 'Rendimiento') + theme_minimal()") })
  
  output$comp_treemap <- renderPlot({
    p <- ggplot(datos_conteo_rendimiento, aes(area = n, fill = cole_naturaleza, subgroup = cole_naturaleza, label = rendimiento_global)) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_text(color = "white", place = "centre", grow = TRUE) +
      theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$comp_treemap_code <- renderPrint({ cat("ggplot(datos_conteo_rendimiento, aes(area = n, fill = cole_naturaleza, subgroup = cole_naturaleza, label = rendimiento_global)) +\n  geom_treemap() +\n  geom_treemap_subgroup_border() +\n  geom_treemap_text(color = 'white', place = 'centre', grow = TRUE) +\n  theme_minimal() + theme(legend.position = 'none')") })
  
  # --- Lógica para Pestaña 4: Avanzadas ---
  output$adv_corrplot <- renderPlot({
    corr_matrix <- datos_saber %>% select(starts_with("punt_")) %>% cor()
    melted_corr <- melt(corr_matrix)
    p <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "#2c7bb6", high = "#d7191c", mid = "white", midpoint = 0) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "", y = "", fill = "Correlación")
    print(p)
  }, res = 96)
  output$adv_corrplot_code <- renderPrint({ cat("corr_matrix <- datos_saber %>% select(starts_with('punt_')) %>% cor()\nmelted_corr <- melt(corr_matrix)\nggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +\n  geom_tile() +\n  scale_fill_gradient2(low = '#2c7bb6', high = '#d7191c', mid = 'white', midpoint = 0) +\n  theme_minimal() +\n  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +\n  labs(x = '', y = '', fill = 'Correlación')") })
  
  output$adv_parallel <- renderPlot({
    p <- ggparcoord(datos_promedio_naturaleza, columns = 2:6, groupColumn = "cole_naturaleza", scale = "uniminmax") +
      theme_minimal() +
      labs(title = "Perfil de Puntajes Promedio por Naturaleza de Colegio")
    print(p)
  }, res = 96)
  output$adv_parallel_code <- renderPrint({ cat("ggparcoord(datos_promedio_naturaleza, columns = 2:6, groupColumn = 'cole_naturaleza', scale = 'uniminmax') +\n  theme_minimal() +\n  labs(title = 'Perfil de Puntajes Promedio por Naturaleza de Colegio')") })
  
  adv_pairs_plot <- eventReactive(input$nav == "4. Avanzadas", {
    p <- ggpairs(datos_saber, columns = c("punt_global", "punt_matematicas", "punt_lectura_critica"),
            aes(color = cole_naturaleza, alpha = 0.5)) +
      theme_minimal()
    return(p)
  })

  output$adv_pairs <- renderPlot({
    print(adv_pairs_plot())
  }, res = 96)
  output$adv_pairs_code <- renderPrint({ cat("p <- ggpairs(datos_saber, columns = c('punt_global', 'punt_matematicas', 'punt_lectura_critica'),\n        aes(color = cole_naturaleza, alpha = 0.5)) +\n  theme_minimal()\n# Se usa print() para asegurar el renderizado estático en Shiny.\nprint(p)") })
  
  output$adv_facet_wrap <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_global)) +
      geom_histogram(aes(fill = cole_naturaleza), binwidth = 15) +
      facet_wrap(~ cole_naturaleza) +
      theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$adv_facet_wrap_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_global)) +\n  geom_histogram(aes(fill = cole_naturaleza), binwidth = 15) +\n  facet_wrap(~ cole_naturaleza) +\n  theme_minimal() + theme(legend.position = 'none')") })
  
  output$adv_facet_grid <- renderPlot({
    p <- ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      geom_point(alpha = 0.2) +
      facet_grid(rendimiento_global ~ cole_naturaleza) +
      theme_minimal()
    print(p)
  }, res = 96)
  output$adv_facet_grid_code <- renderPrint({ cat("ggplot(datos_saber, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  geom_point(alpha = 0.2) +\n  facet_grid(rendimiento_global ~ cole_naturaleza) +\n  theme_minimal()") })
  
  # --- Lógica para Pestaña 5: Storytelling ---
  output$story_highlight <- renderPlot({
    p <- datos_saber %>%
      mutate(highlight = ifelse(rendimiento_global == "Alto", "Sí", "No")) %>%
      ggplot(aes(x = punt_sociales_ciudadanas, y = punt_c_naturales)) +
      geom_point(aes(color = highlight), alpha = 0.6) +
      scale_color_manual(values = c("Sí" = "#ff9900", "No" = "grey50")) +
      labs(
        title = "Estudiantes de Alto Rendimiento Muestran Excelencia en Múltiples Áreas",
        subtitle = "Puntajes en Ciencias Sociales vs. Ciencias Naturales",
        x = "Puntaje Sociales y Ciudadanas", y = "Puntaje Ciencias Naturales"
      ) +
      theme_minimal() + theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$story_highlight_code <- renderPrint({ cat("datos_saber %>%\n  mutate(highlight = ifelse(rendimiento_global == 'Alto', 'Sí', 'No')) %>%\n  ggplot(aes(x = punt_sociales_ciudadanas, y = punt_c_naturales)) +\n  geom_point(aes(color = highlight), alpha = 0.6) +\n  scale_color_manual(values = c('Sí' = '#ff9900', 'No' = 'grey50')) +\n  labs(\n    title = 'Estudiantes de Alto Rendimiento Muestran Excelencia en Múltiples Áreas',\n    subtitle = 'Puntajes en Ciencias Sociales vs. Ciencias Naturales',\n    x = 'Puntaje Sociales y Ciudadanas', y = 'Puntaje Ciencias Naturales'\n  ) +\n  theme_minimal() + theme(legend.position = 'none')") })
  
  output$story_annotated <- renderPlot({
    p <- ggplot(datos_promedio_naturaleza, aes(x = punt_matematicas, y = punt_lectura_critica)) +
      geom_point(aes(color = cole_naturaleza, size = punt_global)) +
      geom_text(aes(label = cole_naturaleza), vjust = -1.5) +
      scale_size_continuous(range=c(5,10)) +
      labs(title = "Colegios No Oficiales Lideran en Promedios de Lectura y Matemáticas",
           x = "Promedio Matemáticas", y = "Promedio Lectura Crítica") +
      annotate("text", x = 60, y = 63, label = "Mayor rendimiento\npromedio general", color = "white", fontface="italic") +
      annotate("curve", x = 60, y = 62.5, xend = 62.5, yend = 63.8, curvature = 0.3, arrow = arrow(length = unit(2, "mm"))) +
      theme_minimal() +
      theme(legend.position = "none")
    print(p)
  }, res = 96)
  output$story_annotated_code <- renderPrint({ cat("ggplot(datos_promedio_naturaleza, aes(x = punt_matematicas, y = punt_lectura_critica)) +\n  geom_point(aes(color = cole_naturaleza, size = punt_global)) +\n  geom_text(aes(label = cole_naturaleza), vjust = -1.5) +\n  scale_size_continuous(range=c(5,10)) +\n  labs(title = 'Colegios No Oficiales Lideran en Promedios de Lectura y Matemáticas',\n       x = 'Promedio Matemáticas', y = 'Promedio Lectura Crítica') +\n  annotate('text', x = 60, y = 63, label = 'Mayor rendimiento\\npromedio general', color = 'white', fontface='italic') +\n  annotate('curve', x = 60, y = 62.5, xend = 62.5, yend = 63.8, curvature = 0.3, arrow = arrow(length = unit(2, 'mm'))) +\n  theme_minimal() +\n  theme(legend.position = 'none')") })

  # --- Lógica para Pestaña 6: Interactividad ---
  output$int_boxplotly <- renderPlotly({
    p <- ggplot(datos_saber, aes(x = rendimiento_global, y = punt_global, fill = rendimiento_global)) +
      geom_boxplot() +
      labs(title = "Puntaje Global por Nivel de Rendimiento", x = "Rendimiento", y = "Puntaje Global") +
      theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  output$int_boxplotly_code <- renderPrint({ cat("p <- ggplot(datos_saber, aes(x = rendimiento_global, y = punt_global, fill = rendimiento_global)) +\n  geom_boxplot() + \n  labs(...) + \n  theme_minimal()\nggplotly(p)") })
  
  output$int_histplotly <- renderPlotly({
    # Nota: El input$hist_bins es reactivo, por lo que este bloque se re-ejecuta
    p <- ggplot(datos_saber, aes(x = punt_global)) +
      geom_histogram(bins = input$hist_bins, fill = "#ff9900", alpha = 0.8) +
      labs(title = "Distribución del Puntaje Global (Bins Interactivos)", x = "Puntaje Global", y = "Frecuencia") +
      theme_minimal()
    ggplotly(p) %>% layout(bargap = 0.1) 
  })
  output$int_histplotly_code <- renderPrint({ cat("# El `input$hist_bins` en geom_histogram() lo hace reactivo.\np <- ggplot(datos_saber, aes(x = punt_global)) +\n  geom_histogram(bins = input$hist_bins, fill = '#ff9900', alpha = 0.8) + \n  labs(...) + \n  theme_minimal()\nggplotly(p)") })
  
  output$int_datatable <- renderDT({
    datatable(
      datos_saber,
      rownames = FALSE,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'collection', buttons = c('copy', 'csv', 'excel', 'pdf'), text = 'Exportar Datos')
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json')
      ),
      filter = 'top'
    )
  })
  output$int_datatable_code <- renderPrint({ cat("datatable(\n  datos_saber,\n  rownames = FALSE,\n  extensions = 'Buttons',\n  options = list(\n    pageLength = 10,\n    dom = 'Bfrtip',\n    buttons = list(\n      list(extend = 'collection', buttons = c('copy', 'csv', 'excel', 'pdf'), text = 'Exportar Datos')\n    ),\n    language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Spanish.json')\n  ),\n  filter = 'top'\n)") })
}

# ------------------------------------------------------------------------------
# SECCIÓN 5: EJECUCIÓN DE LA APLICACIÓN
# ------------------------------------------------------------------------------
shinyApp(ui, server)

