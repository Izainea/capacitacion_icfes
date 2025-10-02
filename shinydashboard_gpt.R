# ==============================================================================
# APLICACIÓN SHINY DASHBOARD – ICFES (Variables reales)
# ==============================================================================
# - Filtros: periodo, estu_genero, cole_jornada, cole_naturaleza, fami_estratovivienda
# - KPIs:
#   (1) Puntaje Global Promedio (punt_global)
#   (2) % de estudiantes con ≥3 áreas en niveles 3–4 (usando desemp_* por área, sin inglés)
#   (3) Mayor brecha interna por área (F vs M) en puntajes (punt_*)
# - Análisis Profundo: distribución de niveles, brechas F vs M, tendencias por periodo, heatmap
# ------------------------------------------------------------------------------

# 1) ENTORNO GLOBAL
# ==============================================================================
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)
options(dplyr.summarise.inform = FALSE)

# --- Helpers
to_title <- function(x) toupper(gsub("_", " ", x))
areas_punt <- c("punt_matematicas","punt_lectura_critica","punt_sociales_ciudadanas","punt_c_naturales")
areas_desemp <- c("desemp_matematicas","desemp_lectura_critica","desemp_sociales_ciudadanas","desemp_c_naturales")

# --- Carga de datos
# Si tienes tu archivo real, se leerá. Si no, se simula con los nombres REALES.
if (file.exists("data/raw/Examen_Saber_11_20242.txt")) {
  datos_saber <- read_delim("data/raw/Examen_Saber_11_20242.txt", delim = ";", show_col_types = FALSE)
} else {
  # Simulación con NOMBRES reales para no romper UI/server
  set.seed(123)
  n <- 4000
  datos_saber <- tibble(
    periodo = sample(c(20191,20192,20201,20202,20211,20212,20221,20222,20231,20232,20241,20242), n, TRUE),
    estu_genero = sample(c("F","M","Otro/No reporta"), n, TRUE, c(0.49,0.49,0.02)),
    cole_jornada = sample(c("MAÑANA","TARDE","NOCTURNA","UNICA","FINES DE SEMANA"), n, TRUE, c(0.45,0.35,0.05,0.1,0.05)),
    cole_naturaleza = sample(c("OFICIAL","NO OFICIAL"), n, TRUE, c(0.7,0.3)),
    fami_estratovivienda = sample(c("Estrato 1","Estrato 2","Estrato 3","Estrato 4","Estrato 5","Estrato 6"), n, TRUE,
                                  c(0.25,0.3,0.25,0.12,0.05,0.03)),
    # Desempeños por área (1–4). *Inglés no se usa en KPI#2*
    desemp_c_naturales = sample(1:4, n, TRUE, c(0.15,0.35,0.35,0.15)),
    desemp_lectura_critica = sample(1:4, n, TRUE, c(0.12,0.33,0.38,0.17)),
    desemp_matematicas = sample(1:4, n, TRUE, c(0.18,0.36,0.32,0.14)),
    desemp_sociales_ciudadanas = sample(1:4, n, TRUE, c(0.14,0.34,0.36,0.16)),
    desemp_ingles = sample(c("A-","A1","A2","B1","B+"), n, TRUE), # solo referencial
    # Puntajes por área (0–100) y global (0–500 aprox.)
    punt_matematicas = pmin(100, pmax(0, round(rnorm(n, 55, 12)))),
    punt_lectura_critica = pmin(100, pmax(0, round(rnorm(n, 56, 11)))),
    punt_sociales_ciudadanas = pmin(100, pmax(0, round(rnorm(n, 54, 12)))),
    punt_c_naturales = pmin(100, pmax(0, round(rnorm(n, 55, 11)))),
    punt_ingles = pmin(100, pmax(0, round(rnorm(n, 50, 15)))),
    punt_global = round(rnorm(n, 280, 60))
  )
}

# Opciones para UI
area_choices <- c("Todas",
                  "Lectura Crítica" = "punt_lectura_critica",
                  "Matemáticas" = "punt_matematicas",
                  "Sociales y Ciudadanas" = "punt_sociales_ciudadanas",
                  "Ciencias Naturales" = "punt_c_naturales")

# 2) UI
# ==============================================================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$a(href = 'https://www.icfes.gov.co', target = '_blank',
                   tags$img(src = 'icfes_logo.svg', class = 'logo-image'),
                   'Portal de Resultados')
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem("Visión General", tabName = "vision", icon = icon("tachometer-alt")),
      menuItem("Análisis Profundo", tabName = "analisis", icon = icon("chart-line")),
      menuItem("Simulador de Puntaje", tabName = "simulador", icon = icon("calculator")),
      menuItem("Datos y Definiciones", tabName = "diccionario", icon = icon("book")),
      br(),
      # --- Filtros con nombres reales ---
      menuItem(text = strong("Filtros Globales"), icon = icon("filter"), startExpanded = TRUE,
               selectInput("periodo", "Periodo:",
                           choices = sort(unique(datos_saber$periodo)), selected = max(datos_saber$periodo)),
               checkboxGroupInput("estu_genero", "Género:",
                                  choices = sort(unique(na.omit(datos_saber$estu_genero))),
                                  selected = intersect(c("F","M"), unique(datos_saber$estu_genero))),
               checkboxGroupInput("cole_jornada", "Jornada:",
                                  choices = sort(unique(na.omit(datos_saber$cole_jornada))),
                                  selected = sort(unique(na.omit(datos_saber$cole_jornada)))),
               checkboxGroupInput("cole_naturaleza", "Carácter del colegio:",
                                  choices = sort(unique(na.omit(datos_saber$cole_naturaleza))),
                                  selected = sort(unique(na.omit(datos_saber$cole_naturaleza)))),
               selectInput("fami_estratovivienda", "Estrato vivienda:",
                           choices = c("Todos", sort(unique(na.omit(datos_saber$fami_estratovivienda)))),
                           selected = "Todos"),
               selectInput("area", "Área de interés:", choices = area_choices, selected = "Todas"),
               selectInput("comparador", "Comparar contra:",
                           choices = c("Nacional","Departamental","Municipal","Grupo de pares"),
                           selected = "Departamental")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@400;700&family=Teko:wght@400;600&display=swap"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    tabItems(
      # ---- Visión General
      tabItem(tabName = "vision",
        fluidRow(
          valueBoxOutput("vb_global_prom", width = 4),  # KPI 1
          valueBoxOutput("vb_pct_altos", width = 4),    # KPI 2
          valueBoxOutput("vb_brecha_area", width = 4)   # KPI 3
        ),
        fluidRow(
          box(title = "Contexto y filtros aplicados", status = "primary", solidHeader = TRUE, width = 12,
              p("Periodo: ", strong(textOutput("txt_periodo", inline = TRUE)),
                " · Género: ", strong(textOutput("txt_genero", inline = TRUE)),
                " · Jornada: ", strong(textOutput("txt_jornada", inline = TRUE)),
                " · Naturaleza: ", strong(textOutput("txt_naturaleza", inline = TRUE)),
                " · Estrato: ", strong(textOutput("txt_estrato", inline = TRUE))),
              p("Comparador elegido: ", strong(textOutput("txt_comp_scope", inline = TRUE)),
                ". Use ", strong("Análisis Profundo"), " para explorar distribución, brechas y tendencias.")
          )
        )
      ),

      # ---- Análisis Profundo
      tabItem(tabName = "analisis",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = "Exploración detallada",
              tabBox(width = 12, id = "tabbox_analitico",
                tabPanel(title = "Niveles de desempeño", icon = icon("layer-group"),
                  p("Distribución de niveles (1–4) global y por género para las áreas Lectura, Matemáticas, Sociales y Naturales."),
                  plotlyOutput("plot_niveles_global", height = "420px"),
                  br(),
                  plotlyOutput("plot_niveles_genero", height = "420px")
                ),
                tabPanel(title = "Brechas por área (F vs M)", icon = icon("columns"),
                  p("Diferencia absoluta de medias de puntaje por área entre F y M (solo si ambos géneros están presentes)."),
                  plotlyOutput("plot_brechas_area", height = "420px"),
                  br(),
                  plotlyOutput("plot_box_areas", height = "420px")
                ),
                tabPanel(title = "Tendencias por periodo", icon = icon("chart-line"),
                  p("Evolución temporal del puntaje global y por área respetando los demás filtros (género/jornada/naturaleza/estrato)."),
                  plotlyOutput("plot_tendencia_global", height = "420px"),
                  br(),
                  plotlyOutput("plot_tendencia_areas", height = "420px")
                ),
                tabPanel(title = "Mapa de calor", icon = icon("fire"),
                  p("Promedios por área × género (solo áreas académicas)."),
                  plotlyOutput("plot_heatmap", height = "560px")
                )
              )
          )
        )
      ),

      # ---- Simulador (se mantiene, sin tocar variables del dataset)
      tabItem(tabName = "simulador",
        fluidRow(
          box(title = "Ingrese los Puntajes", status = "primary", solidHeader = TRUE, width = 4,
              numericInput("sim_lectura", "Puntaje Lectura Crítica:", 70, min = 0, max = 100),
              numericInput("sim_sociales", "Puntaje Sociales:", 70, min = 0, max = 100),
              numericInput("sim_ciencias", "Puntaje Ciencias Naturales:", 70, min = 0, max = 100),
              actionButton("run_prediction", "Predecir Puntaje de Matemáticas", icon = icon("cogs"), width = "100%")
          ),
          box(title = "Resultado de la Predicción", status = "primary", solidHeader = TRUE, width = 8,
              div(class = "prediction-output",
                  h3("El puntaje predicho en matemáticas es:"),
                  h1(textOutput("prediction_result"))
              )
          )
        )
      ),

      # ---- Diccionario
      tabItem(tabName = "diccionario",
        fluidRow(
          box(title = "Definiciones clave", width = 12, status = "info", solidHeader = TRUE,
              h4("KPIs"),
              tags$ul(
                tags$li(strong("Puntaje global promedio (punt_global):"),
                        " promedio simple del puntaje global de los estudiantes filtrados."),
                tags$li(strong("% con ≥3 áreas en niveles 3–4:"),
                        " porcentaje de estudiantes con al menos tres áreas académicas en niveles 3 o 4, usando ",
                        code(paste(areas_desemp, collapse = ", ")), "."),
                tags$li(strong("Mayor brecha por área (F vs M):"),
                        " diferencia absoluta de medias entre F y M en ", code(paste(areas_punt, collapse = ", ")), ".")
              ),
              h4("Filtros"),
              p("Todos los outputs respetan: ",
                code("periodo, estu_genero, cole_jornada, cole_naturaleza, fami_estratovivienda"), ".")
          )
        )
      )
    )
  )
)

# 3) SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ---- Filtro base con variables reales
  datos_filtrados <- reactive({
    df <- datos_saber %>%
      filter(periodo == input$periodo) %>%
      filter(estu_genero %in% input$estu_genero) %>%
      filter(cole_jornada %in% input$cole_jornada) %>%
      filter(cole_naturaleza %in% input$cole_naturaleza)
    if (input$fami_estratovivienda != "Todos") {
      df <- df %>% filter(fami_estratovivienda == input$fami_estratovivienda)
    }
    df
  })

  # ---- KPI 1: Puntaje Global Promedio
  output$vb_global_prom <- renderValueBox({
    df <- datos_filtrados()
    prom <- if (nrow(df)) round(mean(df$punt_global, na.rm = TRUE), 1) else NA
    valueBox(value = ifelse(is.na(prom), "—", prom),
             subtitle = "Puntaje Global Promedio",
             icon = icon("graduation-cap"),
             color = "yellow")
  })

  # ---- KPI 2: % de estudiantes con ≥3 áreas en niveles 3–4 (sin inglés)
  output$vb_pct_altos <- renderValueBox({
    df <- datos_filtrados()
    if (!nrow(df)) {
      return(valueBox("—", "% con ≥3 áreas en niveles 3–4", icon = icon("chart-bar"), color = "yellow"))
    }
    # Aseguramos que existan columnas desemp_*
    if (!all(areas_desemp %in% names(df))) {
      return(valueBox("—", "% con ≥3 áreas en niveles 3–4", icon = icon("chart-bar"), color = "yellow"))
    }
    almenos3_altos <- df %>%
      transmute(n_altas = (desemp_matematicas >= 3) +
                          (desemp_lectura_critica >= 3) +
                          (desemp_sociales_ciudadanas >= 3) +
                          (desemp_c_naturales >= 3)) %>%
      summarise(pct = mean(n_altas >= 3) * 100) %>% pull(pct)
    val <- round(almenos3_altos, 1)
    valueBox(value = paste0(val, "%"),
             subtitle = "% con ≥3 áreas en niveles 3–4",
             icon = icon("chart-bar"),
             color = "yellow")
  })

  # ---- KPI 3: Mayor brecha por área (F vs M) en puntajes
  output$vb_brecha_area <- renderValueBox({
    df <- datos_filtrados() %>% filter(estu_genero %in% c("F","M"))
    if (nrow(df) < 2 || dplyr::n_distinct(df$estu_genero) < 2 || !all(areas_punt %in% names(df))) {
      return(valueBox("—", "Mayor brecha por área (F vs M)", icon = icon("balance-scale"), color = "yellow"))
    }
    br <- sapply(areas_punt, function(a) {
      by_gender <- df %>% group_by(estu_genero) %>%
        summarise(mu = mean(.data[[a]], na.rm = TRUE), .groups = "drop")
      if (nrow(by_gender) < 2) return(NA_real_)
      abs(diff(by_gender$mu))
    })
    area_max <- names(which.max(br))
    delta <- round(max(br, na.rm = TRUE), 1)
    valueBox(value = paste0(to_title(area_max), ": Δ ", delta, " pts"),
             subtitle = "Mayor brecha por área (F vs M)",
             icon = icon("balance-scale"),
             color = "yellow")
  })

  # ---- Textos de contexto
  output$txt_periodo   <- renderText({ input$periodo })
  output$txt_genero    <- renderText({ paste(input$estu_genero, collapse = ", ") })
  output$txt_jornada   <- renderText({ paste(input$cole_jornada, collapse = ", ") })
  output$txt_naturaleza<- renderText({ paste(input$cole_naturaleza, collapse = ", ") })
  output$txt_estrato   <- renderText({ input$fami_estratovivienda })
  output$txt_comp_scope<- renderText({ input$comparador })

  # ==========================
  #       ANÁLISIS PROFUNDO
  # ==========================

  # ---- Niveles globales (conteo % de niveles 1–4 por área, agregados)
  output$plot_niveles_global <- renderPlotly({
    df <- datos_filtrados()
    if (!nrow(df) || !all(areas_desemp %in% names(df))) return(NULL)
    long <- df %>%
      pivot_longer(all_of(areas_desemp), names_to = "area", values_to = "nivel") %>%
      filter(!is.na(nivel)) %>%
      mutate(area = factor(area,
                           levels = areas_desemp,
                           labels = c("MATEMÁTICAS","LECTURA CRÍTICA","SOCIALES Y CIUDADANAS","CIENCIAS NATURALES")))
    dist <- long %>%
      count(area, nivel) %>%
      group_by(area) %>% mutate(pct = 100*n/sum(n)) %>% ungroup()
    p <- ggplot(dist, aes(x = factor(nivel), y = pct, fill = area)) +
      geom_col() +
      facet_wrap(~area, ncol = 2) +
      labs(x = "Nivel (1–4)", y = "Porcentaje", title = "Distribución de niveles por área", fill = "Área") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  # ---- Niveles por género
  output$plot_niveles_genero <- renderPlotly({
    df <- datos_filtrados()
    if (!nrow(df) || !all(areas_desemp %in% names(df))) return(NULL)
    long <- df %>%
      pivot_longer(all_of(areas_desemp), names_to = "area", values_to = "nivel") %>%
      filter(!is.na(nivel)) %>%
      mutate(area = factor(area,
                           levels = areas_desemp,
                           labels = c("MATEMÁTICAS","LECTURA CRÍTICA","SOCIALES Y CIUDADANAS","CIENCIAS NATURALES")))
    dist <- long %>%
      count(estu_genero, area, nivel) %>%
      group_by(estu_genero, area) %>% mutate(pct = 100*n/sum(n)) %>% ungroup()
    p <- ggplot(dist, aes(x = factor(nivel), y = pct, fill = estu_genero)) +
      geom_col(position = "dodge") +
      facet_wrap(~area, ncol = 2) +
      labs(x = "Nivel (1–4)", y = "Porcentaje", title = "Niveles por género y área", fill = "Género") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  # ---- Brechas por área (F vs M) – barras + boxplots
  output$plot_brechas_area <- renderPlotly({
    df <- datos_filtrados() %>% filter(estu_genero %in% c("F","M"))
    if (!nrow(df) || dplyr::n_distinct(df$estu_genero) < 2 || !all(areas_punt %in% names(df))) return(NULL)
    br <- df %>%
      pivot_longer(all_of(areas_punt), names_to = "area", values_to = "punt") %>%
      group_by(area, estu_genero) %>% summarise(mu = mean(punt, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = estu_genero, values_from = mu) %>%
      mutate(delta = abs(`F` - `M`),
             area = factor(area, levels = areas_punt, labels = to_title(areas_punt)))
    p <- ggplot(br, aes(x = area, y = delta)) +
      geom_col() +
      geom_text(aes(label = round(delta,1)), vjust = -0.3) +
      labs(x = NULL, y = "Δ puntos (|F - M|)", title = "Brecha por área (F vs M)") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  output$plot_box_areas <- renderPlotly({
    df <- datos_filtrados()
    if (!nrow(df) || !all(areas_punt %in% names(df))) return(NULL)
    long <- df %>%
      pivot_longer(all_of(areas_punt), names_to = "area", values_to = "punt") %>%
      mutate(area = factor(area, levels = areas_punt, labels = to_title(areas_punt)))
    p <- ggplot(long, aes(x = area, y = punt)) +
      geom_boxplot(outlier.alpha = 0.25) +
      labs(x = NULL, y = "Puntaje (0–100)", title = "Dispersión de puntajes por área") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  # ---- Tendencias por periodo (global y por área), respetando subfiltros
  output$plot_tendencia_global <- renderPlotly({
    df <- datos_saber %>%
      filter(estu_genero %in% input$estu_genero,
             cole_jornada %in% input$cole_jornada,
             cole_naturaleza %in% input$cole_naturaleza) %>%
      { if (input$fami_estratovivienda != "Todos") filter(., fami_estratovivienda == input$fami_estratovivienda) else . } %>%
      group_by(periodo) %>% summarise(prom = mean(punt_global, na.rm = TRUE), .groups = "drop") %>%
      arrange(periodo)
    if (!nrow(df)) return(NULL)
    p <- ggplot(df, aes(x = periodo, y = prom)) +
      geom_line() + geom_point() +
      labs(x = "Periodo", y = "Puntaje global promedio", title = "Tendencia del puntaje global") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  output$plot_tendencia_areas <- renderPlotly({
    df <- datos_saber %>%
      filter(estu_genero %in% input$estu_genero,
             cole_jornada %in% input$cole_jornada,
             cole_naturaleza %in% input$cole_naturaleza) %>%
      { if (input$fami_estratovivienda != "Todos") filter(., fami_estratovivienda == input$fami_estratovivienda) else . } %>%
      group_by(periodo) %>%
      summarise(
        MATEMATICAS = mean(punt_matematicas, na.rm = TRUE),
        LECTURA_CRITICA = mean(punt_lectura_critica, na.rm = TRUE),
        SOCIALES_CIUDADANAS = mean(punt_sociales_ciudadanas, na.rm = TRUE),
        CIENCIAS_NATURALES = mean(punt_c_naturales, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(-periodo, names_to = "area", values_to = "prom") %>%
      arrange(periodo)
    if (!nrow(df)) return(NULL)
    p <- ggplot(df, aes(x = periodo, y = prom, group = area)) +
      geom_line() + geom_point() +
      facet_wrap(~area, ncol = 2, scales = "free_y") +
      labs(x = "Periodo", y = "Puntaje promedio", title = "Tendencias por área") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  # ---- Heatmap por área × género (promedios)
  output$plot_heatmap <- renderPlotly({
    df <- datos_filtrados()
    if (!nrow(df) || !all(areas_punt %in% names(df))) return(NULL)
    hm <- df %>%
      pivot_longer(all_of(areas_punt), names_to = "area", values_to = "punt") %>%
      group_by(estu_genero, area) %>% summarise(media = mean(punt, na.rm = TRUE), .groups = "drop") %>%
      mutate(area = factor(area, levels = areas_punt, labels = to_title(areas_punt)))
    p <- ggplot(hm, aes(x = area, y = estu_genero, fill = media)) +
      geom_tile() +
      geom_text(aes(label = round(media,1)), color = "white") +
      labs(x = "Área", y = "Género", fill = "Promedio", title = "Mapa de calor por áreas × género") +
      theme_minimal(base_family = "Nunito Sans")
    ggplotly(p)
  })

  # ==========================
  #         SIMULADOR
  # ==========================
  prediction_logic <- eventReactive(input$run_prediction, {
    # Demo lineal; sustituye por tu modelo si lo integras
    pred <- 50 + (input$sim_lectura - 50)*0.4 + (input$sim_sociales - 50)*0.3 +
      (input$sim_ciencias - 50)*0.3 + rnorm(1, 0, 4)
    pred <- max(0, min(100, pred))
    round(pred, 1)
  })
  output$prediction_result <- renderText({ prediction_logic() })
}

# 4) EJECUCIÓN
# ==============================================================================
shinyApp(ui, server)
