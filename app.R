# ============================================================
# BLOQUE 1: LIBRERÍAS, DATOS BASE Y CONFIGURACIÓN GENERAL
# ============================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(scales)
library(plotly)
library(ggplot2)
library(DT)
library(leaflet)
library(sf)
library(WDI)
library(gapminder)
library(countrycode)
library(bslib)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

limpiar_texto <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_title()
}

asignar_subregion <- function(continent, country) {
  
  iso <- countrycode(
    country,
    origin = "country.name",
    destination = "iso3c",
    custom_match = c(
      "Micronesia" = "FSM",
      "Netherlands Antilles" = "ANT"
    )
  )
  
  case_when(
    continent == "Africa" ~ "África",
    continent == "Europe" ~ "Europa",
    continent == "Oceania" ~ "Oceanía",
    
    continent == "Americas" & iso %in% c("CAN", "USA", "MEX") ~ "América del Norte",
    
    continent == "Americas" & iso %in% c(
      "ARG", "BOL", "BRA", "CHL", "COL", "ECU",
      "GUY", "PRY", "PER", "SUR", "URY", "VEN"
    ) ~ "América del Sur",
    
    continent == "Americas" & iso %in% c(
      "BLZ", "CRI", "SLV", "GTM", "HND", "NIC", "PAN",
      "CUB", "HTI", "JAM", "DOM",
      "ATG", "BHS", "BRB", "DMA", "GRD", "KNA",
      "VCT", "LCA", "TTO",
      "BMU", "VGB", "CYM", "CUW", "PRI", "SXM",
      "MAF", "TCA", "VIR", "ABW"
    ) ~ "Centroamérica",
    
    continent == "Americas" ~ "Centroamérica",
    continent == "Asia" ~ "Asia",
    TRUE ~ "Otros"
  )
}

homologar_continente <- function(continent) {
  case_when(
    continent == "Africa" ~ "África",
    continent == "Americas" ~ "América",
    continent == "Asia" ~ "Asia",
    continent == "Europe" ~ "Europa",
    continent == "Oceania" ~ "Oceanía",
    TRUE ~ "Otros"
  )
}

datos_gapminder <- gapminder %>%
  mutate(
    fuente = "Gapminder",
    pais = as.character(country),
    continente_original = as.character(continent),
    continente = homologar_continente(continente_original),
    subregion = asignar_subregion(continente_original, pais),
    anio = year,
    esperanza_vida = lifeExp,
    pib_percapita = gdpPercap,
    poblacion = pop,
    iso3c = countrycode(
      pais,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c(
        "Micronesia" = "FSM",
        "Netherlands Antilles" = "ANT"
      )
    )
  ) %>%
  select(
    fuente, pais, iso3c, continente, subregion, anio,
    esperanza_vida, pib_percapita, poblacion
  ) %>%
  filter(!is.na(pais), !is.na(continente), continente != "Otros")

indicadores_wdi <- c(
  esperanza_vida = "SP.DYN.LE00.IN",
  pib_percapita = "NY.GDP.PCAP.CD",
  poblacion = "SP.POP.TOTL"
)

datos_banco_mundial <- WDI(
  country = "all",
  indicator = indicadores_wdi,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  as_tibble() %>%
  filter(region != "Aggregates") %>%
  mutate(
    fuente = "Banco Mundial",
    pais = country,
    continente = case_when(
      region == "Sub-Saharan Africa" ~ "África",
      region == "Middle East & North Africa" ~ "África",
      region == "Europe & Central Asia" ~ "Europa",
      region == "Latin America & Caribbean" ~ "América",
      region == "North America" ~ "América",
      region == "East Asia & Pacific" ~ "Asia",
      region == "South Asia" ~ "Asia",
      TRUE ~ "Otros"
    ),
    subregion = case_when(
      continente == "América" & iso3c %in% c("CAN", "USA", "MEX") ~ "América del Norte",
      
      continente == "América" & iso3c %in% c(
        "ARG", "BOL", "BRA", "CHL", "COL", "ECU",
        "GUY", "PRY", "PER", "SUR", "URY", "VEN"
      ) ~ "América del Sur",
      
      continente == "América" & iso3c %in% c(
        "BLZ", "CRI", "SLV", "GTM", "HND", "NIC", "PAN",
        "CUB", "HTI", "JAM", "DOM",
        "ATG", "BHS", "BRB", "DMA", "GRD", "KNA",
        "VCT", "LCA", "TTO",
        "BMU", "VGB", "CYM", "CUW", "PRI", "SXM",
        "MAF", "TCA", "VIR", "ABW"
      ) ~ "Centroamérica",
      
      continente == "África" ~ "África",
      continente == "Europa" ~ "Europa",
      continente == "Asia" ~ "Asia",
      TRUE ~ "Otros"
    ),
    anio = year
  ) %>%
  select(
    fuente, pais, iso3c, continente, subregion, anio,
    esperanza_vida, pib_percapita, poblacion
  ) %>%
  filter(!is.na(pais), !is.na(continente), continente != "Otros")

mapa_mundial_sf <- ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  mutate(
    iso3c = iso_a3
  )

etiqueta_x <- function() "PIB per cápita (US$)"
etiqueta_y <- function() "Esperanza de vida al nacer (años)"
etiqueta_tamano <- function() "Población total"

paleta_subregion <- c(
  "África" = "#FF6B6B",
  "América del Norte" = "#C49A00",
  "Centroamérica" = "#4ECDC4",
  "América del Sur" = "#2EC4B6",
  "Asia" = "#00A896",
  "Europa" = "#7B61FF",
  "Oceanía" = "#FF85C1"
)

# ============================================================
# BLOQUE 2: INTERFAZ DE USUARIO
# ============================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f6f9;
        font-family: 'Segoe UI', sans-serif;
      }

      .header-box {
        background: linear-gradient(90deg, #003b70, #005ea8);
        color: white;
        padding: 16px 22px;
        border-radius: 0 0 8px 8px;
        margin-bottom: 14px;
      }

      .header-title {
        font-size: 24px;
        font-weight: 700;
        margin-bottom: 4px;
      }

      .header-subtitle {
        font-size: 13px;
        opacity: 0.9;
      }

      .sidebar-box {
        background: white;
        border-radius: 10px;
        padding: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 15px;
      }

      .kpi-title {
        font-weight: 700;
        color: #003b70;
        margin-top: 10px;
        margin-bottom: 10px;
      }

      .kpi-card {
        background: white;
        border-radius: 10px;
        padding: 14px;
        margin-bottom: 12px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border-top: 4px solid #005ea8;
      }

      .kpi-card.blue { border-top-color: #005ea8; }
      .kpi-card.green { border-top-color: #00a896; }
      .kpi-card.orange { border-top-color: #ff7f11; }
      .kpi-card.purple { border-top-color: #7b61ff; }

      .kpi-value {
        font-size: 24px;
        font-weight: 800;
        color: #003b70;
      }

      .kpi-label {
        font-size: 12px;
        color: #6c757d;
      }

      .main-box {
        background: white;
        border-radius: 10px;
        padding: 15px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-bottom: 15px;
      }

      .nav-tabs > li > a {
        font-size: 13px;
        font-weight: 600;
      }

      .btn-download {
        width: 100%;
        background-color: #006fd6;
        color: white;
        font-weight: 700;
        border-radius: 6px;
        margin-top: 10px;
      }

      .btn-download:hover {
        background-color: #0055a4;
        color: white;
      }
    "))
  ),
  
  div(
    class = "header-box",
    div(class = "header-title", "🌍 Gapminder + Banco Mundial — Desarrollo Mundial Interactivo"),
    div(
      class = "header-subtitle",
      "Dashboard interactivo: Gapminder 1952–2007 y Banco Mundial 2000–2023"
    )
  ),
  
  fluidRow(
    
    column(
      width = 3,
      
      div(
        class = "sidebar-box",
        
        h4("🔍 Filtros"),
        
        radioButtons(
          inputId = "fuente",
          label = "Fuente de datos:",
          choices = c(
            "Gapminder (1952–2007)" = "Gapminder",
            "Banco Mundial WDI (2000–2023)" = "Banco Mundial"
          ),
          selected = "Gapminder"
        ),
        
        selectInput("continente", "Continente:", choices = "Todos", selected = "Todos"),
        selectInput("subregion", "Subregión:", choices = "Todos", selected = "Todos"),
        selectInput("pais", "País:", choices = "Todos", selected = "Todos"),
        
        sliderInput(
          inputId = "anio",
          label = "Año:",
          min = 1952,
          max = 2007,
          value = 2007,
          step = 1,
          sep = "",
          ticks = FALSE,
          animate = animationOptions(interval = 1200, loop = TRUE)
        ),
        
        tags$div(
          style = "
            display:flex;
            justify-content:space-between;
            font-size:11px;
            color:#6c757d;
            margin-top:-8px;
            margin-bottom:10px;
          ",
          span("1952"),
          span("2000"),
          span("2007"),
          span("2023")
        ),
        
        h4(class = "kpi-title", "📊 KPIs"),
        
        div(class = "kpi-card blue",
            div(class = "kpi-value", textOutput("kpi_vida")),
            div(class = "kpi-label", "Esperanza de vida")
        ),
        
        div(class = "kpi-card green",
            div(class = "kpi-value", textOutput("kpi_pib")),
            div(class = "kpi-label", "PIB per cápita")
        ),
        
        div(class = "kpi-card orange",
            div(class = "kpi-value", textOutput("kpi_poblacion")),
            div(class = "kpi-label", "Población")
        ),
        
        div(class = "kpi-card purple",
            div(class = "kpi-value", textOutput("kpi_territorio")),
            div(class = "kpi-label", "Ámbito filtrado")
        ),
        
        downloadButton(
          outputId = "descargar_csv",
          label = "📥 Exportar CSV",
          class = "btn-download"
        )
      )
    ),
    
    column(
      width = 9,
      
      div(
        class = "main-box",
        
        tabsetPanel(
          id = "tabs",
          
          tabPanel("🏠 Inicio", br(), plotlyOutput("grafico_inicio", height = "420px")),
          tabPanel("🗺️ Mapa Mundial", br(), leafletOutput("mapa_mundial", height = "520px")),
          tabPanel("📊 Histórico 1952–2023", br(), plotlyOutput("grafico_historico", height = "460px")),
          tabPanel("📈 Tendencias", br(), plotlyOutput("grafico_tendencias", height = "460px")),
          tabPanel("💵 Macro WDI", br(), plotlyOutput("grafico_macro", height = "460px")),
          tabPanel("🔄 Comparar", br(), plotlyOutput("grafico_comparar", height = "460px")),
          tabPanel("🏆 Rankings", br(), DTOutput("tabla_rankings")),
          tabPanel("🌐 Subregiones", br(), plotlyOutput("grafico_subregiones", height = "460px")),
          tabPanel("🌎 Continentes", br(), plotlyOutput("grafico_continentes", height = "460px")),
          tabPanel("🤖 Modelo", br(), plotlyOutput("grafico_modelo", height = "460px")),
          tabPanel("💡 Insights", br(), uiOutput("insights")),
          tabPanel("📋 Datos", br(), DTOutput("tabla_datos"))
        )
      )
    )
  )
)


# ============================================================
# MÓDULO 3: SERVER BASE, FILTROS Y DATOS REACTIVOS
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 1. Fuente de datos activa
  # ----------------------------------------------------------
  
  datos_act <- reactive({
    
    if (input$fuente == "Gapminder") {
      datos_gapminder
    } else {
      datos_banco_mundial
    }
    
  })
  
  # ----------------------------------------------------------
  # 2. Actualizar rango de años y filtros según fuente
  # ----------------------------------------------------------
  
  observeEvent(input$fuente, {
    
    datos <- datos_act()
    
    if (input$fuente == "Gapminder") {
      
      updateSliderInput(
        session,
        inputId = "anio",
        min = 1952,
        max = 2007,
        value = 2007,
        step = 5
      )
      
    } else {
      
      updateSliderInput(
        session,
        inputId = "anio",
        min = 2000,
        max = 2023,
        value = 2023,
        step = 1
      )
      
    }
    
    continentes <- datos %>%
      filter(!is.na(continente)) %>%
      distinct(continente) %>%
      arrange(continente) %>%
      pull(continente)
    
    updateSelectInput(
      session,
      inputId = "continente",
      choices = c("Todos", continentes),
      selected = "Todos"
    )
    
    subregiones <- datos %>%
      filter(!is.na(subregion)) %>%
      distinct(subregion) %>%
      arrange(subregion) %>%
      pull(subregion)
    
    updateSelectInput(
      session,
      inputId = "subregion",
      choices = c("Todos", subregiones),
      selected = "Todos"
    )
    
    paises <- datos %>%
      filter(!is.na(pais)) %>%
      distinct(pais) %>%
      arrange(pais) %>%
      pull(pais)
    
    updateSelectInput(
      session,
      inputId = "pais",
      choices = c("Todos", paises),
      selected = "Todos"
    )
    
  }, ignoreInit = FALSE)
  
  # ----------------------------------------------------------
  # 3. Actualizar subregiones y países según continente
  # ----------------------------------------------------------
  
  observeEvent(input$continente, {
    
    datos <- datos_act()
    
    if (!is.null(input$continente) && input$continente != "Todos") {
      datos <- datos %>%
        filter(continente == input$continente)
    }
    
    subregiones <- datos %>%
      filter(!is.na(subregion)) %>%
      distinct(subregion) %>%
      arrange(subregion) %>%
      pull(subregion)
    
    updateSelectInput(
      session,
      inputId = "subregion",
      choices = c("Todos", subregiones),
      selected = "Todos"
    )
    
    paises <- datos %>%
      filter(!is.na(pais)) %>%
      distinct(pais) %>%
      arrange(pais) %>%
      pull(pais)
    
    updateSelectInput(
      session,
      inputId = "pais",
      choices = c("Todos", paises),
      selected = "Todos"
    )
    
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------
  # 4. Actualizar países según continente y subregión
  # ----------------------------------------------------------
  
  observeEvent(input$subregion, {
    
    datos <- datos_act()
    
    if (!is.null(input$continente) && input$continente != "Todos") {
      datos <- datos %>%
        filter(continente == input$continente)
    }
    
    if (!is.null(input$subregion) && input$subregion != "Todos") {
      datos <- datos %>%
        filter(subregion == input$subregion)
    }
    
    paises <- datos %>%
      filter(!is.na(pais)) %>%
      distinct(pais) %>%
      arrange(pais) %>%
      pull(pais)
    
    updateSelectInput(
      session,
      inputId = "pais",
      choices = c("Todos", paises),
      selected = "Todos"
    )
    
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------
  # 5. Datos filtrados por año, continente, subregión y país
  # ----------------------------------------------------------
  
  datos_filtrados <- reactive({
    
    req(input$anio)
    
    datos <- datos_act() %>%
      filter(anio == input$anio)
    
    if (!is.null(input$continente) && input$continente != "Todos") {
      datos <- datos %>%
        filter(continente == input$continente)
    }
    
    if (!is.null(input$subregion) && input$subregion != "Todos") {
      datos <- datos %>%
        filter(subregion == input$subregion)
    }
    
    if (!is.null(input$pais) && input$pais != "Todos") {
      datos <- datos %>%
        filter(pais == input$pais)
    }
    
    datos
    
  })
  
  # ----------------------------------------------------------
  # 6. Datos históricos filtrados sin restringir a un solo año
  # ----------------------------------------------------------
  
  datos_historicos_filtrados <- reactive({
    
    datos <- datos_act()
    
    if (!is.null(input$continente) && input$continente != "Todos") {
      datos <- datos %>%
        filter(continente == input$continente)
    }
    
    if (!is.null(input$subregion) && input$subregion != "Todos") {
      datos <- datos %>%
        filter(subregion == input$subregion)
    }
    
    if (!is.null(input$pais) && input$pais != "Todos") {
      datos <- datos %>%
        filter(pais == input$pais)
    }
    
    datos
    
  })
  
  # ==========================================================
  # MÓDULO 4: KPIs, EXPORTACIÓN CSV Y CONFIGURACIÓN PLOTLY
  # ==========================================================
  
  # ----------------------------------------------------------
  # 1. KPIs dinámicos según filtros
  # ----------------------------------------------------------
  
  output$kpi_vida <- renderText({
    
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0 || all(is.na(datos$esperanza_vida))) {
      return("Sin datos")
    }
    
    paste0(
      round(mean(datos$esperanza_vida, na.rm = TRUE), 1),
      " años"
    )
    
  })
  
  output$kpi_pib <- renderText({
    
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0 || all(is.na(datos$pib_percapita))) {
      return("Sin datos")
    }
    
    paste0(
      "US$ ",
      comma(round(mean(datos$pib_percapita, na.rm = TRUE), 0))
    )
    
  })
  
  output$kpi_poblacion <- renderText({
    
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0 || all(is.na(datos$poblacion))) {
      return("Sin datos")
    }
    
    poblacion_total <- sum(datos$poblacion, na.rm = TRUE)
    
    if (poblacion_total >= 1e9) {
      
      paste0(
        round(poblacion_total / 1e9, 2),
        " mil M"
      )
      
    } else if (poblacion_total >= 1e6) {
      
      paste0(
        round(poblacion_total / 1e6, 1),
        " M"
      )
      
    } else {
      
      comma(round(poblacion_total, 0))
      
    }
    
  })
  
  output$kpi_territorio <- renderText({
    
    if (!is.null(input$pais) && input$pais != "Todos") {
      
      input$pais
      
    } else if (!is.null(input$subregion) && input$subregion != "Todos") {
      
      input$subregion
      
    } else if (!is.null(input$continente) && input$continente != "Todos") {
      
      input$continente
      
    } else {
      
      "Todos"
      
    }
    
  })
  
  # ----------------------------------------------------------
  # 2. Exportar CSV según filtros activos
  # ----------------------------------------------------------
  
  output$descargar_csv <- downloadHandler(
    
    filename = function() {
      
      fuente_limpia <- input$fuente %>%
        str_replace_all(" ", "_") %>%
        str_replace_all("[^A-Za-z0-9_]", "")
      
      ambito <- if (!is.null(input$pais) && input$pais != "Todos") {
        
        input$pais
        
      } else if (!is.null(input$subregion) && input$subregion != "Todos") {
        
        input$subregion
        
      } else if (!is.null(input$continente) && input$continente != "Todos") {
        
        input$continente
        
      } else {
        
        "Todos"
        
      }
      
      ambito_limpio <- ambito %>%
        str_replace_all(" ", "_") %>%
        str_replace_all("[^A-Za-z0-9_]", "")
      
      paste0(
        "datos_",
        fuente_limpia,
        "_",
        ambito_limpio,
        "_",
        input$anio,
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      datos_exportar <- datos_filtrados() %>%
        arrange(
          continente,
          subregion,
          pais
        )
      
      write.csv(
        datos_exportar,
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
      
    }
    
  )
  
  # ----------------------------------------------------------
  # 3. Configuración global Plotly
  # ----------------------------------------------------------
  
  config_plotly <- function(grafico,
                            nombre_archivo = "grafico_dashboard") {
    
    grafico %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToAdd = c("toImage"),
        toImageButtonOptions = list(
          format = "png",
          filename = nombre_archivo,
          height = 650,
          width = 1100,
          scale = 2
        )
      )
    
  }
  
  
  # ==========================================================
  # MÓDULO 5: INICIO, HISTÓRICO Y TENDENCIAS
  # ==========================================================
  
  # ----------------------------------------------------------
  # 1. Gráfico de inicio
  # ----------------------------------------------------------
  
  output$grafico_inicio <- renderPlotly({
    
    datos <- datos_filtrados() %>%
      filter(
        !is.na(pib_percapita),
        !is.na(esperanza_vida),
        !is.na(poblacion),
        pib_percapita > 0,
        poblacion > 0
      )
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos para los filtros seleccionados")
      )
    }
    
    grafico <- plot_ly(
      data = datos,
      x = ~pib_percapita,
      y = ~esperanza_vida,
      size = ~poblacion,
      color = ~subregion,
      colors = paleta_subregion,
      type = "scatter",
      mode = "markers",
      text = ~paste0(
        "<b>", pais, "</b>",
        "<br>Continente: ", continente,
        "<br>Subregión: ", subregion,
        "<br>Año: ", anio,
        "<br>Esperanza de vida: ", round(esperanza_vida, 1), " años",
        "<br>PIB per cápita: US$ ", comma(round(pib_percapita, 0)),
        "<br>Población: ", comma(round(poblacion, 0))
      ),
      hoverinfo = "text",
      marker = list(
        opacity = 0.75,
        line = list(width = 0.5, color = "#ffffff")
      )
    ) %>%
      layout(
        title = paste0("Desarrollo mundial en ", input$anio),
        xaxis = list(
          title = "PIB per cápita (US$)",
          type = "log"
        ),
        yaxis = list(
          title = "Esperanza de vida al nacer (años)"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.25
        ),
        margin = list(l = 70, r = 30, b = 100, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("inicio_", input$fuente, "_", input$anio)
    )
    
  })
  
  # ----------------------------------------------------------
  # 2. Gráfico histórico
  # ----------------------------------------------------------
  
  output$grafico_historico <- renderPlotly({
    
    datos <- datos_historicos_filtrados() %>%
      filter(
        !is.na(esperanza_vida),
        !is.na(pib_percapita)
      )
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos históricos para los filtros seleccionados")
      )
    }
    
    datos_resumen <- datos %>%
      group_by(anio) %>%
      summarise(
        esperanza_vida = mean(esperanza_vida, na.rm = TRUE),
        pib_percapita = mean(pib_percapita, na.rm = TRUE),
        poblacion = sum(poblacion, na.rm = TRUE),
        .groups = "drop"
      )
    
    grafico <- plot_ly() %>%
      add_lines(
        data = datos_resumen,
        x = ~anio,
        y = ~esperanza_vida,
        name = "Esperanza de vida",
        line = list(width = 3),
        text = ~paste0(
          "Año: ", anio,
          "<br>Esperanza de vida: ", round(esperanza_vida, 1), " años"
        ),
        hoverinfo = "text"
      ) %>%
      add_lines(
        data = datos_resumen,
        x = ~anio,
        y = ~pib_percapita,
        name = "PIB per cápita",
        yaxis = "y2",
        line = list(width = 3, dash = "dot"),
        text = ~paste0(
          "Año: ", anio,
          "<br>PIB per cápita: US$ ", comma(round(pib_percapita, 0))
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = paste0("Evolución histórica — ", input$fuente),
        xaxis = list(title = "Año"),
        yaxis = list(
          title = "Esperanza de vida al nacer (años)"
        ),
        yaxis2 = list(
          title = "PIB per cápita (US$)",
          overlaying = "y",
          side = "right"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.25
        ),
        margin = list(l = 70, r = 90, b = 100, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("historico_", input$fuente)
    )
    
  })
  
  # ----------------------------------------------------------
  # 3. Gráfico de tendencias
  # ----------------------------------------------------------
  
  output$grafico_tendencias <- renderPlotly({
    
    datos <- datos_historicos_filtrados() %>%
      filter(
        !is.na(esperanza_vida),
        !is.na(pib_percapita),
        !is.na(poblacion)
      )
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos de tendencia para los filtros seleccionados")
      )
    }
    
    datos_top <- datos %>%
      group_by(pais) %>%
      summarise(
        poblacion_max = max(poblacion, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(poblacion_max)) %>%
      slice_head(n = 10)
    
    datos_plot <- datos %>%
      semi_join(datos_top, by = "pais")
    
    grafico <- plot_ly(
      data = datos_plot,
      x = ~anio,
      y = ~esperanza_vida,
      color = ~pais,
      type = "scatter",
      mode = "lines+markers",
      text = ~paste0(
        "<b>", pais, "</b>",
        "<br>Año: ", anio,
        "<br>Esperanza de vida: ", round(esperanza_vida, 1), " años",
        "<br>PIB per cápita: US$ ", comma(round(pib_percapita, 0)),
        "<br>Población: ", comma(round(poblacion, 0))
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Tendencias de esperanza de vida por país",
        xaxis = list(title = "Año"),
        yaxis = list(title = "Esperanza de vida al nacer (años)"),
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1
        ),
        margin = list(l = 70, r = 150, b = 70, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("tendencias_", input$fuente)
    )
    
  })
  
  # ==========================================================
  # MÓDULO 6: MAPA MUNDIAL, MACRO WDI Y COMPARAR
  # ==========================================================
  
  # ----------------------------------------------------------
  # 1. Mapa mundial coroplético
  # ----------------------------------------------------------
  
  output$mapa_mundial <- renderLeaflet({
    
    datos <- datos_filtrados() %>%
      filter(!is.na(iso3c)) %>%
      select(
        iso3c,
        pais,
        continente,
        subregion,
        anio,
        esperanza_vida,
        pib_percapita,
        poblacion
      )
    
    datos_mapa <- mapa_mundial_sf %>%
      select(iso3c, geometry) %>%
      left_join(datos, by = "iso3c")
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = datos$esperanza_vida,
      na.color = "#d9d9d9"
    )
    
    leaflet(datos_mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(esperanza_vida),
        fillOpacity = 0.8,
        color = "#555555",
        weight = 0.5,
        smoothFactor = 0.2,
        popup = ~paste0(
          "<b>", ifelse(is.na(pais), "Sin datos", pais), "</b>",
          "<br>Continente: ", ifelse(is.na(continente), "Sin datos", continente),
          "<br>Subregión: ", ifelse(is.na(subregion), "Sin datos", subregion),
          "<br>Año: ", ifelse(is.na(anio), "Sin datos", anio),
          "<br>Esperanza de vida: ",
          ifelse(
            is.na(esperanza_vida),
            "Sin datos",
            paste0(round(esperanza_vida, 1), " años")
          ),
          "<br>PIB per cápita: ",
          ifelse(
            is.na(pib_percapita),
            "Sin datos",
            paste0("US$ ", comma(round(pib_percapita, 0)))
          ),
          "<br>Población: ",
          ifelse(
            is.na(poblacion),
            "Sin datos",
            comma(round(poblacion, 0))
          )
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = datos$esperanza_vida,
        title = "Esperanza de vida<br>(años)",
        opacity = 0.9
      )
    
  })
  
  # ----------------------------------------------------------
  # 2. Gráfico Macro WDI
  # ----------------------------------------------------------
  
  output$grafico_macro <- renderPlotly({
    
    datos <- datos_filtrados() %>%
      filter(!is.na(pib_percapita))
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos para los filtros seleccionados")
      )
    }
    
    datos_plot <- datos %>%
      arrange(desc(pib_percapita)) %>%
      slice_head(n = 20)
    
    grafico <- plot_ly(
      data = datos_plot,
      x = ~reorder(pais, pib_percapita),
      y = ~pib_percapita,
      type = "bar",
      text = ~paste0(
        "<b>", pais, "</b>",
        "<br>PIB per cápita: US$ ", comma(round(pib_percapita, 0)),
        "<br>Esperanza de vida: ", round(esperanza_vida, 1), " años",
        "<br>Población: ", comma(round(poblacion, 0))
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Top 20 países por PIB per cápita — ", input$fuente),
        xaxis = list(title = "País", tickangle = -45),
        yaxis = list(title = "PIB per cápita (US$)", tickprefix = "US$ "),
        margin = list(l = 70, r = 30, b = 150, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("macro_", input$fuente, "_", input$anio)
    )
    
  })
  
  # ----------------------------------------------------------
  # 3. Comparar países o territorios filtrados
  # ----------------------------------------------------------
  
  output$grafico_comparar <- renderPlotly({
    
    datos <- datos_filtrados() %>%
      filter(!is.na(esperanza_vida))
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos para los filtros seleccionados")
      )
    }
    
    datos_plot <- datos %>%
      arrange(desc(esperanza_vida)) %>%
      slice_head(n = 15)
    
    grafico <- plot_ly(
      data = datos_plot,
      x = ~reorder(pais, esperanza_vida),
      y = ~esperanza_vida,
      type = "bar",
      text = ~paste0(
        "<b>", pais, "</b>",
        "<br>Esperanza de vida: ", round(esperanza_vida, 1), " años",
        "<br>PIB per cápita: US$ ", comma(round(pib_percapita, 0)),
        "<br>Población: ", comma(round(poblacion, 0))
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Comparación por esperanza de vida — ", input$anio),
        xaxis = list(title = "País", tickangle = -45),
        yaxis = list(title = "Esperanza de vida al nacer (años)"),
        margin = list(l = 70, r = 30, b = 150, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("comparar_", input$fuente, "_", input$anio)
    )
    
  })
  
  
  # ==========================================================
  # MÓDULO 7: RANKINGS, SUBREGIONES Y CONTINENTES
  # ==========================================================
  
  # ----------------------------------------------------------
  # 1. Tabla de rankings
  # ----------------------------------------------------------
  
  output$tabla_rankings <- renderDT({
    
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      return(
        datatable(
          data.frame(Mensaje = "Sin datos para los filtros seleccionados"),
          rownames = FALSE
        )
      )
    }
    
    tabla <- datos %>%
      filter(!is.na(esperanza_vida)) %>%
      arrange(desc(esperanza_vida)) %>%
      mutate(
        Ranking = row_number(),
        `Esperanza de vida, años` = round(esperanza_vida, 1),
        `PIB per cápita, US$` = comma(round(pib_percapita, 0)),
        `Población, habitantes` = comma(round(poblacion, 0))
      ) %>%
      select(
        Ranking,
        País = pais,
        Continente = continente,
        Subregión = subregion,
        Año = anio,
        `Esperanza de vida, años`,
        `PIB per cápita, US$`,
        `Población, habitantes`
      )
    
    datatable(
      tabla,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
        )
      )
    )
    
  })
  
  # ----------------------------------------------------------
  # 2. Gráfico por subregiones
  # ----------------------------------------------------------
  
  output$grafico_subregiones <- renderPlotly({
    
    datos <- datos_filtrados() %>%
      filter(!is.na(esperanza_vida))
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos para los filtros seleccionados")
      )
    }
    
    if (!is.null(input$pais) && input$pais != "Todos") {
      
      datos_plot <- datos %>%
        group_by(pais) %>%
        summarise(
          esperanza_vida = mean(esperanza_vida, na.rm = TRUE),
          pib_percapita = mean(pib_percapita, na.rm = TRUE),
          poblacion = sum(poblacion, na.rm = TRUE),
          .groups = "drop"
        )
      
      eje_x <- "pais"
      titulo <- paste0("Indicadores del país seleccionado — ", input$pais)
      
    } else if (!is.null(input$subregion) && input$subregion != "Todos") {
      
      datos_plot <- datos %>%
        group_by(pais) %>%
        summarise(
          esperanza_vida = mean(esperanza_vida, na.rm = TRUE),
          pib_percapita = mean(pib_percapita, na.rm = TRUE),
          poblacion = sum(poblacion, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(esperanza_vida))
      
      eje_x <- "pais"
      titulo <- paste0("Países de la subregión: ", input$subregion)
      
    } else {
      
      datos_plot <- datos %>%
        group_by(subregion) %>%
        summarise(
          esperanza_vida = mean(esperanza_vida, na.rm = TRUE),
          pib_percapita = mean(pib_percapita, na.rm = TRUE),
          poblacion = sum(poblacion, na.rm = TRUE),
          paises = n_distinct(pais),
          .groups = "drop"
        ) %>%
        arrange(desc(esperanza_vida))
      
      eje_x <- "subregion"
      titulo <- "Indicadores por subregión"
      
    }
    
    grafico <- plot_ly(
      data = datos_plot,
      x = as.formula(paste0("~reorder(", eje_x, ", esperanza_vida)")),
      y = ~esperanza_vida,
      type = "bar",
      text = as.formula(
        paste0(
          "~paste0('<b>', ", eje_x, ", '</b>',
          '<br>Esperanza de vida: ', round(esperanza_vida, 1), ' años',
          '<br>PIB per cápita: US$ ', comma(round(pib_percapita, 0)),
          '<br>Población: ', comma(round(poblacion, 0)))"
        )
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0(titulo, " — ", input$anio),
        xaxis = list(
          title = ifelse(eje_x == "pais", "País", "Subregión"),
          tickangle = -45
        ),
        yaxis = list(
          title = "Esperanza de vida al nacer (años)"
        ),
        margin = list(l = 70, r = 30, b = 140, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("subregiones_", input$fuente, "_", input$anio)
    )
    
  })
  
  # ----------------------------------------------------------
  # 3. Gráfico por continentes
  # ----------------------------------------------------------
  
  output$grafico_continentes <- renderPlotly({
    
    datos <- datos_filtrados() %>%
      filter(!is.na(esperanza_vida))
    
    if (nrow(datos) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Sin datos para los filtros seleccionados")
      )
    }
    
    datos_plot <- datos %>%
      group_by(continente) %>%
      summarise(
        esperanza_vida = mean(esperanza_vida, na.rm = TRUE),
        pib_percapita = mean(pib_percapita, na.rm = TRUE),
        poblacion = sum(poblacion, na.rm = TRUE),
        subregiones = n_distinct(subregion),
        paises = n_distinct(pais),
        .groups = "drop"
      ) %>%
      arrange(desc(esperanza_vida))
    
    grafico <- plot_ly(
      data = datos_plot,
      x = ~reorder(continente, esperanza_vida),
      y = ~esperanza_vida,
      type = "bar",
      text = ~paste0(
        "<b>", continente, "</b>",
        "<br>Esperanza de vida promedio: ", round(esperanza_vida, 1), " años",
        "<br>PIB per cápita promedio: US$ ", comma(round(pib_percapita, 0)),
        "<br>Población total: ", comma(round(poblacion, 0)),
        "<br>Subregiones: ", subregiones,
        "<br>Países: ", paises
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0("Promedio de esperanza de vida por continente — ", input$anio),
        xaxis = list(title = "Continente"),
        yaxis = list(title = "Esperanza de vida al nacer (años)"),
        margin = list(l = 70, r = 30, b = 90, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("continentes_", input$fuente, "_", input$anio)
    )
    
  })
  
  # ==========================================================
  # MÓDULO 8: MODELO, INSIGHTS, TABLA DE DATOS Y CIERRE
  # ==========================================================
  
  # ----------------------------------------------------------
  # 1. Modelo exploratorio
  # ----------------------------------------------------------
  
  output$grafico_modelo <- renderPlotly({
    
    datos <- datos_filtrados() %>%
      filter(
        !is.na(pib_percapita),
        !is.na(esperanza_vida),
        pib_percapita > 0
      )
    
    if (nrow(datos) < 3) {
      return(
        plot_ly() %>%
          layout(title = "Datos insuficientes para generar el modelo")
      )
    }
    
    modelo <- lm(
      esperanza_vida ~ log(pib_percapita),
      data = datos
    )
    
    datos <- datos %>%
      mutate(
        tendencia = fitted(modelo)
      )
    
    grafico <- plot_ly(
      data = datos,
      x = ~pib_percapita,
      y = ~esperanza_vida,
      color = ~subregion,
      colors = paleta_subregion,
      type = "scatter",
      mode = "markers",
      text = ~paste0(
        "<b>", pais, "</b>",
        "<br>PIB per cápita: US$ ", comma(round(pib_percapita, 0)),
        "<br>Esperanza de vida: ", round(esperanza_vida, 1), " años",
        "<br>Población: ", comma(round(poblacion, 0))
      ),
      hoverinfo = "text"
    ) %>%
      add_lines(
        data = datos %>%
          arrange(pib_percapita),
        x = ~pib_percapita,
        y = ~tendencia,
        inherit = FALSE,
        name = "Tendencia estimada",
        line = list(width = 3, color = "black")
      ) %>%
      layout(
        title = paste0(
          "Modelo exploratorio: PIB per cápita y esperanza de vida — ",
          input$anio
        ),
        xaxis = list(
          title = "PIB per cápita (US$)",
          type = "log"
        ),
        yaxis = list(
          title = "Esperanza de vida al nacer (años)"
        ),
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.25
        ),
        margin = list(l = 70, r = 30, b = 100, t = 70)
      )
    
    config_plotly(
      grafico,
      paste0("modelo_", input$fuente, "_", input$anio)
    )
    
  })
  
  # ----------------------------------------------------------
  # 2. Insights automáticos
  # ----------------------------------------------------------
  
  output$insights <- renderUI({
    
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      
      return(
        div(
          class = "alert alert-warning",
          "No hay datos disponibles para los filtros seleccionados."
        )
      )
      
    }
    
    vida_prom <- mean(datos$esperanza_vida, na.rm = TRUE)
    
    pib_prom <- mean(datos$pib_percapita, na.rm = TRUE)
    
    pob_total <- sum(datos$poblacion, na.rm = TRUE)
    
    top_vida <- datos %>%
      filter(!is.na(esperanza_vida)) %>%
      arrange(desc(esperanza_vida)) %>%
      slice(1)
    
    top_pib <- datos %>%
      filter(!is.na(pib_percapita)) %>%
      arrange(desc(pib_percapita)) %>%
      slice(1)
    
    ambito <- if (!is.null(input$pais) &&
                  input$pais != "Todos") {
      
      input$pais
      
    } else if (!is.null(input$subregion) &&
               input$subregion != "Todos") {
      
      input$subregion
      
    } else if (!is.null(input$continente) &&
               input$continente != "Todos") {
      
      input$continente
      
    } else {
      
      "todos los países disponibles"
      
    }
    
    tagList(
      
      h3("💡 Insights del ámbito filtrado"),
      
      tags$p(
        strong("Fuente: "),
        input$fuente,
        " | ",
        strong("Año: "),
        input$anio,
        " | ",
        strong("Ámbito: "),
        ambito
      ),
      
      tags$ul(
        
        tags$li(
          paste0(
            "La esperanza de vida promedio es de ",
            round(vida_prom, 1),
            " años."
          )
        ),
        
        tags$li(
          paste0(
            "El PIB per cápita promedio es de US$ ",
            comma(round(pib_prom, 0)),
            "."
          )
        ),
        
        tags$li(
          paste0(
            "La población total del ámbito filtrado asciende a ",
            comma(round(pob_total, 0)),
            " habitantes."
          )
        ),
        
        tags$li(
          paste0(
            "El país con mayor esperanza de vida es ",
            top_vida$pais,
            " con ",
            round(top_vida$esperanza_vida, 1),
            " años."
          )
        ),
        
        tags$li(
          paste0(
            "El país con mayor PIB per cápita es ",
            top_pib$pais,
            " con US$ ",
            comma(round(top_pib$pib_percapita, 0)),
            "."
          )
        )
        
      )
      
    )
    
  })
  
  # ----------------------------------------------------------
  # 3. Tabla de datos filtrados
  # ----------------------------------------------------------
  
  output$tabla_datos <- renderDT({
    
    datos <- datos_filtrados()
    
    if (nrow(datos) == 0) {
      
      return(
        datatable(
          data.frame(Mensaje = "Sin datos para los filtros seleccionados"),
          rownames = FALSE
        )
      )
      
    }
    
    tabla <- datos %>%
      mutate(
        `Esperanza de vida, años` = round(esperanza_vida, 1),
        `PIB per cápita, US$` = comma(round(pib_percapita, 0)),
        `Población, habitantes` = comma(round(poblacion, 0))
      ) %>%
      select(
        Fuente = fuente,
        País = pais,
        Continente = continente,
        Subregión = subregion,
        Año = anio,
        `Esperanza de vida, años`,
        `PIB per cápita, US$`,
        `Población, habitantes`
      )
    
    datatable(
      tabla,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/es-ES.json"
        )
      )
    )
    
  })
  
} # Cierre del server

# ============================================================
# EJECUTAR APLICACIÓN
# ============================================================

shinyApp(
  ui = ui,
  server = server
)