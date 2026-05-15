library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)
library(plotly)
library(DT)
library(scales)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f4f6f9;
        font-family: 'Segoe UI', sans-serif;
      }
      .titulo {
        background: linear-gradient(90deg, #0B3C5D, #1D70A2);
        color: white;
        padding: 22px;
        border-radius: 14px;
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: white;
        padding: 15px;
        border-radius: 14px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.10);
      }
      .kpi {
        background-color: white;
        border-radius: 14px;
        padding: 16px;
        text-align: center;
        box-shadow: 0 2px 10px rgba(0,0,0,0.10);
        margin-bottom: 15px;
      }
      .kpi h3 {
        color: #0B3C5D;
        font-weight: bold;
        margin-top: 5px;
      }
      .kpi p {
        color: #555;
        font-size: 13px;
      }
      .insight {
        background-color: #ffffff;
        border-left: 6px solid #1D70A2;
        padding: 16px;
        border-radius: 10px;
        margin-bottom: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
    "))
  ),
  
  div(
    class = "titulo",
    h1("Dashboard Gapminder: Desarrollo Mundial Interactivo"),
    p("Análisis de esperanza de vida, PIB per cápita, población, brechas e índice de desarrollo relativo")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      class = "sidebar",
      
      selectInput(
        "continent",
        "Selecciona un continente:",
        choices = c("Todos", sort(as.character(unique(gapminder$continent)))),
        selected = "Todos"
      ),
      
      selectInput(
        "country",
        "Selecciona un país:",
        choices = c("Todos", sort(as.character(unique(gapminder$country)))),
        selected = "Todos"
      ),
      
      sliderInput(
        "year",
        "Selecciona un año:",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = max(gapminder$year),
        step = 5,
        sep = ""
      ),
      
      hr(),
      helpText("Use los filtros para analizar patrones territoriales, brechas y evolución del desarrollo mundial.")
    ),
    
    mainPanel(
      
      fluidRow(
        column(3, div(class = "kpi", h3(textOutput("kpi_paises")), p("Países analizados"))),
        column(3, div(class = "kpi", h3(textOutput("kpi_vida")), p("Esperanza de vida promedio"))),
        column(3, div(class = "kpi", h3(textOutput("kpi_pib")), p("PIB per cápita promedio"))),
        column(3, div(class = "kpi", h3(textOutput("kpi_poblacion")), p("Población total")))
      ),
      
      tabsetPanel(
        tabPanel("Inicio", br(), plotlyOutput("inicioPlot", height = "520px")),
        tabPanel("Mapa Mundial", br(), plotlyOutput("mapaPlot", height = "560px")),
        tabPanel("Evolución", br(), plotlyOutput("lifePlot", height = "520px")),
        tabPanel("PIB vs Vida", br(), plotlyOutput("scatterPlot", height = "520px")),
        tabPanel("Ranking", br(), plotlyOutput("rankingPlot", height = "520px")),
        tabPanel("Brechas", br(), plotlyOutput("brechaPlot", height = "520px")),
        tabPanel("Índice Desarrollo", br(), plotlyOutput("idrPlot", height = "520px")),
        tabPanel("Insights", br(), htmlOutput("insights")),
        tabPanel("Tabla", br(), DTOutput("tabla")),
        tabPanel("Resumen", br(), tableOutput("resumen"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$continent, {
    
    if (input$continent == "Todos") {
      paises <- sort(as.character(unique(gapminder$country)))
    } else {
      paises <- gapminder %>%
        filter(continent == input$continent) %>%
        pull(country) %>%
        unique() %>%
        as.character() %>%
        sort()
    }
    
    updateSelectInput(
      session,
      "country",
      choices = c("Todos", paises),
      selected = "Todos"
    )
  })
  
  datos_filtrados <- reactive({
    
    datos <- gapminder
    
    if (input$continent != "Todos") {
      datos <- datos %>% filter(continent == input$continent)
    }
    
    if (input$country != "Todos") {
      datos <- datos %>% filter(country == input$country)
    }
    
    datos
  })
  
  datos_anio <- reactive({
    datos_filtrados() %>% filter(year == input$year)
  })
  
  datos_idr <- reactive({
    
    datos <- datos_anio()
    
    if (nrow(datos) <= 1) {
      datos <- datos %>%
        mutate(
          vida_norm = 1,
          pib_norm = 1,
          idr = 100,
          nivel_idr = "Muy alto"
        )
    } else {
      datos <- datos %>%
        mutate(
          vida_norm = (lifeExp - min(lifeExp)) / (max(lifeExp) - min(lifeExp)),
          pib_norm = (log(gdpPercap) - min(log(gdpPercap))) /
            (max(log(gdpPercap)) - min(log(gdpPercap))),
          idr = round((vida_norm * 0.6 + pib_norm * 0.4) * 100, 2),
          nivel_idr = case_when(
            idr >= 75 ~ "Muy alto",
            idr >= 60 ~ "Alto",
            idr >= 45 ~ "Medio",
            TRUE ~ "Bajo"
          )
        )
    }
    
    datos
  })
  
  output$kpi_paises <- renderText({
    n_distinct(datos_anio()$country)
  })
  
  output$kpi_vida <- renderText({
    paste0(round(mean(datos_anio()$lifeExp), 1), " años")
  })
  
  output$kpi_pib <- renderText({
    paste0("$", comma(round(mean(datos_anio()$gdpPercap), 0)))
  })
  
  output$kpi_poblacion <- renderText({
    comma(sum(datos_anio()$pop))
  })
  
  output$inicioPlot <- renderPlotly({
    
    p <- ggplot(
      datos_anio(),
      aes(
        x = gdpPercap,
        y = lifeExp,
        size = pop,
        color = continent,
        text = paste(
          "País:", country,
          "<br>Continente:", continent,
          "<br>Año:", year,
          "<br>Esperanza de vida:", round(lifeExp, 2),
          "<br>PIB per cápita:", round(gdpPercap, 2),
          "<br>Población:", comma(pop)
        )
      )
    ) +
      geom_point(alpha = 0.75) +
      scale_x_log10(labels = comma) +
      labs(
        title = paste("Panorama mundial del desarrollo por países -", input$year),
        subtitle = "PIB per cápita, esperanza de vida y población",
        x = "PIB per cápita",
        y = "Esperanza de vida",
        color = "Continente",
        size = "Población"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$mapaPlot <- renderPlotly({
    
    mapa <- datos_anio()
    
    plot_ly(
      data = mapa,
      type = "choropleth",
      locations = ~country,
      locationmode = "country names",
      z = ~lifeExp,
      text = ~paste(
        "País:", country,
        "<br>Continente:", continent,
        "<br>Año:", year,
        "<br>Esperanza de vida:", round(lifeExp, 2),
        "<br>PIB per cápita:", round(gdpPercap, 2),
        "<br>Población:", comma(pop)
      ),
      colorscale = "Viridis",
      marker = list(line = list(color = "white", width = 0.4)),
      colorbar = list(title = "Esperanza<br>de vida")
    ) %>%
      layout(
        title = paste("Mapa mundial de esperanza de vida -", input$year),
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth")
        )
      )
  })
  
  output$lifePlot <- renderPlotly({
    
    if (input$country != "Todos") {
      
      pais <- gapminder %>% filter(country == input$country)
      continente_pais <- unique(pais$continent)
      
      promedio_continente <- gapminder %>%
        filter(continent == continente_pais) %>%
        group_by(year) %>%
        summarise(vida_promedio = mean(lifeExp), .groups = "drop")
      
      promedio_mundial <- gapminder %>%
        group_by(year) %>%
        summarise(vida_promedio = mean(lifeExp), .groups = "drop")
      
      p <- ggplot() +
        geom_line(
          data = promedio_mundial,
          aes(x = year, y = vida_promedio),
          color = "gray50",
          linewidth = 1,
          linetype = "dashed"
        ) +
        geom_line(
          data = promedio_continente,
          aes(x = year, y = vida_promedio),
          color = "#1D70A2",
          linewidth = 1,
          linetype = "dotted"
        ) +
        geom_line(
          data = pais,
          aes(
            x = year,
            y = lifeExp,
            text = paste(
              "País:", country,
              "<br>Año:", year,
              "<br>Esperanza de vida:", round(lifeExp, 2)
            )
          ),
          color = "#E76F51",
          linewidth = 1.4
        ) +
        geom_point(
          data = pais,
          aes(
            x = year,
            y = lifeExp,
            text = paste(
              "País:", country,
              "<br>Año:", year,
              "<br>Esperanza de vida:", round(lifeExp, 2)
            )
          ),
          color = "#E76F51",
          size = 2.5
        ) +
        labs(
          title = paste("Evolución de esperanza de vida:", input$country),
          subtitle = "País seleccionado vs promedio continental y promedio mundial",
          x = "Año",
          y = "Esperanza de vida"
        ) +
        theme_minimal()
      
    } else if (input$continent != "Todos") {
      
      evolucion_paises <- datos_filtrados()
      
      p <- ggplot(
        evolucion_paises,
        aes(
          x = year,
          y = lifeExp,
          color = country,
          group = country,
          text = paste(
            "País:", country,
            "<br>Año:", year,
            "<br>Esperanza de vida:", round(lifeExp, 2)
          )
        )
      ) +
        geom_line(linewidth = 0.9, alpha = 0.75) +
        geom_point(size = 1.5, alpha = 0.75) +
        labs(
          title = paste("Evolución de esperanza de vida en", input$continent),
          subtitle = "Cada línea representa un país del continente seleccionado",
          x = "Año",
          y = "Esperanza de vida",
          color = "País"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
    } else {
      
      evolucion <- datos_filtrados() %>%
        group_by(year, continent) %>%
        summarise(vida_promedio = mean(lifeExp), .groups = "drop")
      
      p <- ggplot(
        evolucion,
        aes(
          x = year,
          y = vida_promedio,
          color = continent,
          text = paste(
            "Continente:", continent,
            "<br>Año:", year,
            "<br>Esperanza de vida promedio:", round(vida_promedio, 2)
          )
        )
      ) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        labs(
          title = "Evolución promedio de la esperanza de vida por continente",
          subtitle = "Promedio continental cuando no se selecciona país ni continente",
          x = "Año",
          y = "Esperanza de vida promedio",
          color = "Continente"
        ) +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$scatterPlot <- renderPlotly({
    
    p <- ggplot(
      datos_anio(),
      aes(
        x = gdpPercap,
        y = lifeExp,
        color = continent,
        size = pop,
        text = paste(
          "País:", country,
          "<br>Continente:", continent,
          "<br>Año:", year,
          "<br>PIB per cápita:", round(gdpPercap, 2),
          "<br>Esperanza de vida:", round(lifeExp, 2),
          "<br>Población:", comma(pop)
        )
      )
    ) +
      geom_point(alpha = 0.75) +
      geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "gray40", linewidth = 0.8) +
      scale_x_log10(labels = comma) +
      labs(
        title = paste("Relación entre PIB per cápita y esperanza de vida -", input$year),
        subtitle = "Incluye línea de tendencia para apoyar el análisis comparativo",
        x = "PIB per cápita",
        y = "Esperanza de vida",
        color = "Continente",
        size = "Población"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$rankingPlot <- renderPlotly({
    
    ranking_top <- datos_anio() %>%
      arrange(desc(lifeExp)) %>%
      slice_head(n = 10) %>%
      mutate(tipo = "Mayor esperanza de vida")
    
    ranking_bottom <- datos_anio() %>%
      arrange(lifeExp) %>%
      slice_head(n = 10) %>%
      mutate(tipo = "Menor esperanza de vida")
    
    ranking <- bind_rows(ranking_top, ranking_bottom)
    
    p <- ggplot(
      ranking,
      aes(
        x = reorder(country, lifeExp),
        y = lifeExp,
        fill = tipo,
        text = paste(
          "País:", country,
          "<br>Continente:", continent,
          "<br>Clasificación:", tipo,
          "<br>Esperanza de vida:", round(lifeExp, 2)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      facet_wrap(~tipo, scales = "free_y") +
      labs(
        title = paste("Ranking de países según esperanza de vida -", input$year),
        x = "País",
        y = "Esperanza de vida",
        fill = "Clasificación"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$brechaPlot <- renderPlotly({
    
    if (input$continent == "Todos") {
      
      brechas <- datos_anio() %>%
        group_by(continent) %>%
        summarise(
          vida_minima = min(lifeExp),
          vida_maxima = max(lifeExp),
          brecha_vida = vida_maxima - vida_minima,
          .groups = "drop"
        )
      
      p <- ggplot(
        brechas,
        aes(
          x = reorder(continent, brecha_vida),
          y = brecha_vida,
          fill = continent,
          text = paste(
            "Continente:", continent,
            "<br>Vida mínima:", round(vida_minima, 2),
            "<br>Vida máxima:", round(vida_maxima, 2),
            "<br>Brecha:", round(brecha_vida, 2), "años"
          )
        )
      ) +
        geom_col() +
        coord_flip() +
        labs(
          title = paste("Brecha interna de esperanza de vida por continente -", input$year),
          subtitle = "Diferencia entre el país con mayor y menor esperanza de vida en cada continente",
          x = "Continente",
          y = "Brecha en años",
          fill = "Continente"
        ) +
        theme_minimal()
      
    } else {
      
      brechas_paises <- datos_anio() %>%
        arrange(desc(lifeExp))
      
      vida_max <- max(brechas_paises$lifeExp)
      vida_min <- min(brechas_paises$lifeExp)
      brecha_total <- round(vida_max - vida_min, 2)
      
      p <- ggplot(
        brechas_paises,
        aes(
          x = reorder(country, lifeExp),
          y = lifeExp,
          fill = lifeExp,
          text = paste(
            "País:", country,
            "<br>Esperanza de vida:", round(lifeExp, 2),
            "<br>Año:", year,
            "<br>Continente:", continent
          )
        )
      ) +
        geom_col() +
        coord_flip() +
        labs(
          title = paste("Brechas entre países de", input$continent, "-", input$year),
          subtitle = paste("Brecha entre mayor y menor esperanza de vida:", brecha_total, "años"),
          x = "País",
          y = "Esperanza de vida",
          fill = "Esperanza de vida"
        ) +
        theme_minimal()
    }
    
    ggplotly(p, tooltip = "text")
  })
  
  output$idrPlot <- renderPlotly({
    
    idr_data <- datos_idr() %>%
      arrange(desc(idr)) %>%
      slice_head(n = 20)
    
    p <- ggplot(
      idr_data,
      aes(
        x = reorder(country, idr),
        y = idr,
        fill = nivel_idr,
        text = paste(
          "País:", country,
          "<br>Continente:", continent,
          "<br>Índice de desarrollo relativo:", idr,
          "<br>Nivel:", nivel_idr,
          "<br>Esperanza de vida:", round(lifeExp, 2),
          "<br>PIB per cápita:", round(gdpPercap, 2)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Índice de Desarrollo Relativo -", input$year),
        subtitle = "Indicador propio: 60% esperanza de vida + 40% PIB per cápita normalizado",
        x = "País",
        y = "Índice de Desarrollo Relativo",
        fill = "Nivel"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$insights <- renderUI({
    
    data <- datos_anio()
    idr <- datos_idr()
    
    pais_mayor_vida <- data %>% arrange(desc(lifeExp)) %>% slice(1)
    pais_menor_vida <- data %>% arrange(lifeExp) %>% slice(1)
    pais_mayor_pib <- data %>% arrange(desc(gdpPercap)) %>% slice(1)
    
    promedio_mundial <- gapminder %>%
      filter(year == input$year) %>%
      summarise(prom = mean(lifeExp)) %>%
      pull(prom)
    
    bajo_promedio <- data %>%
      filter(lifeExp < promedio_mundial) %>%
      nrow()
    
    mejor_idr <- idr %>% arrange(desc(idr)) %>% slice(1)
    
    HTML(paste0(
      "<div class='insight'><b>Insight 1: Liderazgo en esperanza de vida.</b><br>",
      "En ", input$year, ", el país con mayor esperanza de vida en la selección es <b>",
      pais_mayor_vida$country, "</b>, con ", round(pais_mayor_vida$lifeExp, 2), " años.</div>",
      
      "<div class='insight'><b>Insight 2: Mayor rezago relativo.</b><br>",
      "El país con menor esperanza de vida en la selección es <b>",
      pais_menor_vida$country, "</b>, con ", round(pais_menor_vida$lifeExp, 2), " años.</div>",
      
      "<div class='insight'><b>Insight 3: Economía y desarrollo.</b><br>",
      "El mayor PIB per cápita corresponde a <b>",
      pais_mayor_pib$country, "</b>, con US$ ", comma(round(pais_mayor_pib$gdpPercap, 0)), ".</div>",
      
      "<div class='insight'><b>Insight 4: Alerta de brecha.</b><br>",
      "En la selección actual, <b>", bajo_promedio, "</b> países se encuentran por debajo del promedio mundial de esperanza de vida del año seleccionado.</div>",
      
      "<div class='insight'><b>Insight 5: Índice de Desarrollo Relativo.</b><br>",
      "El país con mayor índice relativo es <b>", mejor_idr$country, "</b>, con un puntaje de <b>", mejor_idr$idr, "</b>.</div>"
    ))
  })
  
  output$popPlot <- renderPlotly({
    
    poblacion <- datos_anio() %>%
      group_by(continent) %>%
      summarise(Poblacion_Total = sum(pop), .groups = "drop")
    
    p <- ggplot(
      poblacion,
      aes(
        x = reorder(continent, Poblacion_Total),
        y = Poblacion_Total,
        fill = continent,
        text = paste(
          "Continente:", continent,
          "<br>Población:", comma(Poblacion_Total)
        )
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste("Población total según selección -", input$year),
        x = "Continente",
        y = "Población total",
        fill = "Continente"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tabla <- renderDT({
    
    datos_idr() %>%
      select(
        Pais = country,
        Continente = continent,
        Año = year,
        Esperanza_de_vida = lifeExp,
        Poblacion = pop,
        PIB_per_capita = gdpPercap,
        Indice_Desarrollo_Relativo = idr,
        Nivel_IDR = nivel_idr
      ) %>%
      arrange(Continente, Pais)
    
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$resumen <- renderTable({
    
    datos_anio() %>%
      group_by(continent) %>%
      summarise(
        Esperanza_Vida_Promedio = round(mean(lifeExp), 2),
        PIB_Promedio = round(mean(gdpPercap), 2),
        Poblacion_Total = sum(pop),
        Numero_Paises = n(),
        .groups = "drop"
      )
  })
}

shinyApp(ui = ui, server = server)