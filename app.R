library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)
library(plotly)
library(DT)

ui <- fluidPage(
  
  titlePanel("Dashboard Gapminder: Desarrollo Mundial"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "continent",
        "Selecciona un continente:",
        choices = c(
          "Todos",
          sort(as.character(unique(gapminder$continent)))
        ),
        selected = "Americas"
      ),
      
      selectInput(
        "country",
        "Selecciona un país:",
        choices = sort(as.character(unique(
          gapminder$country[gapminder$continent == "Americas"]
        ))),
        selected = "Peru"
      ),
      
      sliderInput(
        "year",
        "Selecciona un año:",
        min = min(gapminder$year),
        max = max(gapminder$year),
        value = 2007,
        step = 5,
        sep = ""
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Esperanza de Vida",
          plotlyOutput("lifePlot")
        ),
        
        tabPanel(
          "PIB vs Vida",
          plotlyOutput("scatterPlot")
        ),
        
        tabPanel(
          "Población",
          plotlyOutput("popPlot")
        ),
        
        tabPanel(
          "Tabla",
          DTOutput("tabla")
        ),
        
        tabPanel(
          "Resumen",
          tableOutput("resumen")
        )
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
      choices = paises,
      selected = ifelse("Peru" %in% paises, "Peru", paises[1])
    )
  })
  
  datos_pais <- reactive({
    
    gapminder %>%
      filter(country == input$country)
  })
  
  datos_anio <- reactive({
    
    datos <- gapminder %>%
      filter(year == input$year)
    
    if (input$continent != "Todos") {
      
      datos <- datos %>%
        filter(continent == input$continent)
    }
    
    datos
  })
  
  output$lifePlot <- renderPlotly({
    
    p <- ggplot(
      datos_pais(),
      aes(x = year, y = lifeExp)
    ) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "red", size = 2) +
      labs(
        title = paste("Esperanza de Vida en", input$country),
        x = "Año",
        y = "Esperanza de Vida"
      ) +
      theme_minimal()
    
    ggplotly(p)
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
          "<br>PIB per cápita:", round(gdpPercap, 2),
          "<br>Esperanza de vida:", round(lifeExp, 2),
          "<br>Población:", pop
        )
      )
    ) +
      geom_point(alpha = 0.7) +
      scale_x_log10() +
      labs(
        title = paste("PIB per cápita vs Esperanza de Vida -", input$year),
        x = "PIB per cápita",
        y = "Esperanza de vida"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$popPlot <- renderPlotly({
    
    poblacion <- datos_anio() %>%
      group_by(continent) %>%
      summarise(
        total = sum(pop),
        .groups = "drop"
      )
    
    p <- ggplot(
      poblacion,
      aes(
        x = continent,
        y = total,
        fill = continent
      )
    ) +
      geom_col() +
      labs(
        title = paste("Población por continente -", input$year),
        x = "Continente",
        y = "Población"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$tabla <- renderDT({
    
    datos_anio() %>%
      select(country, continent, year, lifeExp, pop, gdpPercap)
  })
  
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