# =============================================================
# OPCIÓN B — DASHBOARD GAPMINDER + BANCO MUNDIAL (WDI)
# Datos actualizados hasta 2023
# ⚠️ Requiere conexión a internet al iniciar
# =============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(gapminder)
library(plotly)
library(DT)
library(scales)
library(WDI)          # install.packages("WDI")

# ---- DATOS GAPMINDER (base histórica) ----
data("gapminder")

gap_base <- gapminder %>%
  mutate(
    gdp_total = gdpPercap * pop,
    pop_m     = pop / 1e6,
    fuente    = "Gapminder"
  ) %>%
  group_by(country) %>% arrange(year) %>%
  mutate(crec_gdp = (gdpPercap/lag(gdpPercap)-1)*100) %>%
  ungroup()

# ---- DATOS BANCO MUNDIAL (actualizados 2000–2023) ----
# Se descargan al iniciar la app (requiere internet)
wdi_raw <- tryCatch({
  WDI(
    country   = "all",
    indicator = c(
      gdpPercap = "NY.GDP.PCAP.KD",      # PIB per cápita (USD constantes)
      lifeExp   = "SP.DYN.LE00.IN",      # Esperanza de vida
      pop       = "SP.POP.TOTL",         # Población total
      inflacion = "FP.CPI.TOTL.ZG",      # Inflación (%)
      desempleo = "SL.UEM.TOTL.ZS",      # Desempleo (%)
      gini      = "SI.POV.GINI",         # Índice Gini (desigualdad)
      pobreza   = "SI.POV.DDAY",         # Pobreza extrema (%)
      exportac  = "NE.EXP.GNFS.ZS"       # Exportaciones (% PIB)
    ),
    start = 2000, end = 2023,
    extra = TRUE                          # incluye región y país
  )
}, error = function(e) NULL)

# Procesar datos WDI si se descargaron correctamente
if (!is.null(wdi_raw) && nrow(wdi_raw) > 0) {
  wdi <- wdi_raw %>%
    filter(!is.na(gdpPercap), !region %in% c("Aggregates","")) %>%
    select(country, year, gdpPercap, lifeExp, pop,
           inflacion, desempleo, gini, pobreza, exportac, region) %>%
    mutate(
      pop_m     = pop / 1e6,
      gdp_total = gdpPercap * pop,
      fuente    = "Banco Mundial"
    ) %>%
    group_by(country) %>% arrange(year) %>%
    mutate(crec_gdp = (gdpPercap/lag(gdpPercap)-1)*100) %>%
    ungroup()
  usa_wdi <- TRUE
} else {
  wdi     <- NULL
  usa_wdi <- FALSE
}

# Países y años disponibles
paises_gap  <- sort(unique(gap_base$country))
anios_gap   <- sort(unique(gap_base$year))
paises_wdi  <- if(usa_wdi) sort(unique(wdi$country)) else character(0)
continentes <- c("Todos", sort(unique(gap_base$continent)))

COL <- list(
  azul="#003366", celeste="#0066CC", verde="#00A86B",
  naranja="#E85D04", morado="#6A0DAD", gris="#6C757D"
)
tema_corp <- theme_minimal(base_size=12)+
  theme(plot.title=element_text(size=13,face="bold",color="#003366"),
        plot.subtitle=element_text(size=11,color="#6C757D"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_line(color="#EAECEF"),
        legend.position="bottom")

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$style(HTML(paste0("
    body{font-family:'Segoe UI',sans-serif;background:#F4F6F9;}
    .hdr{background:",COL$azul,";color:white;padding:14px 24px;border-radius:8px;margin-bottom:16px;}
    .hdr h2{margin:0;font-size:20px;font-weight:700;}
    .hdr p{margin:4px 0 0;font-size:12px;color:#ADB5BD;}
    .sb{background:white;border-radius:10px;padding:14px;box-shadow:0 1px 6px rgba(0,0,0,.07);}
    .kpi{background:white;border-radius:10px;padding:14px 12px;text-align:center;
         border-top:4px solid ",COL$azul,";box-shadow:0 2px 8px rgba(0,0,0,.08);margin-bottom:10px;}
    .kpi-v{font-size:22px;font-weight:700;color:",COL$azul,";}
    .kpi-l{font-size:10px;color:#6C757D;margin-top:2px;}
    .kpi-d{font-size:11px;font-weight:600;margin-top:3px;}
    .up{color:#00A86B;} .dn{color:#E85D04;}
    .alerta{background:#FFF3CD;border-left:4px solid #FFC107;padding:10px 14px;
            border-radius:6px;font-size:12px;margin-bottom:10px;}
    .nav-tabs .nav-link.active{background:",COL$azul,"!important;
      color:white!important;border-radius:6px 6px 0 0;}
    .tab-content{background:white;border-radius:0 8px 8px 8px;
      padding:18px;box-shadow:0 2px 8px rgba(0,0,0,.06);}
  ")))),
  
  div(class="hdr",
      h2("\U0001F30D  Gapminder + Banco Mundial — Análisis Económico Extendido"),
      p(paste0("Dashboard interactivo · Gapminder 1952–2007 + WDI 2000–2023 · Opción B",
               if(!usa_wdi) " ⚠️ WDI no disponible (sin conexión)" else ""))),
  
  sidebarLayout(
    sidebarPanel(width=3, div(class="sb",
                              h4(style=paste0("color:",COL$azul,";margin-top:0;"),"\U0001F50D Filtros"),
                              
                              # Selector de fuente de datos
                              radioButtons("fuente","Fuente de datos:",
                                           choices = if(usa_wdi) c("Gapminder (1952–2007)"="gap",
                                                                   "Banco Mundial (2000–2023)"="wdi")
                                           else c("Gapminder (1952–2007)"="gap"),
                                           selected="gap"),
                              
                              conditionalPanel("input.fuente=='gap'",
                                               selectInput("continente_g","Continente:",choices=continentes,selected="Todos"),
                                               uiOutput("ui_pais_g"),
                                               sliderInput("anio_g","Años:",min=min(anios_gap),max=max(anios_gap),
                                                           value=c(min(anios_gap),max(anios_gap)),step=5,sep="")),
                              
                              conditionalPanel("input.fuente=='wdi'",
                                               uiOutput("ui_pais_w"),
                                               sliderInput("anio_w","Años:",min=2000,max=2023,value=c(2000,2023),sep="")),
                              
                              hr(),
                              h4(style=paste0("color:",COL$azul,";"),"\U0001F4CA KPIs"),
                              uiOutput("kpis"),
                              hr(),
                              downloadButton("dl_csv","📥 Exportar CSV",
                                             style=paste0("background:",COL$celeste,";color:white;border:none;
                      border-radius:6px;width:100%;")),
                              br(),br(),
                              if(!usa_wdi) div(class="alerta",
                                               "⚠️ Datos del Banco Mundial no disponibles. Verifica tu conexión a internet y reinicia la app.")
    )),
    
    mainPanel(width=9,
              tabsetPanel(id="tabs",
                          
                          # TAB 1: COMPARATIVA HISTÓRICA (solo si WDI disponible)
                          tabPanel("\U0001F4CA Histórico 1952–2023",
                                   br(),
                                   if(usa_wdi){
                                     tagList(
                                       p(style="color:#6C757D;font-size:12px;",
                                         "Combina datos de Gapminder (1952–2007) y Banco Mundial (2000–2023).
                 El período 2000–2007 puede mostrar diferencias metodológicas entre fuentes."),
                                       fluidRow(
                                         column(6, plotlyOutput("p_hist_vida", height="300px")),
                                         column(6, plotlyOutput("p_hist_gdp",  height="300px"))
                                       )
                                     )
                                   } else {
                                     div(class="alerta","Sin conexión al Banco Mundial. Solo disponibles datos Gapminder.")
                                   }
                          ),
                          
                          # TAB 2: TENDENCIAS
                          tabPanel("\U0001F4C8 Tendencias",
                                   br(),
                                   fluidRow(
                                     column(6, plotlyOutput("p_vida",   height="290px")),
                                     column(6, plotlyOutput("p_gdp",    height="290px"))
                                   ),
                                   fluidRow(
                                     column(6, plotlyOutput("p_pop",    height="260px")),
                                     column(6, plotlyOutput("p_gdptot", height="260px"))
                                   )
                          ),
                          
                          # TAB 3: INDICADORES MACROECONÓMICOS (solo WDI)
                          tabPanel("\U0001F4B9 Macro (WDI)",
                                   br(),
                                   if(usa_wdi){
                                     tagList(
                                       fluidRow(
                                         column(6, plotlyOutput("p_inflacion",  height="280px")),
                                         column(6, plotlyOutput("p_desempleo",  height="280px"))
                                       ),
                                       fluidRow(
                                         column(6, plotlyOutput("p_gini",       height="270px")),
                                         column(6, plotlyOutput("p_exportac",   height="270px"))
                                       )
                                     )
                                   } else {
                                     div(class="alerta",
                                         "\U0001F4A1 Los indicadores macroeconómicos del Banco Mundial no están disponibles.
                 Conéctate a internet y reinicia la aplicación.")
                                   }
                          ),
                          
                          # TAB 4: COMPARAR PAÍSES
                          tabPanel("\U0001F500 Comparar",
                                   br(),
                                   fluidRow(
                                     column(4, uiOutput("ui_cmp1")),
                                     column(4, uiOutput("ui_cmp2")),
                                     column(4, uiOutput("ui_cmp3"))
                                   ),
                                   fluidRow(
                                     column(6, plotlyOutput("cmp_vida", height="270px")),
                                     column(6, plotlyOutput("cmp_gdp",  height="270px"))
                                   ),
                                   fluidRow(
                                     column(6, plotlyOutput("cmp_pop",  height="250px")),
                                     column(6, plotlyOutput("cmp_crec", height="250px"))
                                   ),
                                   br(), DTOutput("cmp_tabla")
                          ),
                          
                          # TAB 5: RANKINGS
                          tabPanel("\U0001F3C6 Rankings",
                                   br(),
                                   fluidRow(
                                     column(6, plotlyOutput("p_top_vida", height="300px")),
                                     column(6, plotlyOutput("p_top_gdp",  height="300px"))
                                   ),
                                   fluidRow(
                                     column(6, plotlyOutput("p_top_crec", height="280px")),
                                     column(6, plotlyOutput("p_top_pop",  height="280px"))
                                   )
                          ),
                          
                          # TAB 6: CONTINENTES (solo Gapminder)
                          tabPanel("\U0001F5FA Continentes",
                                   br(),
                                   fluidRow(
                                     column(6, plotlyOutput("p_boxvida",  height="310px")),
                                     column(6, plotlyOutput("p_boxgdp",   height="310px"))
                                   ),
                                   fluidRow(column(12, plotlyOutput("p_burbuja", height="370px")))
                          ),
                          
                          # TAB 7: MODELO
                          tabPanel("\U0001F9EE Modelo",
                                   br(),
                                   fluidRow(
                                     column(8, plotlyOutput("p_reg",    height="350px")),
                                     column(4, verbatimTextOutput("txt_modelo"))
                                   ),
                                   fluidRow(
                                     column(6, plotlyOutput("p_resid",  height="250px")),
                                     column(6, plotlyOutput("p_proyec", height="250px"))
                                   )
                          ),
                          
                          # TAB 8: DATOS
                          tabPanel("\U0001F4CB Datos", br(), DTOutput("tabla"))
              )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Datos activos según fuente
  datos_act <- reactive({
    if (input$fuente == "gap") {
      df <- gap_base %>% filter(year>=input$anio_g[1], year<=input$anio_g[2])
      if (input$continente_g != "Todos")
        df <- df %>% filter(continent==input$continente_g)
      df
    } else {
      if (is.null(wdi)) return(gap_base)
      req(input$pais_w)
      wdi %>% filter(country==input$pais_w, year>=input$anio_w[1], year<=input$anio_w[2])
    }
  })
  
  pais_act <- reactive({
    if (input$fuente=="gap") req(input$pais_g) else req(input$pais_w)
  })
  
  datos_pais <- reactive({
    if (input$fuente=="gap") {
      gap_base %>% filter(country==req(input$pais_g),
                          year>=input$anio_g[1], year<=input$anio_g[2])
    } else {
      if (is.null(wdi)) return(NULL)
      wdi %>% filter(country==req(input$pais_w),
                     year>=input$anio_w[1], year<=input$anio_w[2])
    }
  })
  
  ult <- reactive({ max(datos_act()$year, na.rm=TRUE) })
  
  # UIs dinámicos
  output$ui_pais_g <- renderUI({
    lista <- if(input$continente_g=="Todos") paises_gap else {
      gap_base %>% filter(continent==input$continente_g) %>%
        pull(country) %>% unique() %>% sort()
    }
    selectInput("pais_g","País:", choices=lista, selected="Peru")
  })
  output$ui_pais_w <- renderUI({
    selectInput("pais_w","País:", choices=paises_wdi, selected="Peru")
  })
  output$ui_cmp1 <- renderUI({
    ch <- if(input$fuente=="gap") paises_gap else paises_wdi
    selectInput("cmp1","País 1:", choices=ch, selected="Peru")
  })
  output$ui_cmp2 <- renderUI({
    ch <- if(input$fuente=="gap") paises_gap else paises_wdi
    selectInput("cmp2","País 2:", choices=ch, selected="Chile")
  })
  output$ui_cmp3 <- renderUI({
    ch <- if(input$fuente=="gap") paises_gap else paises_wdi
    selectInput("cmp3","País 3 (opcional):", choices=c("Ninguno",ch), selected="Colombia")
  })
  
  # KPIs
  output$kpis <- renderUI({
    d <- datos_pais(); if(is.null(d)||nrow(d)==0) return(p("Sin datos"))
    dult <- d %>% filter(year==max(year))
    if(nrow(dult)==0) return(NULL)
    tagList(
      div(class="kpi",
          div(class="kpi-v",round(dult$lifeExp,1)),
          div(class="kpi-l","Esperanza de vida (años)")),
      div(class="kpi",style=paste0("border-color:",COL$celeste,";"),
          div(class="kpi-v",paste0("$",formatC(round(dult$gdpPercap),big.mark=","))),
          div(class="kpi-l","PIB per cápita (USD)")),
      div(class="kpi",style=paste0("border-color:",COL$naranja,";"),
          div(class="kpi-v",paste0(round(dult$pop_m,1),"M")),
          div(class="kpi-l","Población")),
      if(input$fuente=="wdi" && !is.null(dult$inflacion)){
        div(class="kpi",style=paste0("border-color:",COL$morado,";"),
            div(class="kpi-v",paste0(round(dult$inflacion,1),"%")),
            div(class="kpi-l","Inflación"))
      }
    )
  })
  
  output$dl_csv <- downloadHandler(
    filename=function() paste0("datos_",pais_act(),"_",Sys.Date(),".csv"),
    content=function(f) write.csv(datos_pais(),f,row.names=FALSE))
  
  # TAB 1: HISTÓRICO 1952–2023
  output$p_hist_vida <- renderPlotly({
    req(input$fuente=="wdi", usa_wdi)
    pg <- gap_base %>% filter(country==req(input$pais_w)) %>%
      select(year,lifeExp) %>% mutate(fuente="Gapminder")
    pw <- wdi %>% filter(country==req(input$pais_w), !is.na(lifeExp)) %>%
      select(year,lifeExp) %>% mutate(fuente="Banco Mundial")
    df <- bind_rows(pg, pw)
    ggplotly(ggplot(df,aes(year,lifeExp,color=fuente))+
               geom_line(linewidth=1.2)+geom_point(size=2.5)+
               scale_color_manual(values=c("Gapminder"=COL$verde,"Banco Mundial"=COL$celeste))+
               labs(title=paste("Esperanza de vida 1952–2023 —",input$pais_w),
                    x="Año",y="Años",color="Fuente")+tema_corp)
  })
  output$p_hist_gdp <- renderPlotly({
    req(input$fuente=="wdi", usa_wdi)
    pg <- gap_base %>% filter(country==req(input$pais_w)) %>%
      select(year,gdpPercap) %>% mutate(fuente="Gapminder")
    pw <- wdi %>% filter(country==req(input$pais_w), !is.na(gdpPercap)) %>%
      select(year,gdpPercap) %>% mutate(fuente="Banco Mundial")
    df <- bind_rows(pg, pw)
    ggplotly(ggplot(df,aes(year,gdpPercap,color=fuente))+
               geom_line(linewidth=1.2)+geom_point(size=2.5)+
               scale_color_manual(values=c("Gapminder"=COL$verde,"Banco Mundial"=COL$celeste))+
               scale_y_continuous(labels=comma)+
               labs(title=paste("PIB per cápita 1952–2023 —",input$pais_w),
                    x="Año",y="USD",color="Fuente")+tema_corp)
  })
  
  # TAB 2: TENDENCIAS
  output$p_vida <- renderPlotly({
    d <- datos_pais(); req(!is.null(d))
    ggplotly(ggplot(d,aes(year,lifeExp))+
               geom_area(fill=COL$verde,alpha=.15)+geom_line(color=COL$verde,linewidth=1.3)+
               geom_point(color=COL$verde,size=3)+
               labs(title=paste("Esperanza de vida —",pais_act()),x="Año",y="Años")+tema_corp)
  })
  output$p_gdp <- renderPlotly({
    d <- datos_pais(); req(!is.null(d))
    ggplotly(ggplot(d,aes(year,gdpPercap))+
               geom_area(fill=COL$celeste,alpha=.15)+geom_line(color=COL$celeste,linewidth=1.3)+
               geom_point(color=COL$celeste,size=3)+scale_y_continuous(labels=comma)+
               labs(title=paste("PIB per cápita —",pais_act()),x="Año",y="USD")+tema_corp)
  })
  output$p_pop <- renderPlotly({
    d <- datos_pais(); req(!is.null(d))
    ggplotly(ggplot(d,aes(year,pop_m))+geom_col(fill=COL$naranja,alpha=.85)+
               labs(title=paste("Población —",pais_act()),x="Año",y="Millones")+tema_corp)
  })
  output$p_gdptot <- renderPlotly({
    d <- datos_pais(); req(!is.null(d))
    ggplotly(ggplot(d,aes(year,gdp_total/1e9))+
               geom_area(fill=COL$azul,alpha=.15)+geom_line(color=COL$azul,linewidth=1.3)+
               geom_point(color=COL$azul,size=3)+scale_y_continuous(labels=comma)+
               labs(title=paste("PIB total —",pais_act()),x="Año",y="Miles de millones USD")+tema_corp)
  })
  
  # TAB 3: MACRO WDI
  output$p_inflacion <- renderPlotly({
    req(usa_wdi, input$fuente=="wdi")
    d <- datos_pais() %>% filter(!is.na(inflacion))
    ggplotly(ggplot(d,aes(year,inflacion,fill=inflacion>5))+
               geom_col(alpha=.85)+
               scale_fill_manual(values=c("FALSE"=COL$verde,"TRUE"=COL$naranja))+
               geom_hline(yintercept=5,linetype="dashed",color=COL$gris)+
               labs(title=paste("Inflación —",input$pais_w),
                    subtitle="Línea roja = umbral 5%",x="Año",y="%")+tema_corp+
               theme(legend.position="none"))
  })
  output$p_desempleo <- renderPlotly({
    req(usa_wdi, input$fuente=="wdi")
    d <- datos_pais() %>% filter(!is.na(desempleo))
    ggplotly(ggplot(d,aes(year,desempleo))+
               geom_line(color=COL$morado,linewidth=1.3)+geom_point(color=COL$morado,size=3)+
               geom_area(fill=COL$morado,alpha=.12)+
               labs(title=paste("Desempleo —",input$pais_w),x="Año",y="%")+tema_corp)
  })
  output$p_gini <- renderPlotly({
    req(usa_wdi, input$fuente=="wdi")
    d <- datos_pais() %>% filter(!is.na(gini))
    if(nrow(d)==0) return(plotly_empty() %>% layout(title="Sin datos Gini disponibles"))
    ggplotly(ggplot(d,aes(year,gini))+
               geom_line(color=COL$naranja,linewidth=1.3)+geom_point(color=COL$naranja,size=3)+
               labs(title=paste("Índice Gini —",input$pais_w),
                    subtitle="Mayor valor = mayor desigualdad",x="Año",y="Gini")+tema_corp)
  })
  output$p_exportac <- renderPlotly({
    req(usa_wdi, input$fuente=="wdi")
    d <- datos_pais() %>% filter(!is.na(exportac))
    ggplotly(ggplot(d,aes(year,exportac))+
               geom_line(color=COL$celeste,linewidth=1.3)+geom_point(color=COL$celeste,size=3)+
               geom_area(fill=COL$celeste,alpha=.12)+
               labs(title=paste("Exportaciones —",input$pais_w),
                    subtitle="% del PIB",x="Año",y="% del PIB")+tema_corp)
  })
  
  # TAB 4: COMPARAR
  datos_cmp <- reactive({
    src <- if(input$fuente=="gap") gap_base else wdi
    req(src)
    ps <- c(req(input$cmp1), req(input$cmp2))
    if(!is.null(input$cmp3) && input$cmp3!="Ninguno") ps <- c(ps,input$cmp3)
    anio_r <- if(input$fuente=="gap") input$anio_g else input$anio_w
    src %>% filter(country %in% ps, year>=anio_r[1], year<=anio_r[2])
  })
  output$cmp_vida <- renderPlotly({
    ggplotly(ggplot(datos_cmp(),aes(year,lifeExp,color=country))+
               geom_line(linewidth=1.3)+geom_point(size=3)+
               scale_color_manual(values=c(COL$azul,COL$verde,COL$naranja))+
               labs(title="Esperanza de vida comparada",x="Año",y="Años",color=NULL)+tema_corp)
  })
  output$cmp_gdp <- renderPlotly({
    ggplotly(ggplot(datos_cmp(),aes(year,gdpPercap,color=country))+
               geom_line(linewidth=1.3)+geom_point(size=3)+
               scale_color_manual(values=c(COL$azul,COL$verde,COL$naranja))+
               scale_y_continuous(labels=comma)+
               labs(title="PIB per cápita comparado",x="Año",y="USD",color=NULL)+tema_corp)
  })
  output$cmp_pop <- renderPlotly({
    ggplotly(ggplot(datos_cmp(),aes(year,pop_m,color=country))+
               geom_line(linewidth=1.3)+geom_point(size=3)+
               scale_color_manual(values=c(COL$azul,COL$verde,COL$naranja))+
               labs(title="Población comparada",x="Año",y="Millones",color=NULL)+tema_corp)
  })
  output$cmp_crec <- renderPlotly({
    df <- datos_cmp() %>% filter(!is.na(crec_gdp))
    ggplotly(ggplot(df,aes(year,crec_gdp,color=country))+
               geom_line(linewidth=1.2)+geom_point(size=2.5)+
               geom_hline(yintercept=0,linetype="dashed",color=COL$gris)+
               scale_color_manual(values=c(COL$azul,COL$verde,COL$naranja))+
               labs(title="Crecimiento PIB comparado",x="Año",y="%",color=NULL)+tema_corp)
  })
  output$cmp_tabla <- renderDT({
    datos_cmp() %>% filter(year==ult()) %>%
      select(any_of(c("country","year","lifeExp","gdpPercap","pop_m","crec_gdp"))) %>%
      mutate(across(where(is.numeric),~round(.,1))) %>%
      datatable(rownames=FALSE,options=list(dom="t"))
  })
  
  # TAB 5: RANKINGS
  output$p_top_vida <- renderPlotly({
    df <- datos_act() %>% filter(year==ult()) %>% arrange(desc(lifeExp)) %>% slice_head(n=10)
    ggplotly(ggplot(df,aes(reorder(country,lifeExp),lifeExp,
                           text=paste0("<b>",country,"</b>: ",round(lifeExp,1)," años")))+
               geom_col(fill=COL$verde,alpha=.85)+coord_flip()+
               labs(title="Top 10 · Esperanza de vida",x=NULL,y="Años")+tema_corp,tooltip="text")
  })
  output$p_top_gdp <- renderPlotly({
    df <- datos_act() %>% filter(year==ult()) %>% arrange(desc(gdpPercap)) %>% slice_head(n=10)
    ggplotly(ggplot(df,aes(reorder(country,gdpPercap),gdpPercap,
                           text=paste0("<b>",country,"</b>: $",formatC(round(gdpPercap),big.mark=","))))+
               geom_col(fill=COL$celeste,alpha=.85)+coord_flip()+scale_y_continuous(labels=comma)+
               labs(title="Top 10 · PIB per cápita",x=NULL,y="USD")+tema_corp,tooltip="text")
  })
  output$p_top_crec <- renderPlotly({
    df <- datos_act() %>% filter(year==ult(),!is.na(crec_gdp)) %>%
      arrange(desc(crec_gdp)) %>% slice_head(n=10)
    ggplotly(ggplot(df,aes(reorder(country,crec_gdp),crec_gdp,
                           text=paste0("<b>",country,"</b>: ",round(crec_gdp,1),"%")))+
               geom_col(fill=COL$naranja,alpha=.85)+coord_flip()+
               labs(title="Top 10 · Mayor crecimiento PIB",x=NULL,y="%")+tema_corp,tooltip="text")
  })
  output$p_top_pop <- renderPlotly({
    df <- datos_act() %>% filter(year==ult()) %>% arrange(desc(pop_m)) %>% slice_head(n=10)
    ggplotly(ggplot(df,aes(reorder(country,pop_m),pop_m,
                           text=paste0("<b>",country,"</b>: ",round(pop_m,1),"M")))+
               geom_col(fill=COL$morado,alpha=.85)+coord_flip()+
               labs(title="Top 10 · Mayor población",x=NULL,y="Millones")+tema_corp,tooltip="text")
  })
  
  # TAB 6: CONTINENTES (solo Gapminder)
  dc_gap <- reactive({
    gap_base %>% filter(year>=input$anio_g[1], year<=input$anio_g[2])
  })
  output$p_boxvida <- renderPlotly({
    ggplotly(ggplot(dc_gap(),aes(reorder(continent,lifeExp,median),lifeExp,fill=continent))+
               geom_boxplot(alpha=.7,outlier.colour=COL$naranja,show.legend=FALSE)+
               scale_fill_brewer(palette="Set2")+
               labs(title="Esperanza de vida por continente",x=NULL,y="Años")+tema_corp)
  })
  output$p_boxgdp <- renderPlotly({
    ggplotly(ggplot(dc_gap(),aes(reorder(continent,gdpPercap,median),gdpPercap,fill=continent))+
               geom_boxplot(alpha=.7,show.legend=FALSE)+scale_y_log10(labels=comma)+
               scale_fill_brewer(palette="Set1")+
               labs(title="PIB per cápita por continente (log)",x=NULL,y="USD (log)")+tema_corp)
  })
  output$p_burbuja <- renderPlotly({
    df <- dc_gap() %>% filter(year==max(year))
    ggplotly(ggplot(df,aes(gdpPercap,lifeExp,size=pop_m,color=continent,
                           text=paste0("<b>",country,"</b><br>PIB: $",round(gdpPercap),"<br>Vida: ",round(lifeExp,1)," años")))+
               geom_point(alpha=.7)+scale_x_log10(labels=comma)+scale_size(range=c(3,20))+
               scale_color_brewer(palette="Set1")+
               labs(title="Gráfico de burbuja — Rosling Style",
                    x="PIB per cápita (log)",y="Esperanza de vida",color="Continente")+tema_corp,
             tooltip="text")
  })
  
  # TAB 7: MODELO
  modelo <- reactive({
    lm(lifeExp~log(gdpPercap)+year, data=datos_act() %>% filter(!is.na(gdpPercap),!is.na(lifeExp)))
  })
  output$p_reg <- renderPlotly({
    df <- datos_act() %>% filter(!is.na(gdpPercap))
    ggplotly(ggplot(df,aes(log(gdpPercap),lifeExp))+
               geom_point(alpha=.3,size=1.5,color=COL$celeste)+
               geom_smooth(method="lm",se=TRUE,color=COL$azul,linewidth=1.1)+
               labs(title="Regresión: log(PIB) vs Esperanza de vida",
                    x="log(PIB per cápita)",y="Esperanza de vida")+tema_corp)
  })
  output$txt_modelo <- renderPrint({
    s <- summary(modelo())
    cat("=== MODELO ===\n")
    cat("R²          =",round(s$r.squared,4),"\n")
    cat("R² ajustado =",round(s$adj.r.squared,4),"\n\n")
    print(round(coef(s)[,c(1,4)],4))
  })
  output$p_resid <- renderPlotly({
    df <- data.frame(aj=fitted(modelo()),re=residuals(modelo()))
    ggplotly(ggplot(df,aes(aj,re))+geom_point(alpha=.35,color=COL$celeste,size=1.5)+
               geom_hline(yintercept=0,color=COL$naranja,linetype="dashed")+
               geom_smooth(se=FALSE,color=COL$azul)+
               labs(title="Residuos vs Ajustados",x="Ajustados",y="Residuos")+tema_corp)
  })
  output$p_proyec <- renderPlotly({
    d <- datos_pais(); req(!is.null(d), nrow(d)>=3)
    m  <- lm(lifeExp~year,data=d)
    nv <- data.frame(year=seq(max(d$year),2030,by=1))
    pr <- cbind(nv,as.data.frame(predict(m,nv,interval="confidence")))
    ggplotly(ggplot()+
               geom_line(data=d,aes(year,lifeExp),color=COL$verde,linewidth=1.2)+
               geom_point(data=d,aes(year,lifeExp),color=COL$verde,size=3)+
               geom_line(data=pr,aes(year,fit),color=COL$azul,linetype="dashed",linewidth=1)+
               geom_ribbon(data=pr,aes(year,ymin=lwr,ymax=upr),fill=COL$azul,alpha=.15)+
               labs(title=paste("Proyección —",pais_act()),subtitle="Hasta 2030 (IC 95%)",
                    x="Año",y="Años de vida")+tema_corp)
  })
  
  # TAB 8: DATOS
  output$tabla <- renderDT({
    datos_act() %>%
      select(any_of(c("country","year","lifeExp","gdpPercap","pop_m",
                      "crec_gdp","inflacion","desempleo","gini","exportac"))) %>%
      mutate(across(where(is.numeric),~round(.,2))) %>%
      datatable(filter="top",rownames=FALSE,
                options=list(pageLength=15,scrollX=TRUE))
  })
}

shinyApp(ui=ui, server=server)
