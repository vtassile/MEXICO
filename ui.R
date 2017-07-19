library(shiny)
library(shinydashboard)
library(FactoMineR)
library(lme4)
library(matrixcalc)
library(cluster)
library(tidyr)
library(dplyr)
library(networkD3)
library(DT)
library(d3Network)
library(RCurl)
library(leaflet)
library(highcharter)
library(dendextend)
library(circlize)
library(ggfortify)
library(d3heatmap)
library(ggplot2)
library(ggdendro)
library(plotly)
library(googleVis)
#library(htmltools)
#library(exportwidget)
#library(webshot)
#library(highcharter)
library(lubridate)
#library(mxmaps) 
library(scales) # needed for comma
library(viridis)

dashboardPage(
  dashboardHeader(title = "Manzana en MÉXICO",titleWidth = 440),
   dashboardSidebar(width = 230,
    sidebarMenu(
     menuItem("Introducción", tabName = "Intro", icon = icon("archive")),
     menuItem("Manzana Local + Importada", tabName = "Local_Importada", icon = icon("th")),
     menuItem("Manzana Local", tabName = "Local", icon = icon("history")),
     menuItem("Relación Precio-Volumen", tabName = "Precio_Volumen", icon = icon("taxi"))
    )
   ),
  dashboardBody(
   tabItems(   # PRIMER TABULACION
    tabItem(tabName = "Intro", 
            box(title = "   ",status = "primary", solidHeader = TRUE,width = 18,
                includeMarkdown("INTRO.Rmd"))), ### FIN TABULACION INTRODUCCION
    tabItem(tabName = "Local_Importada",tabBox(title = "Manzana Local + Importada",width = 18,id = "Gestion", height = "1300px",
                                               tabPanel("Análisis de Componentes Principales",
                                                        fluidRow( 
                                                          box(title = "Análisis de Componentes Principales",status = "primary", solidHeader = TRUE,width = 12,  
                                                              fluidRow(
                                                                column(3,selectInput("ESTRATO_LI3", "Criterio de Agrupamiento:",
                                                                                     c("Zonas" = "ZONAS","Meses" = "MESES"),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(3,selectInput("FRUTA_1", "Origenes a Considerar:",
                                                                                     c("Local+Importada" = 1,"Importada" = 2,"Local"=3),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(2,selectInput("MOMENTO4", "Momento de Venta:",
                                                                                     c("Todas" = 1,"Cosecha" = 2,"Poscosecha"=3),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(3,selectInput("N_CLUSTER", "Grupos:",
                                                                                     c("1" = 1,"2" = 2,"3"=3,"4"=4,"5"=5,"6"=6,"7"=7),
                                                                                     multiple = FALSE,helpText("Elija")))
                                                              )),
                                                          box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,  
                                                            plotlyOutput("localPlotLI",width = "100%", height = "600px")))),
                                               tabPanel("d3heatmap",
                                                        fluidRow( 
                                                          box(title = "3dheatmap",status = "primary", solidHeader = TRUE,width = 12,  
                                                              fluidRow(
                                                                column(3,selectInput("ESTRATO_LI2", "Criterio de Agrupamiento:",
                                                                        c("Zonas" = "ZONAS","Meses" = "MESES"),
                                                                        multiple = FALSE,helpText("Elija")))  ,
                                                                column(3,selectInput("FRUTA_2", "Origenes a Considerar:",
                                                                                     c("Local+Importada" = 1,"Importada" = 2,"Local"=3),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(2,selectInput("MOMENTO3", "Momento de Venta:",
                                                                                     c("Todas" = 1,"Cosecha" = 2,"Poscosecha"=3),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(2, sliderInput("GRUPO_KF1", "Grupos en Filas:",
                                                                                      min = 1, max = 7, value = 2)),
                                                                column(2, sliderInput("GRUPO_KC1", "Grupos en Columnas:",
                                                                                      min = 1, max = 7, value = 2)),
                                                                  column(2,checkboxInput("TRANSPUESTO", "Transponer", FALSE)),
                                                                column(2,downloadButton('Guarda_3', 'Guardar'))
                                                               )),
                                                          box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
                                                        d3heatmapOutput("heatmapLI",width = "100%", height = "600px")))),
                                               tabPanel("Tanglegrama",
                                                        fluidRow( 
                                                          box(title = "Tanglegrama",status = "primary", solidHeader = TRUE,width = 12,  
                                                              fluidRow(   ### INICIO DE fluidRow 6
                                                                column(1,selectInput("E_MODELO1LI", "Año 1:",
                                                                                     c("2012" = 2012,"2013" = 2013,"2014" = 2014),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(1,selectInput("E_MODELO2LI", "Año 2:",
                                                                                     c("2013" = 2013,"2014" = 2014,"2015" = 2015),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(2,selectInput("ESTRATO_LI", "Agrupamiento:",
                                                                                     c("Zonas" = "MESES","Meses" = "ZONAS"),
                                                                                     multiple = FALSE,helpText("Elija"))) ,
                                                                column(2,selectInput("FRUTA_3", "Origenes a Considerar:",
                                                                                     c("Local+Importada" = 1,"Importada" = 2,"Local"=3),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(2,selectInput("MOMENTO2", "Momento de Venta:",
                                                                                     c("Todas" = 1,"Cosecha" = 2,"Poscosecha"=3),
                                                                                     multiple = FALSE,helpText("Elija"))),
                                                                column(2, sliderInput("GRUPO_K2", "Grupos:",
                                                                                      min = 1, max = 5, value = 2)),
                                                                downloadButton('Guarda_1', 'Guardar')
                                                              )
                                                          ), ### FIN DE BOX
                                                          box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
                                                          plotOutput(outputId ="heatmap2LI",width = "100%", height = "600px")))),
                                               tabPanel("Mapas")
                                               ) ) , ### FIN TABULACION LOC+IMP
  tabItem(tabName = "Local",tabBox(title = "Manzana Local",width = 12,id = "Gestion", height = "1300px",
                                   tabPanel("Análisis de Componentes Principales",
                                            box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
                                            plotlyOutput("localPlot",width = "100%", height = "600px"))),
                                   tabPanel("d3heatmap",
                                            fluidRow(downloadButton('Guarda_4', 'Guardar'),
                                            box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
                                            d3heatmapOutput("heatmap",width = "100%", height = "600px")))),
                                   tabPanel("Tanglegrama",
                                            fluidRow( 
                                              box(title = "Tanglegrama",status = "primary", solidHeader = TRUE,width = 12,  
                                                  fluidRow(   ### INICIO DE fluidRow 6
                                                    column(2,selectInput("E_MODELO1", "Primer Temporada:",
                                                                         c("2011" = 2011,"2012" = 2012,"2013" = 2013,"2014" = 2014),
                                                                         multiple = FALSE,helpText("Elija"))),
                                                    column(2,selectInput("E_MODELO2", "Segunda Temporada:",
                                                                         c("2012" = 2012,"2013" = 2013,"2014" = 2014,"2015" = 2015),
                                                                         multiple = FALSE,helpText("Elija"))),
                                                    column(2,selectInput("ESTRATO", "Agrupamiento:",
                                                                         c("Zonas" = "MESES","Meses" = "ZONAS"),
                                                                         multiple = FALSE,helpText("Elija"))),
                                                    column(2,selectInput("MOMENTO", "Momento de Venta:",
                                                                         c("Todas" = 1,"Cosecha" = 2,"Poscosecha"=3),
                                                                         multiple = FALSE,helpText("Elija"))),
                                                    column(2, sliderInput("GRUPO_K", "Grupos:",
                                                                          min = 1, max = 5, value = 2)),
                                                    downloadButton('Guarda_2', 'Guardar')
                                                  )
                                              ), ### FIN DE BOX
                                              box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
                                              plotOutput(outputId ="heatmap2_1",width = "100%", height = "600px")))),
                                   tabPanel("Mapas")) ), ### FIN TABULACION LOCAL
  tabItem(tabName = "Precio_Volumen",
           tabBox(title = "Relación Precio-Volumen",width = 12,id = "Gestion", height = "1300px",
                 tabPanel("Gráfico de Burbujas",
          fluidRow( 
           box(title = "    ",status = "primary", solidHeader = TRUE,width = 12,  
            fluidRow(   ### INICIO DE fluidRow 6
              column(3,selectInput("ANO_S",label = h5("Año:"),
                                   c("2012" = "A12","2013" = "A13","2014" = "A14","2015" = "A15","2016" = "A16"),multiple = FALSE)),
              column(3,uiOutput("REGION_S",align="center")),
              column(3,uiOutput("ORIGEN_S",align="center"))
            )
           ), ### FIN DE BOX
            box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
            htmlOutput("motionchart",width = "100%", height = "600px"))
           #    , DT::dataTableOutput("table")
          )
                 ),tabPanel("Serie de Tiempo",
                            fluidRow( 
                              box(title = "    ",status = "primary", solidHeader = TRUE,width = 12,  
                                  fluidRow(   ### INICIO DE fluidRow 6
                                    column(3,selectInput("ANO_S2",label = h5("Año:"),
                                    c("2012" = "A12","2013" = "A13","2014" = "A14","2015" = "A15","2016" = "A16"),multiple = TRUE)),
                                    column(3,uiOutput("REGION_S2",align="center")),
                                    column(3,uiOutput("ORIGEN_S2",align="center")),
                                    column(3, sliderInput("SPAN", "span:",
                                                          min = 0.1, max = 2, value = 0.5
                                    ))
                                  )
                              )
                              , ### FIN DE BOX
                           box(title = "   ",status = "primary", solidHeader = TRUE,width = 12,
                              fluidRow(
                            #    plotlyOutput("Serie2",width = "100%", height = "600px"))
                               plotOutput("Serie2",width = "100%", height = "600px"))
                              )
                            )
                  ))
         )  
  )
 )
)
