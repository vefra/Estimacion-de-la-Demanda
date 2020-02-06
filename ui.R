require(ggplot2)
require(knitr);require(rmarkdown)
require(shiny)
library(plotly)
#require(shinythemes)

# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
#library(datasets)

# Define the overall UI
shinyUI(
  
  navbarPage(title=div( "Demanda en Ingresos Hospitalarios"),
             theme = shinytheme("cerulean"),

             tabPanel("Series Temporales",
                      img(src="univ.png", style="float:right; padding-right:25px", height="20%", width="20%"),  
                      
                      sidebarPanel(
                      
                      h4("Area Hospitalaria"),
                      
                      # radioButtons("tip", "Tipo de Base",
                      #              choices = c(Comma = "test",
                      #                          Semicolon = "train",
                      #                           selected = "test"),
                      # 
                                   
                      selectInput('areain', 'Escoja el area', 
                                  c('Todos',unique(as.character(prueba$esp_egrpa))),
                                  selectize=TRUE),
                      hr("Cantidad de Ingresos"),
                      verbatimTextOutput("ingresos"),
                      hr("El máximo valor se encuentra acotado por el percentil 99"),
                      verbatimTextOutput("summary"),
                      hr(),
                      verbatimTextOutput('estacion'),
                      hr(),
                      plotlyOutput("plotbox")#,
                      
                      #actionButton("calcular", "Calcular")
                      
                      ),
                      mainPanel(
                        
                        tabsetPanel(
                     
                          tabPanel("Serie",
                                   
                                   fluidRow(
                                     column(4,
                                            hr("ARMA"),
                                            verbatimTextOutput('arimaerr')
                                            ),
                                     column(4,
                                            hr("Holton Winters"),
                                            verbatimTextOutput('expoerr')
                                            ),
                                     column(4,
                                            hr("Redes Neuronales"),
                                            verbatimTextOutput('redneuerr')
                                     )
                                     ),
                                   fluidRow(
                                     hr(),
                                     
                                    plotOutput("seasonplot")
                                    
                                    , dataTableOutput("proy") 
                                    ,verbatimTextOutput('proy2')
                                   )     
                                   
                          ),    
                           
                     tabPanel("ARMA",
                      
                     # Use a fluid Bootstrap layout
                      # fluidPage(theme = "bootstrap.css",    
                      hr(),
                          
                     plotOutput("arimatot"),
                     verbatimTextOutput('arimamod'),
                     plotOutput("arimares"),
                     plotOutput("arimap")
                          
                      ),
             
             tabPanel("Suavización Exponencial",
                          hr(),
                      plotOutput("exposer"),
                      verbatimTextOutput('expomod'),
                      plotOutput("expores")
                        ),
             tabPanel("Redes Neuronales",
                      
                                      hr(),
                      plotOutput("redneu"),
                      verbatimTextOutput('redneumod'),
                      plotOutput("redneures")
                      
                      ))))
             
  
 
  )
   
) 

