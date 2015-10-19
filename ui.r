library(shiny)

shinyUI(fluidPage(
      
      br(),
      fluidRow(
            column(9, h1("toposcope")),
            column(3, textInput("location", " ", "Enter a location (Googleable name in lower 48)"))
      ),
      
      navbarPage(" ",
                 tabPanel("Intro",
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          
                          textOutput(),
                          
                          p("created by Matthew Kling")
                 ),
                 
                 tabPanel("Abiotic",
                          br(),
                          fluidRow(
                                column(3, 
                                       h3(textOutput("analogs_title")),
                                       br(),
                                       wellPanel(
                                             sliderInput('zoom', 'Zoom', 
                                                         8, step=1, min=4, max=12),
                                             sliderInput('neighbors', 'Specificity', 
                                                         .05, step=.01, min=.01, max=.25),
                                             selectInput("reference_period", "Reference period",
                                                         c("1948-1980", "1981-2012", "2041-2070"), "1981-2012"),
                                             selectInput("basemap", "Basemap", 
                                                         c("terrain", "satellite", "roadmap")),
                                             br(),
                                             checkboxInput("analogs_info", "Show explanation")
                                       )),
                                column(9,
                                       plotOutput("analogs", height="600px"))
                          ),
                          
                          conditionalPanel("input.analogs_info == true", textOutput("analogs_description")),
                          
                          br(),
                          fluidRow(
                                column(3,
                                       h3(textOutput("seasons_title")),
                                       br(),
                                       wellPanel(
                                             radioButtons("period_excluded", "Time period NOT to display", 
                                                          c("1948-1980", "1981-2012", "2041-2070"), selected="2041-2070"),
                                             checkboxInput("detrend", "Detrend later time period"),
                                             br(),
                                             checkboxInput("seasons_info", "Show explanation"))
                                ),
                                column(9,
                                       plotOutput("seasonal", height="600px"))),
                          
                          conditionalPanel("input.seasons_info == true", textOutput("seasons_description")),
                          br(),
                          br()
                          
                          
                 ),
                 
                 tabPanel("Biotic",
                          
                          br(),
                          fluidRow(
                                column(3,
                                       h3(textOutput("biogeo_title")),
                                       wellPanel(
                                             br(),
                                             sliderInput('radius', 'Search radius (km)', min=10, max=100, value=25, step=5),
                                             #selectInput("comparison", "Vegetation hierarchy comparison", c("horizontal", "vertical")),
                                             numericInput('prob', 'Probability contour', .90, step=.01, min=.25, max=.99),
                                             hr(),
                                             uiOutput("localtypes"),
                                             br(),
                                             checkboxInput("biotic_info", "Show explanation")
                                       )
                                ),
                                column(9, 
                                       plotOutput('biogeo', height="500px"),
                                       plotOutput('bioclim', height="700px"))
                          ),
                          br(),
                          conditionalPanel("input.biotic_info == true", textOutput("biotic_description")),
                          br()
                          
                          
                          
                          
                 )
      )
      
      
      
      
      
      
))


