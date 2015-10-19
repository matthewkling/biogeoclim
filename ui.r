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
                          #img(src="logo.png"),
                          #imageOutput('logo', height="250px"),
                          
                          p("created by Matthew Kling"),
                          br(),
                          h2(p("Biogeoclim displays relationships between biotic, geographic, and climatic patterns in light of recent and projected climate change.")),
                          br(),
                          h3(p("To get started, type a location of interest in the contiguous US. Optionally, select a radius within which to search for ecosystem types, and a hierarchical level from the National Vegetation Classification system. Then click the tabs across the top to explore aspects of locally-important vegetation types.")),
                          br(),
                          h4(p("Climatic differences drive much of the geographic variation in the ecosystems around us. As these climates change, local species and habitats must either adapt to the new climate, migrate to stay within their baseline climate envelope, or be extripated. While the science of predicting future biogeographic responses to climate change is highly complex and uncertain, biogeoclim aims to help illuminate the underlying qualitative relationships between the dimensions of these systems."))
                 ),
                 
                 tabPanel("Biotic",
                          
                          br(),
                          fluidRow(
                                column(3,
                                       h3(textOutput("biogeo_title")),
                                       wellPanel(
                                             br(),
                                             numericInput('radius', 'Search radius (km)', 25, step=5, min=5),
                                             selectInput("comparison", "Vegetation hierarchy comparison", c("horizontal", "vertical")),
                                             hr(),
                                             uiOutput("localtypes")
                                       )
                                ),
                                column(9, plotOutput('biogeo', height="500px"))
                          ),
                          br(),
                          
                          fluidRow(
                                column(3,
                                       h3(textOutput("bioclim_title")),
                                       wellPanel(
                                             numericInput('prob', 'Probability contour', .90, step=.01, min=.25, max=.99)
                                       )
                                ),
                                column(9, plotOutput('bioclim', height="700px"))
                          )
                          
                          
                          
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
                          
                          
                 )
      )
      
      
      
      
      
      
))


