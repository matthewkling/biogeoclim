library(shiny)

shinyUI(fluidPage(
      
      br(),
      
      sidebarPanel(width=3,
                   
                   tags$style(type="text/css",
                              ".shiny-output-error { visibility: hidden; }",
                              ".shiny-output-error:before { visibility: hidden; }"
                   ),
                   
                   imageOutput('logo', height="250px"),
                   textInput("location", "Location", "Googleable name (or lat, long)"),
                   numericInput('radius', 'Search radius (km)', 25, step=5, min=5),
                   selectInput("comparison", "Vegetation hierarchy comparison", c("horizontal", "vertical")),
                   hr(),
                   
                   uiOutput("localtypes")
      ),
      
      
      
      mainPanel(
            tabsetPanel(type = "tab",
                        tabPanel("Intro",
                                 br(),
                                 h2(p("Biogeoclim displays relationships between biotic, geographic, and climatic patterns in light of recent and projected climate change.")),
                                 br(),
                                 h3(p("To get started, type a location of interest in the contiguous US. Optionally, select a radius within which to search for ecosystem types, and a hierarchical level from the National Vegetation Classification system. Then click the tabs across the top to explore aspects of locally-important vegetation types.")),
                                 br(),
                                 h4(p("Climatic differences drive much of the geographic variation in the ecosystems around us. As these climates change, local species and habitats must either adapt to the new climate, migrate to stay within their baseline climate envelope, or be extripated. While the science of predicting future biogeographic responses to climate change is highly complex and uncertain, biogeoclim aims to help illuminate the underlying qualitative relationships between the dimensions of these systems."))
                        ),
                        tabPanel("Biogeography",
                                 br(),
                                 h2(textOutput("biogeo_title")),
                                 br(),
                                 plotOutput('biogeo', height="500px")), 
                        tabPanel("Bioclimatography",
                                 br(),
                                 h2(textOutput("bioclim_title")),
                                 br(),
                                 numericInput('prob', 'Probability contour', .90, step=.01, min=.25, max=.99), 
                                 plotOutput('bioclim', height="700px")), 
                        tabPanel("Climatogeography",
                                 br(),
                                 h2(textOutput("geoclim_title")),
                                 br(),
                                 fluidRow(
                                       column(4, sliderInput('zoom', 'Zoom', 8, step=1, min=4, max=12)),
                                       column(4, sliderInput('neighbors', 'Specificity', .05, step=.01, min=.01, max=.25)), 
                                       column(4, selectInput("basemap", "Basemap", c("terrain", "satellite", "roadmap")))
                                       
                                 ),
                                 fluidRow(
                                       plotOutput('geoclim', height="600px")))
                        
            )
            
            
            #h1(textOutput('title')),
            #plotOutput('biogeo', height="500px"),
            #plotOutput('bioclim', height="600px")
      )
))