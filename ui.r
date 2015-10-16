library(shiny)



shinyUI(navbarPage("BGC",
                   
                   #title=div(img(src="logo.png")),
                   
                   
                   
                   tabPanel("Intro",
                            tags$style(type="text/css",
                                       ".shiny-output-error { visibility: hidden; }",
                                       ".shiny-output-error:before { visibility: hidden; }"
                            ),
                            #img(src="logo.png"),
                            #imageOutput('logo', height="250px"),
                            textInput("location", "Location", "Googleable name (in lower 48)"),
                            br(),
                            h2(p("Biogeoclim displays relationships between biotic, geographic, and climatic patterns in light of recent and projected climate change.")),
                            br(),
                            h3(p("To get started, type a location of interest in the contiguous US. Optionally, select a radius within which to search for ecosystem types, and a hierarchical level from the National Vegetation Classification system. Then click the tabs across the top to explore aspects of locally-important vegetation types.")),
                            br(),
                            h4(p("Climatic differences drive much of the geographic variation in the ecosystems around us. As these climates change, local species and habitats must either adapt to the new climate, migrate to stay within their baseline climate envelope, or be extripated. While the science of predicting future biogeographic responses to climate change is highly complex and uncertain, biogeoclim aims to help illuminate the underlying qualitative relationships between the dimensions of these systems."))
                   ),
                   
                   tabPanel("Biotic",
                            h3(textOutput("biogeo_title")),
                            br(),
                            
                            sidebarLayout(
                                  sidebarPanel(width=3,
                                        numericInput('radius', 'Search radius (km)', 25, step=5, min=5),
                                        selectInput("comparison", "Vegetation hierarchy comparison", c("horizontal", "vertical")),
                                        hr(),
                                        uiOutput("localtypes"),
                                        numericInput('prob', 'Probability contour', .90, step=.01, min=.25, max=.99)
                                  ),
                                  
                                  mainPanel(
                                        plotOutput('biogeo', height="500px"),
                                        plotOutput('bioclim', height="700px")
                                  )
                            )
                            
                   ), 
                   
                   tabPanel("Abiotic",
                            h3(textOutput("geoclim_title")),
                            br(),
                            
                            sidebarLayout(
                                  sidebarPanel(width=3,
                                        #imageOutput('logo', height="250px"),
                                        sliderInput('zoom', 'Zoom', 
                                                    8, step=1, min=4, max=12),
                                        sliderInput('neighbors', 'Specificity', 
                                                    .05, step=.01, min=.01, max=.25),
                                        selectInput("reference_year", "Reference year",
                                                    c(1980, 2012, 2050), 2012),
                                        selectInput("basemap", "Basemap", 
                                                    c("terrain", "satellite", "roadmap"))
                                        
                                  ),
                                  
                                  mainPanel(
                                        plotOutput("geoclim", height="600px"))
                            )
                   )
                   
                   
                   
))