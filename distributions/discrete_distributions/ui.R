library(shiny)

shinyUI(fluidPage(
    
    withMathJax(),
    titlePanel(textOutput("title"), windowTitle = "Discrete Distributions"),
#     titlePanel("Discrete Distributions"),
    
    sidebarLayout(position = "right",
        sidebarPanel(
            
            # Choice of distribution (menu)
            selectInput("distribution", 
                        label = "Choose a distribution",
                        choices = c("Beta-Binomial",
                                    "Beta-Geometric",
                                    "Binomial",
                                    "Borel",
                                    "Borel-Tanner",
                                    "COM-Poisson",
                                    "Delaporte",
#                                     "FNCHypergeometric",
#                                     "Generalized Poisson",
                                    "Geometric",
                                    "Haight's Zeta",
#                                     "Hypergeometric",
#                                     "Inverse Binomial",
                                    "Logarithmic",
                                    "Negative-Binomial",
                                    "Poisson",
                                    "Poisson-Lognormal",
#                                     "Poisson-Tweedie", # http://cran.r-project.org/web/packages/poistweedie/poistweedie.pdf
#                                     "Sichel",
                                    "Skellam",
#                                     "WNCHypergeometric",
                                    "Yule-Simon",
                                    "Zeta",
                                    "Zipf"),
                        selected = "Poisson"),
            
            # Choice of parametrization (menu)
            uiOutput("parametrization"),
            
            # Parameter values (sliders)
            uiOutput("param1"),
            uiOutput("param2"),
            uiOutput("param3"),
            uiOutput("param4"),
            # Summary statistics of chosen distribution
            uiOutput("stats")
            
        ),
        
        mainPanel(
            plotOutput("plot"),
            uiOutput("description")
                  )
    )
))