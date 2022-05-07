

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "The Diffusion Model?",
                   tabPanel("What is The Diffusion Model",
                            h4("This App creates an interactive widget to demonstrate
                               how altering diffusion models paramters influences 
                               the diffusion process")
                            ),
                   tabPanel("Simulated Diffusion Path",
                            sidebarLayout(
                                sidebarPanel(
                                    h3("Select Values for Diffusion Model Parameters"),
                                    sliderInput("V",
                                                "Select Drift Rate:",
                                                min = -.5,
                                                max = .5,
                                                value = 0),
                                    sliderInput("a",
                                                "Select Boundary Separation:",
                                                min=5,
                                                max=15,
                                                value=10),
                                    sliderInput("z",
                                                "Select Starting Point:",
                                                min=-3,
                                                max=3,
                                                value=0),

                                ),
                                mainPanel(
                                    h2("Gaussian Distribution"),
                                    plotOutput("distPlot")
                                ))
                   
    

    )
))
