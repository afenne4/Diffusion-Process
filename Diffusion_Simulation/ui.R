

library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(grDevices)
library(RColorBrewer)
# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "The Diffusion Model",
                   tabPanel("What is The Diffusion Model?",
                            h4("This App creates an interactive widget to demonstrate
                               how altering diffusion models paramters influences 
                               the diffusion process")
                            ),
                   tabPanel("Simulated Diffusion Path",
                            sidebarLayout(
                                sidebarPanel(
                                    h3("Select Values for Diffusion Model Parameters"),
                                    h5("(Animation takes ~5s to render)"),
                                    sliderInput("v",
                                                "Select Drift Rate:",
                                                min = -.5,
                                                max = .5,
                                                step=.1,
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
                                    sliderInput("sigma",
                                                "Select Noise of Diffusion Process:",
                                                min=.5,
                                                max=1.5,
                                                step=.1,
                                                value=1.0),

                                ),
                                mainPanel(
                                   # textOutput("var"),
                                   #textOutput("time"),
                                    tableOutput("drift")
                                    ,plotOutput("DiffusionPlot")
                                    
                                ))
                   
    

    )
))
