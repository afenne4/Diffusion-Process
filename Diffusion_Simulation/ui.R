

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Random Numbers"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Select number of bins, mean of distribution and SD"),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            sliderInput("mu",
                        "Select mean of distribution:",
                        min=-5,
                        max=5,
                        value=0),
            sliderInput("SD",
                        "Select standard deviation:",
                        min=0.1,
                        max=5,
                        value=1),
            checkboxInput("ShowTitle","Show/Hide Title")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            h2("Gaussian Distribution"),
            plotOutput("distPlot")
        )
    )
))
