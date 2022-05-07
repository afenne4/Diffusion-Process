

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$distPlot <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- rnorm(200,mean=input$mu,sd=input$SD)
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        title<-ifelse(input$ShowTitle,"Normal Distribution","")
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', main=title,border = 'white',xlim=c(-10,10))
        
    })
    
})
