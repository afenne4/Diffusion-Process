

library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(grDevices)
library(RColorBrewer)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(navbarPage(title = "The Diffusion Model",
                   tabPanel("What is The Diffusion Model?",
                            h2("About the Diffusion Model"),
                            ("This application provides an interactive examination of 
                             a basic rendition of a popular model of decision making in psychological
                             research. This model, referred to as the diffusion model, is a sequential
                             sampling model which is an expansion upon the weiner diffusion process 
                             with drift and absorbing boundaries. In the context of psychology and decision
                             making, the model assumes that decision making is a noisy process and that
                             information is accumulated over time towards a decision boundary, and a 
                             decision is initiated once this threshold is crossed. The diffusion model
                             is specifically applied to simple 2 choice decisions that take less than 
                             2 seconds (e.g. is a presented object an apple or not?). In psychology
                             these decisions often have a correct response, but that does not have to be
                             the case. For example, the decision could be do I want an apple or an orange,
                             in which case the decision has no wrong answer, just a subjective valuation
                             on what is preferred. The model produces predicted response time distributions 
                             in addition to the proportion of responses for each response option. 
                             The model parameters are psychologically interpretable
                             and this app helps provide the user an intuitive understanding of how
                             they affect the decision making process."),
                            h2("Parameters of the Diffusion Model"),
                            h4("Drift Rate (v)"),
                            ("Drift rate is rate of information accumulation and dictates how quickly
                             you accumulate information towards one response boundary or another. Larger
                             positive values indicate faster accumulation towards the upper boundary,
                             while negative values indicate faster accumulation towards the lower boundary.
                             A drift of zero indicates a decision process that is not driven by any information.
                             Drift rate is influenced by factors such as stimulus quality or differences in
                             preference for one item over another."),
                            h4("Boundary Separation (a)"),
                            ("Boundary separation is a threshold parameter that represents how much information
                             is required before a response is initiated. This is set by an individual and is
                             interpreted as how cautious someone is in their responding. Higher values indicate
                             more caution and result in longer response times and higher accuracy. This can
                             be influenced by instructions such as 'respond as quickly as possible'."),
                            h4("Starting point (z)"),
                            ("Starting point represents where the information accumulation process begins. This
                             captures biases in preference for one response over another. Positive values make
                             it more likely for a response to terminate at the upper boundary and vice versa for 
                             negative values."),
                            h4("Noise of the Diffusion Process (sigma)"),
                            ("This represents how much noise influences the decision making process. This captures
                             the notion that our minds are not perfect processing machines. It is the reason why
                             'errors' still occur even though a decision is driven by a drift rate. Smaller values
                             indicate less noise, and larger values greater noise. In a typical implementation
                             of the diffusion model, this value is held constant."),
                            h2("Plots in Next Tab"),
                            ("The top plot in the 'Diffusion Model Plots' tab shows the time course of 5 individual
                             decision processes. ")
                             ),
                            
                            
                   tabPanel("Diffusion Model Plots",
                            sidebarLayout(
                                sidebarPanel(
                                    h3("Select Values for Diffusion Model Parameters"),
                                    h5("(Plots take ~10s to render)"),
                                    sliderInput("v",
                                                "Select Drift Rate (v):",
                                                min = -.5,
                                                max = .5,
                                                step=.1,
                                                value = 0),
                                    sliderInput("a",
                                                "Select Boundary Separation (a):",
                                                min=5,
                                                max=15,
                                                value=10),
                                    sliderInput("z",
                                                "Select Starting Point (z):",
                                                min=-3,
                                                max=3,
                                                value=0),
                                    sliderInput("sigma",
                                                "Select Noise of Diffusion Process (sigma):",
                                                min=.5,
                                                max=1.5,
                                                step=.1,
                                                value=1.0)

                                ),
                                mainPanel(
                                    plotOutput("DiffusionPlot",width = "500px", height = "500px"),
                                    plotlyOutput("RTplot",width = "500px", height = "900px")
                                ))
                   
                    ),
))
