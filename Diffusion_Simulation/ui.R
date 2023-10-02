

library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(waiter)
library(bslib)
library(thematic)
library(grDevices)
library(RColorBrewer)
library(plotly)

# Define UI for application With two tabs the first tab explains the diffusion model
# and how the application works. The second tab shows the output from parameter manipulations
# of the diffusion model.
thematic_shiny()
darktheme<-bs_theme(version=5,primary = "#0077EB", bootswatch = "superhero")
lighttheme<-bs_theme(version=5,bootswatch = "cosmo")

shinyUI(navbarPage(theme = darktheme,
                   checkboxInput("dark_mode", "Disable/Enable Dark mode"),
                   title = "The Diffusion Model",
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
                             they affect the decision making process. In practice the model has more parameters,
                             but this instance can be viewed as a minimally functional version.")),
                   navbarMenu("Parameters of the Diffusion Model",
                              tabPanel("Drift Rate",
                                       fluidRow(
                                           column(12,
                            h4("Drift Rate (v)"),
                            ("Drift rate is rate of information accumulation and dictates how quickly
                             you accumulate information towards one response boundary or another. Larger
                             positive values indicate faster accumulation towards the upper boundary,
                             while negative values indicate faster accumulation towards the lower boundary.
                             A drift of zero indicates a decision process that is not driven by any information.
                             Drift rate is influenced by factors such as stimulus quality or differences in
                             preference for one item over another.")))),
                            tabPanel("Boundary Separation",
                                     fluidRow(
                                         column(12,
                            h4("Boundary Separation (a)"),
                            ("Boundary separation is a threshold parameter that represents how much information
                             is required before a response is initiated. This is set by an individual and is
                             interpreted as how cautious someone is in their responding. Higher values indicate
                             more caution and result in longer response times and higher accuracy. This can
                             be influenced by instructions such as 'respond as quickly as possible'.")))),
                            tabPanel("Starting Point",
                                     fluidRow(
                                         column(12,
                            h4("Starting point (z)"),
                            ("Starting point represents where the information accumulation process begins. This
                             captures biases in preference for one response over another. Positive values make
                             it more likely and quicker for a response to terminate at the upper boundary and 
                             vice versa for negative values.")))),
                            tabPanel("Noise Of The Diffusion Process",
                                     fluidRow(
                                         column(12,
                            h4("Noise of the Diffusion Process (sigma)"),
                            ("This represents how much noise influences the decision making process. This captures
                             the notion that our minds are not perfect processing machines. It is the reason why
                             'errors' still occur even though a decision is driven by a drift rate. Smaller values
                             indicate less noise, and larger values greater noise. In a typical implementation
                             of the diffusion model, this value is held constant.")))),
                            tabPanel("Non-Decision Time",
                                     fluidRow(
                                         column(5,h4("Non-Decision Time"),
                                                "While the previous parameters are all related to the
                                            decision process, there are many processes ouside of a decision
                                            that the model accounts for with the non-decision time parameter.
                                            These processes can include functions such as the encoding
                                            of a stimulus, or the motor response output. These processes,
                                            while important, are not the central focus of the SCDM and are
                                            better explored with other models. In the SCDM non-decision time
                                            affects RT distributions by shifting them, but not changing the shape
                                            of the overall distribution. In the simulation plots we included nondecision
                                            time but kept it a fixed value since changes in it only shift the RT distribution.
                                       
                                       "),
                                         column(5,offset=2,h4("Across-Trial Variability in Non-Decision Time"),
                                                "Across-trial variability in non-decision time captures fluctuations in processes
                                                     that occur outside the decision process. This corresponds to differences in encoding
                                                     and response exection given the same stimulus. This is combined with non-decision time
                                                     to create a uniform range of values that reflect processes outside of decision making.
                                                     In relation to behavioral data, this parameter influences shifts of the RT distribution
                                                     specifically relative shifts of correct and error RT distributions (Those corresponding
                                                     to the drift distribution vs. responses far away). Again this parameter was kept a fixed
                                                     value for the sake of completeness as it only shifts the RT distribution.")
                                     ),br(),
                                            
                            )),
                   
                            
                            
                            
                   tabPanel("Diffusion Model Plots",
                            sidebarLayout(
                                sidebarPanel(
                                    h3("Select Values for Diffusion Model Parameters"),
                                    h5("(Plots take ~15s to render)"),
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
                                    tabsetPanel(
                                        tabPanel("Plot Descriptions",
                                                 fluidRow(column(12,
                                                                 h2("Plots in Next Tab"),
                                                                 ("The top plot in the 'Diffusion Model Plots' tab shows the time course of 5 individual
                             decision processes. The middle plot shows a histogram of the correct and error (Top and 
                             bottom boundary responses) RTs from 500 simulations of the diffusion process. The bottom
                             plot is a boxplot of correct and error responses that offers a better understanding of
                             the RT quantiles. The correct and error distributions can be toggled by clicking the
                             legend. As the drift is increased the amount of error responses will decrease and the
                             model will no longer predict error RTs. Adjust the sliders on the left sidebar
                             to see how different parameter combinations influence RT distributions and the 
                             time course of the decision process.")))),
                                        tabPanel("Evidence Accumulation and RT Distributions",
                                                 fluidRow(column(12,
                                    plotOutput("DiffusionPlot",width = "500px", height = "500px"),
                                    plotlyOutput("RTplot",width = "1100px", height = "500px")
                                )))
                   
                    )),
))))
