
 
library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(grDevices)
library(RColorBrewer)
library(plotly)
library(gridExtra)
brownian<-function(T,N,delta,z){
    
    # T is a numeric value that represents the time interval of the diffusion process
    # N is a numeric integer that represents the number of steps
    # delta is a numeric value that determines the speed of brownian motion
    # z is a numeric integer that corresponds to the starting point of the diffusion process
    
    dt<-T/N
    # create vector of random variables
    r<-rnorm(N,0,1*delta*sqrt(dt))
    # incorporate starting point with random variables
    r<-c(z,r)
    # Brownian Motion is calculated by a cumulative sum of the random samples
    brown<-cumsum(r)
    # brown is a numeric vector containing the cumulative sum of the random samples
    return(brown)
}

brownian_drift<-function(T,N,delta,z,v,sigma){
    
    # T is a numeric value that represents the time interval of the diffusion process
    # N is a numeric integer that represents the number of steps
    # delta is a numeric value that determines the speed of brownian motion
    # z is a numeric integer that corresponds to the starting point of the diffusion process
    # v is a numeric value corresponding to the drift rate (coefficient)
    # sigma is a numeric value corresponding to the variability in drift
    
    brown<-brownian(T,N,delta,z)
    dt=T/N
    time_steps<-seq(from=0,to=N*dt,length.out = N+1)
    # add in drift and noise to get diffusion process
    V<-v*time_steps+sigma*brown
    # V is a vector of numeric values, of length N*dt, corresponding to brownian
    # motion with drift
        return(V)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # N is a numeric integer that represents the number of steps
    N=1000
    # nsim is a numeric integer that corresponds to the number of simulations that 
    # will be used
    nsim=500
    # delta is a numeric value that determines the speed of brownian motion-this is
    # a fixed value for this application
    delta=1
    
    driftdata<-reactive({
        # T is a value based on user input that will be used to calculate the
        # timesteps in the diffusion process
        T<-4*abs(input$a/(input$v)*input$sigma)
        # Need a constant added for positive drift values
        if(input$v>=0){T<-4*abs(input$a/(input$v+.1)*input$sigma)}
        #Number of timesteps-used for plotting
        t<-seq(from=0,to=N*(T/N),length.out = N+1)
        brown_drift<-matrix(0,nrow=N+1,ncol=nsim)
        #Loop that creates matrix of brownian drift simulations based on user input
        for(sim in 1:nsim){
            brown_drift[,sim]<-brownian_drift(T,N,delta,input$z,input$v,input$sigma)
            }
        brown_drift<-as.data.frame(brown_drift)
        # Combining timesteps with brownian simulations for plotting
        x<-cbind(t,brown_drift)
        colnames(x)<-c("Timestep",paste0("Sim",seq(1:nsim)))
        # Reformat the matrix into a dataframe that can be plotted as a timeseries
        df<-x %>%
            as.data.frame%>%
            select(Timestep, paste0("Sim",seq(1:nsim))) %>%
            gather(key = "Simulation", value = "Evidence", -Timestep)  

        # This loop is used accomplish 2 things. The first is to select 5 different
        # diffusion simulations and only keep values before they cross a boundary ("a" value).
        # The second part is to gather the reaction times for the processes that 
        # cross the top and bottom boundaries (Correct and Error RTs).
        newdf<-data.frame()
        correctRT<-data.frame()
        errorRT<-data.frame()
        for(i in 1:nsim){
            first<-which(df$Simulation==paste0("Sim",i))[1]  
            if(i<6){
                ind<-which(df$Simulation==paste0("Sim",i)&(df$Evidence>=input$a|df$Evidence<=-input$a))[1]
                    if(is.na(ind)){ind<-first+N}
                newdf<-rbind(newdf,df[first:ind,])
           }
             cind<-which(df$Simulation==paste0("Sim",i)&df$Evidence>=input$a)[1]
             eind<-which(df$Simulation==paste0("Sim",i)&df$Evidence<=-input$a)[1]
             if(is.na(cind)){cind<-first+N}
             if(is.na(eind)){eind<-first+N}
             if(cind<eind){correctRT<-rbind(correctRT,df$Timestep[cind])}
             if(cind>eind){errorRT<-rbind(errorRT,df$Timestep[eind])}
            
        }
        # This code is used to consolidate the correct and error RTs into a single 
        # data frame for plotting.
        if(length(correctRT)>0){correctRT[,2]<-"CorrectRT";colnames(correctRT)<-c('RT','Type')}
        if(length(errorRT)>0){errorRT[,2]<-"ErrorRT";colnames(errorRT)<-c("RT",'Type')}
        RTcomb<-rbind(correctRT,errorRT)
        # The two objects returned are a data frame containing the evidence accumulation
        # trajectories of 5 simulations, and another data frame containing the Reaction Times
        list(data=na.omit(newdf),RTcomb=RTcomb)
        })

        #### RT Histogram Plot #####
    RTplot<-reactive({
              RTp<-ggplot(driftdata()$RTcomb,aes(x=RT,fill=Type))+
                 geom_histogram(position="identity",alpha=0.5,color="black")+
                 ggtitle("RT distributions from 500 simulations")+
                 theme(plot.title=element_text(hjust=.5))
             ggplotly(RTp)
    })
    #### RT Boxplot #####
    bxp<-reactive({
        bxp<-ggplot(driftdata()$RTcomb,aes(x=Type))+
            geom_boxplot(aes(y=RT,fill=Type),notch=TRUE,color="black")+
            coord_flip()
            ggplotly(bxp)
    })
    
    output$RTplot<-renderPlotly({
        subplot(RTplot(),bxp(),nrows=2,shareX = TRUE)
    })

     #### Diffusion Process animated ####
     output$DiffusionPlot<-renderImage({ 
         outfile<-tempfile(fileext='.gif')
         
         p<-ggplot(driftdata()$dat,aes(x=Timestep,y=Evidence,color=Simulation))+
             geom_path()+
             geom_hline(yintercept=c(input$a,-input$a),color='red',size=1.5)+
             scale_color_brewer(palette = "Dark2")+
             ggtitle("Simulation of 5 Individual Decisions",
             subtitle = paste('v = ',input$v,'z = ',input$z,'a = ',input$a,'Sigma = ',input$sigma))+
             theme(plot.title = element_text(hjust=.5),plot.subtitle = element_text(hjust=.5))+
             transition_reveal(Timestep)
        
        anim_save("outfile.gif",animate(p,nframes=40))
        list(src="outfile.gif",
             contentType='image/gif',
             width=500,
             height=500
             )
          },deleteFile=TRUE)
     

})
