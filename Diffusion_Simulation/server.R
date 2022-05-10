

library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(grDevices)
library(RColorBrewer)
library(plotly)
brownian<-function(T,N,delta,z){
    
    # T is time interval of diffusion process
    # N is the number of steps
    # delta determines speed of brownian motion
    # z is starting point of the diffusion process
    dt<-T/N
    # create vector of random variables
    r<-rnorm(N,0,1*delta*sqrt(dt))
    # incorporate starting point with random variables
    r<-c(z,r)
    # Brownian Motion is calculated by a cumulative sum of the random samples
    brown<-cumsum(r)
    return(brown)
}

brownian_drift<-function(T,N,delta,z,v,sigma){
    # T is time interval of diffusion process
    # N is the number of steps
    # delta determines speed of brownian motion
    # z is starting point of the diffusion process
    # v is the drift rate (coefficient)
    # sigma is the variability in drift
    brown<-brownian(T,N,delta,z)
    dt=T/N
    time_steps<-seq(from=0,to=N*dt,length.out = N+1)
    V<-v*time_steps+sigma*brown
        return(V)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    N=1000
    nsim=400
    delta=1
    
    driftdata<-reactive({
        T<-4*abs(input$a/(input$v)*input$sigma)
        if(input$v>=0){T<-4*abs(input$a/(input$v+.1)*input$sigma)}
        t<-seq(from=0,to=N*(T/N),length.out = N+1)
        brown_drift<-matrix(0,nrow=N+1,ncol=nsim)
        for(sim in 1:nsim){
            brown_drift[,sim]<-brownian_drift(T,N,delta,input$z,input$v,input$sigma)
            }
        brown_drift<-as.data.frame(brown_drift)
        x<-cbind(t,brown_drift)
        colnames(x)<-c("Timestep",paste0("Sim",seq(1:nsim)))
        df<-x %>%
            as.data.frame%>%
            select(Timestep, paste0("Sim",seq(1:nsim))) %>%
            gather(key = "Simulation", value = "Evidence", -Timestep)  

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
        if(length(correctRT)>0){correctRT[,2]<-"CorrectRT";colnames(correctRT)<-c('RT','Type')}
        if(length(errorRT)>0){errorRT[,2]<-"ErrorRT";colnames(errorRT)<-c("RT",'Type')}
        RTcomb<-rbind(correctRT,errorRT)
        list(data=na.omit(newdf),RTcomb=RTcomb)
        })

        output$drift<-renderTable({
         head(driftdata()$RTcomb)
     })   
        
        output$RTplot<-renderPlotly({
             RTp<-ggplot(driftdata()$RTcomb,aes(x=RT,fill=Type))+
                geom_histogram(position="identity",alpha=0.5,color="black")+
                ggtitle("RT distributions from 500 simulations \nof Diffusion Process")+
                theme(plot.title=element_text(hjust=.5))
            ggplotly(RTp)
           
        })

     output$DiffusionPlot<-renderImage({ 
         outfile<-tempfile(fileext='.gif')
         
         p<-ggplot(driftdata()$dat,aes(x=Timestep,y=Evidence,color=Simulation))+
             geom_path()+
             geom_hline(yintercept=c(input$a,-input$a),color='red',size=1.5)+
             scale_color_brewer(palette = "Dark2")+
             ggtitle("Simulation of 5 Separate Diffusion Paths \n With Absorbing Boundaries",
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
