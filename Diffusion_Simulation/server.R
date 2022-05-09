

library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(grDevices)
library(RColorBrewer)
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
#    xx<-list(brown,time_steps,V)
#    return(xx)
        return(V)
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    N=1000
    nsim=500
    delta=1
    
    driftdata<-reactive({
        T<-4*abs(input$a/(input$v+.1)*input$sigma)
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
        for(i in 1:5){
            first<-which(df$Simulation==paste0("Sim",i))[1]  
            ind<-which(df$Simulation==paste0("Sim",i)&(df$Evidence>=input$a|df$Evidence<=-input$a))[1]
             if(is.na(ind)){ind<-first+N+1}

             newdf<-rbind(newdf,df[first:ind,])
         }
         na.omit(newdf)
        })

        output$drift<-renderTable({
         head(driftdata())
     })   


     output$DiffusionPlot<-renderImage({ 
         outfile<-tempfile(fileext='.gif')
         
         p<-ggplot(driftdata(),aes(x=Timestep,y=Evidence,color=Simulation))+
             geom_path()+
             geom_hline(yintercept=c(input$a,-input$a),color='red',size=1.5)+
             scale_color_brewer(palette = "Dark2")+
             ggtitle("Simulation of 5 Separate Diffusion Paths \n With Absorbing Boundaries",
             subtitle = paste('v = ',input$v,'z = ',input$z,'a = ',input$a,'Sigma = ',input$sigma))+
             theme(plot.title = element_text(hjust=.5),plot.subtitle = element_text(hjust=.5))+
             transition_reveal(Timestep)
        
        anim_save("outfile.gif",animate(p,nframes=40))
        list(src="outfile.gif",
             contentType='image/gif'
             # width=400,
             #height = 300,
             # alt = 'This is alternate text
             )
     },deleteFile=TRUE)
    
})
