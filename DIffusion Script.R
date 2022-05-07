setwd("D:/Coursera Data Science Courses/Developing Data Products/Course Project")
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
    # incorporate starting point random variables
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


#Parameters



sigma<-1 # diffusion coefficient
v<-.1 #drift rate
z<-0 # Starting point
a<-15 # Boundary

N=1000
T=2*(a/v*sigma)
nsim=5
delta=1

brown_drift<-matrix(0,nrow=N+1,ncol=nsim)
t<-seq(from=0,to=N*(T/N),length.out = N+1)

for(sim in 1:nsim){
    brown_drift[,sim]<-brownian_drift(T,N,delta,z,v,sigma)
}


driftdata<-data.frame(t,brown_drift)
colnames(driftdata)<-c("Timestep",paste0("Sim",seq(1:nsim)))

df <- driftdata %>%
    select(Timestep, paste0("Sim",seq(1:nsim))) %>%
    gather(key = "Simulation", value = "Evidence", -Timestep)

newdf<-data.frame()
for(i in 1:nsim){
    first<-which(df$Simulation==paste0("Sim",i))[1]  
    ind<-which(df$Simulation==paste0("Sim",i)&(df$Evidence>=a|df$Evidence<=a))[1]
    newdf<-rbind(newdf,df[first:ind,])
}  

cols<-brewer.pal(3,"BuGn")  
pal<-colorRampPalette(cols)

p<-ggplot(newdf,aes(x=Timestep,y=Evidence))+geom_path(aes(color=Simulation))+
    geom_hline(yintercept=c(a,-a),color='red',size=1.5)+
    scale_color_brewer(palette = "Dark2")+
    ggtitle("Simulation of 5 Separate Diffusion Paths \n With Absorbing Boundaries",
            subtitle = paste('v = ',v,'z = ',z,'sigma = ',sigma,'a = ',a))+
    theme(plot.title = element_text(hjust=.5),plot.subtitle = element_text(hjust=.5))+
    transition_reveal(Timestep)
animate(p,nframes=75,renderer=gifski_renderer())
anim_save("diffusion path.gif")


