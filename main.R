################################################################################
#        Code for Inference Lecture                                            #
#        October / November 2024                                               #
#        John S. Schuler                                                       #
#                                                                              #
################################################################################
library(tidyverse)
library(cowplot)
# read in bodyfat data
read.table("bodyfat.txt",header=TRUE,sep="\t") -> bodyFat

mu <- mean(bodyFat$Waist)
stdv <- sd(bodyFat$Waist)
supp <- seq(min(bodyFat$Waist),max(bodyFat$Waist),.1)
dnorm(supp,mean=mu,sd=stdv) -> gaussDensity

ggplot() + 
  geom_histogram(data=bodyFat,aes(x=Waist,y=..density..)) +
  geom_density(data=bodyFat,aes(x=Waist),color="red") + 
  geom_line(aes(x=supp,y=gaussDensity),color="blue") + xlim(min(supp),max(supp))
  

# now, we can treat resampling from this as the true population distribution
traditionalInf <- function(mu0,sampSize,alpha,side){
  X <- sample(bodyFat$Waist,sampSize,replace=TRUE)
  xBar=mean(X)
  sHat <- sd(X)
  sampDensity <- dnorm(supp,mean=xBar,sd=sHat)
  ggplot() + geom_histogram(aes(x=X,y=..density..),binwidth = 1) +
    geom_density(aes(x=X),color="red") +
    geom_line(aes(x=supp,y=sampDensity),color="blue") +
    labs(title="Sample Histogram",x="Waist",y="Density") +
     xlim(min(supp),max(supp)) + geom_vline(xintercept = xBar,color="gold")  -> plt1 
  
  # now determine sampling distribution from sample
  sampStDv <- sHat/sqrt(sampSize)
  
  dnorm(x=supp,mean=xBar,sd=sampStDv) -> sampDistDensity
  
  ggplot() + geom_line(aes(x=supp,y=sampDistDensity),color="purple") + 
    xlim(min(supp),max(supp)) + geom_vline(xintercept = xBar,color="gold") -> plt2

  # now, set the alpha level
  if (side=="both"){
    aCut <- alpha /2
    qnorm(aCut,mu0,sampStDv) -> a0
    qnorm(1-aCut,mu0,sampStDv) -> a1
  } else if (side=="right"){
    
  } else {
    
  }
  
  ggplot() + geom_line(aes(x=supp,y=sampDistDensity),color="purple") + 
    xlim(min(supp),max(supp)) + geom_vline(xintercept = xBar,color="gold") -> plt2
  
  
    
  plot_grid(plt1,plt2,ncol=1,align="hv")
  
}
