################################################################################
#        Code for Inference Lecture                                            #
#        October / November 2024                                               #
#        John S. Schuler                                                       #
#                                                                              #
################################################################################
library(tidyverse)
# read in bodyfat data
read.table("bodyfat.txt",header=TRUE,sep="\t") -> bodyFat

mu <- mean(bodyFat$Waist)
stdv <- sd(bodyFat$Waist)
supp <- seq(min(bodyFat$Waist),max(bodyFat$Waist),.1)
dnorm(x=supp,mu,stdv) -> gaussDensity

bodyFat %>% ggplot() + 
  geom_histogram(aes(x=Waist,y=..density..)) +
  geom_density(aes(x=Waist),color="red") + 
  geom_line(aes(x=supp,y=gaussDensity),color="blue")
  
