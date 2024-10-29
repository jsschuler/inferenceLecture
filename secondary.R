library(tidyverse)

read.table("framingham.txt",header=TRUE,sep="\t") -> framingham
framingham %>% select(Cholesterol,Sex) %>%
  ggplot() + geom_histogram(aes(x=Cholesterol,y=..density..,fill=Sex),alpha=.2) +
  labs(x="Cholesterol",y="Density",title="Cholesterol by Sex")



framingham %>% select(Cholesterol,Sex) %>%
  summarise(minChol=min(Cholesterol),
            maxChol=max(Cholesterol)) -> baseSmry

framingham %>% select(Cholesterol,Sex) %>% 
  group_by(Sex) %>% summarise(mn=mean(Cholesterol),
                              cnt=n()
                              ) -> Smry

supp <- seq(baseSmry$minChol,baseSmry$maxChol,.01)

trySampleSize <- function(sampSize){
  # get a sample
  sample(1:nrow(framingham),sampSize,replace=FALSE) -> idx
  
  currSamp <- framingham[idx,]
  
  currSamp %>% select(Cholesterol,Sex) %>% 
    summarise(totCnt=n(),poolStdv=sd(Cholesterol),poolMn=mean(Cholesterol)) %>% 
    transform(poolSampStDv=poolStdv/sqrt(totCnt)) -> pooledStats
  currSamp %>%
    group_by(Sex) %>% summarise(mn=mean(Cholesterol),stdv=sd(Cholesterol),cnt=n()) %>%
    transform(sampStdv=stdv/sqrt(cnt)) -> allDat
  
  
  
 
  
  currSamp %>% ggplot() +
    geom_histogram(aes(x=Cholesterol,y=..density..,fill=Sex),binwidth = 15,alpha=.2) +
    geom_density(aes(x=Cholesterol,color=Sex))
  
  # get male distribution
tibble(X=supp,
       MALE=dnorm(supp,allDat[allDat$Sex=="MALE","mn"],allDat[allDat$Sex=="MALE","stdv"]),
       FEM=dnorm(supp,allDat[allDat$Sex=="FEM","mn"],allDat[allDat$Sex=="FEM","stdv"])) %>%
  pivot_longer(MALE:FEM,names_to = "Sex",values_to="Density") -> densityData
  
  
  currSamp %>% ggplot() +
    geom_histogram(aes(x=Cholesterol,y=..density..,fill=Sex),binwidth = 15,alpha=.2) +
    geom_density(aes(x=Cholesterol,color=Sex))
# now fit normals to each
    ggplot() +
    geom_histogram(data=currSamp,aes(x=Cholesterol,y=..density..,fill=Sex),binwidth = 15,alpha=.2) +
    geom_line(data=densityData,aes(x=X,y=Density,color=Sex))
# now pool for variance
           tibble(X=supp,
                  MALE=dnorm(supp,allDat[allDat$Sex=="MALE","mn"],pooledStats$poolStdv),
                  FEM=dnorm(supp,allDat[allDat$Sex=="FEM","mn"],pooledStats$poolStdv)) %>%
      pivot_longer(MALE:FEM,names_to = "Sex",values_to="Density") -> pooledDensityData
# and fit normals with the pooled variance
           ggplot() +
             geom_histogram(data=currSamp,aes(x=Cholesterol,y=..density..,fill=Sex),binwidth = 15,alpha=.2) +
             geom_line(data=pooledDensityData,aes(x=X,y=Density,color=Sex))

           
# and now the distribution under independence
           tibble(X=supp,
                  Pool=dnorm(supp,pooledStats$poolMn,pooledStats$poolStdv)) -> pooledSamp       
# now determine the sampling distribution of the difference under independence
           
           
}
