##################################################################################
#MAIN FILE FOR ACTIVE SIMULATION
#Eric Schulz, June 2015
##################################################################################
#House keeping
rm(list=ls())
sink(file="/dev/null") 
setwd("/home/hanshalbe/Desktop/activesim")

#Load packages
packages <- c('plyr', 'ggplot2', 'jsonlite', 'MASS', 'MCMCpack', 'plyr', 'rjags', 'fields')
lapply(packages, library, character.only = TRUE)

#Load local source files
sources<-c('trans.R', 'truth.R', 'myttb.R', 'mylogistic.R', 'ttboptim.R', 'logoptim.R')
lapply(sources, source)

checkall<-expand.grid(seq(1,100,5),seq(1,100,5))
names(checkall)<-c("noise", "mix")
checkall$prob<-NA

for (m in 1:nrow(checkall)){
  checker<-rep(0,5)  
  noise<-checkall$noise[m]
  mix<-checkall$mix[m]
  
  for (j in 1:5){

behavior<-sample(c(rep("ttb", mix), rep("log", 100-mix)))
behavior[1:noise]<-"ran"
behavior<-sample(behavior)

#Create the data frame
simdata<-data.frame(x1=sample(c(-1,0,1),410, replace=TRUE),
                    x2=sample(c(-1,0,1),410, replace=TRUE),
                    x3=sample(c(-1,0,1),410, replace=TRUE),
                    x4=sample(c(-1,0,1),410, replace=TRUE))
simdata$y<-apply(simdata,1,truth)
simdata$chosen<-0
simdata$chosen[1:10]<-1
for (k in 1:100){
  index<-k*4+10
  
if (behavior[k]=='ran'){
  simdata$chosen[(index-3):index]<-sample(c(0,0,0,1))
}  
if (behavior[k]=='ttb'){
  X<-subset(simdata, chosen==1)
  X$chosen<-NULL
  input<-simdata[(index-3):index,]
  input$y<-NULL
  input$chosen<-NULL
  pred<-ttboptim(X,input)
  pred<-pred+runif(4, 0, 0.0000000001)
  simdata$chosen[(index-3):index]<-ifelse(pred==max(pred),1,0)
}
if (behavior[k]=='log'){
  X<-subset(simdata, chosen==1)
  X$chosen<-NULL
  input<-simdata[(index-3):index,]
  input$y<-NULL
  input$chosen<-NULL
  pred<-logoptim(X,input)
  pred<-pred+runif(4, 0, 0.0000000001)
  simdata$chosen[(index-3):index]<-ifelse(pred==max(pred),1,0)
}
}
sum(simdata$chosen)
logcount<-0
#analyze it
for (l in 1:100){
  index<-l*4+10
  fitset<-simdata[1:(index-4),1:5]
  candset<-simdata[(index-3):(index),1:5]
  ttbpred<-ttboptim(fitset, candset)
  logpred<-logoptim(fitset, candset)
  actual<-simdata$chosen[(index-3):(index)]==1
  logcount<-logcount+ifelse(logpred[actual]>ttbpred[actual],1,0)
}
checker[j]<-ifelse(logcount>50, 1, 0)
}
checkall$prob[m]<-mean(checker)
write.csv(checkall, 'checkall2.csv')
}


checkall<-data.frame(x=1:10, y=1:10)


