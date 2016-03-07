#Load packages
packages <- c('MCMCpack', 'plyr', 'ggplot2')
lapply(packages, require, character.only = T)

#can't be bothered function change
trans<-function(x){
  y<-as.data.frame(x)
  return(y)
}

#My Take The Best-Function
myttb<-function(x){
  y<-0
  for (i in seq_along(x)){
    y<-ifelse(y==0 & x[i]!=0,x[i],y)
  }
  y<-ifelse(y==0,sample(c(-1,1),1),y)
  return(y)
}


#Tests
#Standard test
myttb(x=c(1,0,0))==1
myttb(x=c(0,-1,0))==-1
myttb(x=c(0,0,1))==1

#Probabilistic test
zeros<-rep(0,30000)
dim(zeros)<-c(3,10000)
sum(apply(zeros,2,myttb))<30

#input space of all availabe inputs
myin<-c(-1,0,1)
inputspace<-expand.grid(myin,myin,myin)
#only 0s contains no information
inputspace<-subset(inputspace, apply(inputspace,1,sum)!=0)


##############################################
#ACTIVE TTB
##############################################
activeperf<-rep(0,20*15)
dim(activeperf)<-c(15,20)
pb <- txtProgressBar(min = 0, max = 20, style = 3)

#averaged over 20 runs
for (m in 1:20){
  #Jump staty with 5 observations
  mydat<-inputspace[sample(1:nrow(inputspace),5),]
  mydat$y<-apply(mydat,1,myttb)
  mydat<-trans(mydat)


count<-k<-0
for (n in 6:20){
  #intialize posterior
  myposterior<-rep(0,6)
  dim(myposterior)<-c(3,2)
  myposterior<-trans(myposterior)
  names(myposterior)<-c("w","sd")
  for (l in 1:3){
    #Bayesian Posterior: Paparpart-Method
    myposterior[l,]<-summary((MCMCregress(mydat$y~-1+mydat[,l])*0.5+0.5))$statistics[c(1,3)]
  }
  
  #Simulate 1000 particles
  simout<-apply(myposterior,1, function(x){rnorm(n=1000,mean=x[1],sd=x[2])})
  #Get ranks
  myranks<-trans(t(apply(simout,1,rank)))
  #transform ranks
  myranks<-apply(myranks,2,function(x){mapvalues(x, from = c(1:3), to = c(3:1),warn_missing = FALSE)})
  
  #Intialize predictions
  out<-rep(0,nrow(inputspace)*1000)
  dim(out)<-c(nrow(inputspace),1000)
  
  #Experctations over particles and input space points
  for (i in 1:1000){
    out[,i]<-apply(inputspace[as.numeric(myranks[i,])],1,myttb)
  }
  
  #predictive variance
  myvar<-apply(out,1,var)
  #track sum of variance
  activeperf[n-5,m]<-sum(myvar)
  
  #next point=point with highest variance at the moment
  nextpointcand<-inputspace[myvar==max(myvar),]
  #If ties, then sample
  nextpoint<-nextpointcand[sample(1:nrow(nextpointcand),1),]
  #Obersver output
  y<-as.numeric(myttb(nextpoint))
  #Update data frame
  nextdat<-data.frame(nextpoint,y=y)
  mydat[n,]<-nextdat
  #R and its Row names...
  rownames(mydat)<-NULL
}
setTxtProgressBar(pb, m)
}
#Get overall mean for active
active<-apply(activeperf,1,mean)

##############################################
#PASSIVE TTB
##############################################

#Intialize
passiveperf<-rep(0,20*15)
dim(passiveperf)<-c(15,20)
pb <- txtProgressBar(min = 0, max = 20, style = 3)
for (m in 1:20){
  #"Jumpstart" even though that unnecessary
  mydat<-inputspace[sample(1:nrow(inputspace),5),]
  mydat$y<-apply(mydat,1,myttb)
  mydat<-trans(mydat)
  
  
  count<-k<-0
  for (n in 6:20){
    #Init posterior
    myposterior<-rep(0,6)
    dim(myposterior)<-c(3,2)
    myposterior<-trans(myposterior)
    names(myposterior)<-c("w","sd")
    #Calculate posterior: Parpart style
    for (l in 1:3){
      myposterior[l,]<-summary((MCMCregress(mydat$y~-1+mydat[,l])*0.5+0.5))$statistics[c(1,3)]
    }
    #Simulate 1000 particles
    simout<-apply(myposterior,1, function(x){rnorm(n=1000,mean=x[1],sd=x[2])})
    #Get ranks
    myranks<-trans(t(apply(simout,1,rank)))
    #Transform ranks
    myranks<-apply(myranks,2,function(x){mapvalues(x, from = c(1:3), to = c(3:1),warn_missing = FALSE)})
    
    #Initialize simulation output
    out<-rep(0,nrow(inputspace)*1000)
    dim(out)<-c(nrow(inputspace),1000)
    
    #Simulate expected output over particles and inputspace
    for (i in 1:1000){
      out[,i]<-apply(inputspace[as.numeric(myranks[i,])],1,myttb)
    }
    
    #Get variance
    myvar<-apply(out,1,var)
    #Track error
    passiveperf[n-5,m]<-sum(myvar)
    
    #PICK NEXT POINT RANDOMLY=> that's the big difference
    nextpoint<-inputspace[sample(1:nrow(inputspace),1),]
    #Observe output
    y<-as.numeric(myttb(nextpoint))
    #Update data frame
    nextdat<-data.frame(nextpoint,y=y)
    mydat[n,]<-nextdat
    #Row names again..
    rownames(mydat)<-NULL
  }
  setTxtProgressBar(pb, m)
}
#Get over all performance mean
passive<-apply(passiveperf,1,mean)

##############################################
#Collect data and plot
##############################################

#Create final data set containing performance over steps for both active and passive learning
dfinal<-data.frame(steps=rep(1:15,2), performance=c(active,passive), method=rep(c("active","passive"),each=15))

#Plot
pdodge <- position_dodge(.1) 
plotcompare<-ggplot(dfinal, aes(x=steps, y=performance, colour=method)) +
  geom_line(position=pdodge)+ggtitle("Generalization error over time")+xlab("Step")+ylab("Performance")

#Save PDF for general use
pdf("comparettb.pdf")
plotcompare
dev.off()

#Save PNG for Email
png("comparettb.png")
plotcompare
dev.off()
