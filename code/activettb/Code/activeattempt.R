#Load packages
packages <- c('MCMCpack', 'plyr', 'ggplot2', 'fields')
lapply(packages, require, character.only = TRUE)


#as.data.frame shortened
trans<-function(x){
  y<-as.data.frame(x)
  return(y)
}


#Stick breaking
ericbreaksastick <- function(alpha) {
  betas <- rbeta(100, 1, alpha)
  remaining_stick_lengths <- c(1, cumprod(1 - betas))[1:100]
  weights <- remaining_stick_lengths * betas
  w<-sort(weights, decreasing=TRUE)[1:4]
  w<-w/sum(w)
  #I return 10 here as this will produce 0.99 probs in logistic reg
  return(w*10)
}

#True underlying function
myfunc<-function(x,w){
  y<-0
  p<-0
  for (i in seq_along(x)){p<-p+x[i]*w[i]}
  y<-ifelse(rbinom(1,1,exp(p)/(1+exp(p)))>0.5,1,-1)
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

#Input space
myin<-c(-1,0,1)
inputspace<-expand.grid(myin,myin,myin,myin)
#alpha
we1<-seq(0.000001,1,len=4)
we2<-sort(1/we1[1:3])
we<-c(we1,we2)
myalpha<-we
#Have a look at alpha
print(myalpha)

#Final data frame to collect error
dfinal<-rep(0,7*30); dim(dfinal)<-c(30,7)


for (q in 7:length(myalpha)){

  activeperf<-rep(0,20*30)
  dim(activeperf)<-c(30,20)
  
  pb <- txtProgressBar(min = 0, max = 20, style = 3)

#averaged over 20 runs
for (m in 1:20){
  
  #Jump staty with 5 observations
  w<-ericbreaksastick(myalpha[q])
  
  mydat<-inputspace[sample(1:nrow(inputspace),5),]
  mydat$y<-apply(mydat,1,function(x){myfunc(x,w=w)})
  mydat<-trans(mydat)
  #establishing goundtruth
  truth<-apply(inputspace,1,function(x){myfunc(x,w=w)})
  
  count<-k<-0
  for (n in 6:35){
    #intialize posterior
    myposterior<-rep(0,8)
    dim(myposterior)<-c(4,2)
    myposterior<-trans(myposterior)
    names(myposterior)<-c("w","sd")
    for (l in 1:4){
      #Bayesian Posterior: Paparpart-Method
      myposterior[l,]<-summary((MCMCregress(mydat$y~-1+mydat[,l])*0.5+0.5))$statistics[c(1,4)]
      #Maybe we should check the mixing of this again!
    }
    
    #Simulate 100 particles
    simout<-apply(myposterior,1, function(x){rnorm(n=100,mean=x[1],sd=x[2])})
    #Get ranks
    myranks<-trans(t(apply(simout,1,rank)))
    #transform ranks
    myranks<-apply(myranks,2,function(x){mapvalues(x, from = c(1:4), to = c(4:1),warn_missing = FALSE)})
    
    #Intialize predictions
    out<-rep(0,nrow(inputspace)*100)
    dim(out)<-c(nrow(inputspace),100)
    
    #Experctations over particles and input space points
    for (i in 1:100){
      out[,i]<-apply(inputspace[as.numeric(myranks[i,])],1,myttb)
    }
    
    #predictive variance
    myvar<-apply(out,1,var)
    pred<-ifelse(apply(out,1,mean)>0,1,-1)
    #track sum of variance
    activeperf[n-5,m]<-mean(ifelse(pred==truth,0,1))
    
    #next point=point with highest variance at the moment
    nextpointcand<-inputspace[myvar==max(myvar),]
    #If ties, then sample
    nextpoint<-nextpointcand[sample(1:nrow(nextpointcand),1),]
    #Obersver output
    y<-as.numeric(myfunc(x=t(nextpoint), w=w))
    #Update data frame
    nextdat<-data.frame(nextpoint,y=y)
    mydat[n,]<-nextdat
    #R and its Row names...
    rownames(mydat)<-NULL
  }
  setTxtProgressBar(pb, m)
}
#Get overall mean for active
dfinal[,q]<-apply(activeperf,1,mean)

print(paste(q, "is done!"))
}

image.plot(as.matrix(1-dfinal))
write.csv(dfinal, "ttbactive.csv")
