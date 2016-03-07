ttboptim<-function(X, inputspace){
  myposterior<-rep(NA,8)
  dim(myposterior)<-c(4,2)
  myposterior<-trans(myposterior)
  myposterior[1,]<-summary(MCMCregress(y~-1+x1+x2+x3+x4, data=X))$statistics[1:2]
  myposterior[2,]<-summary(MCMCregress(y~-1+x2, data=X))$statistics[1:2]
  myposterior[3,]<-summary(MCMCregress(y~-1+x3, data=X))$statistics[1:2]
  myposterior[4,]<-summary(MCMCregress(y~-1+x4, data=X))$statistics[1:2]
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
  uncertainty<-apply(out,1,var)
  return((uncertainty+0.1)/sum(uncertainty+0.1))  
}