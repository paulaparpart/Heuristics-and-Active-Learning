logoptim<-function(X, inputspace){
  inits <- function() {list(b1=0, b2=0, b3=0, b4=0)}
  mylog.jags <- jags.model("log.bug", data = list(N = NROW(X), y=X$y, Var1=X$x1, Var2=X$x2, Var3=X$x3, Var4=X$x4),
                           inits = inits, n.chains = 1, n.adapt = 0)
  #Posterior
  out.sims <- coda.samples(mylog.jags, c("b1", "b2", "b3", "b4"), n.iter = 200)
  proposals<-data.frame(out.sims[101:200,1:4])
  
  out<-rep(0,nrow(inputspace)*100)
  dim(out)<-c(nrow(inputspace),100)
  #Experctations over particles and input space points
  for (i in 1:100){
    out[,i]<-mylogistic(proposals[i,], inputspace)
  }
  
  #predictive variance
  uncertainty<-apply(out,1,var)
  return((uncertainty+0.1)/sum(uncertainty+0.1))  
}