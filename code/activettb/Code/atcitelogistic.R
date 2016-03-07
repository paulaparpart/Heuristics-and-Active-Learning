#Load packages
#You might have to manually install JAGS on your PC
packages <- c('MCMCpack', 'plyr', 'ggplot2', 'rjags', 'fields')
lapply(packages, require, character.only = TRUE)


#as.data.frame shortened
trans<-function(x){
  y<-as.data.frame(x)
  return(y)
}


#Stick breaking
#I break the stick into 100 pieces, but only use the first 4
ericbreaksastick <- function(alpha) {
#Beta initialize
  betas <- rbeta(100, 1, alpha)
#Remaining stick length
  remaining_stick_lengths <- c(1, cumprod(1 - betas))[1:100]
#Weights to be returned
  weights <- remaining_stick_lengths * betas
  w<-sort(weights, decreasing=TRUE)[1:4]
  w<-w/sum(w)
  #I return 10 here as this will produce 0.99 probs in logistic reg
  return(w*10)
}

#True underlying function
#Takes in the weights and a vector, returns wins or losses
myfunc<-function(x,w){
  y<-0
  p<-0
  for (i in seq_along(x)){p<-p+x[i]*w[i]}
#Logistic
  y<-ifelse(rbinom(1,1,exp(p)/(1+exp(p)))>0.5,1,-1)
  return(y)
}

#Write model for jags
##############THIS IS ALL JAGS#####################
write("
model {
  for (i in 1:N) {
      y[i] ~ dbern(p[i])
      logit(p[i]) <- b0 + b1*Var1[i]+ b2*Var2[i]+ b3*Var3[i]+ b4*Var4[i]
  }
  b0 ~ dnorm(0.0, 0.01)
  b1 ~ dnorm(0.0, 0.01)
  b2 ~ dnorm(0.0, 0.01)
  b3 ~ dnorm(0.0, 0.01)
  b4 ~ dnorm(0.0, 0.01)
}
", file = "log.bug")
#################################################

#Intialize the betas
inits <- function() list(b0 =0, b1=0, b2=0, b3=0, b4=0)

#Function for logistic
mylogfunction<-function(p,x){
  mysum<-p[,1]+p[,2]*x[,1]+p[,3]*x[,2]+p[,4]*x[,3]+p[,5]*x[,4]
  y<-ifelse(exp(mysum)/(1+exp(mysum))>0.5,1,-1)
  return(y)
}

#Input space
myin<-c(-1,0,1)
#Inputspace contains all possible combinations of the inputs
inputspace<-expand.grid(myin,myin,myin,myin)
#alpha, a bit heavy handed
we1<-seq(0.000001,1,len=4)
we2<-sort(1/we1[1:3])
myalpha<-c(we1,we2)
print(myalpha)


#Final data frame to collect error
dfinal<-rep(0,7*30); dim(dfinal)<-c(30,7)

#Loop through all the alphas
for (q in 1:length(myalpha)){
  
#Initialize performance
  activeperf<-rep(0,20*30)
  dim(activeperf)<-c(30,20)
  
#Progressbar, you don't need that
  pb <- txtProgressBar(min = 0, max = 20, style = 3)
  
  #averaged over 20 runs
  for (m in 1:20){
    
    #Jump start with 5 observations, we could think about decreasing that
    w<-ericbreaksastick(myalpha[q])
    #Input
    mydat<-inputspace[sample(1:nrow(inputspace),10),]
    #Result
    mydat$y<-apply(mydat,1,function(x){myfunc(x,w=w)})
    #back to data frame
    mydat<-trans(mydat)
    #establishing goundtruth
    truth<-apply(inputspace,1,function(x){myfunc(x,w=w)})
    
    count<-k<-0
    for (n in 6:35){
      
      mydat2<-mydat
      mydat2$y<-ifelse(mydat2$y==-1,0,1)
      
      #Bayesian logistic regression, this is where the magic happens!
      mylog.jags <- jags.model("log.bug",
                             data = list(N = NROW(mydat2), y = mydat2$y, Var1=mydat2$Var1,Var2=mydat2$Var2,Var3=mydat2$Var3,Var4=mydat2$Var4),
                             inits = inits, n.chains = 1, n.adapt = 0)
      #Posterior
      out.sims <- coda.samples(mylog.jags, c("b0", "b1", "b2", "b3", "b4"), n.iter = 200)
      #Posteriors
      proposals<-trans(out.sims[101:200,1:5])
      
      #Intialize predictions
      out<-rep(0,nrow(inputspace)*100)
      dim(out)<-c(nrow(inputspace),100)
              
      #Experctations over particles and input space points
      for (i in 1:100){
        out[,i]<-mylogfunction(proposals[i,], inputspace)
      }
      
      #predictive variance
      myvar<-apply(out,1,var)
      #aggregated prediction
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


image.plot(as.matrix(1-dfinal), zlim=c(0.6,1),  col=heat.colors(100), main="Logistic")
abline(h=0.5)

write.csv(dfinal, "logactive.csv")

