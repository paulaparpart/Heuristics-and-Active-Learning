
#### Function file for active simulation main.R ####################################################
# Functions_active.R


# Softmax - optimization function

# choice = allcomps$chosen
# Util = Uncertainty 

mapll <- function(temppar,Util, choice) {
  #exponentiate to make sure it's positive, even throughout optimization
  # 
  temppar <- exp(temppar)
  #exponetiation of the product done once to save time
  pfac<- exp(temppar*Util)
  #probs: renormalizes everyting between 0 and 1
  probs <- pfac/rowSums(pfac)
  #overflow
  probs <- (pmax(probs, 0.00001))
  #undeflow
  probs <- (pmin(probs, 0.99999))
  
  # negative log likelihood
  negloglik<- -2 * sum(log(probs[cbind(1:nrow(probs),choice)])) + 2*1 # temppar is 1 parameter
  #return it
  return(negloglik)
}

# is temp parameter = 0 equivalent to a random model (prob for each option is 0.5)  yes  
# AIC_rand[q] <- -2 * log(0.5^length(allcomps$chosen)) + 0 # does not optimize any parameters



#testing
#X < X
#inputspace <- input

logoptim<-function(X, inputspace){
  inits <- function() {list(b1=0, b2=0, b3=0, b4=0)}
  mylog.jags <- jags.model("log.bug", data = list(N = NROW(X), y=X$y, Var1=X$x1, Var2=X$x2, Var3=X$x3, Var4=X$x4),
                           inits = inits, n.chains = 1, n.adapt = 0)
  #Posterior: the regression weights after seeing some evidence X
  # so this is where the beta weighst are updated automatically. using the fitted  model to generate predicitve posterior distribution?
  # mylog.jags above is just Bayesian graphical model definition, below is were we sample 200 sampels with mcmc
  out.sims <- coda.samples(mylog.jags, c("b1", "b2", "b3", "b4"), n.iter = 200)
  proposals<-data.frame(out.sims[101:200,1:4]) # why only starting at 101? burn-in?
  # proposal contains 100 possible regression weights instead of rank orders
  
  out<-rep(0,nrow(inputspace)*100)
  dim(out)<-c(nrow(inputspace),100)
  #Expectations over particles and input space points
  for (i in 1:100){
    out[,i]<-mylogistic(proposals[i,], inputspace)#out contains probabilities of answer being alien 2 or 3 (bottom alien)
  }
  
  #predictive variance over the probabilities (out) sample where uncertainty is largest
  uncertainty <-apply(out,1,var)

  # probability of winning: die verschiedenen p's fuer die vergleiche
  probs <- apply(out,1, mean) # is this a probability, is not the mean beta weight?
  
  return(list(variance = uncertainty, probs = probs))  
}



# p <- proposals[i, ]
# x <- inputspace
mylogistic<-function(p,x){
  
  # right side is  a vector, different from matrix multiplication on purpose
  # returns prediction for each of the 2 comparisons
  mysum<-p[,1]*as.numeric(x[,1])+p[,2]*as.numeric(x[,2])+p[,3]*as.numeric(x[,3])+p[,4]*as.numeric(x[,4])
  
  # y is the prob(y=1) for each comparison
   prob<-exp(mysum)/(1+exp(mysum))
  return(prob)
}

# x <- inputspace[ ,as.numeric(myranks[i,])]
# weights <- simout[i, ]

myttb<-function(x, weights){
  # x is now a (ordered by rank) matrix and has dimensions: comparisons by features
  
  ttb_predict <- vector("numeric",nrow(x))   
  for (r in 1: nrow(x)){
    v <- 1
    while(x[r,v] == 0){
      # count to the column where there are no more zeros.      
      if (v == ncol(x)){break}
      v <- v + 1    
    }# while loop only breaks when test ~= 0 
    # v will automatically stop at the first 1 or -1 discriminating cue
    
    
    
    # adjusting the valence for negative weights. elementwise multiplication
    # for each row separately
    valence <- sign(as.numeric(x[r, ]) * weights)
    
    if (v == ncol(x)){ 
      ttb_predict[r] <- sample(c(-1,1),1) # guess
    } else {                                             # why the fuck does x[r,v] return a list element still?
      ttb_predict[r] <- valence[v]  # can either be 1 (then 1) or -1 (then -1)
    }
  } # r loop
  
  return(ttb_predict) 
}




#as.data.frame shortened
trans<-function(x){
  y<-as.data.frame(x)
  return(y)
}


# X is data.frame and inputspace is MATRIX now
# X contains y variable as well
# inputspace are new set of comparisons to be predicted
# inputspace <- input

ttboptim<-function(X, inputspace){
  
  myposterior<-rep(NA,8)
  dim(myposterior)<-c(4,2)
  myposterior<-trans(myposterior)
  
  ## This mechanism ignores one feature (the zero variance feature only when the input matrix has that feature)
  # filter out zero-variance predictors
  cov_mat <- cor(X[ ,1:4]) 
  
  if (any(is.na(cov_mat))){
    #which variable is zero variance
    pos <- which(colSums(var(X[ ,1:4])) == 0) # pos 5 
    myposterior <- myposterior[-pos, ]
    X <- X[ ,-pos] # delete that predictor
    
    variables <- names(X[ ,-ncol(X)])# only predictors
    for (i in 1:(ncol(X)-1)){
      myposterior[i,]<-summary(MCMCregress(paste("y ~ ", paste(variables[i]), paste(" -1")), data=X))$statistics[1:2]
      # gives the same result as formula = y~-1+x1, y~-1+x2, y~-1+x3 etc below
    }  
    
  }  else {
    
    # compute all asusual
    myposterior[1,]<-summary(MCMCregress(y~-1+x1, data=X))$statistics[1:2]
    myposterior[2,]<-summary(MCMCregress(y~-1+x2, data=X))$statistics[1:2]
    myposterior[3,]<-summary(MCMCregress(y~-1+x3, data=X))$statistics[1:2]
    myposterior[4,]<-summary(MCMCregress(y~-1+x4, data=X))$statistics[1:2]
    
  }
  
  ## This is creating the proposal TTBs: there are 100
  
  #Simulate 100 particles
  simout<-apply(myposterior,1, function(x){rnorm(n=100,mean=x[1],sd=x[2])})
  #Get ranks
  myranks<-trans(t(apply(abs(simout),1,rank)))
  #transform ranks
  myranks<-apply(myranks,2,function(x){mapvalues(x, from = c(1:4), to = c(4:1),warn_missing = FALSE)})
  
  #Initialize predictions
  out<-rep(0,nrow(inputspace)*100)
  dim(out)<-c(nrow(inputspace),100)
  
  #Expectations over particles and input space points
  for (i in 1:100){
    # inputspace is now a matrix. no need for apply as myttb can handle matrix,
    # myttb adjusts for negative weights now too
    out[,i]<- myttb(inputspace[  ,as.numeric(myranks[i,])], simout[i, ]) 
  }
  
  #predictive variance: 
  # no Shannon entropy: uncertainty sampling (sample where uncertainty is max) 
  uncertainty<-apply(out,1,function(x) var(x == 1))
  
  # probability of winning: die verschiedenen p's fuer die vergleiche
  probs <- apply(out,1, function(x) mean(x == 1)) #forcing out to be 0/1 values to get positive probabilities
  
  # no softmax, just taking uncertainty the way it is but cannot devide by 0
  return(list(variance = uncertainty,  probs = probs))  
}


 