
#################################################################
# Active pred. var models for Exp 1  
# and testing Confirmatory testing & reward based choices on trial-by-trial basis
# Paula Parpart
#################################################################


rm(list=ls())
packages <- c('plyr', 'ggplot2', 'jsonlite', 'MASS', 'MCMCpack', 'rjags', 'fields', 'httr','plyr', 'curl')
lapply(packages, require, character.only = TRUE)

#################################################################

#as.data.frame shortened
trans<-function(x){
  y<-as.data.frame(x)
  return(y)
}

#Functions
xstring<-function(x){
  y<-as.numeric(substr(x,1,1))
  for (i in 2:4){
    y<-cbind(y,as.numeric(substr(x,i,i)))
  }
  return(y)
}

# truth function for simdata
truth<-function(x){
  y<-1/(1+exp(-(6*x[1]+3*x[2]+1.5*x[3]+0.75*x[4])))
  return(rbinom(1,1,y))
}


#Load local source files
source("Functions_active_stepbystep.R")


#Reading in JSON from Firebase
#################################################
#Gets the data
load("JSON_EXP1.RData")

len<-length(myjson)
#################################################


AIC_TTB <- vector('numeric', len)
AIC_LOG <- vector('numeric', len)
AIC_rand <- vector('numeric', len)
theta <- vector('numeric', len)
theta_log <- vector('numeric', len)
classified_AIC <- vector('numeric', len)

# loop to determine which participant is in what compensatoriness condition
cond <- vector('numeric', len)
id <- vector('numeric', len)
for (q in 1:len){
  id[q] <- q
  d<-myjson[[q]]
  # will already be 2, 4, 6, 8, 10
  cond[q] <- floor(max(d$weights))
}
# frequencies of conditions
cond <- data.frame(id = id, cond)


chosenuncertainty_ratio <- matrix(nrow = 30, ncol = len)
minuncertainty_ratio <-  matrix(nrow = 30, ncol = len)
maxuncertainty_ratio <-  matrix(nrow = 30, ncol = len)

LOG_chosenuncertainty_ratio <- matrix(nrow = 30, ncol = len)
LOG_minuncertainty_ratio <- matrix(nrow = 30, ncol = len)
LOG_maxuncertainty_ratio <- matrix(nrow = 30, ncol = len)


# by keeping length undefined, you can add on to it as much as you want 
alldata <-data.frame()

# fake aliens to start with: cancatenate with the real seen trials
simdata<-data.frame(x1=sample(c(-1,0,1),10, replace=TRUE),
                    x2=sample(c(-1,0,1),10, replace=TRUE),
                    x3=sample(c(-1,0,1),10, replace=TRUE),
                    x4=sample(c(-1,0,1),10, replace=TRUE))
simdata$y<-apply(simdata, 1, truth)
X <- simdata

#All possible comparisons as names
combinations<-c(12, 13, 14 ,23, 24, 34)

for (q in 1:len){
  #Data Munging
  d<-myjson[[q]]
  
  #All comparisons: 30 learning trials
  allcomps<-data.frame(xstring(d$alien1)-xstring(d$alien2),
                       xstring(d$alien1)-xstring(d$alien3),
                       xstring(d$alien1)-xstring(d$alien4),
                       xstring(d$alien2)-xstring(d$alien3),
                       xstring(d$alien2)-xstring(d$alien4),
                       xstring(d$alien3)-xstring(d$alien4))
  
  #Name them 1 to 6
  names(allcomps)<-rep(1:6, each=4)
  
  #Check which one participant has chosen on each of the 30 trials
  chosencomp<-t(apply(d$alienchosen,1, function(x){x[order(x)]}))+1
  allcomps$chosen<-rep(NA,30)
  for (t in 1:30){
    allcomps$chosen[t]<-which(paste0(chosencomp[,1],chosencomp[,2])[t]==paste(combinations))
  }
  
  #Outcome of comparison: if the earlier chosen alien won, make it 1 else 0
  allcomps$outcome<-ifelse(d$tracker1==chosencomp[,1],1,0)
  
  #X <- simdata  # starts with 10 random previous trials and grows every time that a new chosen one was added by the algorithm, until nothing is left with k=100
  allcomps$TTBactive<-rep(NA,30)
  allcomps$LOGactive<-rep(NA,30)
  allcomps$LOGactive_min<-rep(NA,30)
  allcomps$TTBactive_min<-rep(NA,30)
  
  allcomps$Random <-rep(NA,30) 
  Uncertainty <- matrix(ncol=6, nrow=30) 
  Uncertainty_Log <- matrix(ncol=6, nrow=30)
  probs_ttb <- matrix(ncol=6, nrow=30)
  probs_log <- matrix(ncol=6, nrow=30)
  
  allcomps$diss <- rep(NA,30)
  
  for (l in 1:30){ # run through all training trials
    
    conf <- rep(0, 6)
    chosen <- rep(0,6)
    maxvar <- rep(0,6)
    maxvar_log <- rep(0,6)
    conf_log <- rep(0,6)
    
    # after first trial, ttboptim learns from feedback of the last trials too
    if (l > 1){
      lastrial <- c(as.numeric(input[allcomps$chosen[l-1], ]), allcomps$outcome[l-1])
      X <- rbind(X,lastrial)  # starts with 10 random previous trials and grows every time that a new chosen one was added by the algorithm, until nothing is left with k=100         
    }
    
    input <- allcomps[l, ] # next trial 
    input$chosen <- NULL
    input$outcome <- NULL
    input$TTBactive <- NULL
    input$LOGactive <- NULL
    input$Random <- NULL
    input$diss <- NULL
    input$LOGactive_min <- NULL
    input$TTBactive_min <- NULL
    
    # input is now a MATRIX cause i rewrote the myttb.R function
    input <- matrix(input, ncol = 4, nrow=6, byrow = TRUE) # there are 6 rows of comparisons at each trial
    
    # does pred have to be between 0 and 1? it is not, but softmax makes it between 0 and 1
    pred<-ttboptim(X,input) # returns vector of 6 uncertainties (uncertainty sampling) for 6 input comparisons
    p <- pred$probs 
    Uncertainty[l, ] <- pred$variance # matrix of uncertainties for comparison 1 and 2
    allcomps$TTBactive[l]<- which(pred$variance==max(pred$variance))[1] #max var  
    allcomps$TTBactive_min[l]<- which(pred$variance==min(pred$variance))[1] #min var 
    
    # Confirmatory: which comp is furthest away from 0.5, doesnt matter which way around (left > right or right > left)
    thresholded <- ifelse(p > 0.5, p,(1-p))
    conf[which.max(thresholded)[1]] <- 1 
    # contrarty to confirmatory: max uncertainty (closest to 0.5)
    maxvar[which.min(thresholded)[1]] <- 1 
    
    trial <- rep(l, 6)
    id <- rep(q,6)
    chosen[allcomps$chosen[l]] <- 1 
    
    
    ## Active Logistic 
    predlog <-logoptim(X,input) # returns vector of 2 uncertainties for 2 input comparisons
    p_log <- predlog$probs
    Uncertainty_Log[l, ] <- predlog$variance
    
    allcomps$LOGactive[l] <-which(predlog$variance==max(predlog$variance))[1] #ifelse(pred==max(pred),1,0) #which gives position 1 or 2
    allcomps$LOGactive_min[l]<-which(predlog$variance==min(predlog$variance))[1] #min var
    
    # confirmatory: which comp is furthest away from 0.5, doesnt matter which way around (left > right or right > left)
    thresholded_log <- ifelse(p_log > 0.5, p_log,(1-p_log))
    conf_log[which.max(thresholded_log)[1]] <- 1 
    # contrary to confirmatory: max uncertainty (closest to 0.5)
    maxvar_log[which.min(thresholded_log)[1]] <- 1 
    
    confdata <- data.frame(id = id, trial = trial, p_ttb = p, thresholded = thresholded, comparisons = combinations,
                           chosen = chosen, ttbconf = conf, ttbuncertain = maxvar, p_log = p_log, 
                           thresholded_log = thresholded_log, logconf = conf_log, loguncertain = maxvar_log)
    
    
    alldata <- rbind(alldata,confdata)
    
    #  uncertainty ratio
    chosenuncertainty_ratio[l, q] <- Uncertainty[l, allcomps$chosen[l]]/sum(Uncertainty[l, ])
    minuncertainty_ratio[l, q] <- min(Uncertainty[l, ])/sum(Uncertainty[l, ])
    maxuncertainty_ratio[l, q] <- max(Uncertainty[l, ])/sum(Uncertainty[l, ])
    LOG_chosenuncertainty_ratio[l, q] <- Uncertainty_Log[l, allcomps$chosen[l]]/sum(Uncertainty_Log[l, ])
    LOG_minuncertainty_ratio[l, q] <- min(Uncertainty_Log[l, ])/sum(Uncertainty_Log[l, ])
    LOG_maxuncertainty_ratio[l, q] <- max(Uncertainty_Log[l, ])/sum(Uncertainty_Log[l, ])
    
    
    # does this trial discriminate among models?
    allcomps$diss[l] <- (!allcomps$TTBactive[l] == allcomps$LOGactive[l])*1
    
    
    # trial-by-trial prediction of a Random Model, no input
    allcomps$Random[l] <- sample(seq(1:6), 1, prob = rep(1/6, 6)) # sample number between 1-6 with equal prob
    
  } # l loop
  
  
  
  # Optimize over the softmax temperature parameter "theta"
  AIC_TTB[q] <- optim(1, mapll, method="BFGS", Util=Uncertainty, choice=allcomps$chosen, hessian=FALSE)$value  # 
  # $ theta check if its close to 0 as that would mean it didnt work
  theta[q] <- optim(1, mapll, method="BFGS", Util=Uncertainty, choice=allcomps$chosen, hessian=FALSE)$par  # 
  
  ##  Logistic AIC
  AIC_LOG[q]<- optim(1, mapll, method="BFGS", Util=Uncertainty_Log, choice=allcomps$chosen, hessian=TRUE)$value  # 
  theta_log[q] <- optim(1, mapll, method="BFGS", Util=Uncertainty_Log, choice=allcomps$chosen, hessian=TRUE)$par  # 
  
  ## Random Model: likelihood = 0.5 ^ 20. is always going to be same across pp as does not depend on data
  AIC_rand[q] <- -2 * log((1/6)^length(allcomps$chosen[1:30])) + 0 # does not optimize any parameters
  
  # if pp has lower AIC for TTB, TTB model classifies pp, otherwise Log model has lower AIC
  if (AIC_TTB[q] < AIC_LOG[q]){     
    classified_AIC[q]  <- 1     
  } # else stay 0 (Log wins)
  
  
  
} # participant loop


save(alldata, file = "Exp1_paulas_confirmdata.RData")
load(file = "Exp1_paulas_confirmdata.RData")




# average across participants: Uncertainty choices over time
chosen_uncertain <-rowSums(chosenuncertainty_ratio) 
min_uncertain <- rowSums(minuncertainty_ratio)
max_uncertain <- rowSums(maxuncertainty_ratio) 

chosen_uncertain_log <- rowSums(LOG_chosenuncertainty_ratio)
min_uncertain_log <- rowSums(LOG_minuncertainty_ratio)
max_uncertain_log <- rowSums(LOG_maxuncertainty_ratio) 


#   alldata<-cbind(alldata, cond = rep(cond, each = 6*30),theta = rep(theta,each = 30*6), AIC_TTB = rep(AIC_TTB, each = 30*6), 
#                       theta_log = rep(theta_log, each = 30*6), AIC_LOG = rep(AIC_LOG, each = 30*6))


# All data Analysis
m<-glm(chosen~ ttbconf + ttbuncertain + logconf + loguncertain, data=alldata, 
       family="binomial")

summary(m)


# ttb uncerainty alone
mttb<-glm(chosen~ ttbuncertain, data=alldata, 
          family="binomial")
summary(mttb)
with(alldata, {cor(ttbuncertain, as.numeric(chosen))})


# confirmatory doesnt add anything.
mlog<-glm(chosen~  loguncertain, data=alldata, 
          family="binomial")
summary(mlog)
with(alldata, {cor(loguncertain, as.numeric(chosen))})






