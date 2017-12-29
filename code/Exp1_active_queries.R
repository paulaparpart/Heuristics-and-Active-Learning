  
  #################################################################
  # WLDD for Exp 1 over time & Re-analysis of Active Simulation
  # Paula Parpart & Eric Schulz
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
  source("Functions_active.R")
  
  
  #Reading in JSON from Firebase
  #################################################
  #Gets the data
  load("JSON_EXP1.RData")
  
  len<-length(myjson)
  #################################################
  
  freq_ttboptim <- vector('numeric', len)
  freq_logoptim <- vector('numeric', len)
  freq_diff <- vector('numeric', len)
  freq_rand <- vector('numeric', len)
  freq_ttboptim_diss <- vector('numeric', len)
  freq_logoptim_diss <- vector('numeric', len)
  freq_rand_diss <- vector('numeric', len)
  freq_ttboptim_MIN <- vector('numeric', len)
  freq_logoptim_MIN <- vector('numeric', len)
  
  
  diss <- vector('list', len)
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
    
    #All possible comparisons as names
    combinations<-c(12, 13, 14 ,23, 24, 34)
    
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
    
    
    # fake aliens to start with: cancatenate with the real seen trials
    simdata<-data.frame(x1=sample(c(-1,0,1),10, replace=TRUE),
                        x2=sample(c(-1,0,1),10, replace=TRUE),
                        x3=sample(c(-1,0,1),10, replace=TRUE),
                        x4=sample(c(-1,0,1),10, replace=TRUE))
    simdata$y<-apply(simdata, 1, truth)
    X <- simdata
    
    ## START DATA: use first 10 trials 
#     start <- matrix(ncol = 4, nrow = 10)   
#     for (t in 1:10){   #nrow(allcomps) 
#         # put in that comparison that was chosen without sorting
#       start[t, ]<- as.numeric(allcomps[t,allcomps$chosen[t]==names(allcomps)])
#     }
#     X <- data.frame(start)
#     names(X) <- c("x1", "x2","x3", "x4")
#     # and attach the feedback they received on the first 5 trials (1 = top alien won, 0 = chosen alien won)
#     X$y <- allcomps$outcome[1:10]
#     
    
    #X <- simdata  # starts with 10 random previous trials and grows every time that a new chosen one was added by the algorithm, until nothing is left with k=100
    allcomps$TTBactive<-rep(NA,30)
    allcomps$LOGactive<-rep(NA,30)
    allcomps$LOGactive_min<-rep(NA,30)
    allcomps$TTBactive_min<-rep(NA,30)

    allcomps$Random <-rep(NA,30) 
    Uncertainty <- matrix(ncol=6, nrow=30) 
    Uncertainty_Log <- matrix(ncol=6, nrow=30)
    allcomps$diss <- rep(NA,30)

    for (l in 1:30){ # run through all training trials
      
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
      Uncertainty[l, ] <- pred 
      allcomps$TTBactive[l]<- which(pred==max(pred))[1] #max var 
      allcomps$TTBactive_min[l]<- which(pred==min(pred))[1] #min var 
      
    
      ## trial-by-trial prediction with Active Logistic 
      pred_log <-logoptim(X,input) # returns vector of 2 uncertainties for 2 input comparisons     
      Uncertainty_Log[l, ] <- pred_log
      allcomps$LOGactive[l]<-which(pred_log==max(pred_log))[1] #max var
      allcomps$LOGactive_min[l]<-which(pred_log==min(pred_log))[1] #min var
      
      # how does shannon entropy look different/information gain?
      
      
      # does this trial discriminate among models?
      allcomps$diss[l] <- (!allcomps$TTBactive[l] == allcomps$LOGactive[l])*1
      
      
      # trial-by-trial prediction of a Random Model, no input
      allcomps$Random[l] <- sample(seq(1:6), 1, prob = rep(1/6, 6)) # sample number between 1-6 with equal prob
      
    } # l loop
    
   
    # record the trial indices of those trials that could discriminate among models
    diss[[q]] <- allcomps$diss 
    # on discriminating trials, how often did participant choose active TTB query
    freq_ttboptim_diss[q] <- sum(allcomps$chosen[allcomps$diss == 1] == allcomps$TTBactive[allcomps$diss == 1])/sum(allcomps$diss == 1)
    freq_logoptim_diss[q] <- sum(allcomps$chosen[allcomps$diss == 1] == allcomps$LOGactive[allcomps$diss == 1])/sum(allcomps$diss == 1)
    freq_rand_diss[q] <- sum(allcomps$chosen[allcomps$diss == 1] == allcomps$Random[allcomps$diss == 1])/sum(allcomps$diss == 1)

    
    # How often did participant chose according to max var? (predictive variance of ttb or logistic model)
    freq_ttboptim[q] <- sum(allcomps$chosen[1:30] == allcomps$TTBactive[1:30])
    freq_logoptim[q] <- sum(allcomps$chosen[1:30] == allcomps$LOGactive[1:30])
    freq_rand[q] <- sum(allcomps$chosen[1:30] == allcomps$Random[1:30])
    # How often did the 2 models predict differenlty? TTB & Logistic 
    freq_diff[q] <- sum(allcomps$LOGactive[1:30] != allcomps$TTBactive[1:30])

    # How often did participants choose according to min var?
    freq_ttboptim_MIN[q] <- sum(allcomps$chosen[1:30] == allcomps$TTBactive_min[1:30])
    freq_logoptim_MIN[q] <- sum(allcomps$chosen[1:30] == allcomps$LOGactive_min[1:30])

    
     # starting at trial 1
    Uncertainty <- Uncertainty[1:30, ]
    # Optimize over the softmax temperature parameter "theta"
    AIC_TTB[q] <- optim(1, mapll, method="BFGS", Util=Uncertainty, choice=allcomps$chosen[1:30], hessian=FALSE)$value  # 
    # $ theta check if its close to 0 as that would mean it didnt work
    theta[q] <- optim(1, mapll, method="BFGS", Util=Uncertainty, choice=allcomps$chosen[1:30], hessian=FALSE)$par  # 
    
    ##  Logistic AIC
    Uncertainty_Log <- Uncertainty_Log[1:30, ]
    AIC_LOG[q]<- optim(1, mapll, method="BFGS", Util=Uncertainty_Log, choice=allcomps$chosen[1:30], hessian=TRUE)$value  # 
    theta_log[q] <- optim(1, mapll, method="BFGS", Util=Uncertainty_Log, choice=allcomps$chosen[1:30], hessian=TRUE)$par  # 

    ## Random Model: likelihood = 0.5 ^ 20. is always going to be same across pp as does not depend on data
    AIC_rand[q] <- -2 * log((1/6)^length(allcomps$chosen[1:30])) + 0 # does not optimize any parameters
  
    # if pp has lower AIC for TTB, TTB model classifies pp, otherwise Log model has lower AIC
    if (AIC_TTB[q] < AIC_LOG[q]){     
      classified_AIC[q]  <- 1     
    } # else stay 0 (Log wins)
    
    
    ##--------- collect WLDD data in LOOP ---------------------------------------------------------------------------    
    
      # What did participants choose?
    collect <- 1:30   # from trial 11 to 30
    for (t in 1:30){   #nrow(allcomps) 
      # store the comparison as character vector that was chosen by the participant at trial l
      x <- t #+ 10
      collect[t]<-paste0(sort(allcomps[x,allcomps$chosen[x]==names(allcomps)]), collapse='')
    }
    
    # collect those choices that the models made
    model_ttb <- 1:30   # from trial 11 to 30
    for (l in 1:30){   #nrow(allcomps) 
      # store the comparison as character vector that was chosen by the participant at trial l
      x <- l #+ 10
      model_ttb[l]<-paste0(sort(allcomps[x,allcomps$TTBactive[x]==names(allcomps)]), collapse='')
    }
    
    model_log<- 1:30   # from trial 11 to 30
    for (l in 1:30){   #nrow(allcomps) 
      # store the comparison as character vector that was chosen by the participant at trial l
      x <- l #+ 10
      model_log[l]<-paste0(sort(allcomps[x,allcomps$LOGactive[x]==names(allcomps)]), collapse='')
    }
    
    model_rand <- 1:30   # from trial 11 to 30
    for (l in 1:30){   #nrow(allcomps) 
      # store the comparison as character vector that was chosen by the participant at trial l
      x <- l #+ 10
      model_rand[l]<-paste0(sort(allcomps[x,allcomps$Random[x]==names(allcomps)]), collapse='')
    }
    
    if (q==1){
      id<-rep(q, 30) # repeat the id as many times as trials
      chosen<-collect
      dat<-data.frame(id, chosen, model_ttb = model_ttb, model_log = model_log, model_rand = model_rand)
    }
    if(q>1){
      id<-rep(q, 30)
      chosen<-collect
      d<-data.frame(id, chosen, model_ttb = model_ttb, model_log = model_log, model_rand = model_rand) 
      dat<-rbind(dat,d)   
    }
    
    
  } # participant loop
  
  

  # plot frequencies of choosing max var
  freq <- data.frame(id = 1:len, freq_ttboptim = freq_ttboptim, freq_logoptim = freq_logoptim, freq_rand = freq_rand, 
                     freq_diff = freq_diff, freq_ttboptim_diss = freq_ttboptim_diss, freq_logoptim_diss =freq_logoptim_diss, 
                     freq_rand_diss = freq_rand_diss, freq_ttboptim_MIN, freq_logoptim_MIN, AIC_TTB = AIC_TTB, theta = theta, AIC_LOG = AIC_LOG, theta_log = theta_log,
                     AIC_rand = AIC_rand)

  
  #save(freq, file = "AICs_frequencies_Exp1_30trials.RData")
  load("AICs_frequencies_Exp1_30trials.RData")
  
  
  x11(width=10, height=8)
  par(mfrow=c(3,1))
  hist(freq$freq_ttboptim, breaks = 10, freq = TRUE, col="red",main="Exp1: Histogram of frequency of choosing MAX(var) option according to ttboptim", 
       xlab="Number of trials (out of 30 trials overall)")
  hist(freq$freq_logoptim, breaks = 10,freq = TRUE, col="red",main="Exp1: Histogram of frequency of choosing MAX(var) option according to logoptim", 
       xlab="Number of trials (out of 30 trials overall)")
  hist(freq$freq_rand, breaks = 10,freq = TRUE, col="red",main="Exp1: Histogram of frequency of choosing as Random model", 
       xlab="Number of trials (out of 30 trials overall)")
  
  
  
  x11(width=10, height=8)
  hist(freq$freq_diff, breaks = 10,freq = TRUE, col="red",main="Exp1: Histogram of models (logoptim + ttboptim) predicting differently (across all participants)", 
       xlab="Number of trials (out of 30 trials overall)")
  

  # average across all AICs from all participants to find overall model fit of active TTB?
  aic <- vector('numeric', 3)
  aic[1] <- mean(freq$AIC_TTB)
  aic[2]<- mean(freq$AIC_LOG)
  aic[3] <- mean(freq$AIC_rand)
  
  se <- vector('numeric', 3)
  se[1] <- sd(freq$AIC_TTB)/sqrt(len)
  se[2] <- sd(freq$AIC_LOG)/sqrt(len)
  se[3] <- sd(freq$AIC_rand)/sqrt(len)
  
  model <- factor(c("TTB", "LOG", "rand"), levels = c("TTB", "LOG", "rand"))
  all <- data.frame(aic = aic, se = se, model = model)
  pdf("AIC_exp1_30trials.pdf", width=8, height=5)
  library(ggplot2)
  ggplot(all, aes(x=model, y=aic), col = "red") + 
    geom_errorbar(aes(ymin=aic-se, ymax=aic+se), width=.1) +
    geom_line() +
    geom_point()+ 
    ylab("AIC")
  
  dev.off()
  
  plot(freq$AIC_TTB)
  plot(freq$AIC_LOG)
  
  
  
  ## ------------------------------------------ Queries: Pattern plots ----------------------------------------------------
  
  # relevel them all ONCE, and map values from factor levels to patterns
  # sort above makes -1 come before 1
  dat$chosen <- ordered(dat$chosen, levels = c("-1000", "0001", "-1001", "-1-100", "0011", "-1-101", "-1011", "-1-111",
                                               "-1-1-10", "0111", "-1111", "-1-1-11", "-1-1-1-1", "1111"))
  
  patterns = c("+000", "+000", "+-00", "++00",  "++00", "++-0", "++-0", "++--","+++0", "+++0", "+++-", "+++-", "++++", "++++")                                         
  dat$combs<-mapvalues(dat$chosen, 
                       from=levels(dat$chosen),
                       to= patterns)
  dat$choice<-1
  dat$count<-ave(dat$choice, paste(dat$combs, dat$id), FUN=length)
  

  # transform and count TTB model choices
  dat$model_ttb <- ordered(dat$model_ttb, levels = c("-1000", "0001", "-1001", "-1-100", "0011", "-1-101", "-1011", "-1-111",
                                                     "-1-1-10", "0111", "-1111", "-1-1-11", "-1-1-1-1", "1111"))
  dat$ttb_combs<-mapvalues(dat$model_ttb, 
                           from=levels(dat$model_ttb),
                           to= patterns)
  dat$ttb_count<-ave(dat$choice, paste(dat$ttb_combs, dat$id), FUN=length)
  
  
  # transform and count LOG model choices
  dat$model_log <- ordered(dat$model_log, levels = c("-1000", "0001", "-1001", "-1-100", "0011", "-1-101", "-1011", "-1-111",
                                                     "-1-1-10", "0111", "-1111", "-1-1-11", "-1-1-1-1", "1111"))
  dat$log_combs<-mapvalues(dat$model_log, 
                           from=levels(dat$model_log),
                           to= patterns)
  dat$log_count<-ave(dat$choice, paste(dat$log_combs, dat$id), FUN=length)
  
  
  # transform and count Rand model choices
  dat$model_rand <- ordered(dat$model_rand, levels = c("-1000", "0001", "-1001", "-1-100", "0011", "-1-101", "-1011", "-1-111",
                                                       "-1-1-10", "0111", "-1111", "-1-1-11", "-1-1-1-1", "1111"))
  
  dat$rand_combs<-mapvalues(dat$model_rand, 
                            from=levels(dat$model_rand),
                            to= patterns)
  dat$rand_count<-ave(dat$choice, paste(dat$rand_combs, dat$id), FUN=length)
  
  
  #save(dat, file = "wldd_choices_model_Exp1_30trials.RData")
  # These factors are already ordered in the saved data frame
  load("wldd_choices_model_Exp1_30trials.RData")
  
  
  ## Now split the above data in all the ways you want: Split 30 trials into first 15 and last 15  ------------------------------------------------------------------------
#   for (p in 1:max(dat$id)){  
#     if (p > 1){
#       
#       first15 <- rbind(first15, dat[dat$id == p, ][1:15,])
#       
#       last15 <- rbind(last15, dat[dat$id == p, ][16:30,])
#     } else {
#       
#       first15 <-dat[dat$id == p, ][1:15,]
#       
#       last15 <-dat[dat$id == p, ][16:30,]
#     }  
#   }
#   
#   #this data determines what the plot below is generated for
#   dat <- last15
#   dat <- first15


  
  ###################### mean
  se<-function(x){return(sd(x)/sqrt(length(x)))}

  # get rid of doubles: THese are the counts we should use for boxplot instead!
  dplot1a<-ddply(dat,~combs+id,summarise,counttrue=mean(count)) 
  dplot2a<-ddply(dat,~ttb_combs+id,summarise,counttrue=mean(ttb_count))
  dplot3a<-ddply(dat,~log_combs+id,summarise,counttrue=mean(log_count))
  dplot4a<-ddply(dat,~rand_combs+id,summarise,counttrue=mean(rand_count))
  
#   dplot1a$counttrue[order(paste0(dplot1a$id, dplot1a$combs))]
#   dplot1a$counttrue[order(paste0(dplot1a$id, dplot1a$combs))]
#   
  
  # summarises how often each comparison occured overall
  dplot1b<-ddply(dplot1a,~combs,summarise,count=sum(counttrue))
  dplot2b<-ddply(dplot2a,~ttb_combs,summarise,count=sum(counttrue))
  dplot3b<-ddply(dplot3a,~log_combs,summarise,count=sum(counttrue))
  dplot4b<-ddply(dplot4a,~rand_combs,summarise,count=sum(counttrue))
  # and devides by all participants: why not just by the number of those who used the query?
  dplot1b$mu<-dplot1b$count/len
  dplot2b$mu<-dplot2b$count/len
  dplot3b$mu<-dplot3b$count/len
  dplot4b$mu<-dplot4b$count/len
  # its a form of counting missing (0) values and normalizing the counts
  
  names(dplot2b)[1]<-"combs"
  names(dplot3b)[1]<-"combs"
  names(dplot4b)[1]<-"combs"
  dfinal<-rbind(dplot1b, dplot2b, dplot3b, dplot4b)
  dfinal$group<-rep(c("empirical", "ttb","logistic", "rand"), each=nrow(dplot1b))
  
  
  x11(width=20, height=8) 
  par(mfrow=c(1,2))
  
  # true counts boxplot
  plot(dplot1a$combs, dplot1a$counttrue, pch="", col="darkred", 
       main="Exp 1: Frequency of choices: Last 15 trials", ylim = c(0,15), xlab="Queries", ylab="Frequency",
       cex.lab=1.8, cex.main=1.8, cex.axis=1.3)
  
#   # true counts boxplot
#   plot(dplot1a$combs, dplot1a$counttrue, pch="", col="darkred", 
#        main="Exp 1: Frequency of choices: First 15 trials", ylim = c(0,15), xlab="Queries", ylab="Frequency",
#        cex.lab=1.8, cex.main=1.8, cex.axis=1.3)
#   
  
  
  lines(dfinal$mu[dfinal$group == "empirical"], type="b", pch=20, lwd = 2, cex=2, col="#000099", bg="blue")
  lines(dfinal$mu[dfinal$group == "ttb"], type="b", pch=23, lwd = 2, cex=2, col="#000099", bg="yellow")
  lines(dfinal$mu[dfinal$group == "logistic"], type="b", pch=24, lwd = 2, cex=2, col="#000099", bg="pink")
  lines(dfinal$mu[dfinal$group == "rand"], type="b", pch=3, lwd = 2, cex=2, col="red", bg="red")
  legend("topright", c("Empricial", "Active TTB model", "Active Logistic model", "Random"), lty=c(1,1),  cex = 1.5,
         lwd=c(2,2), col=c("#000099","#000099", "#000099", "red"), pch = c(20, 23,24, 3),  pt.bg = c("blue", "yellow","pink", "red"))
  
  

  names(dat)
  par(mfrow=c(2,1))
  
  # CORRELATING RAW DATA
  # ERROR not ordered the same
#   dat$count()
#   cor.test(dat$count, dat$ttb_count)
#   cor.test(dat$count, dat$log_count)
#   
  # INcorrect too because still need to correlate the counts of rawcounts_recoded
#   rawcount <- data.frame(combs = dat$combs, count = dat$count, ttb_combs = dat$ttb_combs, ttb_count = dat$ttb_count, log_combs = dat$log_combs, log_count = dat$log_count, rand_combs = dat$rand_combs, rand_count = dat$rand_count)  
#   # recode into different types of queries (1-8) on a trial-by-trial basis
#   rawcount$combs_recoded<-mapvalues(rawcount$combs, 
#                                     from=levels(rawcount$combs),
#                                     to= c(1,2,3,4,5,6,7,8))
#   rawcount$ttb_combs_recoded<-mapvalues(rawcount$ttb_combs, 
#                                         from=levels(rawcount$ttb_combs),
#                                         to= c(1,2,3,4,5,6,7,8))
#   rawcount$log_combs_recoded<-mapvalues(rawcount$log_combs, 
#                                         from=levels(rawcount$log_combs),
#                                         to= c(1,2,3,4,5,6,7,8))
#   rawcount$rand_combs_recoded<-mapvalues(rawcount$rand_combs, 
#                                          from=levels(rawcount$rand_combs),
#                                          to= c(1,2,3,4,5,6,7,8))
#   
#   # now correlate trial-by-trial: ordinal (rank) correlation
#   # 0.11 (this is averaged across 5 conditions of compensatoriness)
#   cor.test(as.numeric(rawcount$combs_recoded), as.numeric(rawcount$ttb_combs_recoded))
#   # 0.11
#   cor.test(as.numeric(rawcount$combs_recoded), as.numeric(rawcount$log_combs_recoded))
#   # 0.13 
#   cor.test(as.numeric(rawcount$combs_recoded), as.numeric(rawcount$rand_combs_recoded))
#   
  
  # none of these correlations give back what Eric did in paper, so what did we calculate?
  
  
  ### CORRELATING Group Means: aggregate Patterns 
  # 0.94
  cor.test(dfinal$mu[dfinal$group == "empirical"], dfinal$mu[dfinal$group == "ttb"])
  #0.99
  cor.test(dfinal$mu[dfinal$group == "empirical"], dfinal$mu[dfinal$group == "logistic"])
  #0.95
  cor.test(dfinal$mu[dfinal$group == "empirical"], dfinal$mu[dfinal$group == "rand"])
  
  
  
  ## Plot all the mean aggregate curves of all models  
  x11(width=10, height=8) 
  pd <- position_dodge(.1)
  p<-ggplot(dfinal, aes(x=combs, y=mu, colour=group)) +
    geom_line(aes(group = group), size=1) +
    geom_point(size=3)
  #+
  #ylab("Mean Judgement\n")+ggtitle("Predictability")+xlab("\nSample Size")+
  #scale_color_manual(values=c("grey60", "black"))+
  theme_classic()+theme(text = element_text(size=24,  family="serif"), legend.position="top")  
  
  p
  

  
  ##################################################################################################################

    # every participant number with every query
      dummy1<-expand.grid(1:max(dplot1a$id), unique(dplot1a$combs))
      names(dummy1)<-c("id", "combs")
      dim(dummy1)
      dummy1$counttrue<-0
      dfinal1<-rbind(dplot1a, dummy1)# put below each other what they actually chose, and all possible comparisons
      dfinal1$pastem<-paste0(dfinal1$id, dfinal1$combs)
  
  # first 264 have to be doubles as all participants chose the simple query (+000) at least once
      dfinal1<-dfinal1[!duplicated(dfinal1$pastem),] # ????
      nrow(dfinal1)==nrow(dummy1)
  
      dummy2<-expand.grid(1:max(dplot2a$id), unique(dplot2a$ttb_combs))
      dim(dummy2)
      names(dummy2)<-c("id", "ttb_combs")
      dummy2$counttrue<-0
      dfinal2<-rbind(dplot2a, dummy2)
      dfinal2$pastem<-paste0(dfinal2$id, dfinal2$ttb_combs)
      dfinal2<-dfinal2[!duplicated(dfinal2$pastem),]
      nrow(dfinal1)==nrow(dfinal2)
      
      dummy3<-expand.grid(1:max(dplot3a$id), unique(dplot3a$log_combs))
      names(dummy3)<-c("id", "log_combs")
      dim(dummy3)
      dummy3$counttrue<-0
      dfinal3<-rbind(dplot3a, dummy3)
      dfinal3$pastem<-paste0(dfinal3$id, dfinal3$log_combs)
      dfinal3<-dfinal3[!duplicated(dfinal3$pastem),]
      nrow(dfinal3)==nrow(dfinal1)
      
      dummy4<-expand.grid(1:max(dplot4a$id), unique(dplot4a$rand_combs))
      dim(dummy4)
      names(dummy4)<-c("id", "rand_combs")
      dummy4$counttrue<-0
      dfinal4<-rbind(dplot4a, dummy4)
      dfinal4$pastem<-paste0(dfinal4$id, dfinal4$rand_combs)
      dfinal4<-dfinal4[!duplicated(dfinal4$pastem),]
      nrow(dfinal4)==nrow(dfinal2)
          
  # now every id occurs exactly 8 times, and ordering happens in same way below for all 4 dfinals
      dtotal<-dfinal1[order(dfinal1$pastem),] #  dfinal1$pastem[order(dfinal1$pastem)]
#   test <- dfinal4[order(dfinal4$pastem), ]
#   identical(test$id, dtotal$id)
  
      dtotal$ttb_counts<-dfinal2$counttrue[order(dfinal2$pastem)] # dfinal2$pastem[order(dfinal2$pastem)]
      dtotal$log_counts<-dfinal3$counttrue[order(dfinal3$pastem)] # dfinal3$pastem[order(dfinal3$pastem)]
      dtotal$rand_counts<-dfinal4$counttrue[order(dfinal4$pastem)] # dfinal4$pastem[order(dfinal4$pastem)]
      #save(dtotal, file = "wldd_choices_Exp1_30_Missingvalues.RData")
      
  
  
  
  
  
  
  ##----------------------------- Data is saved & participants can be selected ------------------------------------------------------------  

  
    # LOAD dtotal:
      load("wldd_choices_Exp1_20_Missingvalues.RData")
  
      #load("wldd_choices_Exp1_30_Missingvalues.RData")
      # determine the condition you are examining: Select a subset of participants as df
      include <- cond$id[cond$cond == 10]    # 2, 4, 6, 8, 10 
      df <- data.frame()
      for (s in 1: length(include)){
        df <- rbind(df, dtotal[dtotal$id == include[s], ] )   
      }
      dtotal <- df
  
  # double check everything again
      cor_ttb <- cor(dtotal$counttrue, dtotal$ttb_counts)
      cor_log <- cor(dtotal$counttrue, dtotal$log_counts)
      cor_rand <- cor(dtotal$counttrue, dtotal$rand_counts)
    
  # need to get se of correlations, by computing correlation first for each individual?
  # n = nrow(dtotal)
  # get from confidence interval 
      ci <- cor.test(dtotal$counttrue, dtotal$ttb_counts)$conf.int
      se_ttb <-(ci[2] - ci[1])/3.92 
      ci <- cor.test(dtotal$counttrue, dtotal$log_counts)$conf.int
      se_log <- (ci[2] - ci[1])/3.92 
      ci <- cor.test(dtotal$counttrue, dtotal$rand_counts)$conf.int
      se_rand <- (ci[2] - ci[1])/3.92 
      

      all_Conds_correlations2 <- data.frame(model = c("TTB", "Logistic", "Rand"), beta = "2", cor = c(cor_ttb, cor_log, cor_rand), se = c(se_ttb, se_log, se_rand))
      
      all_Conds_correlations4 <- data.frame(model = c("TTB", "Logistic", "Rand"), beta = "4", cor = c(cor_ttb, cor_log, cor_rand), se = c(se_ttb, se_log, se_rand))
      
      all_Conds_correlations6 <- data.frame(model = c("TTB", "Logistic", "Rand"), beta = "6", cor = c(cor_ttb, cor_log, cor_rand), se = c(se_ttb, se_log, se_rand))
      
      all_Conds_correlations8 <- data.frame(model = c("TTB", "Logistic", "Rand"), beta = "8", cor = c(cor_ttb, cor_log, cor_rand), se = c(se_ttb, se_log, se_rand))
      
      all_Conds_correlations10 <- data.frame(model = c("TTB", "Logistic", "Rand"), beta = "10", cor = c(cor_ttb, cor_log, cor_rand), se = c(se_ttb, se_log, se_rand))
      
      all_Conds <- rbind(all_Conds_correlations2, all_Conds_correlations4, all_Conds_correlations6, all_Conds_correlations8, all_Conds_correlations10)
     
      # DO NOT relevel factor here, it had a mistake!
      all_Conds <- all_Conds[with(all_Conds, order(model)), ]
  

      save(all_Conds, file = "all_Conds_corr_20trials.RData")
      #load("all_Conds_corr_30trials.RData")
  

  
  library(lme4)
      
      m1a<-lmer(counttrue~ttb_counts+(1|id), data=dtotal)
      summary(m1a)
      
        
      m2a<-lmer(counttrue~ttb_counts+(1+ttb_counts|id), data=dtotal)
      summary(m2a)
      anova(m1a, m2a)  
      
      m1b<-lmer(counttrue~log_counts+(1|id), data=dtotal)
      anova(m1b)
      
        
      m2b<-lmer(counttrue~log_counts+(1+log_counts|id), data=dtotal)
      anova(m2b)
      anova(m1b, m2b)
      
      anova(m2a, m2b)
      ## ttb is better than log after taking into account log
      mstandard<-glm(counttrue~log_counts+ttb_counts+rand_counts, data=dtotal)
      summary(mstandard)
      
        
        ## independent of linear model, ttb is best model fit
      library(randomForest)
      paulas.rf <- randomForest(counttrue ~ log_counts+ttb_counts+rand_counts, data=dtotal, ntree=1000, keep.forest=FALSE, importance=TRUE)
      importance(paulas.rf)
        