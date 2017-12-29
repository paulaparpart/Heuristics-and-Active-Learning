    
    #################################################################
    # Passive model tests: Behavioural prediction 
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
    
  
    
    
    #Load local source files
    source("Functions_active_stepbystep.R")
    
    
    #Reading in JSON from Firebase
    #################################################
    #Gets the data
    load("JSON_EXP1.RData")
    
    len<-length(myjson)
    #################################################
    
    
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
  
    
    
  
    # by keeping length undefined, you can add on to it as much as you want 
    testdata <-data.frame()
    
    
    #All possible comparisons as names
    combinations<-c(12, 13, 14 ,23, 24, 34)
   
       
    for (q in 1:len){
      
      log.predict <- vector('numeric', 10)
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
      logical <- matrix(nrow = 30, ncol = ncol(allcomps))
      trainset <- matrix(nrow = 30, ncol = 4)
      for (t in 1:30){
        allcomps$chosen[t]<-which(paste0(chosencomp[,1],chosencomp[,2])[t]==paste(combinations))
        
        logical[t, ] <- allcomps$chosen[t] == names(allcomps) 
        trainset[t, ] <- unlist(allcomps[t, logical[t, ]])
        
      }
      #Outcome of comparison: if the earlier chosen alien won, make it 1 else -1 (for cue valdities)
      allcomps$outcome<-ifelse(d$tracker1==chosencomp[,1],1,-1)
      trainset <- as.data.frame(trainset)
      # the same as outcome above as it tracks the alien which won out of the chosen ones
      trainset$outcome <- allcomps$outcome
      
      
      ## ---------  TEST SET  ---------------------------------------------------------------------
      test <-data.frame(xstring(d$alien1b)-xstring(d$alien2b))  # aliens observed at test
      colnames(test)[1] <- "V1"
      test$chosen <- ifelse(d$tracker2 == 1,1,-1) # d$tracker2 = 1(left) or 2 (right)
      # if left = 1 else -1
      ###########################################################
      
      
      
      #######------ Logistic Regression Fit ------------------------------
      DV <- "factor(outcome, levels= c(-1,1))"
      fit <- glm(paste(DV," ~ ", paste(names(trainset)[1:4], collapse= "+"), paste(" -1")), 
                 data = trainset, family = "binomial")
      
      # p(y = 1) probability that it will be left alien
      probs <- predict(fit, newdata = test, type = "response")
      log.predict[probs > 0.5] <- 1
      log.predict[probs < 0.5] <- -1
      #### model predictions ####################
      
      
      
      ############# --------- TTB Fit --------------------------------------------------------
      cue_validities_raw <- vector('numeric', 4) 
      cue_validities <- vector('numeric', 4)
      for (c in 1:4){
        # estimate the ecological cue validity of each cue as v = R/(R+W)
        if (sum(trainset[,c]==trainset[ ,ncol(trainset)]) == 0) { cue_validities[c] <- 0 
        } else  
          cue_validities_raw[c] <- sum(trainset[,c]==trainset[ ,ncol(trainset)])/(sum(trainset[,c]==1)+sum(trainset[,c]==-1))              
          cue_validities[c] <- cue_validities_raw[c] - 0.50    
      }
      cue_order <- order(abs(cue_validities), decreasing = TRUE)
         
      
    
      # crucially the test set is already ordered!
      ttb_prediction <- myttb(test[ ,cue_order],weights = cue_validities)
    
       
      ## collect data
      trial <- seq(1:10)
      id <- rep(q,10)
      ppdata <- data.frame(id = id, trial = trial, test, log.predict = log.predict, probs_log = probs,
                           ttb.predict = ttb_prediction)
                           
                  
      #rbing all participants below each other
      testdata <- rbind(testdata,ppdata)
      
    
    } # n loop for participants
    
    
    
    save(testdata, file = "Exp1_paulas_testdata.RData")

    load(file = "Exp1_paulas_testdata.RData")
    
    ## Random model is added only in the plot file with AIC stuff
    
    
    
    
    
