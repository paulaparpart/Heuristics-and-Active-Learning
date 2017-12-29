    
    
###### Behavioural Model FITS ACROSS ALL PARTICIPANTS ######################################## 
    rm(list=ls())
    packages <- c('plyr', 'ggplot2', 'jsonlite', 'MASS', 'MCMCpack', 'rjags', 'fields', 'httr','plyr', 'curl')
    lapply(packages, require, character.only = TRUE)
    
      
       
############# Predictive accuracy (% correct) of TTB and LOG by CONDITIONS:
      ## Derive pp IDs for conditions from active data
      load(file = "Exp1_paulas_activedata.RData")
      
      load(file = "Exp1_paulas_testdata.RData")
      
      
      ##### by COND
      indice <- sort(unique(alldata$cond))                     
      ACC_TTB <- vector('numeric', 5)
      ACC_LOG <- vector('numeric', 5)
      se_acc_ttb <- vector('numeric', 5)
      se_acc_log <- vector('numeric', 5)
      
      for (c in 1:length(indice)){
        
        # individual correlations by condition
        newdata <- alldata[alldata$cond == indice[c], ]
        pp_cond <- unique(newdata$id)
        
        ttb_acc <- vector('numeric', length(pp_cond))
        log_acc <- vector('numeric', length(pp_cond))
       
        for (i in 1:length(pp_cond)){
        # for each individual first to get standard error
        cond <- subset(testdata,testdata$id == pp_cond[i])
        ttb_acc[i]  <- mean(cond$chosen == cond$ttb.predict)
        log_acc[i]  <- mean(cond$chosen == cond$log.predict)
     
        }
       
        ACC_TTB[c] <- mean(ttb_acc)
        se_acc_ttb[c] <- sd(ttb_acc)/sqrt(length(ttb_acc))
        ACC_LOG[c] <- mean(log_acc)
        se_acc_log[c] <- sd(log_acc)/sqrt(length(log_acc))
       
      }
      
      
      d <- data.frame(cond = c(indice, indice))
      d$acc <- c(ACC_TTB, ACC_LOG)
      d$se <- c(se_acc_ttb, se_acc_log)
      d$Model<-rep(c("Take The Best", "Logistic"), each=5)
      
      pd <- position_dodge(.1)
      p<-ggplot(d, aes(x=cond, y=acc, colour=Model)) +
        geom_errorbar(aes(ymin=acc-se, ymax=acc+se), width=0.5, size=0.5, position=pd) +
        
        geom_line(position=pd, size=1) +
        geom_point(position=pd)+xlab(expression(beta[1]))+
        ylab("% correct")+ggtitle("Prediction")+theme(text = element_text(size=18), legend.position="top")
      
      x11(width=10, height=8)
      p 
      
      
      pdf("Exp1_predictive_acc_byCOND.pdf")
      p
      dev.off()
      
      png('Exp1_predictive_acc_byCOND.png',width = 600, height = 600)
      p
      dev.off()
      
      ##############################################################
      
      
      
      
      
      load(file = "Exp1_paulas_testdata.RData")
      
      
      ########## Aggregate stats:
      # correlation of predictive variance across all participants
      cor_log <- with(testdata, {cor(log.predict, as.numeric(chosen))}) # 
      cor_ttb <- with(testdata, {cor(ttb.predict, as.numeric(chosen))}) # 
      rsquae_ttb <- cor_ttb^2
      rsquae_log <- cor_log^2    # log = 0.30 (large!)  versus  ttb = 0.05 (small)
      
      # compute Pseudo- Rsquare across all participants
      # already has a softmax parameter for each participant from glm fitting
      DV <- "factor(chosen, levels= c(-1,1))"
      fit <- glm(paste(DV," ~ log.predict"), data=testdata, family="binomial")
      pseudo_rsquare_log <- 1 - (fit$deviance/fit$null.deviance)  # 
      
      fit2 <- glm(paste(DV," ~ ttb.predict"), data=testdata, family="binomial")
      pseudo_rsquare_ttb <- 1 - (fit2$deviance/fit2$null.deviance)  #
      ###############################################################################################
      
      
      
      
      
      ## uses testdata 
      load(file = "Exp1_paulas_testdata.RData")
      
      # getting beh. model predictions AIC's and Rsquares for each pp
      
      AIC_weight_TTB <- rep(0, max(testdata$id))
      AIC_weight_Log <- rep(0, max(testdata$id))
      
      DV <- "factor(chosen, levels= c(-1,1))"
      dist_aic <- Log_user_beh <- accuracy_rand <-accuracy_ttb <- accuracy_log <- logaic<-ttbaic<-randaic<-pseudo_rlog<-pseudo_rttb<-pseudo_rrand<-cor_ttb<-cor_log<-cor_rand<-rep(0, max(testdata$id))
      for (i in 1:max(testdata$id)){
        dummy<-subset(testdata, id==i)
        #dummy$rand <- sample(c(1,-1), 10, replace = TRUE)
          
        accuracy_log[i] <- with(dummy, {mean(chosen == log.predict)}) 
        accuracy_ttb[i] <- with(dummy, {mean(chosen == ttb.predict)}) 
        accuracy_rand[i] <- with(dummy, {mean(chosen == sample(c(1,-1), 10, replace = TRUE))}) 
                
        # AIC's of predictive variance predicting the choices
        randaic[i]<-AIC(glm(paste(DV," ~ sample(c(1,-1), 10, replace = TRUE)"), data=dummy, family="binomial"))
        #randaic[i]<- AIC(glm(paste(DV," ~ rand "), data=dummy, family="binomial"))
        logaic[i]<-AIC(glm(paste(DV," ~ log.predict"), data=dummy, family="binomial"))
        ttbaic[i]<-AIC(glm(paste(DV," ~ ttb.predict"), data=dummy, family="binomial"))
        
        
        ## Compute Akaike Weights (Wagenmakers)
        delta_AIC_TTB <- ttbaic[i] - min(logaic[i], ttbaic[i])
        delta_AIC_Log <- logaic[i] - min(logaic[i], ttbaic[i])
        AIC_weight_TTB[i] <- exp(-1/2 * delta_AIC_TTB)/sum(exp(-1/2 * delta_AIC_TTB), exp(-1/2 * delta_AIC_Log))
        AIC_weight_Log[i] <- exp(-1/2 * delta_AIC_Log)/sum(exp(-1/2 * delta_AIC_TTB), exp(-1/2 * delta_AIC_Log))
        
        # measuring distance on individual basis and classify people
        dist_aic[i] <- ttbaic[i] - logaic[i]
        
        if(ttbaic[i] > logaic[i]){
          # if logaic was better, classify as Logistic user (1)
          # compare to users for active models
          Log_user_beh[i] <- 1
        }
         
        # compute Pseudo-Rsquare from AIC
        fitlog <- glm(paste(DV," ~ log.predict"), data=dummy, family="binomial")
        # fit$null.deviance and fit$deviance = residual deviance
        pseudo_rlog[i] <- 1 - (fitlog$deviance/fitlog$null.deviance)
        
        fitttb <- glm(paste(DV," ~ ttb.predict"), data=dummy, family="binomial")
        # fit$null.deviance and fit$deviance = residual deviance
        pseudo_rttb[i] <- 1 - (fitttb$deviance/fitttb$null.deviance)
        
        fitrand <- glm(paste(DV," ~ sample(c(1,-1), 10, replace = TRUE)"), data=dummy, family="binomial")
        # fit$null.deviance and fit$deviance = residual deviance
        pseudo_rrand[i] <- 1 - (fitrand$deviance/fitrand$null.deviance)
        
        # pearsons correlation per participant
        cor_ttb[i] <- with(dummy, {cor(ttb.predict, as.numeric(chosen))}) 
        cor_log[i] <- with(dummy, {cor(log.predict, as.numeric(chosen))}) 
        cor_rand[i] <- with(dummy, {cor(sample(c(1,-1), 10, replace = TRUE), as.numeric(chosen))}) 
        
      }
    ## ----- counting number of people better fit by LOG AIC and TTB AIC in Prediction
      load("Exp1_tabledata.RData")
      cross$Log_user_beh <- Log_user_beh ## attach Prediction measure
      save(cross, file = "Exp1_tabledata.RData")
      
      
      ### add the behavioural AIC weights to the active AIC weights dataframe
      load("AIC_weights_data.RData")
      AIC_weights$AIC_weight_TTB_passive <- AIC_weight_TTB
      AIC_weights$AIC_weight_Log_passive <-  AIC_weight_Log
      
      
      AICs_passive <- data.frame(id = 1:264, passive_randaic = randaic, passive_logaic = logaic, passive_ttbaic = ttbaic, 
                                 pseudo_rrand = pseudo_rrand, pseudo_rlog =pseudo_rlog, pseudo_rttb = pseudo_rttb)
      save(AICs_passive, file = "AICs_passive.RData")
      
      
      
      passive_randaic <- randaic
      #randaic[i]<- AIC(glm(paste(DV," ~ rand "), data=dummy, family="binomial"))
      passive_logaic<- logaic
      passive_ttbaic <- ttbaic
      
      
      se_logaic <- sd(logaic)/sqrt(length(logaic))
      se_ttbaic <- sd(ttbaic)/sqrt(length(ttbaic))
      se_randaic <- sd(randaic)/sqrt(length(randaic))
      se_pseudo_rlog <- sd(pseudo_rlog)/sqrt(length(pseudo_rlog))
      se_pseudo_rttb <- sd(pseudo_rttb)/sqrt(length(pseudo_rttb))
      se_pseudo_rrand <- sd(pseudo_rrand)/sqrt(length(pseudo_rrand))
      
     
      
      
      ##-------------------------- Barplot % predictive accuracy at prediction (Test data):
      # from above
      se_accuracy_log <- sd(accuracy_log)/sqrt(length(accuracy_log))
      se_accuracy_ttb <- sd(accuracy_ttb)/sqrt(length(accuracy_ttb))
      se_accuracy_rand <- sd(accuracy_rand)/sqrt(length(accuracy_rand))

      d <- data.frame(numeric(3))
      d$accs <- c(mean(accuracy_log),mean(accuracy_ttb), mean(accuracy_rand)) * 100
      d$se <- c(se_accuracy_log, se_accuracy_ttb,  se_accuracy_rand) * 100
      d$Model <- factor(c("Logistic", "TTB", "Random"), 
                       levels = c("Logistic", "TTB", "Random"))
      
      # When the data contains y values in a column, use stat="identity"
      pd <- position_dodge(.1)  
      b <- ggplot(d, aes(x = Model, y = accs, colour=Model)) + geom_bar(stat = "identity", colour="black") +
        geom_errorbar(aes(ymin=accs-se, ymax=accs+se), width=0.5, size=0.5, position=pd) +
        ggtitle("Correct Predictions")+
        ylab("Predictive accuracy (%)") + 
        theme(text = element_text(size=18), axis.text.x= element_text(size=30), axis.title.y = element_text(size=20), legend.position="none") + 
        xlab("") +
        coord_cartesian(ylim=c(0,90))
      b
      
      
      
      x11(width=10, height=8)
      b
      
      pdf("Exp1_prediction_barplot_accuracy.pdf")
      b
      dev.off()
      
      #--------------------------------------------------------------
      
      
      
       
      ##-------------------------- Barplot AIC at prediction (Test data):
      load(file = "AICs_passive.RData")
      names(AICs_passive) <- c("id", "randaic", "logaic", "ttbaic")
      
      attach(AICs_passive)
      
      se_logaic <- sd(logaic)/sqrt(length(logaic))
      se_ttbaic <- sd(ttbaic)/sqrt(length(ttbaic))
      se_randaic <- sd(randaic)/sqrt(length(randaic))
      
    
      d <- data.frame(numeric(3))
      d$aic <- c(mean(logaic),mean(ttbaic),mean(randaic))
      d$se <- c(se_logaic, se_ttbaic,  se_randaic)
      d$Model <-factor(c("Logistic", "TTB", "Random"), 
                       levels = c("Logistic", "TTB", "Random")) #for barplot below
      b_data <- d # for plotting below
      
      # When the data contains y values in a column, use stat="identity"
      pd <- position_dodge(.1)  
      b <- ggplot(d, aes(x = Model, y = aic, colour=Model)) + geom_bar(stat = "identity")+ #, fill="white", colour="black") +
        geom_errorbar(aes(ymin=aic-se, ymax=aic+se), width=0.5, size=0.5, position=pd) +
        #ggtitle("Testset Model Fits (accross all participants)")+
        theme(text = element_text(size=18), axis.text.x= element_text(size=30), axis.title.y = element_text(size=30), legend.position="none") + 
        coord_cartesian() + # like ylim but bars dont disappear
        xlab("") +
        ylab("AIC")
      b
        
      
      x11(width=10, height=8)
      b
      
      pdf("Exp1_bahavioural_barplot_aic.pdf")
      b
      dev.off()
      
      #-------------------------------------------------------------------------------------
      
      
      ##--------------------------------- Pseudo- Rsquare Passive: 
      load(file = "AICs_passive.RData")
      attach(AICs_passive)
      
      
      se_pseudo_rlog <- sd(pseudo_rlog)/sqrt(length(pseudo_rlog))
      se_pseudo_rttb <- sd(pseudo_rttb)/sqrt(length(pseudo_rttb))
      se_pseudo_rrand <- sd(pseudo_rrand)/sqrt(length(pseudo_rrand))
      
    
      d <- data.frame(numeric(3))
      d$Rsquare <- c(mean(pseudo_rlog),mean(pseudo_rttb),mean(pseudo_rrand))
      d$se <- c(se_pseudo_rlog, se_pseudo_rttb, se_pseudo_rrand)
      d$Model <-factor(c("Logistic", "TTB", "Random"), 
                       levels = c("Logistic", "TTB", "Random")) #for barplot below
      
      b_data <- d
      
      # When the data contains y values in a column, use stat="identity"
      pd <- position_dodge(.1)  
      b <- ggplot(d, aes(x = Model, y = Rsquare, colour = Model)) + geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin=Rsquare-se, ymax=Rsquare+se), width=0.5, size=0.5, position=pd) +
       # ggtitle("Behavioural Model Fit (Testset)")+
        theme(text = element_text(size=18),axis.title= element_text(size=30), axis.text= element_text(size=22),  legend.position="none") + 
        ylab("Pseudo-Rsquare")
      
      x11(width=10, height=8)
      b
      
      pdf("Exlp1_prediction_barplot_PseudoRsquare.pdf")
      b
      dev.off()
      
      

###### ACTIVE FIT ACROSS ALL PARTICIPANTS ######################################## 
    
    load(file = "Exp1_paulas_activedata.RData")
    ## there are 6 options for each trial 
    ## so chance of model prediction (ttbuncertain or loguncertain) hitting the right choice out of 6 is much lower natrually (1/6th)
    ## in the test phase above there are only 1 out of 2 options per trial that the model and pp chooses, so hitting right choice by chance
    ## is 0.5 (1/2)
    ## this explains the overall larger aic's in active
      
## Aggregate stats       
    # correlation of predictive variance across all participants
    cor_ttb <- with(alldata, {cor(c(p_ttb*(1-p_ttb)), as.numeric(chosen))}) # 0.01 tiny effect
    cor_log <- with(alldata, {cor(c(p_log*(1-p_log)), as.numeric(chosen))}) # 0.1 = small effect
    
    with(alldata, {cor(ttbuncertain, as.numeric(chosen))}) # worse than predictive varaiance above
    with(alldata, {cor(loguncertain, as.numeric(chosen))}) 
    
    # compute Pseudo- Rsquare across all participants
    # already has a softmax parameter for each participant from glm fitting
    fit <- glm(chosen~c(p_log*(1-p_log)), data=alldata, family="binomial")
    pseudo_rsquare_log <- 1 - (fit$deviance/fit$null.deviance)  # 0.011 still just 1% small effect
    
    fit2 <- glm(chosen~c(p_ttb*(1-p_ttb)), data=alldata, family="binomial")
    pseudo_rsquare_ttb <- 1 - (fit2$deviance/fit2$null.deviance)  # 0.0003 not even small effect
###############################################################################################

    ## Active AIC's per participant
    #
    load(file = "Exp1_paulas_activedata.RData")
    
    
    AIC_weight_TTB <- rep(0, max(alldata$id))
    AIC_weight_Log <- rep(0, max(alldata$id))
    
    # testing predictive variance
    dist_aic_active<-Log_user_active<-logaic<-ttbaic<-randaic<-pseudo_rlog<-pseudo_rttb<-pseudo_rrand<-cor_ttb<-cor_log<-cor_rand<-rep(0, max(alldata$id))
    for (i in 1:max(alldata$id)){
      dummy<-subset(alldata, id==i)
      
      # AIC's of predictive variance predicting the choices
      randaic[i]<-AIC(glm(chosen~sample(c(p_log*(1-p_log))), data=dummy, family="binomial"))
      logaic[i]<-AIC(glm(chosen~c(p_log*(1-p_log)), data=dummy, family="binomial"))
      ttbaic[i]<-AIC(glm(chosen~c(p_ttb*(1-p_ttb)), data=dummy, family="binomial"))
      
  
      ## Compute Akaike Weights (Wagenmakers)
      delta_AIC_TTB <- ttbaic[i] - min(logaic[i], ttbaic[i])
      delta_AIC_Log <- logaic[i] - min(logaic[i], ttbaic[i])
      AIC_weight_TTB[i] <- exp(-1/2 * delta_AIC_TTB)/sum(exp(-1/2 * delta_AIC_TTB), exp(-1/2 * delta_AIC_Log))
      AIC_weight_Log[i] <- exp(-1/2 * delta_AIC_Log)/sum(exp(-1/2 * delta_AIC_TTB), exp(-1/2 * delta_AIC_Log))
      
      
      # measuring distance on individual basis and classify people
      dist_aic_active[i] <- ttbaic[i] - logaic[i]
      
      if(ttbaic[i] > logaic[i]){
        # if logaic was better, classify as Logistic user (1)
        # compare to users for active models
        Log_user_active[i] <- 1
      }
      
      # compute Pseudo-Rsquare from AIC
      fitlog <- glm(chosen~c(p_log*(1-p_log)), data=dummy, family="binomial")
      # fit$null.deviance and fit$deviance = residual deviance
      pseudo_rlog[i] <- 1 - (fitlog$deviance/fitlog$null.deviance)
      
      fitttb <- glm(chosen~c(p_ttb*(1-p_ttb)), data=dummy, family="binomial")
      # fit$null.deviance and fit$deviance = residual deviance
      pseudo_rttb[i] <- 1 - (fitttb$deviance/fitttb$null.deviance)
      
      fitrand <- glm(chosen~sample(c(p_log*(1-p_log))), data=dummy, family="binomial")
      # fit$null.deviance and fit$deviance = residual deviance
      pseudo_rrand[i] <- 1 - (fitrand$deviance/fitrand$null.deviance)
      
      # pearsons correlation per participant
      cor_ttb[i] <- with(dummy, {cor(c(p_ttb*(1-p_ttb)), as.numeric(chosen))}) 
      cor_log[i] <- with(dummy, {cor(c(p_log*(1-p_log)), as.numeric(chosen))}) 
      cor_rand[i] <- with(dummy, {cor(sample(c(p_log*(1-p_log))), as.numeric(chosen))}) 
      
    }
    load("Exp1_tabledata.RData")
    cross$Log_user_active <- Log_user_active ## attach Prediction measure
    save(cross, file = "Exp1_tabledata.RData")
    
    AICs_active <- data.frame(id = 1:264, active_randaic = randaic, active_logaic = logaic, active_ttbaic = ttbaic,
                              pseudo_rrand =pseudo_rrand, pseudo_rlog = pseudo_rlog, pseudo_rttb =pseudo_rttb)
    save(AICs_active, file = "AICs_active.RData")
    
    ## Computing AIC weights from the active AIC's per participant
    AIC_weights <- data.frame(id = 1:264, AIC_weight_TTB_active = AIC_weight_TTB, AIC_weight_Log_active = AIC_weight_Log)
    save(AIC_weights, file = "AIC_weights_data.RData")
    # analysed in different R script
    
    
    
    active_randai <- randaic 
    active_logaic <- logaic 
    active_ttbaic <- ttbaic
    
    se_pseudo_rlog <- sd(pseudo_rlog)/sqrt(length(pseudo_rlog))
    se_pseudo_rttb <- sd(pseudo_rttb)/sqrt(length(pseudo_rttb))
    se_pseudo_rrand <- sd(pseudo_rrand)/sqrt(length(pseudo_rrand))
    
    se_logaic <- sd(logaic)/sqrt(length(logaic))
    se_ttbaic <- sd(ttbaic)/sqrt(length(ttbaic))
    se_randaic <- sd(randaic)/sqrt(length(randaic))
    
    se_dist_aic_active <- sd(dist_aic_active)/sqrt(length(dist_aic_active))
    print(mean(dist_aic_active))
    # average distance between TTB AIC and LOG AIC
    
    
  ##--------------- Pseuo-Rsquare barplot Active: 
    
    load(file = "AICs_active.RData")
    attach(AICs_active)
    
    
    se_pseudo_rlog <- sd(pseudo_rlog)/sqrt(length(pseudo_rlog))
    se_pseudo_rttb <- sd(pseudo_rttb)/sqrt(length(pseudo_rttb))
    se_pseudo_rrand <- sd(pseudo_rrand)/sqrt(length(pseudo_rrand))
    
    
    d <- data.frame(numeric(3))
    d$Rsquare <- c(mean(pseudo_rlog),mean(pseudo_rttb),mean(pseudo_rrand))
    d$se <- c(se_pseudo_rlog, se_pseudo_rttb, se_pseudo_rrand)
    d$Model <-factor(c("Logistic", "TTB", "Random"), 
                     levels = c("Logistic", "TTB", "Random")) #for barplot below
    
    
    
    # When the data contains y values in a column, use stat="identity"
    pd <- position_dodge(.1)  
    b <- ggplot(d, aes(x = Model, y = Rsquare, colour = Model)) + geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin=Rsquare-se, ymax=Rsquare+se), width=0.5, size=0.5, position=pd) +
      # ggtitle("Active Model Fits (trial-by-trial)")+
      theme(text = element_text(size=18),axis.title= element_text(size=30), axis.text= element_text(size=22),  legend.position="none") + 
      ylab("Pseudo-Rsquare")
    
    x11(width=10, height=8)
    b
    
    pdf("Exlp1_active_barplot_PseudoRsquare.pdf")
    b
    dev.off()
    
    #---------------------------------------------------------------------------------------------------
    
    
    
    #####################------ correlating active and passive AIC's ###################################
    active_randai <- randaic 
    active_logaic <- logaic 
    active_ttbaic <- ttbaic
    
    cor.test(active_logaic, passive_logaic)
    cor.test(active_ttbaic, passive_ttbaic)
    
    # should be low
    cor.test(active_ttbaic, active_logaic)
    #should be high 
    cor.test(passive_ttbaic, passive_logaic)
    # they are both high

    cor(active_randaic, passive_randaic)
    
    
    
    
    
    ###- ----------------- Rsquare: Combining it with the passive data in one plot ---------------------------
    
    #b_data <- d
    a_data <- d 
    dat <- rbind(a_data, b_data)
    dat$Mode <- rep(c("Active (Training)", "Passive (Test)"), each = 3)
    
  
    pd <- position_dodge(.1)
    p<-ggplot(dat, aes(x=Model, y=Rsquare, colour=Model)) + geom_bar(stat = "identity") +
      geom_errorbar(aes(ymin=Rsquare-se, ymax=Rsquare+se), width=0.5, size=0.5, position=pd) +
      ggtitle("Model Fits")+theme(text = element_text(size=18), legend.position="none") +
      ylab("Pseudo-Rsquare")+ 
      facet_wrap(~ Mode, scales="free", ncol=2)
    p
    
    x11(width=10, height=8)
    p
    
    pdf("Active_and_Passive_Rsquare_bycols.pdf")
    p
    dev.off()
  #-------------------------------------------------------------
    
    
    

  #####------------------------ barplot AIC's Active   ########
    
    load(file = "AICs_active.RData")
 
    attach(AICs_active)
    
    se_active_logaic <- sd(active_logaic)/sqrt(length(active_logaic))
    se_active_ttbaic <- sd(active_ttbaic)/sqrt(length(active_ttbaic))
    se_active_randaic <- sd(active_randaic)/sqrt(length(active_randaic))
    
    
    
    d <- data.frame(numeric(3))
    d$aic <- c(mean(active_logaic),mean(active_ttbaic),mean(active_randaic))
    d$se <- c(se_active_logaic, se_active_ttbaic,  se_active_randaic)
    d$Model <-factor(c("Logistic", "TTB", "Random"), 
                     levels = c("Logistic", "TTB", "Random")) #for barplot below
    # When the data contains y values in a column, use stat="identity"
    pd <- position_dodge(.1)  
    b <- ggplot(d, aes(x = Model, y = aic, colour=Model)) + geom_bar(stat = "identity") + #, fill="white", colour="black") +
      geom_errorbar(aes(ymin=aic-se, ymax=aic+se), width=0.5, size=0.5, position=pd) +
      #ggtitle("Active Model Fits (Trial-by-trial)")+ 
      xlab("") +
      theme(text = element_text(size=18), axis.text.x= element_text(size=30), axis.title.y = element_text(size=30), legend.position="none") +  
      coord_cartesian(ylim=c(155,170))+ # like ylim but bars dont disappear
      #coord_cartesian() + # like ylim but bars dont disappear
      ylab("AIC")
    b
    
  
    x11(width=10, height=8)
    b
    
    pdf("Exlp1_active_barplot_aic.pdf")
    b
    dev.off()
    
    
    
    
    ###- ----------------- AIC: Combining it with the passive data in one plot ---------------------------
    
    #b_data <- d
    a_data <- d 
    dat <- rbind(a_data, b_data)
    dat$Mode <- rep(c("Active (Training)", "Passive (Test)"), each = 3)
    
    
    pd <- position_dodge(.1)
    p<-ggplot(dat, aes(x=Model, y=aic, colour=Model)) + geom_bar(stat = "identity", fill="white", colour="black")+
      geom_errorbar(aes(ymin=aic-se, ymax=aic+se), width=0.5, size=0.5, position=pd) +
      ggtitle("Model Fits")+theme(text = element_text(size=18), legend.position="none") +
      ylab("AIC")+ 
      #coord_cartesian(ylim=c(10,170)) +
      facet_wrap(~ Mode, scales="free", ncol=2)
    p
    
    ## need to change scales!
    
    
    x11(width=10, height=8)
    p
  
    
    pdf("Active_and_Passive_AIC_bycols.pdf")
    p
    dev.off()
    #-------------------------------------------------------------
    
    
    
    
    
    
    
    ## Correlation doesnt look as good
    t.test(cor_ttb,cor_log)
    #   mean of x  mean of y 
    #   0.01818272 0.09938468 
    t.test(cor_ttb,cor_rand)
    
    
    
    logaic<-ttbaic<-rep(0, max(alldata$id))
    for (i in 1:max(alldata$id)){
      dummy<-subset(alldata, id==i)
      logaic[i]<-AIC(glm(chosen~loguncertain, data=dummy, family="binomial"))
      ttbaic[i]<-AIC(glm(chosen~ttbuncertain, data=dummy, family="binomial"))
    }
    
    
    
    
    logcor<-ttbcor<-numeric()
    for (i in 1:max(alldata$id)){
      dummy<-subset(alldata, id==i)
      logcor<-c(logcor, cor(dummy$chosen, dummy$loguncertain))
      ttbcor<-c(ttbcor,cor(dummy$chosen, dummy$ttbuncertain))
    }  
    
    par(mfrow=c(1,2))
    hist(logcor)
    hist(ttbcor)
    mean(logcor)
    mean(ttbcor)
    t.test(logcor, mu=0)
    
    t.test(logaic, ttbaic)
    
    
   
    
    #####-------------------   R-square and correlation with Active BY CONDITION: ------------------------------------
    # 
    # mbase<-glm(chosen~ ttbconf + ttbuncertain, data= alldata[alldata$cond == 10, ], 
    #            family="binomial")
    # summary(mbase)
    # 
    # mlog<-glm(chosen~ logconf + loguncertain, data=alldata[alldata$cond == 10, ], 
    #           family="binomial")
    # summary(mlog)
    
    
    load(file = "Exp1_paulas_activedata.RData")
    
    ##### by COND
    indice <- sort(unique(alldata$cond))                     
    avg_ttb  <- vector('numeric', 5)
    se_ttb <- vector('numeric', 5)
    avg_rsquare_ttb  <- vector('numeric', 5)
    se_rsquare_ttb <- vector('numeric', 5)
    avg_log  <- vector('numeric', 5)
    se_log <-vector('numeric', 5)
    avg_rsquare_log  <- vector('numeric', 5)
    se_rsquare_log <- vector('numeric', 5)
    avg_ttbconf  <- vector('numeric', 5)
    se_ttbconf  <- vector('numeric', 5)
    avg_ttbuncertain  <- vector('numeric', 5)
    se_ttbuncertain  <- vector('numeric', 5)
    avg_logconf  <- vector('numeric', 5)
    se_logconf  <- vector('numeric', 5)
    avg_loguncertain  <- vector('numeric', 5)
    se_loguncertain  <- vector('numeric', 5)
    
    avg_logaic <- se_logaic <- avg_ttbaic <- se_ttbaic <- avg_randaic <-se_randaic<-avg_pseudo_rlog <- se_pseudo_rlog <- avg_pseudo_rttb <- se_pseudo_rttb <-avg_pseudo_rrand <- se_pseudo_rrand<- vector('numeric', 5)
     
    
    for (c in 1:length(indice)){
      
     
      load(file = "Exp1_paulas_activedata.RData")
      
      # individual correlations by condition
      alldata <- alldata[alldata$cond == indice[c], ]
      
      pp <- unique(alldata$id)
      len <- length(unique(alldata$id))
      
      r_ttb <- vector('numeric', len)
      rsquare_ttb <-  vector('numeric', len)
      r_log <-vector('numeric', len)
      rsquare_log <-  vector('numeric', len)
      # correlations of trial by trial predictions
      r_ttbconf<-  vector('numeric', len)
      r_ttbuncertain<-  vector('numeric', len)
      r_logconf<-  vector('numeric', len)
      r_loguncertain<-  vector('numeric', len)
      
     logaic<-ttbaic<-randaic<-pseudo_rlog<-pseudo_rttb<-pseudo_rrand<-rep(0, len)
      
      
      # for that condition group
      for (i in 1:len){ # only those id's that are in the condition 
        
        dummy<-subset(alldata, id == pp[i])
         
        # AIC's of predictive variance predicting the choices
        randaic[i]<-AIC(glm(chosen~sample(c(p_log*(1-p_log))), data=dummy, family="binomial"))
        logaic[i]<-AIC(glm(chosen~c(p_log*(1-p_log)), data=dummy, family="binomial"))
        ttbaic[i]<-AIC(glm(chosen~c(p_ttb*(1-p_ttb)), data=dummy, family="binomial"))
        
        
        # compute Pseudo-Rsquare from GLM
        fitlog <- glm(chosen~c(p_log*(1-p_log)), data=dummy, family="binomial")
        # fit$null.deviance and fit$deviance = residual deviance
        pseudo_rlog[i] <- 1 - (fitlog$deviance/fitlog$null.deviance)
        fitttb <- glm(chosen~c(p_ttb*(1-p_ttb)), data=dummy, family="binomial")
        # fit$null.deviance and fit$deviance = residual deviance
        pseudo_rttb[i] <- 1 - (fitttb$deviance/fitttb$null.deviance)
        fitrand <- glm(chosen~sample(c(p_log*(1-p_log))), data=dummy, family="binomial")
        # fit$null.deviance and fit$deviance = residual deviance
        pseudo_rrand[i] <- 1 - (fitrand$deviance/fitrand$null.deviance)
        
        
        
        r_ttb[i] <- with(dummy, {cor(c(p_ttb*(1-p_ttb)), as.numeric(chosen))})
        rsquare_ttb[i] <- r_ttb[i]^2
        r_log[i] <- with(dummy, {cor(c(p_log*(1-p_log)), as.numeric(chosen))})
        rsquare_log[i] <- r_log[i]^2
        
        # correlations of trial by trial predictions
        r_ttbconf[i] <- with(dummy, {cor(ttbconf, as.numeric(chosen))})  
        r_ttbuncertain[i] <- with(dummy, {cor(ttbuncertain, as.numeric(chosen))})
        r_logconf[i] <- with(dummy, {cor(logconf, as.numeric(chosen))})
        r_loguncertain[i] <-with(dummy, {cor(loguncertain, as.numeric(chosen))})
        
      } # participant (len)
      
     ## the important ones:
     avg_logaic[c]  <- mean(logaic)
     se_logaic[c] <- sd(logaic)/sqrt(len)
     avg_ttbaic[c]  <- mean(ttbaic)
     se_ttbaic[c] <- sd(ttbaic)/sqrt(len)
     avg_randaic[c]  <- mean(randaic)
     se_randaic[c] <- sd(randaic)/sqrt(len)
     
     avg_pseudo_rlog[c]  <- mean(pseudo_rlog)
     se_pseudo_rlog[c] <- sd(pseudo_rlog)/sqrt(len)
     avg_pseudo_rttb[c]  <- mean(pseudo_rttb)
     se_pseudo_rttb[c] <- sd(pseudo_rttb)/sqrt(len)
     avg_pseudo_rrand[c]  <- mean(pseudo_rrand)
     se_pseudo_rrand[c] <- sd(pseudo_rrand)/sqrt(len)
     ###########################################
  
     
      avg_ttb[c]  <- mean(r_ttb)
      se_ttb[c] <- sd(r_ttb)/sqrt(len)
      avg_log[c]  <- mean(r_log)
      se_log[c] <- sd(r_log)/sqrt(length(r_log))
      
      avg_rsquare_ttb[c]  <- mean(rsquare_ttb)
      se_rsquare_ttb[c] <- sd(rsquare_ttb)/sqrt(len)
      avg_rsquare_log[c]  <- mean(rsquare_log)
      se_rsquare_log[c] <- sd(rsquare_log)/sqrt(len)
      
      avg_ttbconf[c]  <- mean(r_ttbconf)
      se_ttbconf[c]  <- sd(r_ttbconf)/sqrt(len)
      avg_ttbuncertain[c]  <- mean(r_ttbuncertain)
      se_ttbuncertain[c]  <- sd(r_ttbuncertain)/sqrt(len)
      avg_logconf[c]  <- mean(r_logconf)
      se_logconf[c]  <- sd(r_logconf)/sqrt(len)
      avg_loguncertain[c]  <- mean(r_loguncertain)
      se_loguncertain[c]  <- sd(r_loguncertain)/sqrt(len)
      
      
    } # condition loop
    
    
    correspondence <- data.frame(cond = indice, 
                                 avg_logaic = avg_logaic, se_logaic = se_logaic, 
                                 avg_ttbaic = avg_ttbaic, 
                                 se_ttbaic =   se_ttbaic, 
                                 avg_randaic= avg_randaic,
                                 se_randaic = se_randaic, 
                                 avg_pseudo_rlog = avg_pseudo_rlog, se_pseudo_rlog = se_pseudo_rlog, 
                                 avg_pseudo_rttb =  avg_pseudo_rttb, se_pseudo_rttb = se_pseudo_rttb,
                                 avg_pseudo_rrand =  avg_pseudo_rrand,  se_pseudo_rrand = se_pseudo_rrand,
  
                                 avg_ttb =avg_ttb,  se_ttb =  se_ttb,  
                                  avg_log = avg_log, se_log =se_log,
                                 avg_rsquare_ttb = avg_rsquare_ttb, 
                                 se_rsquare_ttb =se_rsquare_ttb,  avg_rsquare_log =avg_rsquare_log, 
                                 se_rsquare_log =se_rsquare_log, avg_ttbconf =avg_ttbconf, se_ttbconf =se_ttbconf, 
                                 avg_ttbuncertain = avg_ttbuncertain,  se_ttbuncertain= se_ttbuncertain, 
                                 avg_logconf =avg_logconf, se_logconf = se_logconf, avg_loguncertain =avg_loguncertain, 
                                 se_loguncertain =se_loguncertain)
    
    save(correspondence, file = "correspondence_perCOND_PseudoRsquare.RData")
    
    
    
    
    ####  Just the plot: load the data from above 
    library(ggplot2)
    load(file = "Exp1_paulas_activedata.RData")
    indice <- sort(unique(alldata$cond))                     
    
    
    load("correspondence_perCOND_PseudoRsquare.RData")
    
    
    d <- data.frame(cond = c(indice, indice))
    d$cor <- c(correspondence$avg_pseudo_rttb, correspondence$avg_pseudo_rlog)
    d$se <- c(correspondence$se_pseudo_rttb, correspondence$se_pseudo_rlog)
    d$Model<-rep(c("Take The Best", "Logistic"), each=5)
    d$assess<-"Pseudo-Rsquare"
    # 
    # d2<-d
    # d2$cor<-d2$cor^2
    # d2$se<-d2$se^2
    # d2$assess<-"Regression"
    # 
    # dat<-rbind(d2,d)
    
    pd <- position_dodge(.1)
    
  
    p<-ggplot(d, aes(x=cond, y=cor, colour=Model)) +
      geom_errorbar(aes(ymin=cor-se, ymax=cor+se), width=0.5, size=0.5, position=pd) +
      xlab(expression(theta))+
      ylim(0, 0.06) +   ### what?
      scale_x_continuous(breaks = indice, labels=c("~infinity", 2, 1, 0.5, "~0")) +
      geom_line(position=pd, size=1) +
      geom_point(position=pd)+
      ylab("")+ggtitle("Correspondence")+
      theme(plot.title =element_text(size=25), strip.text.x = element_text(size = 25, face="bold"), # size of the facet labels
            legend.title= element_text(size=22, face="bold"), legend.text=element_text(size=22),
            text = element_text(size=18), legend.position="top",
            axis.text= element_text(size=20), 
            axis.title = element_text(size=22))
    
    p
  
    
    x11(width=10, height=8)
    p 
    
    pdf("Active_Pseudo_Rsquare_BYCOND.pdf")
    p
    dev.off()
    
    
    
    # dev.copy(png,'Active_Pseudo_Rsquare_BYCOND.png',width = 600, height = 600)
    # p
    # dev.off()
    # 
   
    ######
    
    
    
    d <- data.frame(cond = c(indice, indice))
    d$cor <- c(correspondence$avg_ttb, correspondence$avg_log)
    d$se <- c(correspondence$se_ttb, correspondence$se_log)
    d$Model<-rep(c("Take The Best", "Logistic"), each=5)
    d$assess<-"Correlation"
    
    d2<-d
    d2$cor<-d2$cor^2
    d2$se<-d2$se^2
    d2$assess<-"Regression"
    
    dat<-rbind(d2,d)
    
    pd <- position_dodge(.1)
    
    
    p<-ggplot(dat, aes(x=cond, y=cor, colour=Model)) +
      geom_errorbar(aes(ymin=cor-se, ymax=cor+se), width=0.5, size=0.5, position=pd) +
      
      geom_line(position=pd, size=1) +
      geom_point(position=pd)+xlab(expression(beta[1]))+
      ylab("Pearson's r")+ggtitle("Correspondence")+theme(text = element_text(size=18))+
      facet_wrap(~ assess, scales="free", nrow=2)
    
    x11(width=10, height=8)
    p 
    
    
    pdf("active_correspondence_Exp1_stepbystep.pdf")
    
    p
    
    dev.off()
    
    
    
