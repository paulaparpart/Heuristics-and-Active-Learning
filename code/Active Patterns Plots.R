#################################################################
#Plot for Paula
#################################################################
#Preamble

rm(list=ls())
packages <- c('plyr', 'ggplot2', 'jsonlite', 'MASS', 'MCMCpack', 'plyr', 'curl')
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

##############THIS IS WHERE JAGS ENDS#####################

#Reading in JSON from Firebase
#################################################
#myjson<-fromJSON("https://activettb.firebaseio.com/.json")
load("JSON_EXP1.RData")
len<-length(myjson)
#################################################
#save(myjson, file = "JSON_EXP1.RData")

dcollect<-data.frame(id=numeric(), chosen=numeric())
for (q in 1:len){
  #Data Munging
  d<-myjson[[q]]
  
  # used to be earlier minus later aliens
  allcomps<-data.frame(xstring(d$alien1)-xstring(d$alien2),
                       xstring(d$alien1)-xstring(d$alien3),
                       xstring(d$alien1)-xstring(d$alien4),
                       xstring(d$alien2)-xstring(d$alien3),
                       xstring(d$alien2)-xstring(d$alien4),
                       xstring(d$alien3)-xstring(d$alien4))
  
  combinations<-c(12,13, 14 ,23, 24, 34)
  names(allcomps)<-rep(1:6, each=4)
  # ordering important as that makes earlier aliens come first and later aliens come second, 1 - 2 or 1-4 or 2-3
  chosencomp<-t(apply(d$alienchosen,1, function(x){x[order(x)]}))+1
  allcomps$chosen<-rep(NA,30)
  for (l in 1:30){
    allcomps$chosen[l]<-which(paste0(chosencomp[,1],chosencomp[,2])[l]==paste(combinations))
  }
  # has the earlier chosen Alien won or the later chosen Alien? 1 = earlier, 0 = later
  allcomps$outcome<-ifelse(d$tracker1==chosencomp[,1],1,0)
  
  # collect is a character vector, this is an unelegant way to initiate the vector
  collect<-1:30
  for (l in  1:nrow(allcomps) ){
    # store the comparison as character vector that was chosen by the participant at trial l
    collect[l]<-paste0(sort(allcomps[l,allcomps$chosen[l]==names(allcomps)]), collapse='')
  }
  
  if (q==1){
    id<-rep(q, 30)
    chosen<-collect
    dat<-data.frame(id, chosen)
  }
  if(q>1){
    id<-rep(q, 30)
    chosen<-collect
    d<-data.frame(id, chosen) 
    dat<-rbind(dat,d)
    
  }
  
}


 # mapping is not general function, it only works here because that is the outcome of this particular data
  dat$combs<-mapvalues(dat$chosen, 
                       from=unique(dat$chosen),
                       to=c("++00","+-00","++00","+++-", "+000", "++-0", "+000", "++-0", "+++0",
                            "+++0", "++--", "+++-", "++++", "++++"))
  dat$choice<-1
  dat$count<-ave(dat$choice, paste(dat$combs, dat$id), FUN=length) # double counts
  dat$combs <- ordered(dat$combs, levels = c("+000", "+-00", "++00", "++-0", "++--","+++0", "+++-", "++++"))

  save(dat, file = "wldd_rawcounts_Exp1.RData")

  
  
  ########  start with loading data:  Now correcting the actual counts
  load("JSON_EXP1.RData")
  len<-length(myjson)
  
  load("wldd_rawcounts_Exp1.RData")
  

    # THese are the counts we should use for boxplot instead!
    dplot1a<-ddply(dat,~combs+id,summarise,counttrue=mean(count)) # makes counts appear equally often, each combs by each ID: 8*264
    # therefore the counts do not get counted different amounts of tim
    # summarises how often each comparison occured overall across all participants by taking sum()
    dplot1b<-ddply(dplot1a,~combs,summarise,count=sum(counttrue))
    dplot1b$mu<-dplot1b$count/len   # mean = on average per participant this is how often they chose that query

    
    
    pdf("wldd_empirical.pdf", width = 10, height = 10)
  # Plot true counts: boxplot
    #x11(width=10, height=8) 
    # true counts boxplot
    plot(dplot1a$combs, dplot1a$counttrue, ylim=c(0,20), pch="", col="darkred", 
         main="Frequency of selected queries", xlab="Queries", ylab="",
         cex.lab=2, cex.main=2, cex.axis=1.5)
    
    lines(dplot1b$mu, type="b", pch=24, lwd = 2, cex=2, col="#000099", bg="pink")
    legend("topright", "Empirical Mean", lty=c(1,1),  cex = 1.5, lwd=c(2,2), col="#000099", pch = 24,  pt.bg = "pink")
    
    dev.off()


    
    
    
    
    ###-------------------------- WLDD patterns over time 

    load("wldd_rawcounts_Exp1.RData")
    len <- 264

    dat <- cbind(id = dat$id , trial = rep(1:30,len), dat[ , 2:5])
    # average across subset: factor crossings of combs (patterns) and trial number (1-30) and count
    dat$counttrial    <- ave(dat$choice, paste(dat$combs, dat$trial), FUN=length) # couble counts
    
    # careful now: e.g., ++00 occured 44 times on trial 1, that means it will occur 44 times in the vector!
    # cannot average  dat$counttrial without normalising such as above (ddply)
    
    patterns <- c("+000","+-00","++00","++-0","++--", "+++0", "+++-", "++++")
    # the patterns are in the order of above 
    p1 <- p2 <- p3 <- p4 <- p5 <- p6 <- p7 <- p8 <- vector('numeric', 30)
    # count pattern frequency per trial
      for (i in 1:30){ # trials
      
        # no problem of counts here as I only take each frequency once
      p1[i] <- dat$counttrial[dat$combs == patterns[1] & dat$trial == i][1]
      p2[i] <- dat$counttrial[dat$combs == patterns[2] & dat$trial == i][1]
      p3[i] <- dat$counttrial[dat$combs == patterns[3] & dat$trial == i][1]
      p4[i] <- dat$counttrial[dat$combs == patterns[4] & dat$trial == i][1]
      p5[i] <- dat$counttrial[dat$combs == patterns[5] & dat$trial == i][1]
      p6[i] <- dat$counttrial[dat$combs == patterns[6] & dat$trial == i][1]
      p7[i] <- dat$counttrial[dat$combs == patterns[7] & dat$trial == i][1]
      p8[i] <- dat$counttrial[dat$combs == patterns[8] & dat$trial == i][1]
      
      }
      
    p8[is.na(p8)] <- 0
    mean_chosen_pertrial <- c(mean(p1), mean(p2), mean(p3), mean(p4), mean(p5), mean(p6), mean(p7), mean(p8))
    
    
    d <- data.frame(trial = rep(1:30,8))
    d$freq <- c(p1,p2,p3,p4,p5,p6,p7,p8)
    d$query <-rep(patterns, each=30)
    d$query <- factor(d$query, levels = patterns) 
    
    # error bars could be added, inter-individual variability 
    pd <- position_dodge(.1)
  
    p<-ggplot(d, aes(x=trial, y=freq, group = query, colour=query)) +
      #geom_errorbar(aes(ymin=cor-se, ymax=cor+se), width=0.5, size=0.5, position=pd) +
      xlab("Trial")+
      #ylim(0, 0.06) +   ### what?
      #geom_line(position=pd, size=2) +
      #geom_point(position=pd, size = 7, aes(fill=factor(query)))+
      theme_bw() +
      scale_shape_manual(name = "Query Type", values=c(22,21, 23,24,25,0,20,18)) +
      scale_fill_discrete(na.value=NA, guide="none") + # <---- NOTE THIS
      geom_smooth(method="loess", size = 2) +
      scale_x_continuous(breaks = c(1:30)) + 
      
      ylab("Number of participants choosing query")+ggtitle("Frequencies of queries over time")+
      theme(plot.title =element_text(size=25), 
            legend.title= element_text(size=22, face="bold"), legend.text=element_text(size=22),
            text = element_text(size=18), legend.position="right",
            axis.text= element_text(size=15), 
            axis.title = element_text(size=22)) 
 
     pdf("wldd_overtime.pdf", width = 12, height = 10)
     p
     dev.off()
    
     
     ##### Test for significant slopes over time?
     
     slopes <- data.frame(trial = rep(1:30), p1,p2,p3,p4,p5,p6,p7,p8)
     attach(slopes)
     
     
     # has to be with intercept right? Otherwise p7 (flat line) is even significant
     m1 <- lm(p1 ~trial) 
     summary(m1)
     
     m2 <- lm(p2 ~trial ) 
     summary(m2)
     
     m3 <- lm(p3 ~trial) 
     summary(m3)
     
     m4 <- lm(p4 ~trial) 
     summary(m4)
     
     m5 <- lm(p5 ~trial) 
     summary(m5)
     
     m6 <- lm(p6 ~trial) 
     summary(m6)
     
     m7 <- lm(p7 ~trial) 
     summary(m7)
     
     m8 <- lm(p8 ~trial) 
     summary(m8)
     
     
     
     patterns <- c("+000","+-00","++00","++-0","++--", "+++0", "+++-", "++++")
     
     
    
    
    ## the old one: that is not corrected for   
    load("wldd_rawcounts_Exp1.RData")
    # just using this without above correction ends up in incorrect plot below (wldd.pdf)
    
    pdf("wldd.pdf")
    # histogramm averaged automatisch
    
    dat$combs <- ordered(dat$combs, levels = c("+000", "+-00", "++00", "++-0", "++--",
                                                "+++0", "+++-", "++++"))
    
    #problem with using dat$count as dependent is that it biases some queries to higher frequencies as they are counted 13 times 
    # for example when id = 1 used that query 13 times
    x11(width=8, height=8) 
    plot(dat$combs, dat$count, pch="", col="darkred", 
         main="Frequency of choices", xlab="Patterns", ylab="Frequency",
         cex.lab=1.8, cex.main=1.8, cex.axis=1.3)
    
    dev.off()