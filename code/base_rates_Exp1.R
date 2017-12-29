      
      
      
      rm(list=ls())
      packages <- c('plyr', 'ggplot2', 'jsonlite', 'MASS', 'MCMCpack', 'plyr', 'curl', 'dplyr', 'lazyeval')
      lapply(packages, require, character.only = TRUE)
     
      
      #Functions
      xstring<-function(x){
        y<-as.numeric(substr(x,1,1))
        for (i in 2:4){
          y<-cbind(y,as.numeric(substr(x,i,i)))
        }
        return(y)
      }
      
      
      #Reading in JSON from Firebase
      #################################################
      #myjson<-fromJSON("https://activettb.firebaseio.com/.json")
      load("JSON_EXP1.RData")
      len<-length(myjson)
      
       
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
        # give them easier names
        names(allcomps)<-rep(1:6, each=4)
      
        # collect is a character vector, this is an unelegant way to initiate the vector
        first<-second <- third <- fourth <- fifth <- sixth<-1:30
        for (l in  1:nrow(allcomps) ){
          # store the comparison as character vector that was chosen by the participant at trial l
          first[l] <-paste0(sort(allcomps[ l,names(allcomps)==1]), collapse='')
          # take those columns that are the first, second, third, fourth or fifth comparison and check what queries they are
          second[l] <-paste0(sort(allcomps[ l,names(allcomps)==2]), collapse='')
          third[l] <-paste0(sort(allcomps[ l,names(allcomps)==3]), collapse='')
          fourth[l] <-paste0(sort(allcomps[ l,names(allcomps)==4]), collapse='')
          fifth[l] <-paste0(sort(allcomps[ l,names(allcomps)==5]), collapse='')
          sixth[l] <-paste0(sort(allcomps[ l,names(allcomps)==6]), collapse='')
        
        }
        
  
        if (q==1){
          id<-rep(q, 30)
          chosen<-c(first, second, third, fourth, fifth, sixth) # take all comparisons, not throwing any away
          dat<-data.frame(id, chosen)
        }
        if(q>1){
          id<-rep(q, 30)
          chosen<-c(first, second, third, fourth, fifth, sixth)
          d<-data.frame(id, chosen) 
          dat<-rbind(dat,d)
          
        }
        
      }
      
      # dat$chosen now contains ALL possible 6 comparisons on all 30 trails for each participant, instead of just the chosen comparison
    
      dat$chosen <- ordered(dat$chosen, levels = c("-1000", "0001", "-1001", "-1-100", "0011", "-1-101", "-1011", "-1-111",
                                                   "-1-1-10", "0111", "-1111", "-1-1-11", "-1-1-1-1", "1111"))
      patterns = c("+000", "+000", "+-00", "++00",  "++00", "++-0", "++-0", "++--","+++0", "+++0", "+++-", "+++-", "++++", "++++")                                         
      dat$combs<-mapvalues(dat$chosen, 
                           from=levels(dat$chosen),
                           to= patterns)
      #dat$combs contains all comparisons: levels(dat$combs) = "+000" "+-00" "++00" "++-0" "++--" "+++0" "+++-" "++++"
       
      # count how often each query type occured across all pp's across all 30 trials, across all 6 possible comparisons
      dat$choice<-1
      
      # option a)
      dat$count<-ave(dat$choice, paste(dat$combs), FUN=length) # double counts
         # careful now with dat$count again, it shows the frequency of patterns as often as the frequency (cannot be averaged!!)
      #length(dat$count[dat$combs == patterns[1]])
      # count data
     save(dat, file = "wldd_BaseRates_Exp1.RData")
    
      
      # option b): count by ID
      # dat$count<-ave(dat$choice, paste(dat$combs, dat$id), FUN=length) # double counts
      #  # Need to do the same thing as with the chosen counts: correct them first! 
      # #normalize because the counts happen as often as they appear for id: combs 
      # dplot1a<-ddply(dat,~combs+id,summarise,counttrue=mean(count)) # makes counts appear equally often, each combs by each ID: 8*264
      # # it worked:) now summarise by combs across all 264 pp
      # dplot1b<- ddply(dplot1a,~combs,summarise,count=sum(counttrue))# gives the same answer as below
      # dplot1b$count/(30*6*264)  # same result as base_rate_prob below
      
      

      
      #-----    PLot below starts here -----
      load("wldd_BaseRates_Exp1.RData")
      
      patterns <- c("+000","+-00","++00","++-0","++--", "+++0", "+++-", "++++")
      base_rate_perperson <- base_rate_pertrial<- base_rate_count <- base_rate_prob <- vector('numeric',length(patterns))
      for (c in 1:length(patterns)){ # trials
        
        base_rate_count[c] <- dat$count[dat$combs == patterns[c]][1]        # absolute count
      
        #base_rate_pertrial[c] <- dat$count[dat$combs == patterns[c]][1]/(6*30)  # how many participants on average encountered this query per trial?
        #base_rate_perperson[c] <- dat$count[dat$combs == patterns[c]][1]/(6*264) # the number of times any participant should encounter it across 30 trails based on base rate
        # this averaging is where i went wrong
      }
       base_rate_prob <- base_rate_count/sum(base_rate_count) # sum(base_rate_count) = sums all occuring queries possible in the exp (30*6*264)
      
      
      
      
      
      ## Corrrect Chi-square: Compare absolute frequencies (chosen) with base rate probabilities of queries in the whole exp:
      #
      #
      
      
      load("wldd_rawcounts_Exp1.RData")
      dplot1a<-ddply(dat,~combs+id,summarise,counttrue=mean(count)) # makes counts appear equally often, each combs by each ID: 8*264
      dplot1b<-ddply(dplot1a,~combs,summarise,count=sum(counttrue)) # checked its correct
      dplot1b$count   # this is the real absolute frequency count for each pattern (8 entries in vector)
      
      
       #  base_rate_prob vector = relative frequencies of each pattern occurance in the experiment 
      #(across 30 trials * 6 queries * 264 pp = 47520).           
                 
      res  <- chisq.test(dplot1b$count, p = base_rate_prob)
      
      # Woop :) Chi-squared test for given probabilities
      #data:  dplot1b$count
      # X-squared = 460.2291, df = 7, p-value < 2.2e-16          
      # We can conclude that the pp's frequencies are significantly not distributed as the base rate, 
      # with a p-value = 8.80310^{-7}. distributed significantly different than base rate in experiment. not just random!!
      res$expected  # plot this?
          
                 
      chisq.test(c(100,5,1), p=c(.8,.1,.1))
      # chisq.test(vector of 8 real frequencies in the exp. of chosen, base_rate_prob vector)
      
      # if x is a vector and y is not given, then a goodness-of-fit test is performed
      #(x is treated as a one-dimensional contingency table). The entries of x must be non-negative integers. 
      #In this case, the hypothesis tested is whether the population probabilities equal those in p, 
      # or are all equal if p is not given.
      
      
      #--------- Plot observed against expected:

      bop <- data.frame(Patterns = patterns, type = c(rep("Observed",8), rep("Expected",8)), count = c(dplot1b$count,  res$expected))
      bop$Patterns <- factor(bop$Patterns, 
                       levels = c("+000","+-00","++00","++-0","++--", "+++0", "+++-", "++++")) #for barplot below
      bop$type <- factor(bop$type, 
                             levels = c("Expected", "Observed")) #for barplot below
      
      g <- ggplot(bop, aes(x = Patterns, y= count, fill = type)) +
          ylab("Absolute count") + 
          xlab("Queries") +
          geom_bar(stat="identity", width=.8, position = "dodge")  +
        # ggtitle("Active Model Fits (trial-by-trial)")+
              theme(text = element_text(size=18),axis.title.x= element_text(size=35), axis.title.y= element_text(size=25),
              axis.text= element_text(size=22), legend.title=element_blank(), legend.text = element_text(size=25),
              legend.position="top") 
        g
        
        pdf("Observed_vs_Expected.pdf", width = 10, height = 7)
         g
         dev.off()
            
        
        
      
      
      ## ----------What i did: Comparing empricial means with the base rate means: Does average pp differ from average base rate profile?
      load("wldd_rawcounts_Exp1.RData")
      dplot1a<-ddply(dat,~combs+id,summarise,counttrue=mean(count)) # makes counts appear equally often, each combs by each ID: 8*264
      dplot1b<-ddply(dplot1a,~combs,summarise,count=sum(counttrue))
      dplot1b$mu<-dplot1b$count/len   # mean = on average per participant this is how often they chose that query

      chidata <- data.frame(patterns, means = dplot1b$mu, base_rate_count, base_rate_pertrial,  base_rate_perperson)
    
      attach(chidata)
      ## Chi- Square Test of 8x2 table: obeserved mean frequencies vs. base rates averages
      tbl <- as.table(cbind(means, base_rate_perperson))
      dimnames(tbl) <- list(patterns = patterns,
                          source = c("observed","chance"))
      
        
      # does average pp differ from average base rate profile?
      Xsq <- chisq.test(tbl) # As the p-value is greater than the .05 significance level, 
                      #we do not reject the null hypothesis that the pattern frequency is independent of the emp_mean/base_rate, 
                      # suggesting they are similar? which means empirical is not different from chance?
                      # if patterns (rows) depended on it, we would see a difference? 
                      # it has to be significant
      
      Xsq$observed   # observed counts (same as M)
      Xsq$expected   # expected counts under the null
      Xsq$residuals  # Pearson residuals
      Xsq$stdres     # standardized residuals
      
      
      
       #------------------------ check if the base rate per trial is different from the average in the time plot (take average across all 30 trials per query and compare?)
  
  
      # take  mean_chosen_pertrial from wldd.R 
      tbl <- as.table(cbind(mean_chosen_pertrial, base_rate_pertrial))
      
      dimnames(tbl) <- list(patterns = patterns,
                            source = c("observed","chance"))
      
      
      Xsq <- chisq.test(tbl) # As the p-value is greater than the .05 significance level, 
      