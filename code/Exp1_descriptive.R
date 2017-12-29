##### Exp. 1 descriptives ############################################



# empties the environment completely 
rm(list=ls())

#You have to have them installed
packages <- c('plyr', 'ggplot2', 'jsonlite', 'MASS', 'MCMCpack', 'rjags', 'fields', 'httr')
lapply(packages, require, character.only = TRUE)

#Gets the data
myjson<-fromJSON("https://activettb.firebaseio.com/.json")

n<-length(myjson)

#This is the first participant
d<-myjson[[1]]


## Find mean age of sample (N=264) people 
age.all <- vector('numeric', n)
gender.all <- vector('numeric', n)
total <- vector('numeric', n)
for (i in 1:n){
    age.all[i] <- myjson[[i]]$age
    gender.all[i] <- myjson[[i]]$gender
    total[i] <- myjson[[i]]$totalscore
    ##totalscore is adding up the percent correct on each trial (e.g., 93% + 75% + 66% + .) and overall sum is percent correct across 10 trials. Can we average that?

}
table(gender.all)
# 0 = female 137 women
# 1 = male 127 men 
#mean(total)
scores <- (total)/10

t.test(scores, alternative = "greater", mu = 50)
range(scores)



#  <input id="age" type="radio" name="age" value="20" >18-25<br>
#   <input id="age" type="radio" name="age" value="30" >26-35<br>
#   <input id="age" type="radio" name="age" value="40" >36-50<br>
#   <input id="age" type="radio" name="age" value="50" >above 50<br>
  age.all[age.all ==20] <- mean(seq(18,25))
  age.all[age.all ==30] <- mean(seq(26,35))
  age.all[age.all ==40] <- mean(seq(36,50))
  age.all[age.all ==50] <- mean(seq(50,70))
  round(mean(age.all),1) # estimated mean over all groups

# get the mode of the age groups 
  names(table(age.all))[table(age.all)==max(table(age.all))]



###############################################
#Now, this is creating all comparisons and outputs

#Deconcatenate string
xstring<-function(x){
  y<-as.numeric(substr(x,1,1))
  for (i in 2:4){
    y<-cbind(y,as.numeric(substr(x,i,i)))
  }
  return(y)
}

# runs through all participants
for (i in 1:n){
  
d<-myjson[[i]]
#All comparisons: 30 learning trials
allcomps<-data.frame(xstring(d$alien1)-xstring(d$alien2),
                     xstring(d$alien1)-xstring(d$alien3),
                     xstring(d$alien1)-xstring(d$alien4),
                     xstring(d$alien2)-xstring(d$alien3),
                     xstring(d$alien2)-xstring(d$alien4),
                     xstring(d$alien3)-xstring(d$alien4))

#All possible comparisons as names
combinations<-c(12,13, 14 ,23, 24, 34)

#Name them 1 to 6
names(allcomps)<-rep(1:6, each=4)


#Check which one participant has chosen on each of the 30 trials
chosencomp<-t(apply(d$alienchosen,1, function(x){x[order(x)]}))+1
allcomps$chosen<-rep(NA,30)
for (l in 1:30){
  allcomps$chosen[l]<-which(paste0(chosencomp[,1],chosencomp[,2])[l]==paste(combinations))
}

#Outcome of comparison
allcomps$outcome<-ifelse(d$tracker1==chosencomp[,1],1,0)


}












## Boxplot of queries ########################################

## WLDD choices on average/ 30

## WLDD plots for Active TTB and Active Logistic separately, but Eric has already done that once so get those plots from him
# peoples choices plotted in same graph as the 


## figure out how often people chose query with maximum and least uncertainty 
## quantify each person and then make average statistics. 


