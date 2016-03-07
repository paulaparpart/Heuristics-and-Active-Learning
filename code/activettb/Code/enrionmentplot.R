#################################################
#Stick breaking for compensatoriness
#Eric Schulz, November 2014
#################################################
packages <- c('RColorBrewer', 'ggplot2')
lapply(packages, require, character.only = TRUE)
#Stick breaking function
ericbreaksastick <- function(alpha) {
  betas <- rbeta(100, 1, alpha)
  remaining_stick_lengths <- c(1, cumprod(1 - betas))[1:100]
  weights <- remaining_stick_lengths * betas
  w<-sort(weights, decreasing=TRUE)[1:4]
  w<-w/sum(w)
return(w*10)
}

#Generating weights for different alphas
we1<-seq(0.000001,1,len=4)
we2<-sort(1/we1[1:3])
we<-c(we1,we2)
n<-1
x<-rep(0,4*length(we)); dim(x)<-c(4,length(we))
for (i in seq_along(we)){
test<-rep(we[i],100)
p<-lapply(test,ericbreaksastick)
x[,n]<-(apply(t(as.data.frame((p))),2,mean))
n<-n+1
}
plot(we)
#Collecting results
df<-data.frame(Weights=as.vector(as.matrix(x)), alpha=rep(we,each=4), N=rep(1:4,length(we)), mcol=rep(mypalette, each=4))

mypalette<-brewer.pal(7,"Greens")

#Plotting
pd <- position_dodge(0.001)
plot1<-ggplot(df, aes(x=N, y=Weights, group=alpha)) +
  geom_point(size=1.5, fill="white") +
  geom_line(position=pd)+ylim(c(0,10))+
  scale_color_manual(values=paste(mypalette))+
  ggtitle("Simulated Compensatoriness")+ylab(expression(w))+
  xlab(expression(W[n]))

df$Weights
plot1

#Saving private plot
pdf("comp.pdf")
plot1
dev.off()
