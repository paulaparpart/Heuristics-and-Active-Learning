mylogistic<-function(p,x){
  mysum<-p[,1]*x[,1]+p[,2]*x[,2]+p[,3]*x[,3]+p[,4]*x[,4]
  y<-exp(mysum)/(1+exp(mysum))
  return(y)
}