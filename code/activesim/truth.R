truth<-function(x){
  y<-1/(1+exp(-(6*x[1]+3*x[2]+1.5*x[3]+0.75*x[4])))
  return(rbinom(1,1,y))
}