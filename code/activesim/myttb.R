myttb<-function(x){
  y<-0
  for (i in seq_along(x)){
    y<-ifelse(y==0 & x[i]!=0,x[i],y)
  }
  y<-ifelse(y==0,sample(c(-1,1),1),y)
  return(y)
}