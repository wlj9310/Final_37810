Gibbs_<-function(startvalue,iterations,burnIn,B)
{
  x<-c(startvalue[1],rep(NA,iterations-1))
  y<-c(startvalue[2],rep(NA,iterations-1))
  for(i in 1:(iterations-1))
  {
    x[i+1]<-(-log(1-runif(1)*(1-exp(-y[i]*B)))/y[i])
    y[i+1]<-(-log(1-runif(1)*(1-exp(-x[i+1]*B)))/x[i+1])
  }
  x<-x[-(1:burnIn)]
  y<-y[-(1:burnIn)]
  return(data.frame(x,y))
}