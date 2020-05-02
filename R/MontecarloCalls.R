MontecarloCalls <-
function(s0,k,t,r,vol,n){
  s<-s0*exp((r-0.5*vol^2)*t+vol*sqrt(t)*rnorm(n))
  dPayoff<-exp(-r*t)*(ifelse(s>k,s-k,0))
  priceCall<-mean(dPayoff)
  return(priceCall)
}
