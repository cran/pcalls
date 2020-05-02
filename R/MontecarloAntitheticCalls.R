MontecarloAntitheticCalls <-
function(s0,k,t,r,vol,n){
  epsilon<-rnorm(n)
  s1<-s0*exp((r-0.5*vol^2)*t+vol*sqrt(t)*epsilon)
  s2<-s0*exp((r-0.5*vol^2)*t+vol*sqrt(t)*(-epsilon))
  payoff1<-pmax(s1-k,0)     
  payoff2<-pmax(s2-k,0)
  dPayoff<-exp(-r*t)*0.5*(payoff1+payoff2) 
  priceCall<-mean(dPayoff)
  return(priceCall)
}
