BlackscholesCalls <-
function(s0,k,t,r,vol){
  d1<-(log(s0/k)+(r+vol^2/2)*t)/(vol*t^(1/2))
  d2<-d1-vol*t^(1/2)
  callPrice<-s0*pnorm(d1)-k*exp(-r*t)*pnorm(d2)
  return(callPrice)
}
