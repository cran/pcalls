AmericanDigitalCalls <-
function(s0,k,t,r,vol,call_type){
  if (call_type=="C"){
    d1<-(log(s0/k)+(r+vol^2/2)*t)/(vol*t^(1/2))
    d2<-d1-vol*t^(1/2)
    priceCallCash<-exp(-r*t)*pnorm(d2)
    return(priceCallCash)
  } 
  else if (call_type=="A") {
    d1<-(log(s0/k)+(r+vol^2/2)*t)/(vol*t^(1/2))
    d2<-d1-vol*t^(1/2)
    priceCallAsset<-s0*exp(-r*t)*pnorm(d1)
    return(priceCallAsset)
  }
  else { 
    stop("Select A or C as call type")
  }
}
