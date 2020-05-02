BinaryTreeCalls <-
function(s0,k,r,vol,deltaT,nsteps){
  up<-exp(vol*sqrt(deltaT))
  down<-1/up
  p<-((exp(r*deltaT)-down)/(up-down))
  treeMatrix<-matrix(0,nrow=nsteps+1,ncol=nsteps+1)
  for(i in 1:(nsteps+1)){
    for(j in 1:i){
      treeMatrix[i,j]<-s0*up^(j-1)*down^((i-1)-(j-1))
    }
  }
  callValueTree<-matrix(0,nrow=nrow(treeMatrix),ncol=ncol(treeMatrix))
  callValueTree[nrow(callValueTree),]<-pmax(treeMatrix[nrow(treeMatrix),]-k,0)
  for(i in (nrow(treeMatrix)-1):1){
    for(j in 1:i){
      callValueTree[i,j]<-exp(-r*deltaT)*(p*callValueTree[i+1,j+1]+(1-p)*callValueTree[i+1,j])
    }
  }
  priceCall<-callValueTree[1,1]
  return(priceCall)
}
