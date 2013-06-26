getR<- function(n, type){
  outMatrix = diag(numTot)
  firstMatrix = outMatrix[1:numCos,]
  if(numTot>numCos){
    secondMatrix = outMatrix[(numCos+1):(numTot),]
    secondList = as.list(t(secondMatrix))
    secondList[secondList==1]= "r2"
    secondMatrix2 =matrix(secondList,numTot,numTot-numCos)
    firstList = as.list(t(firstMatrix))
    firstList[firstList==1]= "r1"
    firstMatrix2 = matrix(firstList,numTot,numCos)
    outMatrix = cbind(firstMatrix2, secondMatrix2)
  }else{
    firstList = as.list(t(firstMatrix))
    firstList[firstList==1]= "r1"
    firstMatrix2 = matrix(firstList,numTot,numCos)
    outMatrix = firstMatrix2
  }
  return(outMatrix)
 }