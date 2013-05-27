getR2<- function(n, type){
  outMatrix = diag(2*n)
  firstMatrix = outMatrix[1:n,]
  secondMatrix = outMatrix[(n+1):(2*n),]
  firstList = as.list(t(firstMatrix))
  secondList = as.list(t(secondMatrix))
  firstList[firstList==1]= "r1"
  secondList[secondList==1]= 0
  firstMatrix2 = matrix(firstList,2*n,n)
  secondMatrix2 =matrix(secondList,2* n,n)
  outMatrix = cbind(firstMatrix2, secondMatrix2)
  return(outMatrix)
 }