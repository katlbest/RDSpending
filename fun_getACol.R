getACol<- function(n){
  outMatrix = matrix(ncol = 1, nrow = 2*n)
  nameList = rep ("a1", n)
  nameList = c(nameList, rep("a2",n))
  outMatrix[,1]=nameList
  return(outMatrix)
}