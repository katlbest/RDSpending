getQ<- function(n){
  outMatrix = diag(n)
  diagNames = rep("s", n)
  diagNames = paste(diagNames, c(1:n), sep = "")
  outMatrix[outMatrix == 1]= diagNames
  coeffMatrix = matrix(ncol=9, nrow = 9)
  #coeffMatrix[] = "sI"
  coeffMatrix[] = ""
  appendList = rep("*a", n)
  appendList = paste(appendList, c(1:n), sep = "")
  for (i in 1:n){
    for (j in 1:n){
      coeffMatrix[i,j]= paste(coeffMatrix[i,j], appendList[i], appendList[j], sep = "")
      coeffMatrix[i,j]= substring(coeffMatrix[i,j],2)
    }
  }
  outMatrix[outMatrix == 0]= coeffMatrix[outMatrix ==0]
  return(outMatrix)
 }

#additional code for testing
  #outMatrix = diag(n)
  #firstMatrix = outMatrix[1:4,]
  #secondMatrix = outMatrix[(5):(9),]
  #firstList = as.list(t(firstMatrix))
  #secondList = as.list(t(secondMatrix))
  #firstList[firstList==1]= "a1"
  #secondList[secondList==1]= "a1*a2"
  #firstMatrix2 = matrix(firstList,n,4)
  #secondMatrix2 =matrix(secondList,n,5)
  #outMatrix = cbind(firstMatrix2, secondMatrix2)
  #Q1 = outMatrix