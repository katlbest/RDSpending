getZColSmall<- function(n){
    outMatrix = matrix(ncol = 1, nrow = n)
    nameList = rep("z", n)
    nameList = paste(nameList, c(1:n), sep = "")
    outMatrix[,1]=nameList
    return(outMatrix)
  }