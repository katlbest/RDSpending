getZCol<- function(n, type){
  if (type == "unequal"){
    outMatrix = matrix(ncol = 1, nrow = 2*n)
    nameList = rep("z", 2*n)
    nameList = paste(nameList, c(1:(2*n)), sep = "")
    outMatrix[,1]=nameList
    return(outMatrix)
  }else if (type == "equal"){
    outMatrix = matrix(ncol = 1, nrow = 2*n)
    nameList = rep ("z1", n)
    nameList = c(nameList, rep("z2",n))
    outMatrix[,1]=nameList
    return(outMatrix)
  }

 }