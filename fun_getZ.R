getZ<- function(n, type){
 firstMatrix = diag(n)
 secondMatrix = diag(n)
 outMatrix= rbind(firstMatrix, secondMatrix)
 if (type == "identity"){
   return(outMatrix)
 } else if (type == "diagonal"){
   firstList = as.list(firstMatrix)
   secondList = as.list(secondMatrix)
   secondList[secondList==1]= "coeff"
   outList = c(firstList, secondList)
   outMatrix = matrix(outList, n, 2*n)
   return(outMatrix)
 }else{
   print("Error: Unsupported type")
   return(NA)
 }
}