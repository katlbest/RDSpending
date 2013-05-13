getZ<- function(n, type){
 firstMatrix = diag(n)
 secondMatrix = diag(n)
 outMatrix= rbind(firstMatrix, secondMatrix)
 if (type == "identity"){
   return(outMatrix)
 } else if (type == "diagonal"){
   outMatrix[outMatrix ==1]= "z"
   outMatrix[outMatrix == "0"]== as.numeric(outMatrix[outMatrix == "0"])
   return(outMatrix)
 }else{
   print("Error: Unsupported type")
   return(NA)
 }
}