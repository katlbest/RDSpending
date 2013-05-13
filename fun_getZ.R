getZ<- function(n, type){
 firstMatrix = diag(n)
 secondMatrix = diag(n)
 outMatrix= rbind(firstMatrix, secondMatrix)
 if (type == "identity"){
   return(outMatrix)
 } else if (type == "diagonal"){
   firstList = as.list(t(firstMatrix))
   secondList = as.list(t(secondMatrix))
   firstList[firstList==1]= "z1"
   secondList[secondList==1]= "z2"
   firstMatrix2 = matrix(firstList,n,n)
   secondMatrix2 =matrix(secondList,n,n)
   outList = c(firstList, secondList)
   outMatrix = rbind(firstMatrix2, secondMatrix2)
   return(outMatrix)
 }else{
   print("Error: Unsupported type")
   return(NA)
 }
}