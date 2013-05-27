getBwInd<- function(n){
    outMatrix = diag(n+1)
    outMatrix[outMatrix ==1]= "b_diag"
    lastString = rep("b_off1", n)
    lastString2 = rep("b_off2", n)
    outMatrix[n+1,1:n]= lastString
    outMatrix[1:n,n+1]= lastString2
    return(outMatrix)
    
}