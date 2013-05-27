getZwInd<- function(n){
    outMatrix = diag(n+1)
    outMatrix = outMatrix[1:n,]
    outMatrix[outMatrix==1]= "z1"
    outMatrix[,n+1]= "z2"
    return(outMatrix)
    
}