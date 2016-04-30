sortByField<-function(g, field){
  print(length(g[,1]))
  for(i in 1:length(g[,1])){
    maxInd <- 0
    for(j in i:length(g[,1])){
      if(((maxInd == 0) || (g[j,field]) > g[maxInd,field])){
        maxInd <- j
      }
    }
    g <- swap(g,i,maxInd,field)
  }
  return(g)
}

swap <- function(g,i,j,field){
  tempName <- g[i,"name"]
  tempBet <- g[i,field]
  g[i,"name"] <- g[j,"name"]
  g[i,field] <- g[j,field]
  g[j,"name"] <- tempName
  g[j,field] <- tempBet
  return(g)
}