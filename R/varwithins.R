varwithinss <- function(x, centers, cluster)
{
  nrow<-dim(centers)[1]
  nvar<-dim(x)[2]
  varwithins <- matrix(0, nrow,nvar)
  x <- (x - centers[cluster, ])^2
  for(l in 1:nvar){
    for(k in 1:nrow){
      varwithins[k,l] <- sum(x[cluster==k,l])
    }
  }
  return(varwithins)
}





