require(mva)
require(cclust)
clustindex <- function( clres, x, index="all" )
  {
###needed measures 
    ##withins <- withinss1(x, centers, cluster)
    ## varwithins <- varwithinss(x, clres$centers, clres$cluster)
    zgss <- gss(x,clres$size, clres$withins)
    ## zvargss <- vargss(x, clres$size,varwithins)
    zttw <- ttww(x, clres$size, clres$cluster)
    ## minmaxd <-maxmindist(clres$size,distdata)
    ## distdata <- countdist(x)
    
    
###indexes calculations
    index <- pmatch(index, c("calinski", "cindex", "db", "hartigan",
                          "ratkovsky", "scott", "marriot", "ball",
                          "trcovw", "tracew", "friedman",
                          "rubin","ssi","likelihood","all"))

    if (is.na(index)) 
      stop("invalid clustering index")
    if (index == -1) 
      stop("ambiguous index")

    vecallindex <- numeric(14)

    if ((index==1) || (index==15))
      vecallindex[1] <- calinski(zgss,clres$size)
    if ((index==2) || (index==15))
      {
        if (length(unique(x))==2)
          {
            distdata <- countdist(x)
            minmaxd <- maxmindist(clres$size,distdata)
            vecallindex[2] <- cindex(clres$withins, minmaxd, clres$size)
          }
        else  vecallindex[2] <- NA
      }
    if ((index==3) || (index==15))
      vecallindex[3] <- db(clres$withins, clres$centers, clres$cluster)
    if ((index==4) || (index==15))
      vecallindex[4] <- hartigan(zgss)
    if ((index==5) || (index==15))
      {
        varwithins <- varwithinss(x, clres$centers, clres$cluster)
        zvargss <- vargss(x, clres$size,varwithins)
        vecallindex[5] <-ratkowsky(zvargss, clres$size)
      }
    if ((index==6) || (index==15))
       vecallindex[6] <- scott(zttw, clres$size)
    if ((index==7) || (index==15))
       vecallindex[7] <- marriot(zttw,clres$size)
    if ((index==8) || (index==15))
       vecallindex[8] <- ball(clres$withins, clres$size)
    if ((index==9) || (index==15))
       vecallindex[9] <- tracecovw(zttw)
    if ((index==10) || (index==15))
       vecallindex[10] <- tracew(zttw)
    if ((index==11) || (index==15))
       vecallindex[11] <- friedman(zttw)
    if ((index==12) || (index==15))
       vecallindex[12] <- rubin(zttw)
    if ((index==13) || (index==15))
       vecallindex[13] <- ssi(clres$centers, clres$size)$ssiw 
    if ((index==14) || (index==15))
       vecallindex[14] <- likelihood(x,clres$centers, clres$cluster)
    
    names(vecallindex) <- c("calinski", "cindex", "db", "hartigan",
                              "ratkovsky", "scott", "marriot", "ball",
                              "trcovw", "tracew", "friedman",
                              "rubin","ssi","likelihood")

    if (index < 15)
      vecallindex <- vecallindex[index]

    return(vecallindex)
  }









