\name{predict.cclust}
\title{Assign clusters to new data}
\usage{
predict.cclust(clobj, x)
}
\alias{predict.cclust}
\arguments{
 \item{clobj}{Object returned by a clustering algorithm such as
   \code{\link{cclust}}}
 \item{x}{Data matrix}
}
\description{
  Assigns each data point (row in \code{x}) the cluster corresponding to
  the closest center found in \code{clobj}.
}
\value{
  \code{predict.cclust} returns an object of class "cclust".
  Only \code{size} is changed as compared to the argument
  \code{clobj}. 
  \item{cluster}{Vector containing the indices of the clusters where
    the data is mapped.}
  \item{size}{The number of data points in each cluster.}
}
\author{Friedrich Leisch and Andreas Weingessel}
\seealso{\code{\link{cclust}}, \code{\link{plot.cclust}}} 
}
\examples{
# a 2-dimensional example
x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
cl<-cclust(x,2,20,verbose=TRUE,method="kmeans")
plot(cl,x)   

# a 3-dimensional example
x<-rbind(matrix(rnorm(150,sd=0.3),ncol=3),
         matrix(rnorm(150,mean=1,sd=0.3),ncol=3),
         matrix(rnorm(150,mean=2,sd=0.3),ncol=3))
cl<-cclust(x,6,20,verbose=TRUE,method="kmeans")
plot(cl,x)

# assign classes to some new data
y<-rbind(matrix(rnorm(33,sd=0.3),ncol=3),
         matrix(rnorm(33,mean=1,sd=0.3),ncol=3),
         matrix(rnorm(3,mean=2,sd=0.3),ncol=3))
ycl<-predict(cl, y)
plot(cl,y)
}
\keyword{cclust}



