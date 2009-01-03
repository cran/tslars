\name{design}
\alias{design}
\title{Function to create a design matrix.}
\description{
Given the lag length and the forecast horizon, the design-function will
build the appropriate design matrix, which can then be used in a regression analysis.
}
\usage{
design(x = NULL, y, p, h, ary = FALSE)
}
\arguments{
  \item{x}{the matrix of predictors}
  \item{y}{the vector of the response}
  \item{p}{the lag length}
  \item{h}{the forecast horizon}
  \item{ary}{logical, in \code{ary=T}, the design matrix corresponds to that of an autoregressive model.}
}
\value{
The design-function returns an appropriate matrix that can be used to fit a time series regression model. 
It includes laggen values of the response y and of all predictors in x up to lag p.  
}
\author{Sarah Gelper}
\examples{

y <- rnorm(100)
x <- matrix(rnorm(300),ncol=3)
designmatrix <- design(x,y,p=2,h=1,ary=FALSE)

}
\keyword{regression}
\keyword{ts}
