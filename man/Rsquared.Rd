\name{Rsquared}
\alias{Rsquared}
\title{Compute $R^2$}
\description{
Function that returns an Rsquared of a linear OLS regression without intercept.}
\usage{
Rsquared(x, y)
}
\arguments{
  \item{x}{the matrix of predictors}
  \item{y}{the vector of the response}
}
\value{
The $R^2$ of regression y on x.}
\author{Sarah Gelper}
\examples{

y <- rnorm(100)
x <- matrix(rnorm(300),ncol=3)
Rsq <- Rsquared(x,y)
}
\keyword{regression}
