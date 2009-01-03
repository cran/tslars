\name{tslars}
\alias{tslars}
\alias{tslars.p}
\alias{coef.tslars}
\alias{predict.tslars}
\alias{print.summary.tslars}
\alias{print.tslars}
\alias{summary.tslars}
\title{Function to obtain the selected model accoring the the time series LARS algorithm}
\description{
The \code{tslars} function applies a dynamic variable selection procedure. It is an extension of the LARS 
algorithm of Efron et al (2004) which is designed for time series analysis.  It provides a ranking of the 
predictors and a selection of which predictors to include in the final model as well as a selection of the 
appropriate lag length.  
}
\usage{
tslars(formula, h = 1, p.max = 5, max.x = 10, nr.rank = NA)
}
\arguments{
  \item{formula}{a formula describing the model to be fitted}
  \item{h}{the forecast horizon, defaults to 1.}
  \item{p.max}{the maximal number of lags to allow, defaults to 5.}
  \item{max.x}{the maximal number of predictors to include in the final model, defaults to 10.}
  \item{nr.rank}{the number of predictors to be ranked.  This is especially interesting if the 
total number of predictors is really large.}
}
\value{
A \code{tslars}-object is returned, for which \code{print()}, \code{summary()}, \code{predict()} and \code{coef()} are available.  
An object of class "lm" is a list containing the following components:
  \item{active}{the active set, a vector giving the TS-LARS ordering of the predictors, '0' indicates lagged values of the response.}
  \item{fixedp}{indicates whether the lag length was prespecified (\code{TRUE}) or not (\code{FALSE}).}
  \item{laglength.opt}{if \code{fixedp} is \code{TRUE}, the prespecified lag length.  If \code{fixedp} is \code{FALSE}, the optimal lag length selected according to BIC.}
  \item{nrvar.opt}{the optimal number of predictors to include in the final model, according to the BIC.}
  \item{bic}{the BIC values for the nested models.}
  \item{h}{the forecast horizon used.}
  \item{call}{the matched call.}
  \item{response}{the response used.}
  \item{predictors}{the predictors used.}
}
\references{Gelper, S. and Croux, C. (2009) Time series least angle regression for selecting  predictive economic sentiment series. 
www.econ.kuleuven.be/sarah.gelper/public}
\author{Sarah Gelper}
\examples{

n <- 100
m <- 10 #m>5
x <- matrix(rnorm(n*m), ncol=m)
coefs <- c(rep(1,5),rep(0,m-5))
y <- c(rnorm(1),crossprod(t(x[1:(n-1),]),coefs) + rnorm(n-1))

mytslars <- tslars(y~x)
summary(mytslars)
# To obtain an h-step-ahead prediction of the response using the selected model fitted by OLS:
myprediction <- predict(mytslars) 


}
\keyword{regression}
\keyword{ts}
