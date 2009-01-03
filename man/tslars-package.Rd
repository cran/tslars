\name{tslars-package}
\alias{tslars-package}
\docType{package}
\title{The tslars package performs variable selection for high-dimensional linear time series models.}
\description{
The tslars packages applies a dynamic variable selection procedure. It is an extension of the LARS 
algorithm of Efron et al (2004) which is designed for time series analysis.  It provides a ranking of the 
predictors and a selection of which predictors to include in the final model as well as a selection of the 
appropriate lag length.  
}
\details{
\tabular{ll}{
Package: \tab tslars\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-02-06\cr
License: \tab gpl\cr
LazyLoad: \tab yes\cr
}
The most improtant functions are \code{tslars} and \code{tslars.p}.
}
\author{
Sarah Gelper
Maintainer: Sarah Gelper <gelper@ese.eur.nl>
}
\references{
Gelper, S. and Croux, C. (2009) Time series least angle regression for selecting  predictive economic sentiment series.
www.econ.kuleuven.be/sarah.gelper/public
}
