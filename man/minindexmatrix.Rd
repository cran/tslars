\name{minindexmatrix}
\alias{minindexmatrix}
\title{Function that returns the row and column where the matrix reaches its minimum}
\description{
To find the row and column where a matrix reaches its minimum.
}
\usage{
minindexmatrix(x)
}
\arguments{
  \item{x}{A matrix}
}
\value{
A vector of length two is returned, of which the first (second) element indicates at 
which row (column) the matrix \code{x} reaches its minimum.
}
\author{Sarah Gelper}
\seealso{which.min}
\examples{
a <- matrix(rnorm(20),ncol=4)
m <- minindexmatrix(a)
}

