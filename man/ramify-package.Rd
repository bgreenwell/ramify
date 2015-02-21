\name{ramify-package}
\alias{ramify-package}
\alias{ramify}
\docType{package}
\title{
Additional Matrix Functionality
}
\description{
Additional matrix functionality for R. Includes a new wrapper for the built-in matrix function that extends its functionality by allowing matrices to be 
created from character strings and lists. A number of convenience functions have also been added for users more familiar with MATLAB or Julia.
}
\details{
\tabular{ll}{
Package: \tab ramify\cr
Type: \tab Package\cr
Version: \tab 0.2\cr
Date: \tab 2015-02-21\cr
License: \tab GPL (>= 2)\cr
}
The main functions in this package are:
\itemize{
  \item \code{mat} --- matrix wrapper function
  \item \code{ones} --- creates a matrix of all ones
  \item \code{zeros} --- creates a matrix of all zeros
  \item \code{flatten} --- flatten (i.e., collapse) a matrix to one dimension
  \item \code{resize} --- resize a given matrix
}
}
\author{
Brandon M. Greenwell

Brandon M. Greenwell <greenwell.brandon@gmail.com>
}

\keyword{ matrix }

\examples{
## Easily create matrices from character vectors
mat('1, 2, 3; 4, 5, 6')
mat('1, 2, 3; 4, 5, 6', rows = TRUE)
(m <- mat(paste('exp(', 1:9, ')')))
resize(m, nrow = 3)
resize(m, nrow = 3, byrow = FALSE)
matrix(exp(1:9), 3, 3)

## Easily create matrices from a list
mat(list(1:3, 4:6, 7:9))
mat(list(1:3, 4:6, 7:9), rows = TRUE)

## Convenience funcions
zeros(2, 2)
ones(2, 2)
flatten(zeros(4, 2))
}


