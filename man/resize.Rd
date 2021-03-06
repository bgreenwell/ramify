% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience.R
\name{resize}
\alias{resize}
\title{Resize Matrices and Arrays}
\usage{
resize(x, nrow, ncol, ..., across = c("rows", "columns"), byrow = FALSE)
}
\arguments{
\item{x}{A matrix or multi-way array.}

\item{nrow}{The desired number of rows.}

\item{ncol}{The desired number of columns.}

\item{...}{Further dimensions of the array.}

\item{across}{Character string specifying whether to flatten the matrix 
across \code{"rows"} (default) or \code{"columns"}. This option
is ignored for multi-way arrays.}

\item{byrow}{Logical. If FALSE (default) the new matrix is filled by columns, 
otherwise it is filled by rows. This option is ignored
for multi-way arrays.}
}
\value{
A matrix with dimension \code{nrow}-by-\code{ncol}.
}
\description{
Change shape and size of a matrix or array.
}
\examples{
m <- 1:9
resize(m)
resize(m, 3, 3)
resize(m, 2, 2)
}
\seealso{
\code{\link{flatten}}, \code{\link{mat}}, \code{\link{matrix}}.
}

