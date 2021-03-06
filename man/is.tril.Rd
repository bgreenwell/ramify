% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience.R
\name{is.tril}
\alias{is.tril}
\title{Lower Triangular Matrix Test}
\usage{
is.tril(x)
}
\arguments{
\item{x}{A matrix}
}
\value{
Logical indicating whether the given matrix is lower triangular.
}
\description{
Determine if a Matrix is Lower Triangular
}
\examples{
m <- mat("1, 0, 0, 0; -1, 1, 0, 0; -2, -2, 1, 0; -3, -3, -3, 1")
is.tril(m)
is.tril(eye(3, 5))
}

