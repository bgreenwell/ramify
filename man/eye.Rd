% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convenience.R
\name{eye}
\alias{eye}
\title{Identity Matrix}
\usage{
eye(nrow = 1, ncol = nrow)
}
\arguments{
\item{nrow}{The desired number of rows.}

\item{ncol}{The desired number of columns.}
}
\value{
A \code{nrow}-by-\code{ncol} identity matrix.
}
\description{
Creates an \code{nrow}-by-\code{ncol} identity matrix.
}
\examples{
eye(4)  # 4-by-4 identity matrix
eye(4, 4)  # 4-by-4 identity matrix
eye(3, 5)  # 3-by-5 identity matrix
eye(5, 3)  # 5-by-3 identity matrix
}
\seealso{
\code{\link{diag}}.
}

