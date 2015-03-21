## Solve a System of Equations
## 
## Binary operator that solves the equation A %*% x = b for x, where b can be 
## either a vector or a matrix.
## 
## @param A A square numeric or complex matrix containing the coefficients of 
##          the linear system. Logical matrices are coerced to numeric.
## @param b A numeric or complex vector or matrix giving the right-hand side(s) 
##          of the linear system. If missing, \code{b} is taken to be an 
##          identity matrix and solve will return the inverse of \code{A}.
## @export
## @examples
## A <- matrix(rnorm(4), 2, 2)
## b <- c(1, 2)
## A %\% b  
##`%\\%` <- function(A, b) {
##  solve(A, b)
##}


##' Identity Matrix
##' 
##' Creates an \code{nrow}-by-\code{ncol} identity matrix.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @export
eye <- function(nrow = 1, ncol = nrow) {
  diag(1, nrow, ncol)
}


##' Matrix of Logical Values
##' 
##' Creates an \code{nrow}-by-\code{ncol} matrix of \code{FALSE}s.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @export
falses <- function(nrow = 1, ncol = 1) {
  matrix(rep(FALSE, times = nrow * ncol), nrow = nrow, ncol = ncol)
}


##' Fill a Matrix
##'
##' Create a matrix filled with the value \code{x}.
##' 
##' @param x The (single) value to fill the matrix with.
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @export
fill <- function(x, nrow = 1, ncol = 1) {
  matrix(rep(x, times = nrow * ncol), nrow = nrow, ncol = ncol)
}


##' Flatten Matrices
##'
##' Flatten (i.e., collapse) a matrix to one dimension.
##' 
##' @param x A matrix object.
##' @param across Character string specifying whether to flatten the matrix 
##'   across \code{"rows"} (default) or \code{"columns"}.
##' 
##' @return A numeric vector.
##' 
##' @export
flatten <- function(x, across = c("rows", "columns")) {
  ## FIXME: Add across option?
  across <- match.arg(across)
  if (across == "rows") x <- t(x)
  dim(x) <- NULL  # remove dimension attribute
  x
}


##' Matrix Inverse
##' 
##' Calculates the inverse of a square matrix.
##' 
##' @param x A square numeric or complex matrix
##' @param ... Additional optional arguments.
##' 
##' @export
inv <- function(x, ...) {
  if (!is.matrix(x)) {
    stop('Argument should be a matrix.', call. = FALSE)
  }
  if (dim(x)[1L] != dim(x)[2L]) {
    stop('Argument should be a square matrix.', call. = FALSE)
  }
  b <- diag(1, nrow(x))
  colnames(b) <- rownames(x)
  solve(x, b, ...)
}


##' Concatenate Matrices
##' 
##' Concatenate along the second (i.e., column) dimension.
##' 
##' @param ... Arguments to be formed into a list.
##' 
##' @export
hcat <- function(...) {
  do.call(cbind, list(...))
}


##' linearly-spaced Elements
##' 
##' Construct a vector of \code{n} linearly-spaced elements from \code{a} 
##' to \code{b}. 
##' 
##' @param a The starting value of the sequence.
##' @param b The final value of the sequence.
##' @param n The number of samples to generate. Default is 50.
##' 
##' @export
linspace <- function(a, b, n = 50) {
  seq(from = a, to = b, length.out = n)
}


##' Logarithmically-spaced Elements
##' 
##' Construct a vector of \code{n} logarithmically-spaced elements from 
##' 10^\code{a} to 10^\code{b}. 
##' 
##' @param a \code{base^a} is the starting value of the sequence.
##' @param b \code{base^b} is the final value of the sequence.
##' @param n The number of samples to generate. Default is 50.
##' @param base The base of the log space.
##' 
##' @note
##' If \code{b = pi} and \code{base = 10}, the points are between 
##' \code{10^a} and \code{pi}, not \code{10^a} and \code{10^pi}, for 
##' compatibility with the corresponding MATLAB/Octave, and NumPy functions.
##' 
##' @export
logspace <- function(a, b, n = 50, base = 10) {
  if (b == pi && base == 10)  {
    b <- log(b, base = base)
  }
  base ^ seq(from = a, to = b, length.out = n)
}


##' Matrix of Ones
##' 
##' Creates a matrix of all ones.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @return A matrixof dimension \code{nrow}-by-\code{ncol} filled with ones.
##' 
##' @export
ones <- function(nrow, ncol = 1) {
  matrix(rep(1, nrow * ncol), nrow = nrow, ncol = ncol)
}


##' Uniform Distributed Random Numbers
##' 
##' Creates an \code{nrow}-by-\code{ncol} matrix of uniform distributed random
##' numbers.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' @param ... Additional optional arguments to be passed on to \code{matrix}.
##' 
##' @export
rand <- function(nrow = 1, ncol = 1, ...) {
  matrix(runif(nrow * ncol, ...), nrow = nrow, ncol = ncol)
}


##' Normally Distributed Random Numbers
##' 
##' Creates an \code{nrow}-by-\code{ncol} matrix of normally distributed random
##' numbers.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' @param ... Additional optional arguments to be passed on to \code{matrix}.
##' 
##' @export
randn <- function(nrow = 1, ncol = 1, ...) {
  matrix(rnorm(nrow * ncol, ...), nrow = nrow, ncol = ncol)
}


##' Resize Matrix
##' 
##' Returns a new matrix of dimension nrow by ncol using the elements of x.
##' 
##' @param x A \code{"matrix"} or \code{R} object.
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' @param byrow Logical. If FALSE (the default) the matrix is filled by 
##'              columns, otherwise the matrix is filled by rows.
##' @return A matrix of dimension \code{nrow}-by-\code{ncol}.
##' 
##' @export
resize <- function(x, nrow, ncol, byrow = TRUE) {
  
  ## FIXME: Should resize return x by default.
  
  ## Make sure x is a matrix. If it's not, try converting it.
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  
  ## Check dimensions (if supplied)
  if (missing(nrow) && missing(ncol)) {
    nrow <- dim(x)[1]
    ncol <- dim(x)[2]
  }
  if (missing(nrow) && !missing(ncol)) {
    if (length(x) %% ncol != 0) {
      stop("dimension mismatch.", call. = FALSE)
    }
    nrow <- length(x) / ncol
  }
  if (!missing(nrow) && missing(ncol)) {
    if (length(x) %% nrow != 0) {
      stop("dimension mismatch.", call. = FALSE)
    }
    ncol <- length(x) / nrow
  }
  if (nrow * ncol != length(x)) {
    stop("dimension mismatch.", call. = FALSE)
  }
  if (!inherits(x, "matrix")) {
    stop("x must be of class 'matrix'.", call. = FALSE)
  }
  
  ## Return matrix with new dimensions
  if (byrow) {
    attr(x, "dim") <- c(ncol, nrow) 
    t(x)
  } else {
    attr(x, "dim") <- c(nrow, ncol) 
    x
  }
  
}


##' Dimensions of a Matrix
##' 
##' Retrieve the dimensions of a matrix.
##' 
##' @param x A matrix or data frame.
##' 
##' @export
size <- function(x) {
  dim(x)
}


#' Matrix of Logical Values
#' 
#' Creates an \code{nrow}-by-\code{ncol} matrix of \code{TRUE}s.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' 
#' @export
trues <- function(nrow = 1, ncol = 1) {
  matrix(rep(TRUE, times = nrow * ncol), nrow = nrow, ncol = ncol)
}


##' Concatenate Matrices
##' 
##' Concatenate along the first (i.e., row) dimension.
##' 
##' @param ... Arguments to be formed into a list.
##' 
##' @export
vcat <- function(...) {
  do.call(rbind, list(...))
}


##' Matrix of Zeros
##' 
##' Creates a matrix of all zeros.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @return A matrixof dimension \code{nrow}-by-\code{ncol} filled with zeros.
##' 
##' @export
zeros <- function(nrow, ncol = 1) {
  matrix(rep(0, nrow * ncol), nrow = nrow, ncol = ncol)
}
