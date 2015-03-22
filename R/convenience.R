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


#' Identity Matrix
#' 
#' Creates an \code{nrow}-by-\code{ncol} identity matrix.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' 
#' @return A \code{nrow}-by-\code{ncol} identity matrix.
#' 
#' @seealso \code{\link{diag}}.
#' @export
#' 
#' @examples
#' eye(4)  # 4-by-4 identity matrix
#' eye(4, 4)  # 4-by-4 identity matrix
#' eye(3, 5)  # 3-by-5 identity matrix
#' eye(5, 3)  # 5-by-3 identity matrix
eye <- function(nrow = 1, ncol = nrow) {
  diag(1, nrow, ncol)
}


#' Matrix of Logical Values
#' 
#' Creates an \code{nrow}-by-\code{ncol} matrix of logical values.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' 
#' @return An \code{nrow}-by-\code{ncol} matrix of logical values.
#' 
#' @export
#' @seealso \code{\link{fill}}, \code{\link{ones}}, \code{\link{zeros}}.
#' 
#' @examples
#' falses(3)  # column vector of FALSEs
#' fill(FALSE, 3)
#' resize(as.logical(zeros(3)), 3)
#' trues(2, 3)  # 2-by-3 matrix of TRUEs
#' fill(TRUE, 2, 3)
#' resize(as.logical(ones(2, 3)), 2, 3)
falses <- function(nrow = 1, ncol = 1) {
  matrix(rep(FALSE, times = nrow * ncol), nrow = nrow, ncol = ncol)
}


#' @rdname falses
#' @export
trues <- function(nrow = 1, ncol = 1) {
  matrix(rep(TRUE, times = nrow * ncol), nrow = nrow, ncol = ncol)
}


#' Fill a Matrix
#'
#' Create a matrix filled with the value \code{x}.
#' 
#' @param x The (single) value to fill the matrix with.
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' 
#' @export
#' @seealso \code{\link{ones}}, \code{\link{zeros}}, \code{\link{falses}}, \code{\link{trues}}, \code{\link{mat}}, \code{\link{matrix}}.
#' 
#' @examples
#' fill(pi, 3, 5)
#' mat(pi, 3, 5)  # same as 'matrix(pi, 3, 5)'
#' pi * ones(3, 5)
fill <- function(x, nrow = 1, ncol = 1) {
  matrix(rep(x, times = nrow * ncol), nrow = nrow, ncol = ncol)
}


#' Flatten Matrices
#'
#' Flatten (i.e., collapse) a matrix to one dimension.
#' 
#' @param x A matrix object.
#' @param across Character string specifying whether to flatten the matrix 
#'   across \code{"rows"} (default) or \code{"columns"}.
#' 
#' @return A numeric vector.
#' 
#' @export
#' @seealso \code{\link{mat}}.
#' 
#' @examples
#' m <- mat("2, 4, 6, 8; 10, 12, 14, 16")
#' flatten(m)
#' flatten(m, across = "columns")
flatten <- function(x, across = c("rows", "columns")) {
  ## FIXME: Add across option?
  across <- match.arg(across)
  if (across == "rows") x <- t(x)
  dim(x) <- NULL  # remove dimension attribute
  x
}


#' Matrix Inverse
#' 
#' Calculates the inverse of a square matrix.
#' 
#' @param x A square numeric or complex matrix
#' @param ... Additional optional arguments.
#' 
#' @export
#' @seealso \code{\link{solve}}.
#' 
#' @examples
#' m <- 3 * eye(5)
#' inv(m)
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


#' Concatenate Matrices
#' 
#' Concatenate matrices along the first or second dimension.
#' 
#' @param ... Arguments to be formed into a list.
#' 
#' @export
#' @seealso \code{\link{bmat}}, \code{\link{cbind}}, \code{\link{rbind}}.
#' 
#' @examples
#' m1 <- mat("1, 2, 3; 4, 5, 6")
#' m2 <- mat("7, 8, 9; 10, 11, 12")
#' hcat(m1, m2)  # same as 'bmat("m1, m2")'
#' vcat(m1, m2)  # same as 'bmat("m1; m2")'
hcat <- function(...) {
  do.call(cbind, list(...))
}

#' @rdname hcat
#' @export
vcat <- function(...) {
  do.call(rbind, list(...))
}


#' linearly-spaced Elements
#' 
#' Construct a vector of \code{n} linearly-spaced elements from \code{a} 
#' to \code{b}. 
#' 
#' @param a The starting value of the sequence.
#' @param b The final value of the sequence.
#' @param n The number of samples to generate. Default is 50.
#' 
#' @return A vector of linearly-spaced elements.
#' 
#' @export
#' @seealso \code{\link{logspace}}, \code{\link{seq}}.
#' 
#' @examples
#' linspace(0, 1)
#' linspace(1, 5, 5)
#' linspace(1+2i, 10+10i, 8)
#' logspace(0, pi, 10)
linspace <- function(a, b, n = 50) {
  seq(from = a, to = b, length.out = n)
}


#' Logarithmically-spaced Elements
#' 
#' Construct a vector of \code{n} logarithmically-spaced elements from 
#' 10^\code{a} to 10^\code{b}. 
#' 
#' @param a \code{base^a} is the starting value of the sequence.
#' @param b \code{base^b} is the final value of the sequence.
#' @param n The number of samples to generate. Default is 50.
#' @param base The base of the log space.
#' 
#' @note
#' If \code{b = pi} and \code{base = 10}, the points are between 
#' \code{10^a} and \code{pi}, not \code{10^a} and \code{10^pi}, for 
#' compatibility with the corresponding MATLAB/Octave, and NumPy functions.
#' 
#' @return A vector of logarithmically-spaced elements.
#' 
#' @seealso \code{\link{linspace}}, \code{\link{seq}}.
#' 
#' @export
logspace <- function(a, b, n = 50, base = 10) {
  if (b == pi && base == 10)  {
    b <- log(b, base = base)
  }
  base ^ seq(from = a, to = b, length.out = n)
}


#' Matrix of Ones or Zeros
#' 
#' Construct a matrix of all ones or zeros.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' 
#' @return An \code{nrow}-by-\code{ncol} matrix of ones or zeros.
#' 
#' @export
#' @seealso \code{\link{fill}}, \code{\link{falses}}, \code{\link{trues}}.
#' 
#' @examples 
#' ones(3)  # column matrix of ones
#' fill(1, 3)
#' zeros(3, 5)  # 3-by-5 matrix of zeros
#' fill(0, 3, 5)
ones <- function(nrow, ncol = 1) {
  matrix(rep(1, nrow * ncol), nrow = nrow, ncol = ncol)
}


#' @rdname ones
#' @export
zeros <- function(nrow, ncol = 1) {
  matrix(rep(0, nrow * ncol), nrow = nrow, ncol = ncol)
}


#' Matrix of Random Numbers
#' 
#' Construct a matrix of random deviates.
#' 
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param ... Additional optional arguments to be passed on to \code{runif} or
#'            \code{rnorm}.
#' 
#' @return An \code{nrow}-by-\code{ncol} matrix of pseudorandom numbers.
#' 
#' @details \code{rand} generates a matrix of uniform random deviates while
#'   \code{randn} generates a matrix of normal random deviates.
#' 
#' @export
#' @seealso \code{\link{runif}}, \code{\link{rnorm}}.
#' 
#' @examples
#' rand(2, 3)  # 2-by-3 matrix of uniform random numbers
#' rand(2, 3, min = 100, max = 200)  
#' randn(2, 3)  # 2-by-3 matrix of standard normal random variates
#' randn(2, 3, mean = 10, sd = 0.1)
rand <- function(nrow = 1, ncol = 1, ...) {
  matrix(runif(nrow * ncol, ...), nrow = nrow, ncol = ncol)
}


#' @rdname rand
#' @export
randn <- function(nrow = 1, ncol = 1, ...) {
  matrix(rnorm(nrow * ncol, ...), nrow = nrow, ncol = ncol)
}


#' Resize Matrix
#' 
#' Change shape and size of a matrix.
#' 
#' @param x A \code{"matrix"} or \code{R} object.
#' @param nrow The desired number of rows.
#' @param ncol The desired number of columns.
#' @param across Character string specifying whether to flatten the matrix 
#'               across \code{"rows"} (default) or \code{"columns"}.
#' @param byrow Logical. If FALSE (default) the new matrix is filled by columns, 
#'                       otherwise it is filled by rows.
#'              
#' @return A matrix of dimension \code{nrow}-by-\code{ncol}.
#' 
#' @export
#' @seealso \code{\link{flatten}}, \code{\link{mat}}, \code{\link{matrix}}.
#' 
#' @examples
#' m <- 1:9
#' resize(m)
#' resize(m, 3, 3)
#' resize(m, 2, 2)
resize <- function(x, nrow, ncol, across = c("rows", "columns"), 
                   byrow = FALSE) {
  
  ## Make sure x is a matrix. If it's not, try converting it.
  if (!is.matrix(x)) x <- as.matrix(x)
  if (missing(nrow)) nrow <- dim(x)[1L]  # keep first dimension
  if (missing(ncol)) ncol <- dim(x)[2L]  # keep second dimension
  
  # Flatten and reshape/resize matrix.
  across <- match.arg(across)
  matrix(flatten(x, across = across), nrow = nrow, ncol = ncol, byrow = byrow)
  
}


#' Dimensions of a Matrix
#' 
#' Retrieve the dimensions of a matrix.
#' 
#' @param x A matrix or data frame.
#' 
#' @return The dimensions of a matrix.
#' 
#' @export
#' @seealso \code{\link{dim}}.
#'
#' m <- mat("1, 3, 5; 7, 9, 11")
#' size(m)
size <- function(x) {
  dim(x)
}
