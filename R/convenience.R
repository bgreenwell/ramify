##' Matrix of Ones
##' 
##' Creates a matrix of all ones.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @return A matrixof dimension \code{nrow}-by-\code{ncol} filled with ones.
ones <- function(nrow, ncol = 1) {
  matrix(rep(1, nrow * ncol), nrow = nrow, ncol = ncol)
}

##' Matrix of Zeros
##' 
##' Creates a matrix of all zeros.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @return A matrixof dimension \code{nrow}-by-\code{ncol} filled with zeros.
zeros <- function(nrow, ncol = 1) {
  matrix(rep(0, nrow * ncol), nrow = nrow, ncol = ncol)
}

##' Flatten Matrices
##'
##' Flatten (i.e., collapse) a matrix to one dimension.
##' 
##' @param x A matrix object.
##' 
##' @return A numeric vector.
flatten <- function(x) {
  dim(x) <- NULL  # remove dimension attribute
  x
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

##' Matrix of Logical Values
##' 
##' Creates an \code{nrow}-by-\code{ncol} matrix of \code{TRUE}s.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @export
trues <- function(nrow = 1, ncol = 1) {
  matrix(rep(TRUE, times = nrow * ncol), nrow = nrow, ncol = ncol)
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
  matrix(runif(mrow * ncol, ...), nrow = nrow, ncol = ncol)
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

##' Identity Matrix
##' 
##' Creates an \code{nrow}-by-\code{ncol} identity matrix.
##' 
##' @param nrow The desired number of rows.
##' @param ncol The desired number of columns.
##' 
##' @export
eye <- function(nrow = 1, ncol = nrow) {
  if (ncol == nrow) {
    diag(nrow)
  } else if (ncol < nrow) {
    diag(nrow)[, seq_len(ncol)]
  } else {
    cbind(diag(nrow), 0 * diag(ncol - nrow))
  }
}

##' linearly-spaced Elements
##' 
##' Construct a vector of \code{n} linearly-spaced elements from \code{.start} 
##' to \code{.stop}. 
##' 
##' @param .start
##' @param .stop
##' @param n
##' 
##' @export
linspace <- function(.start, .stop, n = 100) {
  seq(from = .start, to = .stop, length.out = n)
}

##' Logarithmically-spaced Elements
##' 
##' Construct a vector of \code{n} logarithmically-spaced elements from 
##' 10^\code{.start} to 10^\code{.stop}. 
##' 
##' @param .start
##' @param .stop
##' @param n
##' 
##' @export
logspace <- function(.start, .stop, n = 100) {
  if (.stop == pi)  {
    .stop <- log10(.stop)
  }
  10 ^ seq(from = .start, to = .stop, length.out = n)
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